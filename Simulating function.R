library(readr)
require(dplyr)
library(caret)
library(MVN)
library(energy)
library(texreg)
library(kableExtra)
library(matrixStats)
library(posterior)
library(cmdstanr)

#Initialization
get_quadratic_coef <- function(data) {
  quadratic_coef <- data |>
    dplyr::mutate(
      cy = 60.5 - extension,
      # t0 is the time corresponding to vx0, vy0, vz0, x0, z0 (i.e. the time when y = 50)
      # Calculate time from y0 to release point, and then negate that time
      t0 = -(-vy0 - sqrt(vy0^2 - 4 * (ay / 2) * (50 - cy))) / (2 * (ay / 2)),
      # Backtrack velocities by t0 time
      bx = vx0 - t0 * ax,
      by = vy0 - t0 * ay,
      bz = vz0 - t0 * az,
      # Backtrack locations by t0 time
      cx = x0 - 0 * vx0 + 0^2 * ax / 2,
      cz = z0 - 0 * vz0 + 0^2 * az / 2
    ) |>
    dplyr::select(ax, ay, az, bx, by, bz, cx, cy, cz)
  return(quadratic_coef)
}
setwd("~/Downloads")
mydata<-read_csv("2017.csv")
pitch="FF"
fastballs<-filter(mydata,pitch_type==pitch & is.na(mydata$ay)!=1 & is.na(mydata$release_speed)!=1 & is.na(mydata$pfx_z)!=1 & is.na(mydata$pfx_x)!=1 & is.na(mydata$release_extension)!=1 & is.na(mydata$release_pos_z)!=1 & is.na(mydata$release_pos_x)!=1 & is.na(mydata$plate_x)!=1 & is.na(mydata$plate_z)!=1 & (mydata$release_speed)>=70)
fastballs<-dplyr::select(fastballs,c("game_year","pitcher","p_throws","stand","balls","strikes","ax","ay","az","vx0","vy0","vz0","x0"="release_pos_x","extension"="release_extension","z0"="release_pos_z","plate_x","plate_z","pfx_x","pfx_z","sz_top","sz_bot","home_team","batter","bat_score","fld_score","inning","inning_topbot","outs_when_up","on_1b","on_2b","on_3b"))
playerseasondata<-fastballs %>% group_by(pitcher,game_year) %>% summarize(count=n())                                                        
mergedmydata<-merge(fastballs,playerseasondata)
mergedmydata<-filter(mergedmydata,mergedmydata$count>=100,mergedmydata$pitcher%%1==0) #cut down to x% of data
scottdata<-get_quadratic_coef(mergedmydata)
mergedmydata[,7:15]<-scottdata
colnames(mergedmydata)[7:15]<-colnames(scottdata)
mergedmydata$ax<-mergedmydata$ax-2*mergedmydata$ax*as.numeric(mergedmydata$p_throws=="L")
mergedmydata$bx<-mergedmydata$bx-2*mergedmydata$bx*as.numeric(mergedmydata$p_throws=="L")
mergedmydata$cx<-mergedmydata$cx-2*mergedmydata$cx*as.numeric(mergedmydata$p_throws=="L")
mergedmydata$samehand<-as.numeric(mergedmydata$stand==mergedmydata$p_throws)
mergedmydata$bshnum<-mergedmydata$samehand*12+mergedmydata$balls*3+mergedmydata$strikes+1
meanssds=data.frame(Mean=colMeans(mergedmydata[,7:15]),Sd=colSds(as.matrix(mergedmydata[,7:15])))
for(i in 7:15){
  mergedmydata[,i]<-(mergedmydata[,i]-mean(mergedmydata[,i]))/sd(mergedmydata[,i])
}
meansztop=mean(mergedmydata$sz_top)
sdsztop=sd(mergedmydata$sz_top)
meanszbot=mean(mergedmydata$sz_bot)
sdszbot=sd(mergedmydata$sz_bot)
ballsmean=mean(mergedmydata$balls)
strikesmean=mean(mergedmydata$strikes)
samehandmean=mean(mergedmydata$samehand)
variablenames<-c("ax","ay","az","bx","by","bz","cx","cy","cz")
players<-unique(mergedmydata$pitcher)
players<-data.frame(pitcher=players,pitchernum=seq(1,length(players)))
mergedmydata<-merge(mergedmydata,players)
numplayers<-length(players$pitchernum)
players$RHP=0
for(i in 1:numplayers){
  playerdata<-filter(mergedmydata,pitcher==players$pitcher[i])
  hands=playerdata %>% group_by(p_throws) %>% summarize(count=n())
  players$RHP[i]=as.numeric("R"==hands$p_throws[which.max(hands$count)])
}

fit_optim<-readRDS(paste("step 8 model simple ",pitch,".RDS",sep=""))

#Get posterior parameters
post_mu = as_draws(fit_optim$draws('mu'))
post_sigma = as_draws(fit_optim$draws('sigma'))
post_pi = as_draws(fit_optim$draws('pivar'))
post_nu = as_draws(fit_optim$draws('nu'))
post_xi = as_draws(fit_optim$draws('xi'))
post_Rho = as_draws(fit_optim$draws('Rho'))
post_lambda = as_draws(fit_optim$draws('lambda'))
post_zeta = as_draws(fit_optim$draws('zeta'))
post_theta = as_draws(fit_optim$draws('theta'))
post_kappa = as_draws(fit_optim$draws('kappa'))
allRhos=array(post_Rho,dim=c(numplayers,9,9))

#Generate tables
playermeans=matrix(post_mu,nrow=numplayers,ncol=9)
playersigmas=matrix(post_sigma,nrow=numplayers,ncol=9)
playerhands=matrix(post_pi,nrow=numplayers,ncol=9)
playerballs=matrix(post_nu,nrow=numplayers,ncol=9)
playerstrikes=matrix(post_xi,nrow=numplayers,ncol=9)
playerposterior<-data.frame(playermeans,playersigmas,playerhands,playerballs,playerstrikes,pitchernum=seq(1,numplayers))
mergedposterior<-merge(mergedmydata,playerposterior)
playerposterior=merge(playerposterior,players,by="pitchernum")
countmeans=matrix(post_lambda,nrow=24,ncol=9)
countsds=matrix(post_zeta,nrow=24,ncol=9)
countposterior=data.frame(countmeans,countsds,bshnum=seq(1,24))
mergedposterior<-merge(mergedposterior,countposterior,by="bshnum")
sz_topvec<-(mergedposterior$sz_top-meansztop)/sdsztop
index=36

datameans=mergedposterior[,index:(index+8)]+mergedposterior[,(index+45):(index+53)]+
  mergedposterior[,(index+18):(index+26)]*(mergedposterior$samehand-mean(mergedposterior$samehand))+
  mergedposterior[,(index+27):(index+35)]*(mergedposterior$balls-mean(mergedposterior$balls))+
  mergedposterior[,(index+36):(index+44)]*(mergedposterior$strikes-mean(mergedposterior$strikes))+
  as.vector((mergedposterior$sz_top-meansztop)/sdsztop) %*% post_theta+as.vector((mergedposterior$sz_bot-meanszbot)/sdszbot) %*% post_kappa
datasds=mergedposterior[,(index+9):(index+17)]*mergedposterior[,(index+54):(index+62)]
mergedposterior[,(index+63):(index+71)]<-(mergedposterior[,9:17]-datameans)/datasds

allcholscores<-data.frame()
for(i in 1:numplayers){
  playerdata<-filter(mergedposterior,pitchernum==i)
  playerzscores<-playerdata[,(index+63):(index+71)]
  playerRho=allRhos[i,,]
  playercholscores<-data.frame(as.matrix(playerzscores,ncol=9) %*% solve(t(playerRho)))
  playercholscores$pitchernum=i
  allcholscores<-rbind(allcholscores,playercholscores)
}

simulateddata<-function(n,pitcherid,context,useecdf){
  #Transforming data into model data
  simcontext<-context[sample(1:length(context$balls),n,replace=TRUE),]
  balls=simcontext$balls
  strikes=simcontext$strikes
  RHB=simcontext$RHB
  sz_top=simcontext$sz_top
  sz_bot=simcontext$sz_bot
  playerposteriordata<-filter(playerposterior,pitcher==pitcherid) %>% slice(rep(1:n(), each=n))
  samehand=1*as.numeric(RHB==playerposteriordata$RHP)
  contextnum=samehand*12+balls*3+strikes+1
  normsz_top=(sz_top-meansztop)/sdsztop
  normsz_bot=(sz_bot-meanszbot)/sdszbot
  countposteriordata<-countposterior[contextnum,]
  #Getting mean/sd/Rho
  mean=playerposteriordata[,2:10]+
    playerposteriordata[,20:28]*(samehand-samehandmean)+
    playerposteriordata[,29:37]*(balls-ballsmean)+
    playerposteriordata[,38:46]*(strikes-strikesmean)+
    countposteriordata[,1:9]+
    normsz_top %*% post_theta+
    normsz_bot %*% post_kappa
  sigma=playerposteriordata[,11:19]*countposteriordata[,10:18]
  Rho=allRhos[playerposteriordata$pitchernum[1],,]
  #Generating sims
  if(useecdf==FALSE){
    simmedcholscores=matrix(rnorm(n*9),nrow=n,ncol=9)
  }
  else{
    simmedcholscores=as.matrix(allcholscores[sample(length(allcholscores[,1]),n,TRUE),1:9])
  }
  simmedzscores=simmedcholscores  %*% t(Rho)
  simmednormdata=simmedzscores
  for(i in 1:9){
    simmednormdata[,i]=simmedzscores[,i]*sigma[,i]+mean[,i]
  }
  simmeddata=as.data.frame(simmednormdata*rep(meanssds$Sd,each=n)+rep(meanssds$Mean,each=n))
  colnames(simmeddata)=variablenames
  simmeddata$ax<-simmeddata$ax-2*simmeddata$ax*as.numeric(playerposteriordata$RHP==0)
  simmeddata$bx<-simmeddata$bx-2*simmeddata$bx*as.numeric(playerposteriordata$RHP==0)
  simmeddata$cx<-simmeddata$cx-2*simmeddata$cx*as.numeric(playerposteriordata$RHP==0)
  return(simmeddata)
}
context=data.frame(balls=mergedmydata$balls,strikes=mergedmydata$strikes,RHB=as.numeric(mergedmydata$stand=="R"),sz_top=mergedmydata$sz_top,sz_bot=mergedmydata$sz_bot)
sims<-simulateddata(1000,112526,data.frame(balls=0,strikes=0,RHB=1,sz_top=3.45,sz_bot=1.56),FALSE)
sims<-simulateddata(1000,112526,context,TRUE)
