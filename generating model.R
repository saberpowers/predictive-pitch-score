pitch="FF"
library(readr)
library(dplyr)
library(caret)
library(MVN)
library(energy)
library(texreg)
library(kableExtra)
library(matrixStats)
library(posterior)
library(cmdstanr)
library(tidyr)

# your code for getting Physical parameters
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

#pulls in data I already have downloaded
setwd("~/Downloads")
mydata<-read_csv("2017.csv")

#Filter out for count over 200 (wanted players that throw fastballs relatively often to start)
#fullplayerseasondata<-mydata %>% group_by(pitcher,game_year) %>% summarize(count=n())
#fullplayerseasondata<-filter(fullplayerseasondata,count>=200,pitcher %% 1==0) #cut down to x% of data

#mydata<-filter(mydata,pitcher %in% fullplayerseasondata$pitcher)

generateposterior<-function(pitch){
fastballs<-filter(mydata,pitch_type==pitch & is.na(mydata$ay)!=1 & is.na(mydata$release_speed)!=1 & is.na(mydata$pfx_z)!=1 & is.na(mydata$pfx_x)!=1 & is.na(mydata$release_extension)!=1 & is.na(mydata$release_pos_z)!=1 & is.na(mydata$release_pos_x)!=1 & is.na(mydata$plate_x)!=1 & is.na(mydata$plate_z)!=1 & (mydata$release_speed)>=70)
#Select needed variables
fastballs<-select(fastballs,c("game_year","pitcher","p_throws","stand","balls","strikes","ax","ay","az","vx0","vy0","vz0","x0"="release_pos_x","extension"="release_extension","z0"="release_pos_z","plate_x","plate_z","pfx_x","pfx_z","sz_top","sz_bot","home_team","batter","bat_score","fld_score","inning","inning_topbot","outs_when_up","on_1b","on_2b","on_3b"))

#Filter out for count over 200 (wanted players that throw fastballs relatively often to start)
playerseasondata<-fastballs %>% group_by(pitcher,game_year) %>% summarize(count=n())                                                        
mergedmydata<-merge(fastballs,playerseasondata)
mergedmydata<-filter(mergedmydata,mergedmydata$count>=100,mergedmydata$pitcher%%1==0) #cut down to x% of data
scottdata<-get_quadratic_coef(mergedmydata)
mergedmydata[,7:15]<-scottdata
colnames(mergedmydata)[7:15]<-colnames(scottdata)

#Flips data for LHP
mergedmydata$ax<-mergedmydata$ax-2*mergedmydata$ax*as.numeric(mergedmydata$p_throws=="L")
mergedmydata$bx<-mergedmydata$bx-2*mergedmydata$bx*as.numeric(mergedmydata$p_throws=="L")
mergedmydata$cx<-mergedmydata$cx-2*mergedmydata$cx*as.numeric(mergedmydata$p_throws=="L")
mergedmydata$samehand<-as.numeric(mergedmydata$stand==mergedmydata$p_throws)
mergedmydata$bshnum<-mergedmydata$samehand*12+mergedmydata$balls*3+mergedmydata$strikes+1

meanssds=data.frame(Mean=colMeans(mergedmydata[,7:15]),Sd=colSds(as.matrix(mergedmydata[,7:15])))

variablenames=c("ax","ay","az","bx","by","bz","cx","cy","cz")
rownames(meanssds)<-variablenames
' #For table in rmarkdown
meanssdsround<-round(meanssds,2)
meanssdsround %>%
  kable(
    caption = "Means and Standard Deviations"
  )
'
#normalizes data
prenorm=mergedmydata
for(i in 7:15){
  mergedmydata[,i]<-(mergedmydata[,i]-mean(mergedmydata[,i]))/sd(mergedmydata[,i])
}
meansztop=mean(mergedmydata$sz_top)
sdsztop=sd(mergedmydata$sz_top)
meanszbot=mean(mergedmydata$sz_bot)
sdszbot=sd(mergedmydata$sz_bot)

mergedmydata$sz_top<-(mergedmydata$sz_top-meansztop)/sdsztop
mergedmydata$sz_bot<-(mergedmydata$sz_bot-meanszbot)/sdszbot

#Gets data ready for stan
players<-unique(mergedmydata$pitcher) #Adds player sequence number
players<-data.frame(pitcher=players,pitchernum=seq(1,length(players)))
mergedmydata<-merge(mergedmydata,players)
bshweights<-mergedmydata %>% group_by(bshnum) %>% summarize(weight=n()/length(mergedmydata$bshnum))
fullbshweights<-bshweights
for(i in 1:24){
  row<-filter(bshweights,bshnum==i)
  if(length(row$bshnum)==1){
    fullbshweights[i,]=row
  }
  else{
    fullbshweights[i,1]<-i
    fullbshweights[i,2]<-0
  }
}
bshweights<-fullbshweights
dat = list(n=length(mergedmydata$ay),
           s=length(players$pitcher),
           c=9,
           v=mergedmydata[,7:15],
           sz_top=mergedmydata$sz_top,
           sz_bottom=mergedmydata$sz_bot,
           balls=mergedmydata$balls-mean(mergedmydata$balls),
           strikes=mergedmydata$strikes-mean(mergedmydata$strikes),
           hand=mergedmydata$samehand-mean(mergedmydata$samehand),
           bsh=mergedmydata$bshnum,
           p=mergedmydata$pitchernum,
           bshweights=bshweights$weight
)
inits<-mergedmydata %>% group_by(pitchernum) %>% summarize(axmean=mean(ax),
                                                           aymean=mean(ay),
                                                           azmean=mean(az),
                                                           bxmean=mean(bx),
                                                           bymean=mean(by),
                                                           bzmean=mean(bz),
                                                           cxmean=mean(cx),
                                                           cymean=mean(cy),
                                                           czmean=mean(cz),
                                                           axsd=sd(ax),
                                                           aysd=sd(ay),
                                                           azsd=sd(az),
                                                           bxsd=sd(bx),
                                                           bysd=sd(by),
                                                           bzsd=sd(bz),
                                                           cxsd=sd(cx),
                                                           cysd=sd(cy),
                                                           czsd=sd(cz))
ballast=(1/colSds(as.matrix(inits[,2:10]))^2)/(1/colMeans(as.matrix(inits[,11:19]))*0.95)^2
varprior=1/(colMeans(as.matrix(inits[,11:19])^-2))
ballast2=2*colMeans(as.matrix(inits[,11:19])^2)/(colMeans(as.matrix(inits[,11:19])^2)-varprior)
inits1.5<-mergedmydata %>% group_by(pitchernum) %>% summarize(axmean=sum(ax)/(n()+ballast[1]), #Was struggling at getting the batter stuff to work well. Took it out for now
                                                              aymean=sum(ay)/(n()+ballast[2]),
                                                              azmean=sum(az)/(n()+ballast[3]),
                                                              bxmean=sum(bx)/(n()+ballast[4]),
                                                              bymean=sum(by)/(n()+ballast[5]),
                                                              bzmean=sum(bz)/(n()+ballast[6]),
                                                              cxmean=sum(cx)/(n()+ballast[7]),
                                                              cymean=sum(cy)/(n()+ballast[8]),
                                                              czmean=sum(cz)/(n()+ballast[9]),
                                                              axsd=((var(ax)*n()+varprior[1]*ballast2[1])/(n()+ballast2[1]))^0.5,
                                                              aysd=((var(ay)*n()+varprior[2]*ballast2[2])/(n()+ballast2[2]))^0.5,
                                                              azsd=((var(az)*n()+varprior[3]*ballast2[3])/(n()+ballast2[3]))^0.5,
                                                              bxsd=((var(bx)*n()+varprior[4]*ballast2[4])/(n()+ballast2[4]))^0.5,
                                                              bysd=((var(by)*n()+varprior[5]*ballast2[5])/(n()+ballast2[5]))^0.5,
                                                              bzsd=((var(bz)*n()+varprior[6]*ballast2[6])/(n()+ballast2[6]))^0.5,
                                                              cxsd=((var(cx)*n()+varprior[7]*ballast2[7])/(n()+ballast2[7]))^0.5,
                                                              cysd=((var(cy)*n()+varprior[8]*ballast2[8])/(n()+ballast2[8]))^0.5,
                                                              czsd=((var(cz)*n()+varprior[9]*ballast2[9])/(n()+ballast2[9]))^0.5)
inits2<-mergedmydata %>% group_by(bshnum) %>% summarize(axmean=mean(ax),
                                                        aymean=mean(ay),
                                                        azmean=mean(az),
                                                        bxmean=mean(bx),
                                                        bymean=mean(by),
                                                        bzmean=mean(bz),
                                                        cxmean=mean(cx),
                                                        cymean=mean(cy),
                                                        czmean=mean(cz),
                                                        axsd=replace_na(sd(ax),1),
                                                        aysd=replace_na(sd(ay),1),
                                                        azsd=replace_na(sd(az),1),
                                                        bxsd=replace_na(sd(bx),1),
                                                        bysd=replace_na(sd(by),1),
                                                        bzsd=replace_na(sd(bz),1),
                                                        cxsd=replace_na(sd(cx),1),
                                                        cysd=replace_na(sd(cy),1),
                                                        czsd=replace_na(sd(cz),1))
inits5<-mergedmydata %>% group_by(pitchernum) %>% summarize(axnu=cov(ax,balls)/(0.001+var(balls)),
                                                            aynu=cov(ay,balls)/(0.001+var(balls)),
                                                            aznu=cov(az,balls)/(0.001+var(balls)),
                                                            bxnu=cov(bx,balls)/(0.001+var(balls)),
                                                            bynu=cov(by,balls)/(0.001+var(balls)),
                                                            bznu=cov(bz,balls)/(0.001+var(balls)),
                                                            cxnu=cov(cx,balls)/(0.001+var(balls)),
                                                            cynu=cov(cy,balls)/(0.001+var(balls)),
                                                            cznu=cov(cz,balls)/(0.001+var(balls)),
                                                            axxi=cov(ax,strikes)/(0.001+var(strikes)),
                                                            ayxi=cov(ay,strikes)/(0.001+var(strikes)),
                                                            azxi=cov(az,strikes)/(0.001+var(strikes)),
                                                            bxxi=cov(bx,strikes)/(0.001+var(strikes)),
                                                            byxi=cov(by,strikes)/(0.001+var(strikes)),
                                                            bzxi=cov(bz,strikes)/(0.001+var(strikes)),
                                                            cxxi=cov(cx,strikes)/(0.001+var(strikes)),
                                                            cyxi=cov(cy,strikes)/(0.001+var(strikes)),
                                                            czxi=cov(cz,strikes)/(0.001+var(strikes)),
                                                            axpi=cov(ax,samehand)/(0.001+var(samehand)),
                                                            aypi=cov(ay,samehand)/(0.001+var(samehand)),
                                                            azpi=cov(az,samehand)/(0.001+var(samehand)),
                                                            bxpi=cov(bx,samehand)/(0.001+var(samehand)),
                                                            bypi=cov(by,samehand)/(0.001+var(samehand)),
                                                            bzpi=cov(bz,samehand)/(0.001+var(samehand)),
                                                            cxpi=cov(cx,samehand)/(0.001+var(samehand)),
                                                            cypi=cov(cy,samehand)/(0.001+var(samehand)),
                                                            czpi=cov(cz,samehand)/(0.001+var(samehand)))
inits5<-inits5-rep(colMeans(inits5),each=dat$s)
zscoreinit=merge(mergedmydata,inits1.5)
zscoreinit[,54:62]=(zscoreinit[,8:16]-zscoreinit[,36:44]*0.97)/(zscoreinit[,45:53]*0.9+0.1)
zscoreinit=zscoreinit[,54:62]

leagueRhoinit=array(rep(t(chol(cor(zscoreinit))),each=dat$s),dim=c(dat$s,9,9))
playerRhoinit=array(rep(t(chol(cor(zscoreinit))),each=dat$s),dim=c(dat$s,9,9))
for(i in 1:dat$s){
  playerdata<-filter(mergedmydata,pitchernum==i)
  playercorr=cor(playerdata[,7:15])
  playerRhoinit[i,,]=t(chol(playercorr))
}

Rhoinit=array(rep(t(chol(cor(zscoreinit))),each=dat$s),dim=c(dat$s,9,9))
leaguecorr=cor(zscoreinit)
for(i in 1:dat$s){
  playerdata<-filter(mergedmydata,pitchernum==i)
  playercorr=cor(playerdata[,7:15])
  weight=length(playerdata$batter)^0.5/(length(playerdata$batter)^0.5+7)
  Rhoinit[i,,]=t(chol(playercorr*weight+leaguecorr*(1-weight)))
}

fullinits2<-inits2
for(i in 1:24){
  row<-filter(inits2,bshnum==i)
  if(length(row$bshnum)==1){
    fullinits2[i,]=row
  }
  else{
    fullinits2[i,1]<-i
    fullinits2[i,2:10]<-0
    fullinits2[i,11:19]<-1
  }
}
inits2<-fullinits2

mod <- cmdstan_model('step 8 model simple.stan')

fit_optim <- mod$optimize(data = dat, seed = 123,iter=15000, algorithm="lbfgs",tol_rel_grad = 1e+3,tol_param=1e-9,
                          init = list(
                            list(gamma = colSds(as.matrix(inits[,2:10])),
                                 epsilon=colMeans(as.matrix(inits[,11:19]))*0.98,
                                 tau=colMeans(as.matrix(inits[,2:10]))*0.97,
                                 eta=colSds(as.matrix(inits[,11:19]))*0.86,
                                 sigma=0.95*inits1.5[,11:19],
                                 mu=inits1.5[,2:10],
                                 leagueRho=t(chol(cor(zscoreinit))),
                                 Rho=Rhoinit,
                                 lambdanorm=0.82*inits2[2:24,2:10]/0.2,
                                 zetanorm=(inits2[2:24,11:19]-1)/0.1,
                                 theta=rep(0,dat$c),
                                 kappa=rep(0,dat$c),
                                 nunorm=0.5*inits5[2:dat$s,2:10]/0.05,
                                 xinorm=0.5*inits5[2:dat$s,11:19]/0.05,
                                 pinorm=0.6*inits5[2:dat$s,20:28]/0.1)))
fit_optim$save_object(paste("step 8 model simple ",pitch,"v2.RDS",sep=""))


#Posterior analysis


post_tau = as_draws(fit_optim$draws('tau'))
post_gamma = as_draws(fit_optim$draws('gamma'))
post_epsilon = as_draws(fit_optim$draws('epsilon'))
post_eta = as_draws(fit_optim$draws('eta'))
post_mu = as_draws(fit_optim$draws('mu'))
post_sigma = as_draws(fit_optim$draws('sigma'))
post_lambda = as_draws(fit_optim$draws('lambda'))
post_zeta = as_draws(fit_optim$draws('zeta'))
post_lambdanorm = as_draws(fit_optim$draws('lambdanorm'))
post_zetanorm = as_draws(fit_optim$draws('zetanorm'))
post_leagueRho = as_draws(fit_optim$draws('leagueRho'))
post_Rho = as_draws(fit_optim$draws('Rho'))
post_nunorm = as_draws(fit_optim$draws('nunorm'))
post_nu = as_draws(fit_optim$draws('nu'))
post_xinorm = as_draws(fit_optim$draws('xinorm'))
post_xi = as_draws(fit_optim$draws('xi'))
post_pinorm = as_draws(fit_optim$draws('pinorm'))
post_pi = as_draws(fit_optim$draws('pivar'))
post_theta = as_draws(fit_optim$draws('theta'))
post_kappa = as_draws(fit_optim$draws('kappa'))
#post_phi = as_draws(fit_optim$draws('phi'))

allRhos=array(post_Rho,dim=c(dat$s,9,9))
colMeans(allRhos)

Rhosds=matrix(0,nrow=9,ncol=9)
phimatrix=matrix(0,nrow=9,ncol=9)
Rhosdvector=0
Rhosdprior=0
for(i in 2:9){
  for(j in 1:(i-1)){
    Rhosds[i,j]=sd(allRhos[,i,j])
    #phimatrix[i,j]=post_phi[j+((i-1)*(i-2)) %/% 2]
    Rhosdvector[j+((i-1)*(i-2)) %/% 2]=sd(allRhos[,i,j])
    Rhosdprior[j+((i-1)*(i-2)) %/% 2]=sd(Rhoinit[,i,j])
  }
}
#plot(Rhosdvector,post_phi)
#plot(Rhosdprior,post_phi)
plot(Rhosdprior,Rhosdvector)


leagueRhoinit=array(rep(t(chol(cor(zscoreinit))),each=dat$s),dim=c(dat$s,9,9))
playerRhoinit=array(rep(t(chol(cor(zscoreinit))),each=dat$s),dim=c(dat$s,9,9))
for(i in 1:dat$s){
  playerdata<-filter(mergedmydata,pitchernum==i)
  playercorr=cor(playerdata[,9:17])
  playerRhoinit[i,,]=t(chol(playercorr))
}
summary(lm(as.numeric(post_tau)~colMeans(as.matrix(inits[,2:10])))) 
summary(lm(as.numeric(post_gamma)~colSds(as.matrix(inits[,2:10]))))
summary(lm(as.numeric(post_epsilon)~colMeans(as.matrix(inits[,11:19]))))
summary(lm(as.numeric(post_eta)~colSds(as.matrix(inits[,11:19]))))
summary(lm(as.numeric(post_mu)~as.numeric(as.matrix(inits1.5[,2:10]))))
summary(lm(as.numeric(post_sigma)~as.numeric(as.matrix(inits1.5[,11:19]))))
plot(as.numeric(t(chol(cor(zscoreinit)))),as.numeric(post_leagueRho))
summary(lm(as.numeric(post_lambdanorm)~as.numeric(as.matrix(inits2[2:24,2:10]))))
summary(lm(as.numeric(post_zetanorm)~as.numeric(as.matrix(inits2[2:24,11:19]))))
summary(lm(as.numeric(post_nunorm)~as.numeric(as.matrix(inits5[2:dat$s,2:10]))))
summary(lm(as.numeric(post_xinorm)~as.numeric(as.matrix(inits5[2:dat$s,11:19]))))
summary(lm(as.numeric(post_pinorm)~as.numeric(as.matrix(inits5[2:dat$s,20:28]))))
summary(lm(as.numeric(post_Rho)~as.numeric(Rhoinit)))
summary(lm(as.numeric(post_Rho)~as.numeric(Rhoinit)+as.numeric(playerRhoinit)+as.numeric(leagueRhoinit)))



playermeans=matrix(post_mu,nrow=dat$s,ncol=9)
playersigmas=matrix(post_sigma,nrow=dat$s,ncol=9)
playerhands=matrix(post_pi,nrow=dat$s,ncol=9)
playerballs=matrix(post_nu,nrow=dat$s,ncol=9)
playerstrikes=matrix(post_xi,nrow=dat$s,ncol=9)
playerposterior<-data.frame(playermeans,playersigmas,playerhands,playerballs,playerstrikes,pitchernum=seq(1,dat$s))
mergedposterior<-merge(mergedmydata,playerposterior)
countmeans=matrix(post_lambda,nrow=24,ncol=9)
countsds=matrix(post_zeta,nrow=24,ncol=9)
countposterior=data.frame(countmeans,countsds,bshnum=seq(1,24))
mergedposterior<-merge(mergedposterior,countposterior,by="bshnum")
index=36

datameans=mergedposterior[,index:(index+8)]+mergedposterior[,(index+45):(index+53)]+
  mergedposterior[,(index+18):(index+26)]*(mergedposterior$samehand-mean(mergedposterior$samehand))+
  mergedposterior[,(index+27):(index+35)]*(mergedposterior$balls-mean(mergedposterior$balls))+
  mergedposterior[,(index+36):(index+44)]*(mergedposterior$strikes-mean(mergedposterior$strikes))+
  mergedposterior$sz_top %*% post_theta+mergedposterior$sz_bot %*% post_kappa
datasds=mergedposterior[,(index+9):(index+17)]*mergedposterior[,(index+54):(index+62)]
mergedposterior[,(index+63):(index+71)]<-(mergedposterior[,9:17]-datameans)/datasds
zscores<-(mergedposterior[,9:17]-datameans)/datasds
par(mfrow=c(3,3))
for(i in 1:9){
  hist(zscores[,i],xlab=variablenames[i],main=paste("Z Scores for", variablenames[i]),breaks=100)
}

quantiles=data.frame()
for(i in 1:9){
  quantiles<-rbind(quantiles,quantile(zscores[,i],c(0,0.01,0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975,0.99,1)))
}
rownames(quantiles)=variablenames
colnames(quantiles)=c("Min", "1%", "2.5%", "5%", "10%","25%","50%","75%","90%","95%","97.5","99%","Max")
quantiles

allcholscores<-data.frame()
for(i in 1:dat$s){
  playerdata<-filter(mergedposterior,pitchernum==i)
  playerzscores<-playerdata[,(index+63):(index+71)]
  playerRho=allRhos[i,,]
  playercholscores<-data.frame(as.matrix(playerzscores,ncol=9) %*% solve(t(playerRho)))
  playercholscores$pitchernum=i
  allcholscores<-rbind(allcholscores,playercholscores)
}
colnames(allcholscores)[1:9]<-paste(colnames(mergedposterior)[9:17],"chol",sep="")

for(i in 1:9){
  hist(allcholscores[,i],xlab=variablenames[i],main=paste("Cholesky Scores for", variablenames[i]),breaks=100)
}
}

generateposterior("CH")
generateposterior("FF")
generateposterior("SI")
generateposterior("SL")
generateposterior("CU")
generateposterior("FC")
generateposterior("KC")
generateposterior("FS")



