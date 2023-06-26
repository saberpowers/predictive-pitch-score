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
#pulls in data I already have downloaded
setwd("~/Downloads")
mydata<-read_csv("2017.csv")

pitches<-filter(mydata,is.na(mydata$ay)!=1 & is.na(mydata$release_speed)!=1 & is.na(mydata$pfx_z)!=1 & is.na(mydata$pfx_x)!=1 & is.na(mydata$release_extension)!=1 & is.na(mydata$release_pos_z)!=1 & is.na(mydata$release_pos_x)!=1 & is.na(mydata$plate_x)!=1 & is.na(mydata$plate_z)!=1 & (mydata$release_speed)>=70 & is.na(mydata$pitch_type)!=1)
#Select needed variables
pitches<-select(pitches,c("game_year","pitcher","pitch_type", "p_throws","stand","balls","strikes","sz_top","sz_bot","home_team","batter","bat_score","fld_score","inning","inning_topbot","outs_when_up","on_1b","on_2b","on_3b"))

#Filter out for count over 200
playerseasondata<-pitches %>% group_by(pitcher,game_year) %>% summarize(count=n())
mergedmydata<-merge(pitches,playerseasondata)
mergedmydata<-filter(mergedmydata,mergedmydata$count>=200,mergedmydata$pitcher%%1==0)

#Gets 24 contexts ready
mergedmydata$samehand<-as.numeric(mergedmydata$stand==mergedmydata$p_throws)
mergedmydata$bshnum<-mergedmydata$samehand*12+mergedmydata$balls*3+mergedmydata$strikes+1

#Adds player sequence number
players<-unique(mergedmydata$pitcher) 
players<-data.frame(pitcher=players,pitchernum=seq(1,length(players)))
mergedmydata<-merge(mergedmydata,players)
bshweights<-mergedmydata %>% group_by(bshnum) %>% summarize(weight=n()/length(mergedmydata$bshnum))

#Makes pitch type into numeric
mergedmydata$pitch_typenum=9-as.numeric(mergedmydata$pitch_type=="FF")*8-
                             as.numeric(mergedmydata$pitch_type=="SI")*7-
                             as.numeric(mergedmydata$pitch_type=="SL")*6-
                             as.numeric(mergedmydata$pitch_type=="CH")*5-
                             as.numeric(mergedmydata$pitch_type=="CU")*4-
                             as.numeric(mergedmydata$pitch_type=="FC")*3-
                             as.numeric(mergedmydata$pitch_type=="KC")*2-
                             as.numeric(mergedmydata$pitch_type=="FS")*1
#As well as a matrix
mergedmydata$FF=as.numeric(mergedmydata$pitch_type=="FF")
mergedmydata$SI=as.numeric(mergedmydata$pitch_type=="SI")
mergedmydata$SL=as.numeric(mergedmydata$pitch_type=="SL")
mergedmydata$CH=as.numeric(mergedmydata$pitch_type=="CH")
mergedmydata$CU=as.numeric(mergedmydata$pitch_type=="CU")
mergedmydata$FC=as.numeric(mergedmydata$pitch_type=="FC")
mergedmydata$KC=as.numeric(mergedmydata$pitch_type=="KC")
mergedmydata$FS=as.numeric(mergedmydata$pitch_type=="FS")
mergedmydata$Other=as.numeric(mergedmydata$pitch_typenum==9)

#Normalizes Strike Zone
meansztop=mean(mergedmydata$sz_top)
sdsztop=sd(mergedmydata$sz_top)
meanszbot=mean(mergedmydata$sz_bot)
sdszbot=sd(mergedmydata$sz_bot)
mergedmydata$sz_top<-(mergedmydata$sz_top-meansztop)/sdsztop
mergedmydata$sz_bot<-(mergedmydata$sz_bot-meanszbot)/sdszbot

#Set up for stan model
dat = list(n=length(mergedmydata$pitch_type),
           s=length(players$pitcher),
           y=mergedmydata$pitch_typenum,
           k=9,
           balls=mergedmydata$balls-mean(mergedmydata$balls),
           strikes=mergedmydata$strikes-mean(mergedmydata$strikes),
           hand=mergedmydata$samehand-mean(mergedmydata$samehand),
           sz_top=mergedmydata$sz_top,
           sz_bottom=mergedmydata$sz_bot,
           bsh=mergedmydata$bshnum,
           p=mergedmydata$pitchernum
)

inits1<-mergedmydata %>% group_by(pitcher) %>% summarize(FF=mean(FF),
                                                                   SI=mean(SI),
                                                                   SL=mean(SL),
                                                                   CH=mean(CH),
                                                                   CU=mean(CU),
                                                                   FC=mean(FC),
                                                                   KC=mean(KC),
                                                                   FS=mean(FS),
                                                                   Other=mean(Other))
inits1[,2:10]<-log((inits1[,2:10]+0.001)/1.009)
inits3<-vector()
for(i in 1:9){
  inits3[i]=sum(as.numeric(as.matrix(inits1)[,i+1]>-6))/dat$s
}
inits2<-(colMeans(inits1[,2:10])-(log(0.001))*(1-inits3))/inits3
inits4<-vector()
for(i in 1:9){
  inits4[i]<-sd(as.matrix(inits1)[inits1[,i+1]>-6,i+1])
}

#run model
mod <- cmdstan_model('pitch type prediction.stan')

fit_optim <- mod$optimize(data = dat, seed = 123,iter=5000, algorithm="lbfgs",tol_rel_grad = 1e+3,tol_param=1e-9,
                          init = list(
                            list(mu=as.matrix(inits1[,2:10]),
                                 tau=inits2,
                                 epsilon=inits3,
                                 gamma=inits4,
                                 delta=inits2+7,
                                 lambda=matrix(0,nrow=24,ncol=9),
                                 xi=matrix(0,nrow=dat$s,ncol=9),
                                 pivar=matrix(0,nrow=dat$s,ncol=9),
                                 nu=matrix(0,nrow=dat$s,ncol=9),
                                 theta=rep(0,9),
                                 kappa=rep(0,9))))
                                 #phi=array(0,dim=c(dat$s,24,9)))))
fit_optim$save_object("pitch prediction.RDS")

#Get posterior
post_mu = as_draws(fit_optim$draws('mu'))
post_tau = as_draws(fit_optim$draws('tau'))
post_epsilon = as_draws(fit_optim$draws('epsilon'))
post_gamma = as_draws(fit_optim$draws('gamma'))
post_delta = as_draws(fit_optim$draws('delta'))
post_lambda = as_draws(fit_optim$draws('lambda'))
post_xi = as_draws(fit_optim$draws('xi'))
post_nu = as_draws(fit_optim$draws('nu'))
post_pivar = as_draws(fit_optim$draws('pivar'))
post_theta = as_draws(fit_optim$draws('theta'))
post_kappa = as_draws(fit_optim$draws('kappa'))

#Look at intits
summary(lm(as.numeric(post_mu)~as.numeric(as.matrix(inits1[,2:10]))))
summary(lm(as.numeric(post_tau)~inits2))
summary(lm(as.numeric(post_epsilon)~inits3))
summary(lm(as.numeric(post_gamma)~inits4))
summary(lm(as.numeric(post_delta)~inits2))

#Look at adjustments
hist(post_lambda)
hist(post_xi)
hist(post_nu)
hist(post_pivar)
hist(post_theta)
hist(post_kappa)


#Builds expectations
mumatrix<-matrix(post_mu,ncol=9)
lambdamatrix<-matrix(post_lambda,ncol=9)
pimatrix<-matrix(post_pivar,ncol=9)
numatrix<-matrix(post_nu,ncol=9)
ximatrix<-matrix(post_xi,ncol=9)
playerposterior<-data.frame(mumatrix,pimatrix,numatrix,ximatrix,pitchernum=seq(1,dat$s))
mergedposterior<-merge(mergedmydata,playerposterior)
countposterior=data.frame(lambdamatrix,bshnum=seq(1,24))
mergedposterior<-merge(mergedposterior,countposterior,by="bshnum")
means=mergedposterior[,34:42]+mergedposterior[,70:78]+mergedposterior[,43:51]*(mergedposterior$samehand-mean(mergedposterior$samehand))+
  mergedposterior[,52:60]*(mergedposterior$balls-mean(mergedposterior$balls))+
  mergedposterior[,61:69]*(mergedposterior$strikes-mean(mergedposterior$strikes))+
  mergedposterior$sz_top %*% post_theta+mergedposterior$sz_bot %*% post_kappa
means=exp(means)/(rowSums(exp(means)))
colnames(means)=c("FFpred","SIpred","SLpred","CHpred","CUpred","FCpred","KCpred","FSpred","Otherpred")
mergedposterior[,79:87]=means

totalright=sum(as.numeric(mergedposterior$FFpred)==rowMaxs(as.matrix(means))*mergedposterior$FF)+
  sum(as.numeric(mergedposterior$SIpred)==rowMaxs(as.matrix(means))*mergedposterior$SI)+
  sum(as.numeric(mergedposterior$SLpred)==rowMaxs(as.matrix(means))*mergedposterior$SL)+
  sum(as.numeric(mergedposterior$CHpred)==rowMaxs(as.matrix(means))*mergedposterior$CH)+
  sum(as.numeric(mergedposterior$CUpred)==rowMaxs(as.matrix(means))*mergedposterior$CU)+
  sum(as.numeric(mergedposterior$FCpred)==rowMaxs(as.matrix(means))*mergedposterior$FC)+
  sum(as.numeric(mergedposterior$KCpred)==rowMaxs(as.matrix(means))*mergedposterior$KC)+
  sum(as.numeric(mergedposterior$FSpred)==rowMaxs(as.matrix(means))*mergedposterior$FS)+
  sum(as.numeric(mergedposterior$Otherpred)==rowMaxs(as.matrix(means))*mergedposterior$Other)

#Accuracy metrics
totalright/length(mergedposterior$FFpred) #% correct
likelihoodoftrue=rowSums(mergedposterior[,25:33]*mergedposterior[,79:87])
mean(log(likelihoodoftrue)) #Log loss
mean(likelihoodoftrue/rowSums(mergedposterior[,79:87]^2)^0.5) #Spherical score


