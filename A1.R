#I referred to the professor's codes used in this course when finishing the assignment

setwd("E:/programming/econ613/A1")
library(AER)
library(dplyr)
library(tidyr)

#read all datasets 
datstu<-read.csv("./dat/datstu.csv")
datjss<-read.csv("./dat/datjss.csv")
datsss<-read.csv("./dat/datsss.csv")


################## PART 1 #####################

########Exercise 1

###1.Number of students
nrow(datstu)


###2.Number of schools?
nrow(datsss)


###3.Number of programs
#create datatframe datstu1, which is the long form of datstu. datstu1 contanins all the choice made by students.
datstu1<-pivot_longer(datstu,c(`schoolcode1`,`schoolcode2`,`schoolcode3`,`schoolcode4`,`schoolcode5`,`schoolcode6`), names_to = "schoolchoice", names_prefix="schoolcode", values_to = "schoolcode")
datstu1<-pivot_longer(datstu1,c(`choicepgm1`,`choicepgm2`,`choicepgm3`,`choicepgm4`,`choicepgm5`,`choicepgm6`), names_to = "programchoice", names_prefix="choicepgm", values_to = "program")
datstu1<-filter(datstu1,schoolchoice==programchoice)
#extract unique programs
program<-unique(datstu1$program)
#delete empty observations
program<-program[program!=""]
length(program)
#the result is 32

###4.Number of choices (school,program)
choice<-datstu1[,c('schoolcode','program')]
choice<-unique(choice)
choice<-filter(choice,complete.cases(choice))
nrow(choice)
#the result is 3080

###5.Missing test score?
sum(is.na(datstu$score))
#the result is 179887


###6.Apply to the same school (different programs)
#create data frame datstu2, which contains all tbe students who apply for the same school.
datstu2<-filter(datstu,schoolcode1==schoolcode2&schoolcode1==schoolcode3&schoolcode1==schoolcode4&schoolcode1==schoolcode5&schoolcode1==schoolcode6)
#delete empty observations
datstu2<-filter(datstu2,!is.na(`schoolcode1`),!is.na(`schoolcode2`),!is.na(`schoolcode3`),!is.na(`schoolcode4`),!is.na(`schoolcode5`),!is.na(`schoolcode6`))
nrow(datstu2)
#the result is 174

###7.Apply to less than 6 choices
#create datstu3 which contains all the choices with empty programs or schoolcodes (for each student, datstu3 will only include 1 observation of choice)
  #extract observations with either emptty chocie of program or empty choice of school from datstu1 (datstu1 contains all choices madeby students)
datstu3<-filter(datstu1,is.na(schoolcode)|program=="")
  #only keep 1 observation for each student
datstu3<-distinct(datstu3,X,.keep_all = TRUE)
nrow(datstu3)
#the result is 21001




###################################################
#Exercise2

#creat datsss1 to represent modified datjsss and datsss respectively.
datsss1<-datsss[,-1]
#remove redundent observations and m,issing values in datsss
datsss1<-unique(datsss1)
datsss1<-filter(datsss1,!is.na(schoolcode),!is.na(ssslong),!is.na(ssslat))
#create datstu4, which contains all the school choice bundle without incomplete observations
datstu4<-filter(datstu1,!is.na(schoolcode),program!="")
#combine datstu4 and datsss1
stu4_sss1<-left_join(datstu4, datsss1,by='schoolcode')
stu4_sss1<-subset(stu4_sss1,select=-c(schoolname))
stu4_sss1<-unique(stu4_sss1)
#only keep the school choice bundle where the student is admitted
adm_stu4_sss1<-filter(stu4_sss1,schoolchoice==rankplace)
#by examination, there isn't any missing in variable score in stu4_sss1
cqs<-adm_stu4_sss1%>%group_by(schoolcode)%>%summarise(cutoff=min(score),quality=mean(score),size=n())
#merge
school_level<-left_join(adm_stu4_sss1,cqs,by='schoolcode')
#drop irrelevant variable
school_level<-subset(school_level,select=-c(X,score,agey,male,rankplace,schoolchoice,programchoice))
school_level<-distinct(school_level,schoolcode,.keep_all=TRUE)
head(school_level, n=20)


####################################################
#Exercise3
#By examination, there is only 1 observation in datjss where district is missing, which is completely useless.We can drop it without lose any information.
datjss1<-datjss[,-1]
datjss1$jssdistrict[datjss1$jssdistrict==""]<-NA
datjss1<-datjss1[complete.cases(datjss1[,'jssdistrict']),]
#merge
alldata<-left_join(stu4_sss1,datjss1,by='jssdistrict')
#get the distance of each choice bundle of each student
dist<-alldata%>%group_by(X,schoolchoice,schoolcode)%>%summarise(distance=sqrt(((69.172*(ssslong-point_x)*cos(point_y/57.3))^2)+(69.172*(ssslat-point_y))^2))
head(dist, n=20)



##############################################
#Exercise4
ex4<-alldata%>%group_by(X,schoolchoice,schoolcode,score)%>%summarise(distance=sqrt(((69.172*(ssslong-point_x)*cos(point_y/57.3))^2)+(69.172*(ssslat-point_y))^2))
ex4<-left_join(ex4,cqs,by='schoolcode')
ex4<-ex4[complete.cases(ex4),]
#for each ranked choice
ex4_1<-ex4%>%group_by(schoolchoice)%>%summarise(cutoff_mean=mean(cutoff),cutoff_sd=sd(cutoff),quality_mean=mean(quality),quality_sd=sd(quality),distance_mean=mean(distance),distance_sd=sd(distance))
head(ex4_1,n=20)

#by quantile
ex4_2<-ex4%>%group_by(G=cut(score,breaks=quantile(score,probs=seq(0,1,0.25))))%>%summarise(cutoff_mean=mean(cutoff),cutoff_sd=sd(cutoff),quality_mean=mean(quality),quality_sd=sd(quality),distance_mean=mean(distance),distance_sd=sd(distance))
head(ex4_2,n=20)







###########################################################
###########################Part 2########################

#exercise5
set.seed(123)
nobs<-10000

#uniform distribution
x1<-runif(nobs,min=1,max=3)
#gamma distribution
x2<-rgamma(nobs,3,scale=2)
#binomial distribution
x3<-rbinom(nobs,1,0.3)
#normal distribution
e<-rnorm(nobs,mean=2,sd=1)

y<- 0.5+1.2*x1-0.9*x2+0.1*x3+e
ydum<-y
ydum[ydum>mean (y)]<-1
ydum[!ydum>mean (y)]<-0

ex5<-as.data.frame(cbind(y,ydum,x1,x2,x3,e))
head(ex5,n=20)



###########################################################
#Exercise6 OLS
#6.1
corr_y_x1<-cov(y,x1)
corr_y_x1
#Correlation between Y and X1 is 0.41348. It is 0.78652 lesser than 1.2. It is significant different.


#6.2; 6.3: get coefficents
intercept<-rep(1,nobs)
X<-as.matrix(cbind(intercept,x1,x2,x3))
Y<-as.matrix(y)
beta<-solve(t(X)%*%X)%*%t(X)%*%Y
beta

#6.4:get sd
residual<-y-X%*%beta
#calculate variance of residual
df<-nrow(X)-ncol(X)
resvar<-sum(residual^2)/df
beta_se<-sqrt(diag((resvar*solve(t(X)%*%X)%*%t(X)%*%X%*%solve(t(X)%*%X))))
beta_se




###################################################
#Exercise7 discrete choice
#probit
probit_like = function(par,x1,x2,x3,yvar)
{
  xbeta           = par[1] + par[2]*x1 + par[3]*x2 + par[4]*x3
  pr              = pnorm(xbeta)
  #  pr              = exp(beta)/(1+exp(beta)) logit
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like           = yvar*log(pr) + (1-yvar)*log(1-pr)
  return(-sum(like))
}
start<-runif(4)
probit<-optim(start,fn=probit_like,method="BFGS",x1=x1,x2=x2,x3=x3,yvar=ydum)
probit


#logit
logit_like = function(par,x1,x2,x3,yvar)
{
  xbeta           = par[1] + par[2]*x1 + par[3]*x2 + par[4]*x3
  pr              = exp(xbeta)/(1+exp(xbeta))
  #  pr              = exp(beta)/(1+exp(beta)) logit
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like           = yvar*log(pr) + (1-yvar)*log(1-pr)
  return(-sum(like))
}
start<-runif(4)
logit<-optim(start,fn=logit_like,method="BFGS",x1=x1,x2=x2,x3=x3,yvar=ydum)
logit


#linear probability


intercept<-rep(1,nobs)
X<-as.matrix(cbind(intercept,x1,x2,x3))
Y<-as.matrix(ydum)
beta<-solve(t(X)%*%X)%*%t(X)%*%Y

residual<-ydum-X%*%beta
#calculate variance of residual
df<-nrow(X)-ncol(X)
resvar<-sum(residual^2)/df

beta_se<-sqrt(diag((resvar*solve(t(X)%*%X)%*%t(X)%*%X%*%solve(t(X)%*%X))))
beta_se

#############################################
#Excise8
#probit ME mean
glm_probit<-glm(ydum~x1+x2+x3,family = binomial(link = "probit"))
probit_pdf<-mean(dnorm(predict(glm_probit,type="link")))
probit.me.mean<-probit_pdf*coef(glm_probit)
probit.me.mean
#logit ME mean
glm_logit<-glm(ydum~x1+x2+x3,family = binomial(link = "logit"))
logit_pdf<-mean(dlogis(predict(glm_logit,type="link")))
logit.me.mean<-logit_pdf*coef(glm_logit)
logit.me.mean


#probit ME standard error
ex8<-as.data.frame(cbind(ydum,x1,x2,x3))

R    = 999;                     
nind = nrow(ex8);           
nvar = 4
outs = mat.or.vec(R,nvar)

for (i in 1:R)
{
  samp     = sample(1:nind,nind,rep=TRUE)
  dat_samp = ex8[samp,]
  glm_probit<-glm(ydum~x1+x2+x3,family = binomial(link = "probit"),data=dat_samp)
  probit_pdf<-mean(dnorm(predict(glm_probit,type="link")))
  probit.me.mean<-probit_pdf*coef(glm_probit)
  outs[i,] = probit.me.mean
}

mean_est = apply(outs,2,mean)
sd_est   = apply(outs,2,sd)

probit_est = cbind(mean_est,sd_est)
colnames(probit_est) = c("probit_ME_mean","probit_ME_sd")
probit_est


#logit ME standard error
ex8<-as.data.frame(cbind(ydum,x1,x2,x3))

R    = 999;                     
nind = nrow(ex8);           
nvar = 4
outs = mat.or.vec(R,nvar)

for (i in 1:R)
{
  samp     = sample(1:nind,nind,rep=TRUE)
  dat_samp = ex8[samp,]
  glm_logit<-glm(ydum~x1+x2+x3,family = binomial(link = "logit"),data=dat_samp)
  logit_pdf<-mean(dnorm(predict(glm_logit,type="link")))
  logit.me.mean<-logit_pdf*coef(glm_logit)
  outs[i,] = logit.me.mean
}

mean_est = apply(outs,2,mean)
sd_est   = apply(outs,2,sd)

logit_est = cbind(mean_est,sd_est)
colnames(logit_est) = c("logit_ME_mean","logit_ME_sd")
logit_est


