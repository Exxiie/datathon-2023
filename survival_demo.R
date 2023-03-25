
#--Demo code
#Some of this was borrowed from the Chapter 11 lab at https://urldefense.com/v3/__https://www.statlearning.com/resources-second-edition__;!!DZ3fjg!42wNAPQGaiwrd4CpceClNQvWakbAYbTmseIpHotHCl4ISyX1R5aTKrPPn7Y2R_7YGxbsxYQ1sWZ9NHd4Sg$ 

#Sandia National Laboratories is a multimission laboratory managed and operated by National Technology & Engineering Solutions of Sandia, LLC, a wholly owned subsidiary of Honeywell International Inc., for the U.S. Department of Energy’s National Nuclear Security Administration under contract DE-NA0003525.

#Load data and packages
library(ISLR2)
library(survival)
data("BrainCancer") #This comes from the ISLR2 library.

#What's in here?
?BrainCancer
head(BrainCancer)
summary(BrainCancer)

#Plots of data
hist(BrainCancer$ki,breaks=20)
table(BrainCancer$ki)

hist(BrainCancer$gtv,breaks=20)

plot(BrainCancer$time, BrainCancer$gtv)
plot(BrainCancer$time, BrainCancer$ki)

#Look at some of these by censoring status
prop.table(table(BrainCancer$sex,BrainCancer$status),margin=2)
prop.table(table(BrainCancer$diagnosis,BrainCancer$status),margin=2)
prop.table(table(BrainCancer$loc,BrainCancer$status),margin=2)
prop.table(table(BrainCancer$stereo,BrainCancer$status),margin=2)

boxplot(time ~ factor(status),data=BrainCancer)
boxplot(ki ~ factor(status),data=BrainCancer)
boxplot(gtv ~ factor(status),data=BrainCancer)

#Kaplan Meier analysis for EDA purposes
?survfit
?survdiff

km.sex <- survfit(Surv(time,status) ~ sex, data=BrainCancer)
plot(km.sex, xlab="Months",ylab="Survival Probability",col=3:4, main="Survival by Sex")
survdiff(Surv(time,status) ~ sex, data=BrainCancer)

km.diag <- survfit(Surv(time,status) ~ diagnosis, data=BrainCancer)
plot(km.diag, xlab="Months",ylab="Survival Probability",col=2:5, main="Survival by Diagnosis")
survdiff(Surv(time,status) ~ diagnosis, data=BrainCancer)

km.loc <- survfit(Surv(time,status) ~ loc, data=BrainCancer)
plot(km.loc, xlab="Months",ylab="Survival Probability",col=2:5, main="Survival by Location")
survdiff(Surv(time,status) ~ diagnosis, data=BrainCancer)

km.stereo <- survfit(Surv(time,status) ~ stereo, data=BrainCancer)
plot(km.stereo, xlab="Months",ylab="Survival Probability",col=2:5, main="Survival by Stereo") #Note sample size difference
survdiff(Surv(time,status) ~ stereo, data=BrainCancer)

km.ki <- survfit(Surv(time,status) ~ ki, data=BrainCancer)
plot(km.ki,xlab="Months",ylab="Survival Probability",col=2:5, main="Survival by Ki") #Terrible, don't do this



#--Cox Ph
?coxph
#Model form
fit.all <- coxph(Surv(time,status) ~., data=BrainCancer)
summary(fit.all) #Concordance is .794

fit.partial <- coxph(Surv(time,status) ~ diagnosis + loc + (ki) + gtv, data=BrainCancer)
summary(fit.partial) #Concordance is .798

anova(fit.all, fit.partial) #No difference....

#Run some diagnostics

#--proportional hazards assumption

#log log plots, might not do
plot(km.diag, xlab="Months",ylab="Survival Probability",col=2:6, main="Survival by Diagnosis",fun="cloglog")
plot(km.loc, xlab="Months",ylab="Survival Probability",col=2:6, main="Survival by Diagnosis",fun="cloglog")

#schoenfield resids.
zph <- cox.zph(fit.partial)
plot(zph,var=1)
abline(h=0,col="red")

plot(zph,var=2)
abline(h=0,col="red")

plot(zph,var=3)
abline(h=0,col="red")

plot(zph,var=4)
abline(h=0,col="red")

#Also might not run
fit.strat <- update(fit.partial,.~. - diagnosis + strata(diagnosis))
summary(fit.strat)
zph <- cox.zph(fit.strat)
zph

#---Linearity
yhat <- predict(fit.partial)
resid <- residuals(fit.partial,type="martingale")
plot(yhat,resid, xlab="fitted",ylab="residuals")
abline(h=0)

BrainCancer[which(yhat < -2),]

#which one might be nonlinear? (Probably won't run this.)
ionly <- coxph(Surv(time,status) ~ 1, data=BrainCancer)
resid2 <- residuals(ionly,type="martingale")
plot(BrainCancer$gtv,resid2)
abline(h=0)
plot(BrainCancer$ki,resid2)
abline(h=0)


#-Visualize adjusted survival curves

plot(km.diag, xlab="Months",ylab="Survival Probability",col=2:5, main="Survival by Diagnosis")

modaldata <- data.frame(
  diagnosis = levels(BrainCancer$diagnosis),
  sex = rep("Female", 4),
  loc = rep("Supratentorial", 4),
  ki = rep(mean(BrainCancer$ki), 4),
  gtv = rep(mean(BrainCancer$gtv), 4),
  stereo = rep("SRT", 4)
)
survplots <- survfit(fit.partial, newdata = modaldata)
plot(survplots, xlab = "Months",
     ylab = "Survival Probability", col = 2:5)
legend("bottomleft", levels(BrainCancer$diagnosis), col = 2:5, lty = 1)

#-Predict using predict.coxph
?predict.coxph

predictondata <- data.frame(
  diagnosis = "Meningioma",
  sex = "Female",
  loc = "Supratentorial",
  ki = mean(BrainCancer$ki),
  gtv = mean(BrainCancer$gtv),
  stereo = "SRT",
  time=100, #will also do 50 and 200
  status=0 #status isn't used
)
predictondata
nevents <- predict(fit.partial,predictondata,type="expected")
exp(-nevents) 



