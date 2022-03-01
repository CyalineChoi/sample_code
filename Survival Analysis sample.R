install.packages("devtools")
library(devtools)
install_github("OpenIntroStat/OIsurv")
library(OIsurv)
library(survival)
# right-censored data
data(tongue)
attach(tongue)  
# create a subset for just the first group by using [type==1]
my.surv.object <- Surv(time[type==1], delta[type==1])
my.surv.object
detach(tongue)
#left-censored data
data(psych) 
attach(psych)
my.surv.object <- Surv(age, age+time, death)
my.surv.object
detach(psych)
# KM model
data(tongue)
attach(tongue)
my.surv <- Surv(time[type==1], delta[type==1])
my.fit <- survfit(my.surv ~ 1)
summary(my.fit)$surv
summary(my.fit)$time
summary(my.fit)$n.risk
summary(my.fit)$n.event
summary(my.fit)$std.err  # standard error of the K-M estimate at {t_i}
summary(my.fit)$lower
str(my.fit)
str(summary(my.fit)) # full summary of the my.fit object
# plot
plot(my.fit, main="Kaplan-Meier estimate with 95% confidence bounds",
         xlab = "time", ylab="survival function")
autoplot(my.fit)
# KM model 2
my.fit1 <- survfit( Surv(time, delta) ~ type ) 
detach(tongue)
# try another dataset
data(bmt)
attach(bmt)
my.surv <- Surv(t2[group==1], d3[group==1])
my.cb <- confBands(my.surv, confLevel=0.95, type="hall")
plot(survfit(my.surv ~ 1), xlim=c(100, 600), xlab="time",
       ylab="Estimated Survival Function",
       main="Reproducing Confidence Bands for Example 4.2 in Klein/Moeschberger")
lines(my.cb$time, my.cb$lower, lty=3, type="s")
lines(my.cb$time, my.cb$upper, lty=3, type="s")
legend(100, 0.3, legend=c("K-M survival estimate",
                          "pointwise intervals","confidence bands"), lty=1:3)
detach(bmt)

#cumulative hazard model
data(tongue); attach(tongue)
my.surv <- Surv(time[type==1], delta[type==1])
my.fit  <- summary(survfit(my.surv ~ 1))
H.hat   <- -log(my.fit$surv)
H.hat   <- c(H.hat, tail(H.hat, 1))
#plot
h.sort.of <- my.fit$n.event / my.fit$n.risk
H.tilde   <- cumsum(h.sort.of)
H.tilde   <- c(H.tilde, tail(H.tilde, 1)) 
plot(c(my.fit$time, 250), H.hat, xlab="time", ylab="cumulative hazard",
       main="comparing cumulative hazards", ylim=range(c(H.hat, H.tilde)), type="s")
points(c(my.fit$time, 250), H.tilde, lty=2, type="s")
legend("topleft", legend=c("H.hat","H.tilde"), lty=1:2)
detach(tongue)
# KM model 3 with different dataset
data(drug6mp)
attach(drug6mp)
my.surv <- Surv(t1, rep(1, 21))   # all placebo patients observed
my.surv2 <- survfit(my.surv ~ 1)
print(my.surv2, print.rmean=TRUE)
detach(drug6mp)
# two samples survival analysis
data(btrial); attach(btrial)       # time variable warning omitted
survdiff(Surv(time, death) ~ im)
survdiff(Surv(time, death) ~ im, rho=1)
detach(btrial)
# Cox proportional hazards modelm [constant covariates]
data(burn); attach(burn)
summary(burn)
my.surv <- Surv(T1, D1)
coxph.fit <- coxph(my.surv ~ Z1 + as.factor(Z11), method="breslow")
coxph.fit
# 
co <- coxph.fit$coefficients  # may use coxph.fit$coeff instead
va <- coxph.fit$var           # I^(-1), estimated cov matrix of the estimates
ll <- coxph.fit$loglik        # log-likelihood for alt and null MLEs, resp.
my.survfit.object <- survfit(coxph.fit)
detach(burn)
# Cox proportional hazards modelm [time dependent covariates]
data(relapse)
N  <- dim(relapse)[1] # 300 observations
t1 <- rep(0, N+sum(!is.na(relapse$int)))  # initialize start time at 0
t2 <- rep(-1, length(t1)) # build vector for end times
d  <- rep(-1, length(t1)) # whether event was censored
g  <- rep(-1, length(t1)) # gender covariate
i  <- rep(FALSE, length(t1)) # initialize intervention at FALSE
j <-1
for(ii in 1:dim(relapse)[1]){
  if(is.na(relapse$int[ii])){
    t2[j] <- relapse$event[ii]
    d[j]  <- relapse$delta[ii]
    g[j]  <- relapse$gender[ii]
    j<-j+1
  } else {
    g[j+0:1] <- relapse$gender[ii] # gender is same for each time
    d[j] <-0 # no relapse observed pre-intervention 
    d[j+1] <- relapse$delta[ii] # relapse occur post-intervention?
    i[j+1] <- TRUE # intervention covariate, post-intervention 
    t2[j] <- relapse$int[ii]-1 # end of pre-intervention
    t1[j+1] <- relapse$int[ii]-1 
    t2[j+1] <- relapse$event[ii] # end of post-intervention 
    j<-j+2
  }
}
mySurv <- Surv(t1, t2, d)        # pg 3 discusses left-trunc. right-cens. data
myCPH  <- coxph(mySurv ~ g + i)
detach(relapse)
# Accelerated failure-time models
data(larynx)
attach(larynx)
srFit <- survreg(Surv(time, delta) ~ as.factor(stage) + age, dist="weibull")
summary(srFit)
# exponential model
srFitExp <- survreg(Surv(time, delta) ~ as.factor(stage) + age, dist="exponential")
summary(srFitExp)
srFitExp$coeff
srFitExp$icoef
srFitExp$var
srFitExp$loglik
srFit$scale
detach(larynx)