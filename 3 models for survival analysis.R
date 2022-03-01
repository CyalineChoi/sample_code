install.packages("ranger")
install.packages("ggfortify")

library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
#dataset
veteran
# The first thing to do is to use Surv() to build the standard survival object. 
# The variable time records survival time; 
# status indicates whether the patient’s death was observed (status = 1) or that survival time was censored (status = 0).
# Note that a “+” after the time in the print out of km indicates censoring.
km <- with(veteran, Surv(time, status))
# fit km survival model
km_fit <- survfit(Surv(time, status) ~ 1, data=veteran)
summary(km_fit, times = c(1,30,60,90*(1:10)))
# base graphics is always ready
autoplot(km_fit)
# plot(km_fit, xlab="Days", main = 'Kaplan Meyer Plot') 
# survival curves by treatment
km_trt_fit <- survfit(Surv(time, status) ~ trt, data=veteran)
# plot(km_trt_fit, xlab="Days", main = 'Kaplan Meyer Plot') 
autoplot(km_trt_fit)
# stratified age "less than 60" and "over 60"
vet <- mutate(veteran, AG = ifelse((age < 60), "<60", ">60"),
              AG = factor(AG),
              trt = factor(trt,labels=c("standard","test")),
              prior = factor(prior,labels=c("N0","Yes")))

km_AG_fit <- survfit(Surv(time, status) ~ AG, data=vet)
autoplot(km_AG_fit) 
# Fit Cox Proportional Hazards Model
cox <- coxph(Surv(time, status) ~ trt + celltype + karno + diagtime + age + prior, data = vet)
summary(cox)
cox_fit <- survfit(cox)
#plot(cox_fit, main = "cph model", xlab="Days")
autoplot(cox_fit)
# assess model performance
aa_fit <-aareg(Surv(time, status) ~ trt + celltype + karno + diagtime + age + prior, data = vet)
aa_fit
# provides a more complete summary of results
summary(aa_fit)  
autoplot(aa_fit)
# fit ranger model - random forest
r_fit <- ranger(Surv(time, status) ~ trt + celltype + karno + diagtime + age + prior,
                data = vet,
                mtry = 4,
                importance = "permutation",
                splitrule = "extratrees",
                verbose = TRUE)
# Average the survival models
death_times <- r_fit$unique.death.times 
surv_prob <- data.frame(r_fit$survival)
avg_prob <- sapply(surv_prob,mean)
# Plot the survival models for each patient
plot(r_fit$unique.death.times,r_fit$survival[1,], 
     type = "l", 
     ylim = c(0,1),
     col = "red",
     xlab = "Days",
     ylab = "survival",
     main = "Patient Survival Curves")
#
cols <- colors()
for (n in sample(c(2:dim(vet)[1]), 20)){
  lines(r_fit$unique.death.times, r_fit$survival[n,], type = "l", col = cols[n])
}
lines(death_times, avg_prob, lwd = 2)
legend(500, 0.7, legend = c('Average = black'))
# ranger() ranks variable importance
vi <- data.frame(sort(round(r_fit$variable.importance, 4), decreasing = TRUE))
names(vi) <- "importance"
vi
# Harrell’s c-index 
cat("Prediction Error = 1 - Harrell's c-index = ", r_fit$prediction.error)
# compared 3 plots
# Set up for ggplot
kmi <- rep("KM",length(km_fit$time))
km_df <- data.frame(km_fit$time,km_fit$surv,kmi)
names(km_df) <- c("Time","Surv","Model")

coxi <- rep("Cox",length(cox_fit$time))
cox_df <- data.frame(cox_fit$time,cox_fit$surv,coxi)
names(cox_df) <- c("Time","Surv","Model")

rfi <- rep("RF",length(r_fit$unique.death.times))
rf_df <- data.frame(r_fit$unique.death.times,avg_prob,rfi)
names(rf_df) <- c("Time","Surv","Model")

plot_df <- rbind(km_df,cox_df,rf_df)

p <- ggplot(plot_df, aes(x = Time, y = Surv, color = Model))
p + geom_line()




