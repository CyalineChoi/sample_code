#install and load packages
install.packages("survival")
install.packages("tidyverse")
install.packages("forcats")

library(tidyverse)
library(survival)
library(forcats)

#exam dataset
?cancer
data(cancer)

dim(cancer)
str(cancer)
summary(cancer)
class(cancer)
colnames(cancer)
rownames(cancer)
head(cancer)
tail(cancer)
View(cancer)
describe(cancer)

cor(cancer)

mean(cancer$ph.ecog, na.rm=TRUE)
mean(cancer$time, na.rm=TRUE)
mean(cancer$age, na.rm=TRUE)
mean(cancer$ph.karno, na.rm=TRUE)
mean(cancer$pat.karno, na.rm=TRUE)
median(cancer$ph.ecog, na.rm=TRUE)
median(cancer$time, na.rm=TRUE)
median(cancer$age, na.rm=TRUE)
median(cancer$ph.karno, na.rm=TRUE)
median(cancer$pat.karno, na.rm=TRUE)

table(cancer$ph.ecog)
table(cancer$ph.karno, cancer$pat.karno)
table(cancer$sex, cancer$ph.ecog)

# association between age and status: yes, significant assocaition found
cor(cancer$age,cancer$status)
?wilcox.test()
wilcox.test(cancer$age, cancer$status)
# subset age by 50 years old
class(cancer$sex)

cancer$sex[cancer$sex == "1"] <- "Male"
cancer$sex[cancer$sex == "2"] <- "Female"

cancer %>% 
  filter(age >= 50) %>% 
  mutate(sex) %>% 
  arrange(status, time)

# time to survival - age: the association between the survival time of patients 
# and age is significant.
attach(cancer)
surv_age <- coxph(formula = Surv(time, status) ~ age, data = cancer)
# plot
ggsurvplot(survfit(surv_age), color = "#2E9FDF",data=cancer,
           ggtheme = theme_minimal())
detach(cancer)

# create toy dataset
data <- data.frame("column I" = 1,
                   "COL II" = 2,
                   "col umn THREE x." =3)
#replacing with underscore 
colnames(data) <- gsub(" ", "_", colnames(data))
data
# lowercase
names(data) <- tolower(names(data))
data

#helper function
lower_omitSpace <- function(data){
  colnames(data) <- gsub(" ", "_", colnames(data))
  names(data) <- tolower(names(data)) 
  return (data)
}
lower(data)

# regex: see the bookmark

# PSA
set.seed(64)
df_psa <- data.frame(
  pid = unlist(mapply(rep, 1:5, c(5, 5, 10, 10, 10))),
  psa_date = unlist(mapply(function(x) 
    as.character(seq(as.Date("2000/1/1"), 
                     by = "month", length.out = x)), 
    c(5, 5, 10, 10, 10))),
  psa_value = unlist(mapply(function(x, y) 
    # sort for simplicity
    sort(abs(rnorm(x, mean = y, sd = y*0.5))), 
    c(5, 5, 10, 10, 10), 
    c(1, 5, 4, 2, 10))))

# put mess the PSA value order up for PID 3 
df_psa <- df_psa %>% 
  group_by(pid) %>% 
  mutate(psa_value = ifelse(pid == 3 & row_number() == 1, 
                            median(psa_value), psa_value))

df_psa %>% head(15)
View(df_psa)

# new 



for (pid in 1:5)
{
  df_psa$psa_initv <- df_psa$psa_value[df_psa$psa_date == "2000-01-01"]
#  df_psa$psa_biofailure <- df_psa$psa_value/df_psa$psa_initv
#  df_psa$biofailure[as.numeric(df_psa$psa_biofailure) < 2] <- "0"
#  df_psa$biofailure[as.numeric(df_psa$psa_biofailure) >= 2] <- "1"
#  df_psa$initdate <- df_psa$psa_date[min(which(df_psa$biofailure == 1))]
}
df_psa

# status of biochemical failure: (1,0), 2*initial value, time: earliest time of biochemical failure;
# id=1
df_pid1 <- df_psa %>% 
  filter(pid == 1)
df_pid1$psa_initv <- df_pid1$psa_value[df_pid1$psa_date == "2000-01-01"]
df_pid1$psa_biofailure <- df_pid1$psa_value/df_pid1$psa_initv

df_pid1$biofailure[as.numeric(df_pid1$psa_biofailure) < 2] <- "0"
df_pid1$biofailure[as.numeric(df_pid1$psa_biofailure) >= 2] <- "1"
df_pid1$initdate <- df_pid1$psa_date[min(which(df_pid1$biofailure == 1))]

df_pid1
# id=2
df_pid2 <- df_psa %>% 
  filter(pid == 2)
df_pid2$psa_initv <- df_pid2$psa_value[df_pid2$psa_date == "2000-01-01"]
df_pid2$psa_biofailure <- df_pid2$psa_value/df_pid2$psa_initv

df_pid2$biofailure[as.numeric(df_pid2$psa_biofailure) < 2] <- "0"
df_pid2$biofailure[as.numeric(df_pid2$psa_biofailure) >= 2] <- "1"
df_pid2$initdate <- df_pid2$psa_date[min(which(df_pid2$biofailure == 1))]

df_pid2
# id=3
df_pid3 <- df_psa %>% 
  filter(pid == 3)
df_pid3$psa_initv <- df_pid3$psa_value[df_pid3$psa_date == "2000-01-01"]
df_pid3$psa_biofailure <- df_pid3$psa_value/df_pid3$psa_initv

df_pid3$biofailure[as.numeric(df_pid3$psa_biofailure) < 2] <- "0"
df_pid3$biofailure[as.numeric(df_pid3$psa_biofailure) >= 2] <- "1"
# test if 1 %in% df_pid3$biofailure
1 %in% df_pid3$biofailure
# no biofailure
df_pid3$initdate <- "."

df_pid3

# id=4
df_pid4 <- df_psa %>% 
  filter(pid == 4)
df_pid4$psa_initv <- df_pid4$psa_value[df_pid4$psa_date == "2000-01-01"]
df_pid4$psa_biofailure <- df_pid4$psa_value/df_pid4$psa_initv

df_pid4$biofailure[as.numeric(df_pid4$psa_biofailure) < 2] <- "0"
df_pid4$biofailure[as.numeric(df_pid4$psa_biofailure) >= 2] <- "1"
# test if 1 %in% df_pid4$biofailure
1 %in% df_pid4$biofailure
# there's biofailure
df_pid4$initdate <- df_pid4$psa_date[min(which(df_pid4$biofailure == 1))]
df_pid4

# id=5
df_pid5 <- df_psa %>% 
  filter(pid == 5)
df_pid5$psa_initv <- df_pid5$psa_value[df_pid5$psa_date == "2000-01-01"]
df_pid5$psa_biofailure <- df_pid5$psa_value/df_pid5$psa_initv

df_pid5$biofailure[as.numeric(df_pid5$psa_biofailure) < 2] <- "0"
df_pid5$biofailure[as.numeric(df_pid5$psa_biofailure) >= 2] <- "1"
# test if 1 %in% df_pid5$biofailure
1 %in% df_pid5$biofailure
# there's biofailure
df_pid5$initdate <- df_pid5$psa_date[min(which(df_pid5$biofailure == 1))]
df_pid5

#merge datasets to a full one
df_psa_new <- rbind(df_pid1, df_pid2, df_pid3, df_pid4, df_pid5)

