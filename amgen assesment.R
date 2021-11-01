# Setup
install.packages("ggplot2")
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyverse")
options(scipen=999)  # turn off scientific notation like 1e+06
library(ggplot2)
library("readxl")
library(tidyverse)
# load the data
df <- read_excel("R_Python_interview questions_v4.xlsx", sheet="Longitudinal")
View(df)
# Q1 Ggplot
df$timepoint <- factor(df$timepoint, levels=c("DAY1", "DAY8", "DAY15","DAY22","DAY29"))
ggplot(df, aes(x=timepoint, y=analyte_value,group=subject))+ 
  geom_line(aes(color=subject))+
  geom_point(aes(color=subject))+
  facet_wrap(~marker)+
  labs(title="Analyte value in each time point for each subject", y="Analyte value", x="time point")
# Q2 Ggplot
library(dplyr)

df_new <- df %>% 
  group_by(timepoint, treatment_group, subject) %>% 
  summarise(analyte_value_mean = mean(analyte_value), analyte_value_sd = sd(analyte_value))
ggplot(df_new, aes(x=timepoint, y=analyte_value_mean,group=subject))+ 
  geom_line(aes(color=treatment_group))+
  geom_errorbar(aes(ymin=analyte_value_mean-analyte_value_sd, ymax=analyte_value_mean+analyte_value_sd, color=treatment_group), width=.2)+
  #facet_wrap(~treatment_group)+
  labs(title="Mean and Standard deviation for analyte value in each time point for each treatment group", y="Analyte value", x="time point")

# Q3
#subset
df_d1 <- subset(df, df$timepoint=="DAY1")
df_d8 <- subset(df, df$timepoint=="DAY8")
df_d15 <- subset(df, df$timepoint=="DAY15")
df_d22 <- subset(df, df$timepoint=="DAY22")
df_d29 <- subset(df, df$timepoint=="DAY29")
#rename
df_1 <- plyr::rename(df_d1, c("analyte_value" = "day1"))
df_2 <- plyr::rename(df_d8, c("analyte_value" = "day8"))
df_3 <- plyr::rename(df_d15, c("analyte_value" = "day15"))
df_4 <- plyr::rename(df_d22, c("analyte_value" = "day22"))
df_5 <- plyr::rename(df_d29, c("analyte_value" = "day29"))
#delete column
df_1 <- subset (df_1, select = -timepoint)
df_2 <- subset (df_2, select = -timepoint)
df_3 <- subset (df_3, select = -timepoint)
df_4 <- subset (df_4, select = -timepoint)
df_5 <- subset (df_5, select = -timepoint)
#merge
multiFull <- merge(merge(merge(merge(
  df_1,
  df_2, all = TRUE),
  df_3, all = TRUE),
  df_4, all = TRUE),
  df_5, all = TRUE)
#export to csv
write.csv(multiFull,"Q3.csv", row.names = FALSE)
#Q4
#subset
C4 <- subset(multiFull, multiFull$marker=="C4")
C8 <- subset(multiFull, multiFull$marker=="C8")
TG <- subset(multiFull, multiFull$marker=="TG")
#difference pre and post
t.test(C4$day1, C4$day8, paired = TRUE)
t.test(C8$day1, C8$day8, paired = TRUE)
t.test(TG$day1, TG$day8, paired = TRUE)

#Q5
install.packages("ggpubr")
install.packages("rstatix")
library(tidyverse)
library(ggpubr)
library(rstatix)

#AE Questions
# load the data
AE <- read_excel("R_Python_interview questions_v4.xlsx", sheet="AE")
HO <- read_excel("R_Python_interview questions_v4.xlsx", sheet="HO")
View(AE)
View(HO)
#Q1
#merge AE and HO
AE[with(AE, order("ae_start_time_num","subject")), ] 
HO[with(HO, order("ho_start_time_num","subject")), ] 
total <- merge(AE, HO,by.x = "subject",all.x = TRUE)
#convert date to numeric
total$ae_start_time_num <- gsub("[a-zA-Z ]", "", total$ae_start_time)
total$ho_start_time_num <- gsub("[a-zA-Z ]", "", total$ho_start_time)
total$ae_start_time_num <- as.numeric(total$ae_start_time_num)
total$ho_start_time_num <- as.numeric(total$ho_start_time_num)
#setup the new column
ICU <- subset(total,total$ward == "ICU") 
ICU$AE_ICU <- 0
ICU$AE_ICU[ICU$ho_start_time_num - ICU$ae_start_time_num == -1] <- 1
ICU$AE_ICU[ICU$ho_start_time_num - ICU$ae_start_time_num == 0] <- 1
ICU$AE_ICU[ICU$ho_start_time_num - ICU$ae_start_time_num == 1] <- 1
ICU$AE_ICU[ICU$ho_start_time_num - ICU$ae_start_time_num == 2] <- 1
ICU$AE_ICU[ICU$ho_start_time_num - ICU$ae_start_time_num == 3] <- 1
general <- subset(total,total$ward == "general")
general$AE_ICU <- 0
total_new <- rbind(ICU, general)
#export AEQ1 dataset
write.csv(total_new,"AEQ1.csv", row.names = FALSE)

#AEQ2
require(data.table) 
total_new <- as.data.table(total_new)
total_ae_highest_grade <- total_new[total_new[, .I[which.max(ae_grade)], by=subject]$V1]
#indicate if the AE grade is >= 3
total_ae_highest_grade$max_ae_grade <- 0
total_ae_highest_grade$max_ae_grade[total_ae_highest_grade$ae_grade == 3] <- 1
#merge for each subject
#find unique in longditutional dataset
total_subject <- unique(df$subject, incomparables = FALSE)
total_subject <- data.frame(matrix(unlist(total_subject), nrow=length(total_subject), byrow=TRUE))
total_subject <- plyr::rename(total_subject, c("matrix.unlist.total_subject...nrow...length.total_subject...byrow...TRUE." = "subject"))
print(total_subject)
total_ae <- merge(total_subject, total_ae_highest_grade, by="subject",all.x = TRUE)
total_ae$ae_grade[is.na(total_ae$ae_grade)] <- 0
#export AEQ2 dataset
write.csv(total_ae,"AEQ2_1.csv", row.names = FALSE)

#AEQ3 
#merge
master <- merge(df, total_ae, by="subject",all.x = TRUE)
#Ggplot
ggplot(master, aes(x=timepoint, y=analyte_value,group=subject))+ 
  geom_line(aes(color=treatment_group))+
  geom_point(aes(shape=as.factor(ae_grade)), size=2.5)+
  facet_wrap(~marker)+
  labs(title="Marker subgroup visualization for analyte value in each time point for each treatment group", y="Analyte value", x="time point")


 
 
 
 
 
