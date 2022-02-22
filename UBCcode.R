install.packages("ggplot2")
install.packages("epitools", dependencies = TRUE)
install.packages("car", dependencies = TRUE)
install.packages("emmeans", dependencies = TRUE)
install.packages("binom", dependencies = TRUE) # optional
install.packages("tidyr", dependencies = TRUE)  # only if not yet installed
install.packages("survival", dependencies = TRUE)   # only if not yet installed
install.packages("survminer", dependencies = TRUE)  # only if not yet installed

library(dplyr)
library(readr)
library(ggplot2)
library(epitools)
library(car)
library(nlme)
library(emmeans)
library(ggformula) # optional
library(MASS)
library(tidyr) # optional
library(boot)
library(survival)
library(survminer)

#lab1
titanicData <- read_csv("titanic.csv")
summary(titanicData)
#display an exsiting column
titanicData$age
#add a new column
titanicData$log_age = log(titanicData$age)
#test new column
head(titanicData)
#subset
titanicDataFemalesOnly <- filter(titanicData, sex == "female")

#lab2
#q1
ACE <- c(640, 1070, 780, 70, 160, 130, 60, 50, 2110, 70, 350, 30, 210, 90, 470, 580, 250, 310, 460, 430, 140, 1070, 130)
mean(ACE)
#420 ng/L
ACE_pool <- ACE/4000
ACE_pool
mean(ACE_pool)
#1.05 ng ACE / L
mean(ACE_pool)*4000
sum(ACE)/length(ACE)
#500,000 L pool:
mean(ACE_pool)*500000
#52500 ml 

#q2
d_dive <- c(71.0, 77.3, 82.6, 96.1, 106.6, 112.8, 121.2, 126.4, 127.5, 143.1)
n_dive <- c(42.2, 51.7, 59.8, 66.5, 81.9, 82.0, 81.3, 81.3, 96.0, 104.1)
#number of animal - same
length(d_dive)
length(n_dive)
#diff
MetabolismDifference <- d_dive - n_dive
#mean
mean(MetabolismDifference)
#ratio
MetabolismRatio <- d_dive/n_dive
log(MetabolismRatio)
mean(log(MetabolismRatio))

#q3
countries <- read_csv("countries.csv", stringsAsFactors = TRUE)
summary(countries)
filter(countries,continent=="Africa")
#54
#continents - char
#cell_phone_subscriptions_per_100_people_2012 - num
#total_population_in_thousands_2015 - num
#fines_for_tobacco_advertising_2014 - char
countries$diff <- countries$ecological_footprint_2012 - countries$ecological_footprint_2000
mean(countries$diff, na.rm = TRUE)
#subset Africa
AfricaData <- filter(countries,continent=="Africa")
sum(AfricaData$total_population_in_thousands_2015)

#lab3
ggplot(titanicData, aes(x=age)) + geom_histogram()
ggplot(titanicData, aes(x=sex)) + geom_bar(stat="count")
ggplot(titanicData, aes(x=sex, y=age)) + geom_boxplot()
ggplot(titanicData, aes(x = sex, y = sex)) + geom_point()
ggplot(titanicData,   
       aes(x = age, y = sex)) +
  geom_point() +
  xlab("Age") + 
  ylab("Gender")+
  theme_classic()
#q1
#describe - measles_immunization_oneyearolds 
ggplot(countries, aes(x=measles_immunization_oneyearolds)) + geom_histogram() +
  xlab("  percentage of 1-year-olds that have been vaccinated against measles") 
#continent & countries
ggplot(countries, aes(x=continent)) + geom_bar(stat="count")
#scatter plot - life_expectancy_at_birth_male / life_expectancy_at_birth_female
ggplot(countries, aes(x = life_expectancy_at_birth_male, y = life_expectancy_at_birth_female)) + geom_point()
#scatter plot - ecological_footprint_2000 and ecological_footprint_2012
ggplot(countries, aes(x = ecological_footprint_2000, y = ecological_footprint_2012)) + geom_point() +
  geom_abline(intercept = 0, slope = 1) 
#box plot - continent and female life expectancy at birth
ggplot(countries, aes(x = continent, y = life_expectancy_at_age_60_female)) + geom_boxplot()
#q5

#lab5
caffeine <- read_csv("caffeine.csv")
mean(caffeine$caffeine_mg_16oz)
#188.0643
t.test(caffeine$caffeine_mg_16oz)$conf.int
# 167.5237 208.6049
ggplot(caffeine, aes(x = caffeine_mg_16oz)) + geom_histogram()
sd(caffeine$caffeine_mg_16oz, na.rm = TRUE) / mean(caffeine$caffeine_mg_16oz,na.rm = TRUE)*100

t.test(caffeine$caffeine_mg_16oz,conf.level = 0.99)$conf.int
#159.4238 216.7047
quantile(caffeine$caffeine_mg_16oz, c(0.025, 0.975), na.rm =TRUE)
#144.765 254.685 

#lab6
#binomial
install.packages("binom", dependencies = TRUE)
library(binom)
binom.confint(x = 30, n = 87, method = "ac")
binom.test(x = 14, n = 18, p = 0.05)
#chi square
MandMlist <- read_csv("MandMlist.csv")
MMtable <- table(MandMlist$color)
expected_proportions <- c(0.24, 0.14, 0.16, 0.20, 0.13, 0.13)
55 * expected_proportions #55 total M and M beans
chisq.test(MMtable, p = expected_proportions)
#Poisson
MassExtinctions <- read_csv("MassExtinctions.csv")
number_of_extinctions <- MassExtinctions$numberOfExtinctions
#frequency
table(number_of_extinctions)
mean(number_of_extinctions)
#4.210526
dpois(x = 3, lambda = 4.210526)
#There is a 18.5% chance to find out the number of exinction is 3. 
0:20
expected_probability <- dpois(x = 0:20, lambda = 4.21)
expected_probability
length(number_of_extinctions) * expected_probability
#combine 0-1, and 8+ for expected frequencies
expected_combined <- c(5.878568, 9.999264, 14.032300, 14.768996, 12.435494,  8.725572, 5.247808, 4.911998)
observed_combined <- c(13, 15, 16, 7, 10, 4, 2, 9)
chisq.test(observed_combined, p = expected_combined, rescale.p = TRUE)$statistic
pchisq(q = 23.93919, df = 6, lower.tail = FALSE)
#q1
binom.confint(x = 6, n = 510, method = "ac")
# [0.005, 0.03]
binom.confint(x = 0, n = 300, method = "ac")
# [-0.003, 0.02]

#q2
stockings <- read_csv("stockings.csv")
table(stockings)
#expected frenquecies: 52/4=13
expected <- c(0.25,0.25,0.25,0.25)
observed <- c(6,9,16,21)
chisq.test(x = observed, p = expected)
#reject the null, the frequency is not equal for 4 different positions.

#q3
soccer_birth_quarter <- read_csv("soccer_birth_quarter.csv")
table(soccer_birth_quarter)
ggplot(soccer_birth_quarter, aes(x=birth_quarter)) + geom_bar(stat="count")
# more August born athelets
Canadian_births <- read_csv("Canadian_births.csv")
# condense Canadian_births to 3 month interval
canada_births_proportion <- c(0.236,0.2464, 0.2615, 0.2561)
soccer_birth <- c(135, 37, 22, 94)
chisq.test(x = soccer_birth, p = canada_births_proportion)
#reject the null hypothesis, that the distribution of soccer athelet birthdays is not equally drawn from Canadian population

#q4
hospital <- read_csv("cardiac arrests out of hospital.csv")
table(hospital)
mean(hospital$out_of_hospital_cardiac_arrests)
# 2.015326
dpois(x=0, lambda = 2.015326)
# 13.32769% chance of having 0 heart attack in a week
expected <- c(34.785295,70.103698,70.640891,47.454800,23.909219,9.636973,4.469124)
observed <- c(36,79,60,41,28,10,7)
chisq.test(x = observed, p = expected, rescale.p = TRUE)$statistic
pchisq(q = 5.799068, df = 5, lower.tail = FALSE)
# yes, the frequency distribution of out-of-hospital cardiac events follow a Poisson distribution.

#lab7 
titanicData <- read_csv("titanic.csv", stringsAsFactors=TRUE)
titanicData$survive <- factor(titanicData$survive, levels = c("yes", "no"))
levels(titanicData$survive)
titanicData$sex <- factor(titanicData$sex, levels = c("female", "male"))
levels(titanicData$sex)
#frequency table
sex_survive_table <- table(titanicData$sex, titanicData$survive)
sex_survive_table
sex_survive_table_direct <- data.frame(yes = c(307,142), no = c(156,708),
                                       row.names = c("female","male"))
sex_survive_table_direct
#plot
mosaicplot(sex_survive_table)
mosaicplot(sex_survive_table, color = c("darkred", "gold"), xlab ="Sex", ylab = "Survival")
#Odds Ratio
sex_survive_fisher <- fisher.test(sex_survive_table)
sex_survive_fisher$estimate
# odds ratio: 9.792284, 95%CI [7.471136, 12.887759]
sex_survive_fisher$conf.int
#expected table
chisq.test(sex_survive_table)$expected
#chi square contengency test
chisq.test(sex_survive_table, correct=FALSE)
# p<0.0001, reject the null hypothesis of no association between sex and survival on the Titanic.
# same as results in [fisher.test(sex_survive_table)]
#q1
oswego <- read_csv("oswego.csv")
table(oswego$ill)
oswego$ill <- factor(oswego$ill, levels = c("Healthy", "Sick"))
levels(oswego$ill)
table_friutsalad <- table(oswego$fruit_salad, oswego$ill)
chisq.test(table_friutsalad)$expected
#cell less than 5, cannot use chi square
#fisher exact test to be used
#spinach, baked ham, vanilla ice cream and chocolate ice cream
spinach <- table(oswego$spinach, oswego$ill)
basck_ham <- table(oswego$baked_ham, oswego$ill)
vanilla_ice <- table(oswego$vanilla_ice_cream, oswego$ill)
choco_ice <- table(oswego$chocolate_ice_cream, oswego$ill)
fisher.test(spinach) #not spinach
fisher.test(basck_ham) #not baked ham
fisher.test(vanilla_ice) #yesssss
fisher.test(choco_ice) #not chocolate ice cream
#95% CI [5.216064, 138.865034]
mosaicplot(vanilla_ice, color = c("darkred", "gold"), xlab ="vanilla_ice_cream", ylab = "ill")

#q3 Shufflebottoms
shufflebottoms <- read_csv("shufflebottoms.csv")

table <- table(shufflebottoms$name, shufflebottoms$movement_type)
fisher.test(table)
#OR=1.061703, 95% CI[0.3269777, 3.4491262]

#epitools
RR <- riskratio(t(table), rev = "both", method = "wald")
OR <- oddsratio(table, method = "wald")
RR$measure[2,]
OR$measure[2,]

#lab8 
#q1
normal_vector <- rnorm(n = 2500, mean = 15, sd = 3)
hist(normal_vector)
qqnorm(normal_vector)
#q2
bumpus <- read_csv("bumpus.csv")
ggplot(bumpus, aes(x=total_length_mm)) + geom_bar(stat="count")
qqnorm(bumpus$total_length_mm)
mean(bumpus$total_length_mm)
t.test(bumpus$total_length_mm)$conf.int
# 158.9403 160.1480
#q5
mammals <- read_csv("mammals.csv")
hist(mammals$body_mass_kg)
#logged - yes, normal distribution
hist(log(mammals$body_mass_kg))

#lab9
ggplot(lizard, aes(x = squamosalHornLength)) + 
  geom_histogram(fill = "firebrick", col = "black", binwidth = 2, 
                 boundary = 0, closed = "left") +
  facet_wrap( ~ Survival, ncol = 1, scales = "free_y") +
  labs(x = "Horn length (mm)", y = "Frequency") + 
  theme_classic()
# 95% CI for difference between means
t.test(squamosalHornLength ~ Survival, data = lizard, var.equal = TRUE)$conf.int
# two sample t test with same variance
t.test(squamosalHornLength ~ Survival, data = lizard, var.equal = TRUE)
#ggplot error bar
ggplot(chinook, aes(x = troutTreatment, y = proportionSurvived)) +
  geom_point(color = "firebrick", size = 3, shape = 1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.1, position=position_nudge(x = 0.15)) +
  stat_summary(fun.y = mean, geom = "point", color = "firebrick",
               size = 3, position=position_nudge(x = 0.15)) +
  labs(x = "Treatment", y = "Proportion surviving") + 
  theme_classic()
#two sample t test with different variance - Welch's
t.test(proportionSurvived ~ troutTreatment, data = chinook, var.equal = FALSE)

ggplot(titanicData, aes(x = survive, y = age)) + geom_jitter(position = position_jitter(0.05)) + theme_minimal()
ggplot(titanicData, aes(x = age)) +   
  geom_histogram() + 
  facet_wrap(~ survive, ncol = 1)
ggplot(titanicData, aes(x=survive, y=age, fill = survive)) + 
  geom_violin() +
  xlab("Survival") + ylab("Age") + 
  theme_classic()+scale_fill_manual(values=c("#FFB531","#BC211A"))+ 
  stat_summary(fun.y=mean,  geom="point", color="black")+ 
  theme(legend.position="none")+ 
  theme(aspect.ratio=1)
t.test(age ~ survive, data = titanicData, var.equal = TRUE)
t.test(age ~ survive, data = titanicData, var.equal = FALSE)
leveneTest(data = titanicData, age ~ survive, center = mean)

#q1
ggplot(bumpus, aes(x = weight_g)) +   
  geom_histogram() + 
  facet_wrap(~ survival, ncol = 1)
t.test(bumpus$weight_g ~ bumpus$survival, var.equal = FALSE)
# 95% CI: 0.1351376 1.1339597
leveneTest(data = bumpus, weight_g ~ survival, center = mean)
# no variance difference
#diff plot
countries$d <- countries$ecological_footprint_2012 - countries$ecological_footprint_2000
hist(countries$d)
t.test(countries$ecological_footprint_2012, countries$ecological_footprint_2000, paird = TRUE)
#reject null, there's differences in the ecological footprint between 2000 and 2012, which is decreased
#q3
leg_shaving <- read_csv("leg shaving.csv")
t.test(leg_shaving$hair_width_change_control, leg_shaving$hair_width_change_test, paird = TRUE)
#cannot reject null, no change on the thickness
t.test(leg_shaving$hair_width_change_control, leg_shaving$hair_width_change_test, paird = TRUE)$conf.int
#95% CI: -11.63304  29.23304
t.test(leg_shaving$hair_growth_change_control, leg_shaving$hair_growth_change_test, paird = TRUE)
# no change for growing rate

#lab10
ggplot(titanicData, aes(x = age)) +   
  geom_histogram() + 
  facet_wrap(~ passenger_class, ncol = 1) 

titanic_by_passenger_class <- group_by(titanicData,passenger_class)
summarise(titanic_by_passenger_class, group_mean = mean(age, na.rm=TRUE))
summarise(titanic_by_passenger_class, group_mean = mean(age, na.rm=TRUE), group_sd = sd(age, na.rm=TRUE))

titanicANOVA <- lm(age ~ passenger_class, data = titanicData)
anova(titanicANOVA)
TukeyHSD(aov(titanicANOVA))

kruskal.test(age ~ passenger_class, data = titanicData)
#q1
cuckooeggs <- read_csv("cuckooeggs.csv")
ggplot(cuckooeggs, aes(x = egg_length)) +   
  geom_histogram() + 
  facet_wrap(~ host_species, ncol = 1)
egglength_by_species <- group_by(cuckooeggs,host_species)
summarise(egglength_by_species, group_mean = mean(egg_length, na.rm=TRUE), group_sd = sd(egg_length, na.rm=TRUE))
ANOVA <- lm(egg_length ~ host_species, data = cuckooeggs)
anova(ANOVA)
#reject null, yes they are different lengthes
TukeyHSD(aov(ANOVA))
# Meadow Pipit-Hedge Sparrow
# Wren-Hedge Sparrow
# Tree Pipit-Meadow Pipit 
# Wren-Meadow Pipit 
# Wren-Pied Wagtail
# Wren-Robin 
# Wren-Tree Pipit
#q2
mm <- read_csv("malaria vs maize.csv")
ggplot(mm, aes(x = incidence_rate_per_ten_thousand)) +   
  geom_histogram() + 
  facet_wrap(~ maize_yield, ncol = 1)
MMtable <- group_by(mm,maize_yield)
summarise(MMtable, group_mean = mean(incidence_rate_per_ten_thousand, na.rm=TRUE), group_sd = sd(incidence_rate_per_ten_thousand, na.rm=TRUE))
# the High maize yield has much higher mean and sd for the incidence rate
summarise(MMtable, group_mean = mean(log(incidence_rate_per_ten_thousand), na.rm=TRUE), group_sd = sd(log(incidence_rate_per_ten_thousand), na.rm=TRUE))
ANOVA1 <- lm(log(incidence_rate_per_ten_thousand) ~ maize_yield, data = mm)
anova(ANOVA1)
#reject the null, different maize yield different incidence of malaria
TukeyHSD(aov(ANOVA1))
# Low-High , Medium-Low 
kruskal.test(incidence_rate_per_ten_thousand ~ maize_yield, data = mm)
#q3
circadian <- read_csv("circadian mutant health.csv")
ggplot(circadian, aes(x = days_to_death)) +   
  geom_histogram() + 
  facet_wrap(~ genotype, ncol = 1)
circadianTable <- group_by(circadian,genotype)
summarise(circadianTable, group_mean = mean(days_to_death, na.rm=TRUE), group_sd = sd(days_to_death, na.rm=TRUE))
summarise(circadianTable, group_mean = mean(log(days_to_death), na.rm=TRUE), group_sd = sd(log(days_to_death), na.rm=TRUE))
# not normally distributed
kruskal.test(days_to_death ~ genotype, data = circadian)
#reject the null, lifespan differs between the three groups of flies

#lab11
#plot a line
ggplot(lion, aes(proportionBlack, ageInYears)) + 
  geom_point(size = 3, col = "firebrick") + 
  geom_smooth(method = "lm", se = FALSE, col = "black") + #fit a line
  labs(x = "Proportion black", y = "Age (years)") + 
  theme_classic()
#display 95% CI band
ggplot(lion, aes(proportionBlack, ageInYears)) + 
  geom_smooth(method = "lm", se = TRUE, col = "black") + #95% CI bankd
  geom_point(size = 3, col = "firebrick") + 
  labs(x = "Proportion black", y = "Age (years)") + 
  theme_classic()
#spline
ggplot(desertFish, aes(poolArea, nFishSpecies)) + 
  geom_point(size = 3, col = "firebrick") + 
  geom_spline(col = "black", df = 2) +
  labs(x = expression(Area~(m^2)), y = "Number of species") + 
  theme_classic()
#plot residuals
irisRegression <- lm(grainsDeposited ~ tubeLengthMm, data = iris)
iris$residuals <- residuals(irisRegression)
ggplot(iris, aes(tubeLengthMm, residuals)) + 
  geom_point(size = 3, col = "firebrick") + 
  geom_hline(yintercept=0, color = "black") +
  labs(x = "Floral tube length (mm)", y = "Residuals") + 
  theme_classic()
#plot curve 
ggplot(phytoplankton, aes(ironConcentration, phytoGrowthRate)) + 
  geom_smooth(method = "nls", method.args = 
                list(formula = y ~ a * x / (b + x), start = list(a = 1, b = 1)), 
              data = phytoplankton, se = FALSE, col = "black") + 
  geom_point(size = 3, col = "firebrick") + 
  labs(x = expression(paste("Iron concentration (", mu, "mol)")), 
       y = "Specific growth rate (no./day)") + 
  theme_classic()

#q1
telomere <- read_csv("telomere inheritance.csv")
ggplot(telomere, aes(father_telomere_length, offspring_telomere_length)) + 
  geom_point(size = 3, col = "firebrick") + 
  geom_smooth(method = "lm", se = FALSE, col = "black") + #fit a line
  labs(x = "father and offspring", y = "Length") + 
  theme_classic()
# no need for transformation
regression <- lm(offspring_telomere_length ~ father_telomere_length, data=telomere)
# offspring_telomere_length = 0.9792 * father_telomere_length + 0.2252

#q2
numberline <- read_csv("numberline.csv")
ggplot(numberline, aes(fourth_graders_guess, true_value)) + 
  geom_point(size = 3, col = "firebrick") + 
  geom_smooth(method = "lm", se = FALSE, col = "black") + #fit a line
  labs(x = "correlation", y = "correlation") + 
  theme_classic()
#linear
ggplot(numberline, aes(second_graders_guess, true_value)) + 
  geom_point(size = 3, col = "firebrick") + 
  geom_smooth(method = "lm", se = FALSE, col = "black") + #fit a line
  labs(x = "correlation", y = "correlation") + 
  theme_classic()
#not linear
regression1 <- lm(true_value ~ fourth_graders_guess, data=numberline)

#q3
ggplot(mammals, aes(body_mass_kg, brain_mass_g)) + 
  geom_point(size = 3, col = "firebrick") + 
  geom_smooth(method = "lm", se = FALSE, col = "black") + #fit a line
  labs(x = "body size", y = "brain size") + 
  theme_classic()
#not linear
ggplot(mammals, aes(log(body_mass_kg), log(brain_mass_g))) + 
  geom_point(size = 3, col = "firebrick") + 
  geom_smooth(method = "lm", se = FALSE, col = "black") + #fit a line
  labs(x = "logged body size", y = "logged brain size") + 
  theme_classic()
#linear
corr <- cor.test(log(mammals$body_mass_kg), log(mammals$brain_mass_g))
#yes, correlated
regression2 <- lm(log(mammals$brain_mass_g) ~ log(mammals$body_mass_kg), data = mammals)
summary(regression2)
plot(residuals(regression2) ~ log(body_mass_kg), data=mammals)
summary(aov(log(mammals$brain_mass_g) ~ log(mammals$body_mass_kg)))
# 3 more unit on logged body size
ggplot(mammals, aes(x = body_mass_kg, y = brain_mass_g)) +  
  geom_line(aes(y = predict(regression2))) +
  geom_point(size = 5, col = "firebrick", alpha = 0.5) + 
  labs(x = "body weight", y = "brain weight") + 
  theme_classic()
# R=0.9590
3*0.9590
#log(brain size) = 2.877

#lab 18
tapply(zooplankton$diversity, list(Treatment = zooplankton$treatment, 
                                   Location = zooplankton$block), unique)
#or
spread(data = zooplankton, key = block, value = diversity)
ggplot(zooplankton, aes(x = treatment, y = diversity)) +  
  geom_line(aes(group = block)) +
  geom_point(size = 5, col = "firebrick", alpha = 0.5) + 
  labs(x = "Treatment", y = "Zooplankton diversity (D)") + 
  theme_classic()
# F test for fitness
anova(regression)

#lab19
ggplot(data = haphazard, aes(x = numberSelected)) + 
  geom_histogram(fill = "firebrick", col = "black", binwidth = 1, 
                 closed = "left", boundary = 0) + 
  scale_x_continuous(breaks = seq(from = 10, to = 100, by = 10)) +
  labs(x = "Number thought of by volunteer", y = "Frequency") + 
  theme_classic()

#lab21
ebolaSurv <- Surv(time = ebola$time, event = ebola$outcome)
ebolaKM <- survfit(ebolaSurv ~ 1, data = ebola, type="kaplan-meier") 
#nonjudgy ratio
ggsurvplot(ebolaKM, conf.int = TRUE, pval = FALSE, risk.table = FALSE, 
           legend = "none", censor.shape = "|", censor.size = 4, palette = c("firebrick"), 
           ylab = "Proportion surviving", xlab = "Time (days since admission)")
#hazrd ratio
tumorDiff <- survdiff(tumorSurv ~ Subtype, data = tumors)
print(tumorDiff, digits = 4)





















