if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, survival, ggfortify, survminer, plotly, gridExtra, 
               Epi, KMsurv, gnm, cmprsk, mstate, flexsurv, splines, epitools, 
               eha, shiny, ctqr, scales)

orca <- read.table("http://www.stats4life.se/data/oralca.txt", stringsAsFactors = TRUE)
head(orca)
glimpse(orca)
summary(orca)

table(orca$event)

orca <- mutate(orca, all = event != "Alive")
table(orca$all)

ggplotly(
  orca %>%
    mutate(
      text = paste("Subject ID = ", id, "<br>", "Time = ", time, "<br>", "Event = ",  
                   event, "<br>", "Age = ", round(age, 2), "<br>", "Stage = ", stage)
    ) %>%
    ggplot(aes(x = id, y = time, text = text)) +
    geom_linerange(aes(ymin = 0, ymax = time)) +
    geom_point(aes(shape = event, color = event), stroke = 1, cex = 2) +
    scale_shape_manual(values = c(1, 3, 4)) +
    labs(y = "Time (years)", x = "Subject ID") + coord_flip() + theme_classic(),
  tooltip = "text"
)

su_obj <- Surv(orca$time, orca$all)
str(su_obj)

# Kaplan–Meier estimator
fit_km <- survfit(Surv(time, all) ~ 1, data = orca)
print(fit_km, print.rmean = TRUE)

dat_km <- fortify(fit_km)
head(dat_km)

ggsurvplot(fit_km, risk.table = TRUE, xlab = "Time (years)", censor = F)

glist <- list(
  ggsurvplot(fit_km, fun = "event", main = "Cumulative proportion"),
  ggsurvplot(fit_km, fun = "cumhaz",  main = "Cumulative Hazard"),
  ggsurvplot(fit_km, fun = "cloglog", main = "Complementary log−log")
)
arrange_ggsurvplots(glist, print = TRUE, ncol = 3, nrow = 1)

# Lifetable or actuarial estimator
cuts <- seq(0, 23, 1)
lifetab_dat <- orca %>%
  mutate(time_cat = cut(time, cuts)) %>%
  group_by(time_cat) %>%
  summarise(nlost = sum(all == 0),
            nevent = sum(all == 1))

dat_lt <- with(lifetab_dat, lifetab(tis = cuts, ninit = nrow(orca), 
                                    nlost = nlost, nevent = nevent))
round(dat_lt, 4)

# Nelson-Aalen estimator
fit_fh <- survfit(Surv(time, all) ~ 1, data = orca, type = "fleming-harrington", conf.type = "log-log")
dat_fh <- fortify(fit_fh)
## for the Nelson-Aalen estimator of the cumulative hazard
dat_fh_ch <- fortify(fit_fh, fun = "cumhaz")
head(dat_fh)
head(dat_fh_ch)

# plot 3 different estimates
ggplotly(
  ggplot() +
    geom_step(data = dat_km, aes(x = time, y = surv, colour = "K-M")) +
    geom_step(data = dat_fh, aes(x = time, y = surv, colour = "N-A")) +
    geom_step(data = dat_lt, aes(x = cuts[-length(cuts)], y = surv, colour = "LT")) +
    labs(x = "Time (years)", y = "Survival", colour = "Estimator") +
    theme_classic()
  )

# Measures of central tendency
(mc <- data.frame(q = c(.25, .5, .75),
                  km = quantile(fit_km),
                  fh = quantile(fit_fh)))
# plot above
ggsurvplot(fit_km, xlab = "Time (years)", censor = F)$plot +
  geom_segment(data = mc, aes(x = km.quantile, y = 1-q, xend = km.quantile, yend = 0), lty = 2) +
  geom_segment(data = mc, aes(x = 0, y = 1-q, xend = km.quantile, yend = 1-q), lty = 2)

# Parametric estimators: accelerated failure time (AFT) models
fit_exp <- flexsurvreg(Surv(time, all) ~ 1, data = orca, dist = "exponential")
fit_exp

fit_w <- flexsurvreg(Surv(time, all) ~ 1, data = orca, dist = "weibull")
fit_w
fit_ll <- flexsurvreg(Surv(time, all) ~ 1, data = orca, dist = "llogis")
fit_ll
fit_sp <- flexsurvspline(Surv(time, all) ~ 1, data = orca, k = 1, scale = "odds")
fit_sp

ggflexsurvplot(fit_exp, xlab = "Time (years)", censor = F)

grid.arrange(
  ggplot(data.frame(summary(fit_exp)), aes(x = time)) + 
    geom_line(aes(y = est, col = "Exponential")) +
    geom_line(data = data.frame(summary(fit_w)), aes(y = est, col = "Weibull")) +
    geom_line(data = data.frame(summary(fit_ll)), aes(y = est, col = "Log-logistic")) +
    geom_line(data = data.frame(summary(fit_sp)), aes(y = est, col = "Flex splines")) +
    labs(x = "Time (years)", y = "Survival", col = "Distributions") + theme_classic(),
  ggplot(data.frame(summary(fit_exp, type = "hazard")), aes(x = time)) + 
    geom_line(aes(y = est, col = "Exponential")) +
    geom_line(data = data.frame(summary(fit_w, type = "hazard")), aes(y = est, col = "Weibull")) +
    geom_line(data = data.frame(summary(fit_ll, type = "hazard")), aes(y = est, col = "Log-logistic")) +
    geom_line(data = data.frame(summary(fit_sp, type = "hazard")), aes(y = est, col = "Flex splines")) +
    labs(x = "Time (years)", y = "Hazard", col = "Distributions") + theme_classic(),
  ncol = 2
)

# compare 2 or more groups
#ci.exp(glm(all ~ 0 + stage, data = orca, family = "poisson", offset = log(time)))
group_by(orca, stage) %>%
  summarise(
    D = sum(all),
    Y = sum(time)
  ) %>%
  cbind(
    pois.approx(x = .$D, pt = .$Y)
  )

su_stg  <- survfit(Surv(time, all) ~ stage, data = orca)
su_stg

ggsurvplot(su_stg, fun = "event", censor = F, xlab = "Time (years)")

lifetab_stg <- fortify(su_stg)
lifetab_stg %>%
  group_by(strata) %>%
  do(head(., n = 3))

glist <- list(
  ggsurvplot(su_stg, fun = "cumhaz"),
  ggsurvplot(su_stg, fun = "cloglog")
)
# plot(su_stg, fun = "cloglog")
arrange_ggsurvplots(glist, print = TRUE, ncol = 2, nrow = 1)





