library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)

files <- list.files(path = "Data", pattern = "diuron_light_rep", full.names = TRUE)

xdiuron_light <- lapply(files, function(x) read.csv(x)) %>%
  bind_rows(.id = "id") %>% within(hours[hours == 0.3] <- 0.33)


xdiuron_light2 <- xdiuron_light  %>% 
  filter(Replicate == "AVERAGE", Diuron != "Blank", hours >= 0) %>%
  group_by(hours, Light, Diuron, id) %>% 
  summarise(Absorbance = mean(Absorbance), X = paste(X, collapse = " ")) %>% ungroup() %>% 
  within({
    
    Diuron_num <-  case_when(Diuron == "AlgaeControl" ~ 0,
                             Diuron == "MeOH" ~ 0,
                             TRUE ~ as.numeric(Diuron)
    )
    Light_num <-  as.numeric(Light)
    block <-  factor(id)
    hours_fact <- factor(hours)
    celld <- 4179.6 * Absorbance - 172.48
    t0 <- ifelse(hours == 0, celld, NA)
    
  }) %>% group_by(Light, Diuron, id ) %>% fill(t0) %>% 
  ungroup() %>%  
  mutate(sample_id = factor(gsub("."," ", x = paste(Diuron, Light, block), fixed = TRUE))) %>% 
  filter(hours != 0, Diuron != "MeOH") %>% 
  mutate(Diuron_fact = factor(ifelse(Diuron == "AlgaeControl", "0", Diuron)))  %>%
  mutate(lncelld = log(celld),
         lnt0 = log(t0))


#
##### Figure 2A - growth (diuron, optimal light)
#
xdiuron_light2$Diuron <- factor(xdiuron_light2$Diuron)
xdiuron_light2$Diuron <- relevel(xdiuron_light2$Diuron, ref = "AlgaeControl")
levels(xdiuron_light2$hours_fact)

view(xdiuron_light2)

# Diuron, optimal light
diurondat <- filter(xdiuron_light2, Light == "80")
nrow(diurondat)
view(diurondat)
#Select the best model 
m1 <- lmer(lncelld ~ Diuron*hours_fact + offset(lnt0) + (1|block),
           data = diurondat)
qqnorm(resid(m1))
qqline(resid(m1))
m2 <- lmer(lncelld ~ Diuron+hours_fact + offset(lnt0) + (1|block),
           data = diurondat)
anova(m1, m2) #likelihood ratio test, tells me if interaction between light and hours
#report p-value and chi-square statistic

summary(m1)


#
##### Figure 2B - growth (light, no diuron)
#
xdiuron_light2$Light <- factor(xdiuron_light2$Light)
xdiuron_light2$Light <- relevel(xdiuron_light2$Light, ref = "80")
levels(xdiuron_light2$hours_fact)

view(xdiuron_light2)

#Light, zero diuron. 

lightdat <- filter(xdiuron_light2, Diuron == "AlgaeControl")
nrow(lightdat)
view(lightdat)
#Select the best model 
m3 <- lmer(lncelld ~ Light*hours_fact + offset(lnt0) + (1|block),
           data = lightdat)
qqnorm(resid(m3))
qqline(resid(m3))
m4 <- lmer(lncelld ~ Light+hours_fact + offset(lnt0) + (1|block),
           data = lightdat)
anova(m3, m4) #likelihood ratio test, tells me if interaction between light and hours
#report p-value and chi-square statistic

summary(m3)

# Compare differences with "profile confidence intervals"
confint(m3)


#
##### Dose response curves
#
view(xdiuron_light2)
library(mgcv)
library(visreg)
mDR <- gam(lncelld ~ s(Diuron_num, by = Light, k = 4) + Light + 
             offset(lnt0), 
           data = xdiuron_light2,
           method = "REML")
summary(mDR)
visreg(mDR, xvar = "Diuron_num", by = "Light")
