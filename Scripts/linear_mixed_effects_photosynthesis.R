#Linear Mixed Effects Model - with quadratic function

# Photosynthesis linear mixed effects model

install.packages("lme4")
install.packages("ggplot")
install.packages("mgcv")

# Load in libraries
library(tidyverse)
library(mgcv)
library(visreg)
library(ggplot)
library(lme4)

# Read in data
psii_dat <- read_csv("Data/diuron_light_PSII_long_NEW.csv") %>% 
  rename(hours = `Time (hours)`, Diuron = `Diuron (ug/L)`,
         Light_num = `Light (u mol photons m/s/)`, block = Block) %>% 
  group_by(hours, Light_num, Diuron, block) %>% 
  summarise(photo_inhib = mean(`Photosynthetic inhibition (%)`), 
            Yield = mean(`Y(II)`), 
            Yield_C = mean(`Average Y (controls)`, na.rm = TRUE)) %>% 
  within({
    
    hours[hours == 0.3] <- 0.33
    
    Diuron_num <-  case_when(Diuron == "Control" ~ 0,
                             Diuron == "MeOH" ~ 0,
                             TRUE ~ as.numeric(Diuron)
    )
    
    block <-  factor(as.character(block))
    
    hours_fact <- factor(hours)
    
    t0 <- ifelse(hours == 0, Yield, NA)
    inhibt0 <- ifelse(hours == 0, photo_inhib, NA)
    
    treatment <- factor(paste(Diuron, Light_num, sep = "_"))
  }) %>% group_by(Light_num, Diuron, block ) %>% 
  fill(t0, inhibt0) %>% ungroup() %>% 
  mutate(sample_id = factor(gsub("."," ", x = paste(Diuron, Light_num, block), fixed = TRUE)))


view(psii_dat)


#
##### Figure 2A - photosynthesis (diuron, optimal light)
#
psii_dat$Diuron <- factor(psii_dat$Diuron)
psii_dat$Diuron <- relevel(psii_dat$Diuron, ref = "Control")
levels(psii_dat$hours_fact)

view(psii_dat)

# Diuron, optimal light
diurondatphoto <- filter(psii_dat, Light_num == "80")
nrow(diurondatphoto)
view(diurondatphoto)
#Select the best model 
m5 <- lmer(Yield ~ Diuron*hours_fact + offset(t0) + (1|block),
           data = diurondatphoto)
qqnorm(resid(m5))
qqline(resid(m5))
m6 <- lmer(Yield ~ Diuron+hours_fact + offset(t0) + (1|block),
           data = diurondatphoto)
anova(m5, m6) #likelihood ratio test, tells me if interaction between light and hours
#report p-value and chi-square statistic


#
##### Figure 2B - photosynthesis (diuron, optimal light)
#
psii_dat$Light_num <- factor(psii_dat$Light_num)
psii_dat$Light_num <- relevel(psii_dat$Light_num, ref = "80")
levels(psii_dat$hours_fact)

view(psii_dat)

# Light, no diuron
lightdatphoto <- filter(psii_dat, Diuron == "Control")
nrow(lightdatphoto)
view(lightdatphoto)
#Select the best model ######### MODELS DON'T WORK???
m7 <- lmer(Yield ~ Light_num*hours_fact + offset(t0) + (1|block),
           data = lightdatphoto)
qqnorm(resid(m7))
qqline(resid(m7))
m8 <- lmer(Yield ~ Light_num+hours_fact + offset(t0) + (1|block),
           data = lightdatphoto)
anova(m5, m6) #likelihood ratio test, tells me if interaction between light and hours
#report p-value and chi-square statistic

#
##### Dose response curves
#
view(psii_dat)
library(mgcv)
library(visreg)
mDR <- gam(Yield ~ s(Diuron_num, by = Light_num, k = 4) + Light_num + 
             offset(t0), 
           data = psii_dat,
           method = "REML")
summary(mDR)
visreg(mDR, xvar = "Diuron_num", by = "Light_num")










##extras below, when went through with Max
dat1 <- filter(psii_dat, Diuron == "Control", Light_num == 80) # This data frame is just controls of both stressors
m1 <- lm(Yield ~ hours + I(hours^2), data = dat1) #linear model with quadratic relationship
visreg(m1) #illustrates control-control (no diuron and 80 light) model
m2 <- gam(Yield ~ treatment + s(hours, k = 5) + s(block, bs = "re"), data = psii_dat)
visreg(m2) #illustrates control-control (no diuron and 80 light) model, BUT with random effect
m3 <- lme(Yield ~ hours + I(hours^2), random = ~1|block,
              data = dat1) #same as m2 using a different package
visreg(m3)
par(mfrow = c(2,2))
plot(m1)
summary(m2)
visreg(m2)
gam.check(m2)

# Linear mixed effects model

dat2 <- filter(psii_dat, Diuron == "Control") %>%
  group_by(block, Light_num, hours) %>%
  summarize(photo_inhib = mean(photo_inhib)) %>%
  mutate(lF = factor(Light_num),
         tF = factor(hours))

dat2$lF <- relevel(dat2$lF, ref= '80')
dat2$tF <- relevel(dat2$tF, ref= '0')

dat2 %>% group_by(lF, tF) %>%
  summarize(n())

nrow(dat2)

#Linear mixed effects model with block 
model1 <- lme(PI ~ lF*tF, random = ~1|Block,
              data = dat2)

summary(model1)
lmerTest::ls_means(model1)
#Anova, but techincally this is wrong becuase
# we should account for blocks. 
anova1 <- aov(PI ~ lF*tF, data = dat2)
anova(anova1)
TukeyHSD(anova1)