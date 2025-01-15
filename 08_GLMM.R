# Calculating Home Range & Overlap
# Author: Tatiana Proboste & Abigail Turnlund

library(lme4)
library(readxl)
library(tidyverse)
library(lmerTest)


# Direct contact --------------------------------------------------------
df <- read_csv("./Contact_Rate_Direct.csv")

#separate df based on the type of contact

df_1 <- df %>% filter(contact_type == 1) # within sounders
df_0 <- df %>% filter(contact_type == 0) # between sounders


# Model within sounders ----------------------------------------------------
model1 <- lmer(log(mean) ~ combined_sex  + (1 | population), data = df_1)
summary(model1)

plot(resid(model1) ~ fitted(model1))

hist(resid(model1))
qqnorm(resid(model1))


# Model between sounders-------------------------------------------------------
model2 <- lmer (log(mean) ~ combined_sex + (1|population), data = df_0)
summary(model2)

plot(resid(model2) ~ fitted(model2))

hist(resid(model2))
qqnorm(resid(model2))


# Overall -----------------------------------------------------------------
model3 <- lmer (log(mean) ~ combined_sex  + as.factor(contact_type) + (1|population),  data = df)
summary(model3)

plot(resid(model2) ~ fitted(model2))

hist(resid(model2))
qqnorm(resid(model2))



# Indirect contact  -----------------------------------------------------
df <- read_csv("./Contact_Rate_Indirect.csv")


#separate df based on the type of contact

df_1 <- df %>% filter(contact_type == 1) # within sounders
df_0 <- df %>% filter(contact_type == 0) # between sounders


# Model within sounders ----------------------------------------------------
model1 <- lmer (log(mean) ~ combined_sex  +(1|population),  data = df_1)
summary(model1)
isSingular(model1)

plot(resid(model1) ~ fitted(model1))

hist(resid(model1))
qqnorm(resid(model1))


# Model between sounders------------------------------------------------------------------
model2 <- lmer (log(mean) ~ combined_sex +(1|population),   data = df_0)
summary(model2)

plot(resid(model2) ~ fitted(model2))

hist(resid(model2))
qqnorm(resid(model2))


# Overall -----------------------------------------------------------------
model3 <- lmer (log(mean) ~ combined_sex  + as.factor(contact_type) + (1|population),   data = df)
summary(model3)

plot(resid(model3) ~ fitted(model3)

hist(resid(model3))
qqnorm(resid(model3))
