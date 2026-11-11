# Lab 3

# Load libraries
# install.packages("car") # only install once!
library(car)
library(tidyverse)

# Load data
homicide <- read_csv("./Lab3/homicide_lab3.csv")

# Check out data
homicide %>% select(hr, pov, ps, log10ps)

# Data dictionary: 
# homicide rate = # deaths per 100,000 individuals
# poverty = % of families with yearly incomes <$5,000
# population size = population size in 1,000s (i.e. ps = 5 --> 5000)


# 1. Visually explore relationships between homicide 
# rate (hr), poverty level (pov), and population size (ps)
pairs(~ ps + pov + hr, data = homicide, upper.panel = NULL)


# FYI, can plot individual pairs
qplot(x = pov, y = hr, data = homicide)

# 2a. Consider the simple linear regression of 
# homicide rate (hr) on poverty level (pov)
# Model: hr = B0 + B1*pov + epsilon, where epsilon ~ N(0, sigma^2)

# TODO: interpret regression coefficients. Comment 
# on standard erros, MSE, F, and R^2.

# Fit model
model1 <- lm(hr ~ pov, data = homicide)
summary(model1)
anova(model1)
confint(model1)



# 2b. FYI, we could have fit the same model using 
# "glm" function
model2 <- glm(hr ~ pov, data = homicide)
summary(model2)


# 3. Comment on the form of the fitted simple linear
# regression line graphed below.
qplot(x = pov, y = hr, data = homicide) + 
  geom_smooth(method = "lm", se = FALSE)


# 4. Interpret results of a simple linear regression 
# of homicide rate (hr) on centered poverty level 
# (pov19) where pov19 = pov - 19, because 19 is the 
# median value of pov. Comment on the estimated
# regression coefficients, associated confidence 
# intervals and p-values, and R^2 values.

# Model: 
# hr = B0 + B1*pov19 + epsilon
#    = B0 + B1*(pov - 19) + epsilon, 
# where epsilon ~ N(0, sigma^2)

model3 <- lm(hr ~ pov19, data = homicide)
summary(model3)
confint(model3)

# 5. Peform a **multiple linear regression** of
# homicide rate on poverty and population size.
# Focus on the poverty coefficient: what scientific 
# question does this regression address? Comment on 
# all regression coefficients, associated SEs, MSE,
# F, and R^2 values.

model4 <- lm(hr ~ pov19 + ps, data = homicide)
summary(model4)
anova(model4)
confint(model4)

# 6. Create **adjusted variable plots** (aka added-
# variable plot) for poverty and homicide rate from
# the multiple linear regression in step 5.

avPlots(model4)


# 7. Dichotomize population size at its median, and 
# perform simple linear regression of homicide rate
# on population size. Interpret the results and the 
# regression coefficients for the dichotomized 
# variable of population size.

# Model: hr ~ B0 + B1*psmedian + epsilon, 
# where epsilon ~ N(0, sigma^2)

med_ps <- median(homicide$ps)
med_ps

homicide <- homicide %>%
  mutate(psmedian = ifelse(ps >= med_ps, 1, 0))

model5 <- lm(hr ~ psmedian, data = homicide)
summary(model5)

# 8. Perform separate simple linear regression 
# of homicide rate on poverty for the two strata
# defined by population size. Interpret the 
# results. What do you observe about the two
# different regression coefficients for poverty?

homicide <- homicide %>%
  mutate(psmedian2 = recode_factor(psmedian,
                                   "0" = "Below median",
                                   "1" = "Above median"))

dev.new()
qplot(x = pov19, y = hr, colour = psmedian2, 
      shape = psmedian2, 
      data = homicide)

homicide.psmed0 <- homicide %>%
  filter(psmedian == 0)
model6 <- lm(hr ~ pov19, data = homicide.psmed0)
summary(model6)

homicide.psmed1 <- homicide %>%
  filter(psmedian == 1)
model7 <- lm(hr ~ pov19, data = homicide.psmed1)
summary(model7)


# 9. Perform multiple linear regression of
# homicide rate on poverty level and the 
# dichotomized population size. Interpret the 
# multiple regression coefficients for poverty
# and population size.

# Model: hr = B0 + B1*pov19 + B2*psmedian + epsilon
# where epsilon ~ N(0, sigma^2)

model8 <- lm(hr ~ pov19 + psmedian, data = homicide)

# 10. It is possible that the relationship between
# homicide rate and poverty differs by population
# size. Perform an Analysis of Covariance (ANCOVA)
# using a multiple linear regression of homicide
# rate on poverty level, population size, and the
# **interaction** of poverty level and population 
# size. Interpret the regression coefficients.

# Model: 
# hr = B0 + B1*pov19 + B2*psmedian + B3*pov19*psmedian

model9 <- lm(hr ~ pov19 + psmedian + pov19*psmedian,
             data = homicide)
summary(model9)

# Q: What do you conclude?

# Graph the relationship:
dev.new()
qplot(x = pov19, y = hr, color = psmedian2, 
      shape = psmedian2, data = homicide) + 
  geom_smooth(method = "lm", se = FALSE)
