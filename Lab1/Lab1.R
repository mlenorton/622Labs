# Load packages
library(tidyverse)

# Create dataset from CDC
# mammData <- read_csv("/path/to/this/file/mammData.csv")
mammData <- data.frame(year = 1990:2000,
                 US = c(42, 38, 37, 34, 33, 31, 31, 30, 28, 27, 24),
                 Maryland = c(25, 28, 26, 26, 23, 23, 21, 20, 21, 19, 18))

# 1) What trends do we observe in the data? 
# Create scatterplot to visualize trends for each population
ggplot(mammData, aes(x = year)) +
  ylab("Percent with no mammogram") +
  geom_point(aes(y = Maryland, col = "Maryland")) +
  geom_point(aes(y = US, col = "US")) +
  scale_color_manual("", breaks = c("Maryland", "US"), values = c("red", "blue"))

# Trends in data:
# 1. Both US and MD rates are relatively linear over time
# 2. Both decreasing over time
# 3. US has higher percent with no mammogram
# 4. Slopes are similar but MD slope is slightly less steep (and also much lower intercept)

# 2) Interpret the sample correlation coefficient estimating the linear
# relationship between the percentage of women with no mammogram and year, for each
# population (the US and Maryland) separately.

# Calculate correlation
cor(mammData$year, mammData$US)
cor(mammData$year, mammData$Maryland)

# Interpretation of correlation coefficients:
# Both correlations are strongly negative, but the US vs year correlation is slightly stronger


# 3) From the attached R output, estimate the least squares intercept (B_0) and 
# slope (B_1) for each population from these data using the equation 
# Y = B_0 + B_1X + eps_1

# Fit linear regression
modelUS <- lm(US ~ year, data = mammData)
summary(modelUS)
modelMD <- lm(Maryland ~ year, data = mammData)
summary(modelMD)

# Y = B0 + B1*X
# US = B0 + B1*year
# US = 3133 - 1.55*year

# B0: 3133% of US women have not had a recent mammogram in year 0 
# B1: For a 1 year increase, we expect that 1.55% fewer women will have not had 
# a recent mammogram (in the US)

# Visualize linear regression
ggplot(mammData, aes(x = year)) +
  ylab("Percent with no mammogram") +
  geom_point(aes(y = Maryland, col = "Maryland")) +
  geom_smooth(aes(y = Maryland, col = "Maryland"), method = "lm") +
  geom_point(aes(y = US, col = "US")) +
  geom_smooth(aes(y = US, col = "US"), method = "lm") +
  scale_color_manual("", breaks = c("Maryland", "US"), values = c("red", "blue")) +
  theme_minimal()

# 4) Test the null hypothesis of no linear relationship between percentage of 
# women without mammograms and year, for each population.

# Calculate confidence intervals for model coefficients
confint(modelUS)
confint(modelMD)

# 95% CI: mean +/- 1.96*SE
# 95% confident that -1.79% to -1.32% covers the true slope (B1) for the US
# Q: Is there a linear relationship between mammogram rates and year?
# H0: B1 = 0
# we found that 95% CI for B1 did *not* cover 0, so we reject H0

# 5) From the R output, estimate the mean squared residual (\sigma^2) and the 
# residual standard deviation (\sigma)

# Option 1: from the lm output: residual standard deviation:
# For the US: residual sd (i.e. residual se) = sigma = 1.073 --> sigma^2 = 1.073^2 = 1.15 = mean squared residual
# For MD: ...

# Run ANOVA
anova(modelUS)
anova(modelMD)
 
# Variance that's explained by the model: sum{(y_hat - y_bar)^2}/1 = SSR/df
# Unexplained variance = mean squared residual = sum{(y_i - y_hat)^2}/9 = SSE/df 

# 6) Discuss the concept of “centering” by referring to the last page of the R output.
# Visualize linear regression
median_year <- median(mammData$year)
median_year
mammData <- mammData %>% mutate(yearcen = year - median_year)
ggplot(mammData, aes(x = yearcen)) +
  ylab("Percent with no mammogram") +
  geom_point(aes(y = Maryland, col = "Maryland")) +
  geom_smooth(aes(y = Maryland, col = "Maryland"), method = "lm", se = FALSE) +
  geom_point(aes(y = US, col = "US")) +
  geom_smooth(aes(y = US, col = "US"), method = "lm", se = FALSE) +
  scale_color_manual("", breaks = c("Maryland", "US"), values = c("red", "blue")) 

# Refit regression with mean-centered data
modelUS_cen <- lm(US ~ yearcen, data = mammData)
summary(modelUS_cen)
modelMD_cen <- lm(Maryland ~ yearcen, data = mammData)
summary(modelMD_cen)

# Calculate confidence intervals for model coefficients
confint(modelUS_cen)
confint(modelMD_cen)

# 7) What other general R command could have been used instead of lm() to 
# perform these regressions?
# Additional fitting options:
modelUS_cen_glm = glm(US ~ yearcen, data=mammData, family=gaussian(link="identity"))
summary(modelUS_cen_glm)

modelMD_cen_glm = glm(Maryland ~ yearcen, data=mammData, family=gaussian(link="identity"))
summary(modelMD_cen_glm)


# (Optional) Feedback: https://forms.gle/KmkmBqgfApF5eNpS9
