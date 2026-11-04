# Load libraries
library(tidyverse)

# Load data
class621 <- read_csv("2017class621.csv")

# 1. Inspect data
table(class621$exhrs)

# 2. Re-code exercise hours
class621 <- class621 %>%
  mutate(exhrs3 = recode(exhrs, "14 or more hours" = "4+",
                         "8-13" = "4+",
                         "4-7" = "4+"))
table(class621$exhrs3)

# 3. Examine relationship between BMI and exercise group
## Calculate BMI
class621 <- class621 %>%
  mutate(bmi = (weight/height^2)*704.5) # weight in lbs, height in feet?

## Create boxplot to visualize relationship
### Base R plot
boxplot(bmi ~ exhrs3, data = 621) 

### Alternatively, use ggplot (from tidyverse)
class621 %>%
  ggplot() +
  geom_boxplot(aes(x = exhrs3, y = bmi))

# 4. Summarize BMI by exercise group
class621 %>%
  group_by(exhrs3) %>%
  summarize(obs = n(),
            mean = mean(bmi, na.rm = TRUE),
            sd = sd(bmi, na.rm = TRUE))

# 5. To investigate the relationship between BMI and exercise hours, we could 
# perform linear regression. Write out the model and interpret the regression 
# coefficients. 



# 6. Actually fit this linear regression model
model1 <- lm(bmi ~ exhrs3, data = class621)
summary(model1)

# 7: Is there a significant association between weekly exercise hours and BMI?
# How would you set up and test this hypothesis?


anova(model1)
summary(model1)

# TODO: set up contrasts to test if B1 = B2

# 8. What do you conclude regardint the pairwise comparisons of BMI between 
# weekly exercise groups and the group with 0 hours of exercise? How would you
# set up and test this hypothesis?


summary(model1)

# 9. Interpret the table partitioning the variability in BMI into sources. What 
# do you conclude regarding residual variance? Residual standard deviation? How
# do you interpret R^2, the coefficient of determination?
anova(model1)



