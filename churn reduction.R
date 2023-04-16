rm(list=ls())   # Clean environment

subscription <- read.csv("online_subscription_data.csv")

library(ggplot2)
library(dplyr)

#Find representative data set churn value
representative <- subscription[subscription$representative==1, ]
mean(representative$churn)

#Find training data set churn value
training_set <- subscription[(subscription$training==1 & subscription$representative==0), ]
mean(training_set$churn)

## Summary of all variables
representative_summary <-  subscription %>%
  filter(representative == 1) %>%
  select(consldays, crackle, creditaa, ctrlrs, custcare, disneyp, gchange, hulu, mchange, mdrops, mmins, months, netflix, numgames, over18, refurb, retired, rural, spotify, youtube) %>%
  summarise_all(list(temp_mean = mean, temp_sd = sd, temp_min = min, temp_max = max)) %>%
  gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_temp_") %>%
  spread(stat, val) %>%
  select(var, mean, sd, min, max)

representative_summary

## The observations are over-sampled at 50% churn, so they need to be weighted
## The "true" churn probability is 8.72%
original_prob <- 0.0872
reweighted_prob <- 0.5

subscription$sweight <- ifelse(subscription$churn == 1,
                       original_prob / reweighted_prob,
                       (1 - original_prob) / (1 - reweighted_prob))

logit_churn <- glm(churn ~ consldays + crackle + creditaa + ctrlrs + custcare + disneyp + gchange + hulu + mchange + mdrops + mmins + months + netflix + numgames + over18 + refurb + retired + rural + spotify + youtube, 	
                   family = binomial(link='logit'), 
                   weight = sweight,
                   subset = (training == 1),
                   data = subscription)

# Find odds ratios to understand how changing certain variables effects the odds of churn
library(sandwich)
library(lmtest)
coeftest(logit_churn, vcov = vcovHC(logit_churn, type="HC1"))

#Predict and plot churn probability
subscription$churnprob <- predict.glm(logit_churn, subscription, type="response")
ggplot(subscription) + geom_histogram(aes(x = churnprob))




# Determine which variables are important to churn rate

#List p-values and odds ratios for each variable
logit_churn_variablenames <- variable.names(logit_churn)
logit_churn_pvalues <- coeftest(logit_churn, vcov = vcovHC(logit_churn, type="HC1"))[,4]
logit_churn_oddsratios <- exp(logit_churn$coef)

# Create a summary table where each row is an explanatory variable
variables1 <- tibble(
  variable = logit_churn_variablenames,
  odds_ratio = logit_churn_oddsratios,
  p_value = logit_churn_pvalues
)


# Drop the intercept row
variables2 <- slice(variables1, 2:nrow(variables1))

# Arrange variables alphabetically
variables2 <- variables2 %>% arrange(variable)

# Include the standard deviations from the representative sample
variables2$std_dev <- representative_summary$sd

# Denote dummy vs. non-dummy variables
variables2$dummy <- 
  ifelse(variables2$variable == "over18", 1,
         ifelse(variables2$variable == "refurb", 1,
                ifelse(variables2$variable == "crackle", 1,                
                       ifelse(variables2$variable == "creditaa", 1,  
                              ifelse(variables2$variable == "disneyp", 1,  
                                     ifelse(variables2$variable == "retired", 1,
                                            ifelse(variables2$variable == "hulu",1,
                                                   ifelse(variables2$variable =="rural",1,
                                                          ifelse(variables2$variable == "spotify",1,
                                                                 ifelse(variables2$variable == "netflix",1,
                                                                        ifelse(variables2$variable == "youtube",1,
                                                                               0 )))))))))))

# Calculate odds ratio ^ (2 * SD) for significant & non-dummy variables
## missing for non-significant or dummy variables
variables2$OR_2_SD <- 
  ifelse(variables2$dummy == 0 & variables2$p_value < 0.05,
         variables2$odds_ratio ^ (2 * variables2$std_dev),
         NA)

# Calculate the importance, and create missing values if the variable is non-significant
variables2$X_original <- ifelse(variables2$dummy == 1,
                                       variables2$odds_ratio,
                                       variables2$OR_2_SD)

variables2$X <- ifelse(variables2$p_value < 0.05,
                              variables2$X_original,
                              NA)

variables2$importance <- ifelse(variables2$X > 1,
                                       variables2$X,
                                       1 / variables2$X)


# Get rid of the "X" and "X_original" variables and sort variables alphabetically
variables3 <- variables2 %>%
  select(variable, odds_ratio, p_value, std_dev, dummy, OR_2_SD, importance) %>%
  arrange(variable)


# Keep the significant variables and sort the remaining variables by importance
variables3 <- variables3 %>%
  filter(p_value < 0.05) %>%
  arrange(desc(importance))


# Keep only the variable name and importance; add a new column for whether the effect is positive or negative
variables3 <- variables3 %>%
  mutate(effect = ifelse(odds_ratio > 1, "+", "-")) %>%
  select(variable, importance, effect)

View(variables3)




# Simulate what happens to churn when we incorporate a change to alter a specific metric

# Save the original values of the "avg. articles per week" variable
subscription$consldays_original <- subscription$consldays


## Console days reduced by 40% by successfully enticing users to buy a new console. 
## Only targets customers who have a console for longer than 1 year
## This reduces their console days to 0, and on avg reduced total days by 40%
subscription$consldays <- ifelse(subscription$consldays_original > 360,subscription$consldays_original * 0.6,subscription$consldays_original)
subscription$churnprob2 <- predict.glm(logit_churn, subscription, type = "response")

subscription %>% 
  filter(representative == 1) %>%
  select(churnprob, churnprob2) %>%
  summary()

subscription$consldays <- subscription$consldays_original

##Change in minutes spent playing online multiplayer increases by 180 mins
##Assumes new game-add ons increases user online play time
subscription$mchange_original <- subscription$mchange


subscription$mchange <- subscription$mchange_original + 180
subscription$churnprob3 <- predict.glm(logit_churn, subscription, type = "response")

subscription %>% 
  filter(representative == 1) %>%
  select(churnprob, churnprob3) %>%
  summary()

subscription$mchange <- subscription$mchange_original
