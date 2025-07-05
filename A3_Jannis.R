# Install required packages 
install.packages(c("tidyverse", "pwr", "broom", "see", "performance", "report", "faux"))

# Load required packages 
library(tidyverse)  
library(pwr)         
library(broom)       
library(see)         
library(performance) 
library(report)     
library(faux)
library(readr)
library(psych)
library(corrplot)
library(dplyr)
library(skimr)
library(tidyverse)


# Set Working Directory 

setwd("~/Desktop/RIB - Ass3")

# Load the dataset and call it d 

d <- read_csv("grades.csv")

# Compute the correlation matrix

cor_matrix <- cor(d, use = "complete.obs")

# Print the correlation matrix 

cor_matrix

# Fit the linear regression model 


mod1 <- lm(grade ~ lecture, data = d) 

# Regression results 

summary (mod1)

# Extract R-squared from the model

summary(mod1)$r.squared

# Install and load stargazer 
install.packages("stargazer")
library(stargazer)

# Create a formatted regression table
stargazer(mod1, type = "text",
          title = "Regression Results: Effect of Lecture Attendance on Grades",
          dep.var.labels = "Student Grade",
          covariate.labels = c("Intercept", "Lecture Attendance"),
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001))

# Run multiple regression

mod2 <- lm(grade ~ lecture + nclicks, data = d)

# Regression summary

summary(mod2)

# Install and load stargazer (if not installed yet)
install.packages("stargazer")
library(stargazer)

# Create an academically formatted table comparing both models
stargazer(mod1, mod2, type = "text",
          title = "Regression Results: Effect of Lecture Attendance & Online Engagement on Grades",
          dep.var.labels = "Student Grade",
          covariate.labels = c("Intercept", "Lecture Attendance", "Number of Clicks"),
          column.labels = c("Model 1: Lecture Only", "Model 2: Lecture + Nclicks"),
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001))

# Compare models using ANOVA

anova(mod1, mod2)

# Extract residuals from the model
residuals_mod2 <- mod2$residuals

# Load ggplot2 (if not already loaded)
library(ggplot2)

# Create a histogram of residuals
ggplot(data.frame(residuals_mod2), aes(x = residuals_mod2)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency")

# QUESTION 12 

# Read the insurance dataset into object d1

d1 <- read_csv("insurance.csv")

# Run simple linear regression: Charges ~ Smoker status
mod_smoker <- lm(charges ~ smoker, data = d1)

# Regression summary
summary(mod_smoker)

# QUESTION 12a

# Create a boxplot for insurance charges by smoking status

ggplot(d1, aes(x = smoker, y = charges, fill = smoker)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 16) +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal() +
  labs(title = "Boxplot of Insurance Charges by Smoking Status",
       x = "Smoking Status",
       y = "Insurance Charges") +
  theme(legend.position = "none")

# QUESTION 13

# Run multiple regression: Charges ~ BMI + Age
mod_bmi_age <- lm(charges ~ bmi + age, data = d1)

# Display regression summary
summary(mod_bmi_age)

# Create an academically formatted regression table
stargazer(mod_bmi_age, type = "text",
          title = "Regression Results: Insurance Charges as a Function of BMI & Age",
          dep.var.labels = "Insurance Charges",
          covariate.labels = c("Intercept", "BMI", "Age"),
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001))

# QUESTION 14

# Check multiple assumptions visually

check_model(mod_bmi_age)

# Linearity and homoscedasticity check (residuals vs fitted plot) 

ggplot(data.frame(fitted = fitted(mod_bmi_age), residuals = residuals(mod_bmi_age)), 
       aes(x = fitted, y = residuals)) +
  geom_point() + 
  geom_smooth(method = "loess", color = "red") +
  theme_minimal() +
  labs(title = "Residuals vs Fitted Plot", x = "Fitted Values", y = "Residuals")

# Multicollinearity check 

# Load car package
install.packages("car")
library(car)

# Compute Variance Inflation Factor (VIF)
vif(mod_bmi_age)

# Check normality 

# Histogram of residuals
ggplot(data.frame(residuals = residuals(mod_bmi_age)), aes(x = residuals)) +
  geom_histogram(binwidth = 500, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency")

# Q-Q Plot
qqnorm(residuals(mod_bmi_age))
qqline(residuals(mod_bmi_age), col = "red")

#QUESTION 15

# Log-transform the variables
d1 <- d1 %>%
  mutate(log_charges = log(charges),
         log_bmi = log(bmi),
         log_age = log(age))

# Run the regression with log-transformed variables
mod_log <- lm(log_charges ~ log_bmi + log_age, data = d1)

# Display regression summary
summary(mod_log)

