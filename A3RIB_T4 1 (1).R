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



# Question 6
# Load the dataset and call it d 

d <- read.csv("grades.csv")

# Compute the correlation matrix

cor_matrix <- cor(d, use = "complete.obs")
cor_matrix


# Question 7
# Fit the linear regression model 

mod1 <- lm(grade ~ lecture, data = d) 

# Regression results 

summary (mod1)


# Question8
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



# Question 9
# Run multiple regression

mod2 <- lm(grade ~ lecture + nclicks, data = d)

# Regression summary

summary(mod2)


# Question 10
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

# Nicely formatted table
# Load necessary libraries
library(broom)
library(kableExtra)
library(dplyr)

# Extract regression results for both models
mod1_results <- tidy(mod1) %>%
  mutate(model = "Model 1: Lecture Only")

mod2_results <- tidy(mod2) %>%
  mutate(model = "Model 2: Lecture + Nclicks")

# Combine both models into one table
regression_table <- bind_rows(mod1_results, mod2_results) %>%
  select(model, term, estimate, std.error, statistic, p.value) %>%
  mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    statistic = round(statistic, 3),
    p.value = round(p.value, 4)
  )

# Rename columns for clarity
colnames(regression_table) <- c("Model", "Term", "Estimate", "Std. Error", "t Value", "p Value")

# Create a formatted table
regression_table %>%
  kable(format = "html", caption = "Regression Results: Effect of Lecture Attendance & Online Engagement on Grades") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE) %>%
  column_spec(1, bold = TRUE) %>%
  row_spec(0, bold = TRUE)


# ANOVA
# Compare models using ANOVA

anova(mod1, mod2)


#Question 11
# Extract residuals from the model
residuals_mod2 <- mod2$residuals

# Load ggplot2 (if not already loaded)
library(ggplot2)

# Create a histogram of residuals
ggplot(data.frame(residuals_mod2), aes(x = residuals_mod2)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency")



# Question 12
# Read the insurance dataset into object d1

d1 <- read.csv("insurance.csv")

# Run simple linear regression: Charges ~ Smoker status
mod_smoker <- lm(charges ~ smoker, data = d1)

# Regression summary
summary(mod_smoker)

# Question 12a
# Create a boxplot for insurance charges by smoking status

ggplot(d1, aes(x = smoker, y = charges, fill = smoker)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 16) +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal() +
  labs(title = "Boxplot of Insurance Charges by Smoking Status",
       x = "Smoking Status",
       y = "Insurance Charges") +
  theme(legend.position = "none")

# Question 13
# Run multiple regression: Charges ~ BMI + Age
mod_bmi_age <- lm(charges ~ bmi + age, data = d1)

# Display regression summary
summary(mod_bmi_age)

# Create an academically formatted regression table
# Load necessary libraries
library(apaTables)

# Generate an APA-style regression table and display it in R
apa.reg.table(mod_bmi_age)
apa.reg.table(mod_bmi_age, filename = "Regression_Table.doc")

# Question 14
# Check multiple assumptions visually
check_model(mod_bmi_age)

# Check for Linearity and homoscedasticity check (residuals vs fitted plot) 

ggplot(data.frame(fitted = fitted(mod_bmi_age), residuals = residuals(mod_bmi_age)), 
       aes(x = fitted, y = residuals)) +
  geom_point() + 
  geom_smooth(method = "loess", color = "red") +
  theme_minimal() +
  labs(title = "Residuals vs Fitted Plot", x = "Fitted Values", y = "Residuals")

# Check for Normality of Residuals

qqnorm(residuals(mod_bmi_age))
qqline(residuals(mod_bmi_age), col = "red")

# Histogram of residuals
ggplot(data.frame(residuals = residuals(mod_bmi_age)), aes(x = residuals)) +
  geom_histogram(binwidth = 500, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency")

# Multicollinearity check 

install.packages("car")
library(car)

# Compute Variance Inflation Factor (VIF)
vif(mod_bmi_age)

# Check for Independence
# Durbin-Watson Test
install.packages("lmtest")
library(lmtest)
dwtest(mod_bmi_age)

# Check for influential data points
# Generate Residuals vs. Leverage Plot
plot(mod_bmi_age, which = 5)  # The 5th plot in plot.lm() generates this graph


#Question 15
# Log-transform the variables
d1 <- d1 %>%
  mutate(log_charges = log(charges),
         log_bmi = log(bmi),
         log_age = log(age))

# Run the regression with log-transformed variables
mod_log <- lm(log_charges ~ log_bmi + log_age, data = d1)

# Display regression summary
summary(mod_log)


#Question 16
# Fit the regression model with region as a predictor
mod_region <- lm(charges ~ region, data = d1)

# View regression results
summary(mod_region)

# Check how R codes the categorical variable

d1$region <- as.factor(d1$region)
contrasts(d1$region)
