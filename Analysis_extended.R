
library(readr)
library(olsrr)
library(corrplot)

data <- read.csv("Country_Data_original.csv",header=TRUE,stringsAsFactors =FALSE)
data
colnames(data)

df<-data.frame(data)
df

numericc_data <- df[sapply(df, is.numeric)]
numericc_data

model <- lm(gdpp ~ child_mort + exports + health + imports + income + inflation+life_exp+total_fer+inv_child_mort+inv_total_fer, data = df)
summary(model)

library(dplyr)
library(knitr)

#correlation matrix
numeric_data <- df %>%
    select_if(is.numeric)
correlation_matrix <- round(cor(numeric_data),4)
correlation_matrix

kable(correlation_matrix, 
      caption = "Correlation Table of Numeric Variables")
summary_data <- summary(df)
summary_data

#residual plot
plot(model, which = 1)

#q-q plot for normality
qqnorm(residuals(model))
qqline(residuals(model), col = "red")

dependent_variable <- df$gdpp
hist(rstandard(model), 
     main = "Histogram of Residuals", 
     xlab = "Residuals", 
     col = "skyblue", 
     border = "white")

#Homoscedasticity
plot(fitted(model),rstandard(model),
     main = "Residuals vs Fitted (Check Homoscedasticity)",
     xlab = "Fitted values",
     ylab = "Residuals",
     pch =19, col = "navyblue")
abline(h = 0, col = "firebrick", lwd = 2) 
# Get the standardized residuals from the model

library(ggplot2)

ggplot(data.frame(fitted = fitted(model), resid =rstandard(model) ),
       aes(x = fitted, y = resid)) +
  geom_point(color = "navyblue")  +
  labs(title = "Residuals vs Fitted",
       x = "Fitted values",
       y = "Standardized Residuals") +
  theme_minimal()

standardized_residuals <- rstandard(model)

# Get the fitted (predicted) values from the model
fitted_values <- fitted(model)

# Plot the standardized residuals against the fitted values
plot(fitted_values, standardized_residuals,
     main = "Standardized Residuals vs Fitted Values",
     xlab = "Fitted (Predicted) Values",
     ylab = "Standardized Residuals")

# Add a horizontal line at y=0 for reference
abline(h=0,col = "red", lty = 2)

#full model
model <- lm(gdpp ~ exports + health + imports + income + inflation+life_exp+ inv_child_mort+inv_total_fer, data = df)
summary(model)
anova(model)
coef(model)

library(olsrr)

#forward elimination
ols_step_forward_p(model,pent=0.05,details = T)
el.forward<- ols_step_forward_p(model,pent=0.05,details = F)
el.forward

#back elimination
ols_step_backward_p(model,perm=0.05,details = T)
el.backward<- ols_step_forward_p(model,pent=0.05,details = F)
el.backward

#best subset
ml.subset<-ols_step_best_subset(model)
ml.subset

#Full model
full_model <- lm(gdpp ~ exports + health + imports + income + inflation+life_exp+ inv_child_mort+inv_total_fer, data = df)
summary(full_model)$r.squared
summary(reduced_model)$r.squared

#Reduced model
reduced_model <- lm(gdpp ~ exports + health + imports + income + inflation + inv_child_mort+inv_total_fer, data = df)
reduced_model

summary(model_final)
anova(model_final)

anova(full_model)
anova(reduced_model)

summary(full_model)$r.squared
summary(reduced_model)$r.squared

#mean square error of full model
mse_full <- deviance(full_model) / df.residual(full_model)
mse_full

cmp<-anova(reduced_model,full_model)
cmp
df1 <- cmp$Df[2]
df1
df2 <- df.residual(full_model)
df2
alpha <- 0.05

# Critical F-value
f_critical <- qf(1 - alpha, df1, df2)
f_critical

#VIF values
library(car)
vif(full_model)
vif(reduced_model)



