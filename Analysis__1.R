data <- read.csv("Country_Data_original.csv")
library(ggplot2)
library(tidyr)
library(dplyr)

colnames(data)
head(data)
colnames(data)
summary(data)

library(psych)
library(knitr)
library(kableExtra)

# Descriptive statistics for all variables
desc_stats <- summary(data)
print(desc_stats)
# Load your dataset
data <- read.csv("Country_Data_original.csv")

# Create a custom function to extract required summary stats
custom_summary <- function(x) {
  stats <- quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  mean_val <- mean(x, na.rm = TRUE)
  return(c(Min = stats[1],
           `1st Qu` = stats[2],
           Median = stats[3],
           Mean = mean_val,
           `3rd Qu` = stats[4],
           Max = stats[5]))
}

# Apply the function to numeric variables only
numeric_vars <- sapply(data, is.numeric)
summary_table <- sapply(data[, numeric_vars], custom_summary)

# Transpose for better display
summary_table <- t(summary_table)

# Round for neat output
summary_table <- round(summary_table, 2)

# Print the summary table
print(summary_table)

#box plots
library(ggplot2)
library(tidyr)
library(dplyr)
colnames(data)

# Reshape data to long format
long_data <- data %>%
  select(child_mort) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# ggplot boxplot with gray colors
ggplot(long_data, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +
  scale_fill_manual(values = c("child_mort" = "slategray")) +
  labs(title = "Boxplot of child mortility rates ", x = "", y = "Value") +
  theme_minimal()

#Histogram
install.packages("RColorBrewer")
library(RColorBrewer)
colnames(data)

# Create histogram of GDP per capita
ggplot(data, aes(x =gdpp )) +
 geom_histogram( binwidth= 500, fill = "steelblue", color = "black") +
  labs(title = "Histogram of GDP per capita",
       x = "GDP per capita",
      y = "Frequency") +
  theme_minimal()
## Plot histogram with density curvecolnames(data)
# Load necessary libraries


#Reggression line with scatter plot
colnames(data)
# Scatter plot with regression line
ggplot(data, aes(x = life_exp, y = gdpp)) +
  geom_point(color = "navyblue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick", size = 1) +
  labs(title = "Scatter Plot of GDP per Capita vs Life expectancy",
       x = "life expectancy",
       y = "GDP per Capita") +
  theme_minimal()

#cormatrix
library(corrplot)
library(readr)
library(dplyr)

# Select only numeric columns including gdpp and independent variables
numeric_data <- select_if(data, is.numeric)

# Optional: check variable names
names(numeric_data)
# Compute correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs", method = "pearson")

# View the matrix
print(round(cor_matrix, 2))

# Correlation plot
corrplot(cor_matrix,
         method = "color",        # Use colored squares
         type = "upper",          # Show upper triangle only
         addCoef.col = "black",   # Add coefficient text in black
         tl.col = "black",        # Text label color
         number.cex = 0.7,        # Size of the number labels
         tl.cex = 0.8,            # Size of variable labels
         diag = FALSE)   

# Build the multiple linear regression model
model <- lm(gdpp ~ income + imports +exports+ child_mort + health + inflation + life_exp + total_fer +inv_child_mort+inv_total_fer, data = data)
colnames(data)

# Calculate VIF values
vif_values <- vif(model)

# Print the VIFs
print(vif_values)
#.....................................................................................
ggplot(data, aes(x = income)) +
  geom_histogram(aes(y = ..density..), 
                 binwidth = 1000,       # Adjust bin width as needed
                 fill = "steelblue", 
                 color = "black", 
                 alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram with Density Curve of Income",
       x = "Income",
       y = "Density") +
  theme_minimal()
#.............................................................................
ggplot(data, aes(x = exports)) +
  geom_histogram(aes(y = ..density..),
                 bins = 30  ,       # Adjust binwidth if needed
                 fill = "steelblue",
                 color = "black",
                 alpha = 0.7) +
  geom_density(color = "red", size = 1.15) +
  labs(title = "Histogram of Total Fertility Rate",
       x = "Exports (% of GDP per capita)",
       y = "Density") +
  theme_minimal()
#............................................................
ggplot(data, aes(x = health)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram of Health ", 
       x = "Health Spending per Capita (%)", 
       y = "Density")




