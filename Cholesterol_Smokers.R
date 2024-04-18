# Lab 2 - Model 2
# Date: 2024-04-09

# Dividing Data for Model 1
# --------------------------

# Loading Data
data <- read.csv("Actual_Dataset.csv")

# Splitting Data into training and testing datasets
set.seed(123)
num_rows <- nrow(data)

num_train <- round(0.3 * num_rows)  # 30% for training
num_test <- num_rows - num_train

train_indices <- sample(1:num_rows, num_train)
test_indices <- setdiff(1:num_rows, train_indices)

train_data <- data[train_indices, ]
test_data <- data[test_indices, ]

write.csv(train_data, "train_data_Model2.csv")
write.csv(test_data, "test_data_Model2.csv")

# Creating Model 1
# ----------------

lmmodel <- lm(formula = totChol ~ age + factor(male) + factor(currentSmoker) + BMI, data = train_data)
summary(lmmodel)

# Set up the plotting layout
par(mfrow = c(2, 2))

# Plot all diagnostic plots
plot(lmmodel, which = 1, main = "Residuals vs Fitted")
plot(lmmodel, which = 2, main = "Normal Q-Q Plot")
plot(lmmodel, which = 3, main = "Scale-Location Plot")
plot(lmmodel, which = 4, main = "Cook's Distance Plot")

# Creating Residuals vs Predicted Plot
# -------------------------------------

# Get predicted values
predicted_model2 <- predict(lmmodel)

# Get residuals
residuals_model2 <- residuals(lmmodel)

# Set larger plotting device
png("residuals_vs_predicted_model2.png", width = 10, height = 6, units = "in", res = 300)

# Plot residuals vs predicted values
par(mar = c(5, 5, 2, 2))
plot(predicted_model2, residuals_model2, xlab = "Predicted Values", ylab = "Residuals",
     main = "Residuals vs Predicted Values (Model 2)")
abline(h = 0, col = "red")  # Add horizontal line at y = 0

# Save plot and close device
dev.off()

# Geom Smoothed Plot for Residuals vs Predicted
# ----------------------------------------------

library(ggplot2)

# Create a data frame with predicted values and residuals
residual_df_model2 <- data.frame(Predicted = predicted_model2, Residuals = residuals_model2)

# Set larger plotting device
png("smoothed_residuals_vs_predicted_model2.png", width = 10, height = 6, units = "in", res = 300)

# Plot residuals vs predicted values with smoothed line
ggplot(residual_df_model2, aes(x = Predicted, y = Residuals)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(x = "Predicted Values", y = "Residuals", title = "Residuals vs Predicted Values (Model 2)") +
  theme_minimal()

# Save plot and close device
dev.off()

# Creating Residuals vs Input Variable for each variable
# -------------------------------------------------------

# Define input variables used in Model 2
input_vars_model2 <- c("age", "male", "BMI", "currentSmoker")

# Loop through each input variable
for (var in input_vars_model2) {
  # Set larger plotting device
  png(paste0("residuals_vs_", var, "_model2.png"), width = 10, height = 6, units = "in", res = 300)
  
  # Plot residuals vs input variable
  par(mar = c(5, 5, 2, 2))
  plot(train_data[[var]], residuals_model2, xlab = var, ylab = "Residuals",
       main = paste("Residuals vs", var, "(Model 2)"))
  abline(h = 0, col = "red")  # Add horizontal line at y = 0
  
  # Save plot and close device
  dev.off()
}
