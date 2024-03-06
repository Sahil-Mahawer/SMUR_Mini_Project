# Load necessary libraries
library(ggplot2)

# Load the dataset
dataset <- read.csv("D:/urban_traffic.csv")

# Exploring the dataset
summary(dataset)
str(dataset)
pairs(~PopulationDensity + PublicTransportAvailability + AverageCommuteDistance + NumberOfVehicles + TrafficCongestionLevel, data = dataset)

# Correlation matrix to check for multicollinearity
cor(dataset[, -5]) # Assuming TrafficCongestionLevel is the 5th column

# Splitting the dataset into training and testing sets
set.seed(123) # For reproducibility
indexes <- sample(1:nrow(dataset), size = 0.7 * nrow(dataset))
train_data <- dataset[indexes, ]
test_data <- dataset[-indexes, ]

# Fitting a linear regression model
model <- lm(TrafficCongestionLevel ~ ., data = train_data)
summary(model)

# Diagnostic plots
par(mfrow=c(2,2))
plot(model)

# Predicting on the test set
predictions <- predict(model, newdata = test_data)

# Model evaluation
library(Metrics)
mse <- mse(test_data$TrafficCongestionLevel, predictions)
print(paste("Mean Squared Error: ", mse))

# Visualization of actual vs. predicted values
ggplot() +
  geom_point(aes(x = test_data$TrafficCongestionLevel, y = predictions), color = 'blue') +
  labs(x = 'Actual', y = 'Predicted') +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = 'red') +
  ggtitle("Actual vs Predicted Traffic Congestion Levels")