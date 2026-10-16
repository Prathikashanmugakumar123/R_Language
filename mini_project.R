#dataset link and also print dataset
url <- "https://raw.githubusercontent.com/selva86/datasets/master/BostonHousing.csv"
data <- read.csv(url)
print(data)

head(data)#it will shows first 5 rows of the dataset
dim(data)#it will show how much of rows and columns
names(data)#it will show all the names of the columns


colSums(is.na(data))#it will remove null value from column
sum(duplicated(data))#it will list all the duplicate values

data <- na.omit(data)#it will removes all the null values from rows
data <- data[!duplicated(data), ]#it will remove all the duplicate from dataset
print(data)
data <- data[, c("rm", "medv")]

head(data)#it will shows first 6 rows of the dataset
str(data)#it will shows how much column are there and also what are all the datatype in this

set.seed(123)#it will helps to pic the data randomly from the dataset

train_index <- sample(
  seq_len(nrow(data)),
  size = floor(0.8 * nrow(data))
)#it will train and test the dataset

train <- data[train_index, ]
test <- data[-train_index, ]

cat("Training rows:", nrow(train), "\n")#it will shows how much data are trained
cat("Testing rows:", nrow(test), "\n")#it will shows how much data are tested

model <- lm(medv ~ rm, data = train)#it will train the model using training data

summary(model)#it  will summarize the model

pred <- predict(model, newdata = test)#it will helps to evaluate the model
actual <- test$medv

results <- data.frame(
  Actual = actual,
  Predicted = as.numeric(pred)
)#it convert actual and predicted values in a data frame format

print(head(results, 10))#it will shows the predicted and actual value

SSE <- sum((actual - pred)^2)
SST <- sum((actual - mean(actual))^2)

R2 <- 1 - SSE / SST
RMSE <- sqrt(mean((actual - pred)^2))
MAE <- mean(abs(actual - pred))
#model accuracy stmts
cat("R-squared:", round(R2, 3), "\n")
cat("RMSE:", round(RMSE, 3), "\n")
cat("MAE:", round(MAE, 3), "\n")

rooms <- as.numeric(
  readline("Enter average number of rooms: ")
)

if (length(rooms) == 1 &&
    is.finite(rooms) &&
    rooms > 0) {
  
  user_data <- data.frame(rm = rooms)
  
  prediction <- predict(
    model,
    newdata = user_data,
    interval = "prediction"
  )
  
  cat("Entered rooms:", rooms, "\n")
  cat("Predicted house value:",
      round(prediction[1, "fit"], 2), "\n")
  cat("Lower prediction limit:",
      round(prediction[1, "lwr"], 2), "\n")
  cat("Upper prediction limit:",
      round(prediction[1, "upr"], 2), "\n")
  
} else {
  cat("Please enter a valid positive number.\n")
}

library(ggplot2)

graph1 <- ggplot(train, aes(x = rm, y = medv)) +
  geom_point(color = "purple", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "House Price Prediction",
    x = "Average Number of Rooms",
    y = "Median House Value"
  ) +
  theme_minimal()

print(graph1)
library(ggplot2)
graph2 <- ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", size = 2) +
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "red",
    linetype = "dashed"
  ) +
  labs(
    title = "Actual vs Predicted House Values",
    x = "Actual Value",
    y = "Predicted Value"
  ) +
  theme_minimal()

print(graph2)