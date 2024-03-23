# Install and load required packages
packages <- c("caret", "randomForest", "e1071", "kernlab", "class")
install.packages(packages, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)

set.seed(123)

# Read the data
data <- read.csv("CardiacPrediction.csv")
print("Data loaded successfully.")

# Convert 'CoronaryHeartDisease' to a factor
data$CoronaryHeartDisease <- factor(data$CoronaryHeartDisease)
print("Outcome variable converted to factor.")

# Subset the data to select 1508 rows of each class
class_1 <- subset(data, CoronaryHeartDisease == 1)
class_0 <- subset(data, CoronaryHeartDisease == 0)
class_0_sample <- class_0[sample(nrow(class_0), 1508), ]

# Combine both classes into a balanced dataset
balanced_data <- rbind(class_1, class_0_sample)
balanced_data <-  balanced_data[sample(nrow(balanced_data)), ]
print("Balanced dataset created.")

# Split the data into training and testing sets
train_index <- createDataPartition(balanced_data$CoronaryHeartDisease, p = 0.75, list = FALSE)
train_data <- balanced_data[train_index, ]
test_data <- balanced_data[-train_index, ]

# Define the control parameters for 10-fold cross-validation
ctrl <- trainControl(method = "cv", number = 10)

# Train and evaluate models
models <- list()  # Store trained models
model_names <- c("LR", "SVM","XGBoost", "RF")

for (model_name in model_names) {
  # Train the model
  if (model_name == "LR") {
    model <- train(CoronaryHeartDisease ~ ., data = train_data, method = "glm", trControl = ctrl, metric = "Accuracy")
  } else if (model_name == "XGBoost") {
    model <- train(CoronaryHeartDisease ~ ., data = train_data, method = "xgbTree", trControl = ctrl, metric = "Accuracy")
  } else if (model_name == "SVM") {
    model <- train(CoronaryHeartDisease ~ ., data = train_data, method = "svmRadial", trControl = ctrl, metric = "Accuracy")
  } else if (model_name == "RF") {
    model <- train(CoronaryHeartDisease ~ ., data = train_data, method = "rf", trControl = ctrl, metric = "Accuracy")
  }
  
  # Store the trained model
  models[[model_name]] <- model
  
  # Print the trained model
  cat("Trained Model for ", model_name, ":\n")
  print(model)
  
  # Evaluate the model on the testing set
  predictions <- predict(model, newdata = test_data)
  conf_matrix <- confusionMatrix(predictions, test_data$CoronaryHeartDisease)
  
  # Print confusion matrix and other evaluation metrics
  cat("Confusion matrix for ", model_name, ":\n")
  print(conf_matrix)
  cat("Accuracy for ", model_name, ":\n")
  print(conf_matrix$overall["Accuracy"])
  cat("\n")
}
