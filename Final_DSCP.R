# Install and load necessary packages
install.packages(c('caret', 'xgboost', 'C50', 'gbm'))
library(caret)
library(xgboost)
library(C50)
library(gbm)

# Read the dataset
df <- read.csv('Final_Dataset.csv')

# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(df$CoronaryHeartDisease, times = 1, p = 0.75, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Preprocess the data
train_data$CoronaryHeartDisease <- as.factor(train_data$CoronaryHeartDisease)
test_data$CoronaryHeartDisease <- as.factor(test_data$CoronaryHeartDisease)

# Set up the train control
trainControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = TRUE)

# Train the model using the random forest algorithm
set.seed(123)
model <- train(CoronaryHeartDisease ~ ., data = train_data, method = "rf", trControl = trainControl, metric = "Accuracy")

# Extract variable importance using Mean Decrease in Gini (MDG)
importance <- varImp(model, scale = FALSE)
# Select top 20 features based on importance scores
top_features <- rownames(importance$importance[1:20, , drop = FALSE])

# Correlations for multicollinearity concerns ----  
# # Subset the training data to include only the top features
# train_data_subset <- train_data[, c("CoronaryHeartDisease", top_features)]
# 
# # Calculate the correlation matrix for the selected features
# correlation_matrix <- cor(train_data_subset[, -1]) # Exclude the target variable
# 
# # Melt the correlation matrix for plotting
# melted_correlation <- melt(correlation_matrix)
# 
# # Plot the heatmap
# ggplot(data = melted_correlation, aes(Var2, Var1, fill = value)) +
#   geom_tile(color = "white") +
#   scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
#                        midpoint = 0, limit = c(-1,1), space = "Lab", 
#                        name="Correlation") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1)) +
#   coord_fixed()


# Features to remove
features_to_remove <- c("Segmented.Neutrophils", "Total.Cholesterol", "Glucose")
# Remove specified features from top_features vector
top_features <- top_features[!top_features %in% features_to_remove]
# Select top 10 features
top_10_features <- top_features[1:10]

# Prepare the data using the selected features
train_data_subset <- train_data[, c("CoronaryHeartDisease", top_10_features)]

# Train models
# XGBoost
set.seed(123)
xgb_model <- train(
  CoronaryHeartDisease ~ ., 
  data = train_data_subset, 
  method = "xgbTree", 
  trControl = trainControl, 
  metric = "Accuracy"
)

# Bagged CART
set.seed(123)
bagged_cart_model <- train(
  CoronaryHeartDisease ~ ., 
  data = train_data_subset, 
  method = "treebag", 
  trControl = trainControl, 
  metric = "Accuracy"
)

# C5
set.seed(123)
c5_model <- train(
  CoronaryHeartDisease ~ ., 
  data = train_data_subset, 
  method = "C5.0", 
  trControl = trainControl, 
  metric = "Accuracy"
)

# GBM
set.seed(123)
gbm_model <- train(
  CoronaryHeartDisease ~ ., 
  data = train_data_subset, 
  method = "gbm", 
  trControl = trainControl, 
  metric = "Accuracy"
)

# Train the model using the Random Forest algorithm
set.seed(123)
rf_model <- train(
  CoronaryHeartDisease ~ ., 
  data = train_data_subset, 
  method = "rf", 
  trControl = trainControl, 
  metric = "Accuracy"
)

# Train the model using Support Vector Machines (SVM)
set.seed(123)
svm_model <- train(
  CoronaryHeartDisease ~ ., 
  data = train_data_subset, 
  method = "svmRadial", 
  trControl = trainControl, 
  metric = "Accuracy"
)

# Train the model using Generalized Linear Model (GLM)
set.seed(123)
glm_model <- train(
  CoronaryHeartDisease ~ ., 
  data = train_data_subset, 
  method = "glm", 
  trControl = trainControl, 
  metric = "Accuracy"
)

# Predictions and evaluation
models <- list(xgb_model, bagged_cart_model, c5_model, gbm_model, rf_model, svm_model, glm_model)
model_names <- c("XGBoost", "Bagged CART", "C5", "GBM", "Random Forest", "SVM", "GLM")

for (i in seq_along(models)) {
  predictions <- predict(models[[i]], newdata = test_data)
  conf_matrix <- confusionMatrix(predictions, test_data$CoronaryHeartDisease)
  
  cat(model_names[i], "Model Performance on Test Data:\n")
  cat("Accuracy:", conf_matrix$overall["Accuracy"], "\n")
  cat("Precision:", conf_matrix$byClass["Precision"], "\n")
  cat("Recall:", conf_matrix$byClass["Recall"], "\n")
  cat("F1 Score:", conf_matrix$byClass["F1"], "\n\n")
}

# Gather accuracy values
accuracies <- sapply(models, function(model) {
  conf_matrix <- confusionMatrix(predict(model, newdata = test_data), test_data$CoronaryHeartDisease)
  conf_matrix$overall["Accuracy"]
})

# Plot bar graph with better color theme and scale
bar_plot <- ggplot(data.frame(Model = model_names, Accuracy = accuracies), aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  coord_cartesian(ylim = c(0.7, 0.95)) +  # Adjust y-axis limits
  labs(title = "Model Accuracies", y = "Accuracy", x = "Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d() +  # Use viridis color palette
  scale_y_continuous(expand = c(0, 0))  # Remove padding

# Show the plot
print(bar_plot)


# Line graph for Comparative accuracies of models with 
# Vectors with your data
algorithms <- c("Treebag", "GBM", "C5.0", "XGBoost", "RF", "SVM", "GLM")
accuracy_all_features <- c(0.8484, 0.8481, 0.8582, 0.8739, 0.8449, 0.8297, 0.82)
accuracy_10_features <- c(0.8211, 0.8235, 0.8220, 0.8371, 0.8318, 0.8270, 0.7950)

# Create a data frame for plotting
data <- data.frame(
  Algorithm = rep(algorithms, 2),
  Accuracy = c(accuracy_all_features, accuracy_10_features),
  Features = factor(rep(c("All Features", "10 Features"), each = length(algorithms)))
)

# Plot
ggplot(data, aes(x = Algorithm, y = Accuracy, color = Features, group = Features)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("All Features" = "blue", "10 Features" = "red")) +
  labs(title = "Algorithm Accuracy Comparison",
       subtitle = "Comparison of Accuracy with All vs. 10 Selected Features",
       x = "Algorithm",
       y = "Accuracy (%)",
       color = "Feature Set") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

