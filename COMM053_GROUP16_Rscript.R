
#------- INSTALLING REQUIRED FUNCTIONS -------

install.packages("ggplot2")
install.packages("caret")
install.packages("randomForest")
install.packages("e1071")
install.packages("rpart")
install.packages("dplyr")
install.packages("psych")
install.packages("tidyr") # Correlation
install.packages("ROSE")
install.packages("corrplot")
install.packages("class")
install.packages("naivebayes")
install.packages("pROC")
install.packages("reshape2")

library(ggplot2)
library(caret)
library(randomForest)
library(e1071)
library(rpart)
library(dplyr)
library(psych)
library(tidyr)
library(ROSE)
library(corrplot)
library(class)
library(naivebayes)
library(pROC)
library(reshape2)
library(class)


#------- DATA IMPORTING AND UNDERSTANDING -------

dataset <- read.csv("/Users/wilson/Downloads/diabetes_prediction_dataset.csv", header = TRUE, sep = ",")
head(dataset)
str(dataset)
dim(dataset)
summary(dataset)

#Identify categorical and numerical columns
categorical_columns <- sapply(dataset, function(x) is.factor(x) || is.character(x))
numerical_columns <- sapply(dataset, function(x) is.numeric(x))
#Print the names of categorical and numerical columns
cat("Categorical Columns:", names(dataset)[categorical_columns], "\n")
cat("Numerical Columns:", names(dataset)[numerical_columns],"\n")


# ------- DATA UNDERSTANDING  -------


#Gender count across dataset 
print(table(dataset$gender))
ggplot(dataset, aes(x=gender)) +
  geom_bar() +
  labs(x = "Gender", y = "Count") +
  theme_minimal()
#Smoking history across dataset
print(table(dataset$smoking_history))
ggplot(dataset, aes(x=smoking_history)) +
  geom_bar() +
  labs(x = "Smoking History", y = "Count") +
  theme_minimal()
#Age observed across dataset
ggplot(dataset, aes(x=age)) +
  geom_histogram(bins = 20, fill="blue", color="black") +
  labs(x = "Age", y = "Frequency") +
  theme_minimal()
# Hypertension observed across dataset
print(table(dataset$hypertension))
ggplot(dataset, aes(x=hypertension)) +
  geom_histogram(bins = 20, fill="blue", color="black") +
  labs(x = "Hypertension", y = "Frequency") +
  theme_minimal()
# HbA1c level observed across dataset
ggplot(dataset, aes(x=HbA1c_level)) +
  geom_histogram(bins = 20, fill="blue", color="black") +
  labs(x = "HbA1c Level", y = "Frequency") +
  theme_minimal()
# Blood glucose level observed across dataset
ggplot(dataset, aes(x=blood_glucose_level)) +
  geom_histogram(bins = 20, fill="blue", color="black") +
  labs(x = "Blood Glucose Level", y = "Frequency") +
  theme_minimal()
# Diabetes count and histogram
print(table(dataset$diabetes))
ggplot(dataset, aes(x=diabetes)) +
  geom_histogram(bins = 20, fill="blue", color="black") +
  labs(x = "Diabetes", y = "Frequency") +
  theme_minimal()
# BMI histogram
ggplot(dataset, aes(x=bmi)) +
  geom_histogram(bins = 20, fill="blue", color="black") +
  labs(x = "BMI", y = "Frequency") +
  theme_minimal()


# ------- RELATION AND CORRELATION  -------


# Distribution of diabetes over gender groups
gender_diabetes_counts <- dataset %>%
  group_by(gender, diabetes) %>%
  summarise(count = n(), .groups = 'drop')
gender_diabetes_wide <- tidyr::spread(gender_diabetes_counts, key = diabetes, value = count, fill = 0)
# Calculate and Plot percentages of diabetes over gender group 
gender_diabetes_percentages <- gender_diabetes_wide %>%
  mutate(across(-gender, ~ . / sum(.) *100 ))
print("Percentage of Men and Women with Diabetes:")
print(gender_diabetes_percentages)
ggplot(dataset, aes(x = gender, fill = factor(diabetes))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("blue", "black"),
                    name = "Diabetes",
                    labels = c("Negative", "Positive")) +
  labs(x = "Gender", y = "Count", title = "Distribution of Diabetes Status Across Genders") +
  theme_minimal() +
  theme(legend.position = "top")

# Distribution of diabetes over glucose level groups
glucose_level_counts <- dataset %>%
  group_by(blood_glucose_level, diabetes) %>%
  summarise(count = n(), .groups = 'drop')
# Spread the data for readability
glucose_level_wide <- glucose_level_counts %>%
  pivot_wider(names_from = diabetes, values_from = count, values_fill = list(count = 0))

# Calculate percentage of diabetes across glucose level
glucose_level_percentages <- glucose_level_wide %>%
  mutate(across(-blood_glucose_level, ~ . / sum(.) * 100))
print("Percentage of Diabetes across different Glucose levels:")
print(glucose_level_percentages)

# Bin the glucose levels and plot for diabetes count
dataset$glucose_level <- cut(dataset$blood_glucose_level, breaks = c(50, 100, 150, 200, 250, 300, 350), right = FALSE)
ggplot(dataset, aes(x = glucose_level, fill = factor(diabetes))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("blue", "black"),
                    name = "Diabetes",
                    labels = c("Negative", "Positive")) +
  labs(x = "Glucose Level", y = "Count", title = "Distribution of Diabetes Status Across Glucose Levels") +
  theme_minimal() +
  theme(legend.position = "top")

# Create age intervals and plot it against diabetes 
dataset <- dataset %>%
  mutate(age_interval = cut(age, breaks = c(0, 20, 30, 40, 50, 60, 70), right = FALSE, labels = c("0-20", "20-30", "30-40", "40-50", "50-60", "60-70")))
ggplot(dataset, aes(x = age_interval, fill = as.factor(diabetes))) +
  geom_bar(position = "dodge") +
  labs(x = "Age Group", y = "Count", fill = "Diabetes") +
  scale_fill_manual(values = c("blue", "black"), labels = c("Negative", "Positive")) +
  ggtitle("Distribution of Diabetes Status Across Age Groups") +
  theme_minimal()

# Create weight intervals based on BMI and plot against diabetes
dataset <- dataset %>%
  mutate(weight_interval = cut(bmi, breaks = c(0, 10, 20, 30, 40, 50, 60), right = FALSE, labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60")))
ggplot(dataset, aes(x = weight_interval, fill = as.factor(diabetes))) +
  geom_bar(position = "dodge") +
  labs(x = "Weight Interval", y = "Count", fill = "Diabetes") +
  scale_fill_manual(values = c("blue", "black"), labels = c("Negative", "Positive")) +
  ggtitle("Distribution of Diabetes Status Across Weight Intervals") +
  theme_minimal()

#Create haemoglobin level intervals and plot against diabetes
dataset <- dataset %>%
  mutate(haemoglobin_level = cut(HbA1c_level, breaks = c(2, 4, 6, 8, 10), right = FALSE))
ggplot(dataset, aes(x = haemoglobin_level, fill = as.factor(diabetes))) +
  geom_bar(position = "dodge") +
  labs(x = "Haemoglobin Level", y = "Count", fill = "Diabetes") +
  scale_fill_manual(values = c("blue", "black"), labels = c("Negative", "Positive")) +
  ggtitle("Distribution of Diabetes Status Across Different Haemoglobin Levels") +
  theme_minimal()

# Calculating counts and percentages for haemoglobin level against diabetes
HbA1c_level_counts <- dataset %>%
  group_by(HbA1c_level, diabetes) %>%
  summarise(count = n(), .groups = 'drop')
HbA1c_level_percentages <- HbA1c_level_counts %>%
  group_by(HbA1c_level) %>%
  mutate(percentage = (count / sum(count)) * 100)
print("Percentage of Diabetes across different Glucose levels:")
print(HbA1c_level_percentages)


# ------- DATA PRE-PROCESSING -------


# Dropping specified columns (which were used during interval calculation)
dataset <- dataset %>%
  select(-c(glucose_level, age_interval, weight_interval, haemoglobin_level))

# Function to Convert categorical data to numerical data and applying on dataset
replace_categorical_with_numerical <- function(dataset, column_name) {
  dataset[[column_name]] <- as.numeric(as.factor(dataset[[column_name]]))
  return(dataset)
dataset <- replace_categorical_with_numerical(dataset, 'gender')
dataset <- replace_categorical_with_numerical(dataset, 'smoking_history')

# Null Values check and treatment
na_count <- dataset %>%
  summarise_all(~sum(is.na(.)))
print(na_count)

#Duplicate values check and treatment
duplicates <- duplicated(dataset)
num_duplicates <- sum(duplicated(dataset))
cat("\nNumber of Duplicate Instances:", num_duplicates, "\n")
# Removing duplicates and keeping the last occurrence
dataset <- dataset %>% distinct(., .keep_all = TRUE)
cat('After duplicate treatment, size of the dataset is:', dim(dataset)[1], 'rows and', dim(dataset)[2],'columns\n')

# Data Imbalance check and treatment
# Calculating the imbalance ratio
diabetes_counts <- table(dataset$diabetes)
minority_class <- min(diabetes_counts)
majority_class <- max(diabetes_counts)
imbalance_ratio <- (minority_class / majority_class) * 100
cat("Imbalance Ratio:", imbalance_ratio, "%\n")
# Plotting the distribution of the target variable to check balance ratio
ggplot(dataset, aes(x = diabetes)) +
  geom_bar() +
  labs(title = "Distribution of Target Variable", x = "Diabetes", y = "Count") +
  theme_minimal()

#OVERSAMPLING - Data Imbalance Treatment
# Selecting features for X and assigning target variable for y
X <- subset(dataset, select = c(gender, age, hypertension, heart_disease, smoking_history, bmi, HbA1c_level, blood_glucose_level))
# Selecting the target variable for y
y <- dataset$diabetes
set.seed(42) # For reproducibility
split <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[split,]
y_train <- y[split]
X_test <- X[-split,]
y_test <- y[-split]
# Display class distribution before oversampling
cat("Class Distribution Before Oversampling:\n")
print(table(y_train) / length(y_train))
# Identify the number of samples in the majority class
majority_class_size <- max(table(y_train))
# Set N to twice the size of the majority class for balanced oversampling
N_samples <- 2 * majority_class_size
# Apply oversampling using ROSE
data_balanced <- ovun.sample(diabetes ~ ., data = data.frame(X_train, diabetes = y_train), method = "over", 
                             N = N_samples, seed = 42)
X_resampled <- data_balanced$data[, -ncol(data_balanced$data)]
y_resampled <- data_balanced$data$diabetes
# Display class distribution after oversampling
cat("\nClass Distribution After Oversampling:\n")
print(table(y_resampled) / length(y_resampled))

# Outliers Check and Treatment
# Function to check for outliers using the IQR method
check_outliers <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  return(column < (Q1 - 1.5 * IQR) | column > (Q3 + 1.5 * IQR))
}
# Function to handle outliers by capping 
handle_outliers <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(pmin(pmax(column, lower_bound, na.rm = TRUE), upper_bound, na.rm = TRUE))
}
# Exclude the target variable 'diabetes' from outlier handling
columns_to_check <- setdiff(names(dataset), "diabetes")
for (col in columns_to_check) {
  original_column <- dataset[[col]]
  outliers_before <- sum(check_outliers(original_column), na.rm = TRUE)
   cat("Number of outliers in", col, "before handling:", outliers_before, "\n")
   modified_column <- handle_outliers(original_column)
   outliers_after <- sum(check_outliers(modified_column), na.rm = TRUE)
  dataset[[col]] <- modified_column
  cat("Number of outliers in", col, "after handling:", outliers_after, "\n\n")
}


# ------- CORRELATION MATRIX  -------


# Keep only numeric columns
numeric_dataset <- dataset[, sapply(dataset, is.numeric)]
correlation_matrix <- cor(numeric_dataset, use = "complete.obs")
corrplot(correlation_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("red", "yellow", "blue"))(200))
# Create Feature importance graph
correlation <- abs(cor(dataset)[, 'diabetes'])
correlation <- sort(correlation, decreasing = FALSE)
# Convert to data frame to fit ggplot and plot the same
correlation_dataset <- data.frame(Feature = names(correlation), Correlation = correlation)
ggplot(correlation_dataset, aes(x = Feature, y = Correlation)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Correlation Coefficient (Absolute Value)", y = "") +
  ggtitle("Feature Importance based on Correlation with Target") +
  theme_minimal()


# ------- IMPACT GRAPH  -------


# Fit the linear model and check summary
linearModel <- lm(diabetes ~ gender + age + hypertension + heart_disease + smoking_history + bmi + HbA1c_level + blood_glucose_level, data = dataset)
summary(linearModel)
importance <- as.data.frame(varImp(linearModel, scale = TRUE))
# Clean the variable names incase of any non-alphabetic character
row.names(importance) <- gsub("[[:punct:][:blank:]]+", "", row.names(importance))
# Order the importance data and plot the importance% in ascending order
ordered_importance <- importance[order(importance$Overall), , drop = FALSE]
barplot(t(ordered_importance), 
        main = "Variable Importance",
        xlab = "Variables",
        ylim = c(0, 40),
        ylab = "Importance",
        las = 2, # makes axis labels perpendicular to axis
        cex.names = 0.7) # adjusts size of variable names
#Handle the code incase the target variable doesnt exist
if (!"diabetes" %in% names(dataset)) {
  stop("Column 'diabetes' not found in the dataset")
}


# ------- DATA SPLITTING  -------


#Consider the threshold as 35% and consider variables greater than threshold to create another dataset
#Splitting the data into train and test sets for both datasets
#Original Dataset - considering all the variables -- (hereby used as 'dataset)
set.seed(123)
splitIndex <- createDataPartition(dataset$diabetes, p = .70, list = FALSE)
train_data <- dataset[splitIndex,]
test_data <- dataset[-splitIndex,]
#Created Dataset - considering variables whose impact is greater than threshold 35% -- hereby used as 'dataset1'
dataset1 <- subset(dataset,select=c('age','HbA1c_level','blood_glucose_level','diabetes'))
splitIndex1 <- createDataPartition(dataset1$diabetes, p = .70, list = FALSE)
train_data1 <- dataset1[splitIndex1,]
test_data1 <- dataset1[-splitIndex1,]


# ------- MODELLING  -------


# For Original Dataset


# Split the dataset into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(dataset$diabetes, p = 0.7, list = FALSE)
train_data <- dataset[trainIndex, ]
test_data <- dataset[-trainIndex, ]
#Define a function to calculate all metrics : Accuracy,Precision,Recall and F1_Score
calculate_metrics <- function(predictions, actuals) {
  if (class(predictions) == "factor") {
    predicted_classes <- predictions
  } else {
    predicted_classes <- ifelse(predictions > 0.5, "1", "0")
    predicted_classes <- factor(predicted_classes, levels = c("0", "1"))
  }
  actuals <- factor(actuals, levels = c("0", "1"))
  predicted_classes <- factor(predicted_classes, levels = c("0", "1"))
  accuracy <- confusion_matrix$overall['Accuracy']
  precision <- confusion_matrix$byClass['Pos Pred Value']
  recall <- confusion_matrix$byClass['Sensitivity']
  F1_score <- 2 * (precision * recall) / (precision + recall)
  return(c(accuracy, precision, recall, F1_score))
}

# Build models on original dataset and evaluate using the defined function
# Logistic Regression
logistic_model <- glm(diabetes ~ ., data = train_data, family = "binomial")
logistic_predictions <- predict(logistic_model, newdata = test_data, type = "response")
logistic_predicted_classes <- ifelse(logistic_predictions > 0.5, "1", "0")
logistic_metrics <- calculate_metrics(logistic_predicted_classes, test_data$diabetes)
# Decision Tree
tree_model <- rpart(diabetes ~ ., data = train_data, method = "class")
tree_predictions <- predict(tree_model, newdata = test_data, type = "class")
tree_metrics <- calculate_metrics(tree_predictions, test_data$diabetes)
# Random Forest
random_forest_model <- randomForest(diabetes ~ ., data = train_data)
random_forest_predictions <- predict(random_forest_model, newdata = test_data)
random_forest_metrics <- calculate_metrics(random_forest_predictions, test_data$diabetes)
# KNN
knn_predictions <- knn(train = trainData[, -which(names(train_data) == "diabetes")], 
                       test = testData[, -which(names(test_data) == "diabetes")], 
                       cl = trainData$diabetes, k = 5)
knn_metrics <- calculate_metrics(knn_predictions, test_data$diabetes)
# Naive Bayes
naive_bayes_model <- naiveBayes(diabetes ~ ., data = train_data)
naive_bayes_predictions <- predict(naive_bayes_model, newdata = test_data)
naive_bayes_metrics <- calculate_metrics(naive_bayes_predictions, test_data$diabetes)
# SVM
svm_model <- svm(diabetes ~ ., data = train_data)
svm_predictions <- predict(svm_model, newdata = test_data)
svm_metrics <- calculate_metrics(svm_predictions, test_data$diabetes)
head(svm_metrics)
# Create a dataframe to store the calculated metrics of all models
model_metrics <- data.frame(
  Model = c("Logistic Regression", "Decision Tree", "Random Forest", "KNN", "Naive Bayes", "SVM"),
  Accuracy = c(logistic_metrics$Accuracy, tree_metrics$Accuracy, random_forest_metrics$Accuracy, knn_metrics$Accuracy, naive_bayes_metrics$Accuracy, svm_metrics$Accuracy),
  Precision = c(logistic_metrics$Precision, tree_metrics$Precision, random_forest_metrics$Precision, knn_metrics$Precision, naive_bayes_metrics$Precision, svm_metrics$Precision)
)

#For Dataset created based on threshold defined


set.seed(123)
trainIndex1 <- createDataPartition(dataset$diabetes, p = 0.7, list = FALSE)
train_data1 <- dataset1[trainIndex1, ]
test_data1 <- dataset1[-trainIndex1, ]
#Define function to calculate all metrix
calculate_metrics <- function(predictions, actuals) {
  if (class(predictions) == "factor") {
    predicted_classes <- predictions
  } else {
    predicted_classes <- ifelse(predictions > 0.5, "1", "0")
    predicted_classes <- factor(predicted_classes, levels = c("0", "1"))
  }
  actuals <- factor(actuals, levels = c("0", "1"))
  predicted_classes <- factor(predicted_classes, levels = c("0", "1"))
  accuracy1 <- confusion_matrix1$overall['Accuracy']
  precision1 <- confusion_matrix1$byClass['Pos Pred Value']
  recall1 <- confusion_matrix1$byClass['Sensitivity']
  F1_score1 <- 2 * (precision1 * recall1) / (precision1 + recall1)
  return(c(accuracy1, precision1, recall1, F1_score1))
}

# Build models on original dataset and evaluate using the defined function
# Logistic Regression
logistic_model1 <- glm(diabetes ~ ., data = train_data1, family = "binomial")
logistic_predictions1 <- predict(logistic_model1, newdata = test_data1, type = "response")
logistic_predicted_classes1 <- ifelse(logistic_predictions1 > 0.5, "1", "0")
logistic_metrics1 <- calculate_metrics(logistic_predicted_classes1, test_data1$diabetes)
head(logistic_metrics1)
# Decision Tree
tree_model1 <- rpart(diabetes ~ ., data = train_data1, method = "class")
tree_predictions1 <- predict(tree_model1, newdata = test_data1, type = "class")
tree_metrics1 <- calculate_metrics(tree_predictions1, test_data1$diabetes)
head(tree_metrics1)
# Random Forest
random_forest_model1 <- randomForest(diabetes ~ ., data = train_data1)
random_forest_predictions1 <- predict(random_forest_model1, newdata = test_data1)
random_forest_metrics1 <- calculate_metrics(random_forest_predictions1, test_data1$diabetes)
head(random_forest_metrics1)
# KNN
knn_predictions1 <- knn(train = train_data1[, -which(names(train_data1) == "diabetes")], 
                       test = test_data1[, -which(names(test_data1) == "diabetes")], 
                       cl = train_data1$diabetes, k = 5)
knn_metrics1 <- calculate_metrics(knn_predictions1, test_data1$diabetes)
head(knn_metrics1)
# Naive Bayes
naive_bayes_model1 <- naiveBayes(diabetes ~ ., data = train_data1)
naive_bayes_predictions1 <- predict(naive_bayes_model1, newdata = test_data1)
naive_bayes_metrics1 <- calculate_metrics(naive_bayes_predictions1, test_data1$diabetes)
head(naive_bayes_metrics1)
# SVM
svm_model1 <- svm(diabetes ~ ., data = train_data1)
svm_predictions1 <- predict(svm_model1, newdata = test_data1)
svm_metrics1 <- calculate_metrics(svm_predictions1, test_data1$diabetes)
head(svm_metrics1)

#Creating ROC-AUC curve for dataset 1

# Assuming logistic_model1 is your trained logistic regression model
#Logistic Regression
actual <- factor(test_data$diabetes, levels = c(0, 1))
logistic_predicted_classes <- factor(logistic_predicted_classes, levels = levels(actual))
logistic_roc <- pROC::roc(test_data$diabetes, logistic_predictions)
logistic_auc <- pROC::auc(logistic_roc)
logistic_confusion <- caret::confusionMatrix(logistic_predicted_classes, test_data$diabetes)
#Decision Tree
tree_predictions <- factor(tree_predictions, levels = levels(actual))
tree_roc <- pROC::roc(test_data$diabetes, as.numeric(tree_predictions))
tree_auc <- pROC::auc(tree_roc)
tree_confusion <- caret::confusionMatrix(tree_predictions, test_data$diabetes)
#Random Forest
random_forest_predictions <- factor(random_forest_predictions, levels = levels(actual))
random_forest_roc <- pROC::roc(test_data$diabetes, as.numeric(random_forest_predictions))
random_forest_auc <- pROC::auc(random_forest_roc)
random_forest_confusion <- caret::confusionMatrix(random_forest_predictions, test_data$diabetes)
#K Nearest Neighbour
knn_model <- factor(knn_model, levels = levels(actual))
knn_roc <- pROC::roc(test_data$diabetes, as.numeric(knn_model))
knn_auc <- pROC::auc(knn_roc)
knn_confusion <- caret::confusionMatrix(knn_model, test_data$diabetes)
#Naive Bayes
naive_bayes_predictions <- factor(naive_bayes_predictions, levels = levels(actual))
naive_bayes_roc <- pROC::roc(test_data$diabetes, as.numeric(naive_bayes_predictions))
naive_bayes_auc <- pROC::auc(naive_bayes_roc)
naive_bayes_confusion <- caret::confusionMatrix(naive_bayes_predictions, test_data$diabetes)
#SVM
svm_predictions <- factor(svm_predictions, levels = levels(actual))
svm_roc <- pROC::roc(test_data$diabetes, as.numeric(svm_predictions))
svm_auc <- pROC::auc(svm_roc)
svm_confusion <- caret::confusionMatrix(svm_predictions, test_data$diabetes)

#Creating ROC-AUC curve for dataset 2 

#Logistic Regression
actual <- factor(test_data1$diabetes, levels = c(0, 1))
logistic_predicted_classes1 <- factor(logistic_predicted_classes1, levels = levels(actual))
logistic_roc1 <- pROC::roc(test_data1$diabetes, logistic_predictions)
logistic_auc1 <- pROC::auc(logistic_roc1)
logistic_confusion1 <- caret::confusionMatrix(logistic_predicted_classes1, test_data1$diabetes)
#Decision Tree
tree_predictions1 <- factor(tree_predictions1, levels = levels(actual))
tree_roc1 <- pROC::roc(test_data1$diabetes, as.numeric(tree_predictions1))
tree_auc1 <- pROC::auc(tree_roc1)
tree_confusion1 <- caret::confusionMatrix(tree_predictions1, test_data1$diabetes)
#Random Forest
random_forest_predictions1 <- factor(random_forest_predictions1, levels = levels(actual))
random_forest_roc1 <- pROC::roc(test_data1$diabetes, as.numeric(random_forest_predictions1))
random_forest_auc1 <- pROC::auc(random_forest_roc1)
random_forest_confusion1 <- caret::confusionMatrix(random_forest_predictions1, test_data1$diabetes)
#K Nearest Neighbour
# Assuming 'diabetes' is your target variable and it's located in the last column of your dataset
knn_model1 <- knn(train = train_data1[, -ncol(train_data1)], 
                  test = test_data1[, -ncol(test_data1)], 
                  cl = train_data1$diabetes, 
                  k = 5) 
knn_model1 <- factor(knn_model1, levels = levels(actual))
knn_roc1 <- pROC::roc(test_data1$diabetes, as.numeric(knn_model))
knn_auc1 <- pROC::auc(knn_roc1)
knn_confusion1 <- caret::confusionMatrix(knn_model1, test_data1$diabetes)
#Naive Bayes
naive_bayes_predictions1 <- factor(naive_bayes_predictions1, levels = levels(actual))
naive_bayes_roc1 <- pROC::roc(test_data1$diabetes, as.numeric(naive_bayes_predictions1))
naive_bayes_auc1 <- pROC::auc(naive_bayes_roc1)
naive_bayes_confusion1 <- caret::confusionMatrix(naive_bayes_predictions1, test_data1$diabetes)
#SVM
svm_predictions1 <- factor(svm_predictions1, levels = levels(actual))
svm_roc1 <- pROC::roc(test_data1$diabetes, as.numeric(svm_predictions1))
svm_auc1 <- pROC::auc(svm_roc1)
svm_confusion1 <- caret::confusionMatrix(svm_predictions1, test_data1$diabetes)

# ------- COMPARISON OF METRICES  -------


# PLOTTING METRICES FOR DATASET 1:
#Accuray Plot
accuracy_logistic_regression <- 0.9578333
accuracy_naive_bayes <- 0.9561000
accuracy_decision_tree <- 0.9718667
accuracy_random_forest <- 0.9718667
accuracy_knn <- 0.9601667
accuracy_svm <- 0.9681667
# Create a data frame for plotting
accuracy_data <- data.frame(
  Model = c("Logistic Regression", "Naive Bayes", "Decision Tree", "Random Forest", "KNN", "SVM"),
  Accuracy = c(accuracy_logistic_regression, accuracy_naive_bayes, accuracy_decision_tree, accuracy_random_forest, accuracy_knn, accuracy_svm)
)
# Plotting
ggplot(accuracy_data, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.2f", Accuracy)), vjust = -0.5, color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Comparison of Classifier Accuracies for Data 2", x = "Model", y = "Accuracy") 


# PLOTTING METRICES FOR DATASET 2:
#Accuracy Plot 
accuracy_logistic_regression <- 0.9600333
accuracy_naive_bayes <- 0.9041333
accuracy_decision_tree <- 0.9718667
accuracy_random_forest <- 0.9719000
accuracy_knn <- 0.9531333
accuracy_svm <- 0.9627333
# Create a data frame for plotting
accuracy_data <- data.frame(
  Model = c("Logistic Regression", "Naive Bayes", "Decision Tree", "Random Forest", "KNN", "SVM"),
  Accuracy = c(accuracy_logistic_regression, accuracy_naive_bayes, accuracy_decision_tree, accuracy_random_forest, accuracy_knn, accuracy_svm)
)
# Plotting
ggplot(accuracy_data, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.2f", Accuracy)), vjust = -0.5, color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Comparison of Classifier Accuracies for Data 2", x = "Model", y = "Accuracy")


# ------- ACCURACY COMPARISON ACROSS ALL MODELS -------

# Create a data frame
accuracy_data <- data.frame(
  Model = rep(c("Logistic Regression", "Naive Bayes", "Decision Tree", "Random Forest", "KNN", "SVM"), times = 2),
  Dataset = rep(c("Dataset", "Dataset 1"), each = 6),
  Accuracy = c(0.9600333, 0.9041333, 0.9718667, 0.9719000, 0.9531333, 0.9627333,  # Dataset 1 accuracies
               0.9578333, 0.9561000, 0.9718667, 0.9718667, 0.9601667, 0.9681667)  # Dataset 2 accuracies
)
#Plot the comparison
ggplot(accuracy_data, aes(x = Model, y = Accuracy, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Comparison of Model Accuracies", x = "Model", y = "Accuracy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#------- PRECISION COMPARISON ACROSS ALL MODELS -------

#Precision across dataset1
precision_logistic_regression_dataset1 <- 0.9661872
precision_naive_bayes_dataset1 <- 0.9651348
precision_decision_tree_dataset1 <- 0.9701704
precision_random_forest_dataset1 <- 0.9705029
precision_knn_dataset1 <- 0.9571689
precision_svm_dataset1 <- 0.9626142
#Precision across Dataset2
precision_logistic_regression_dataset2 <- 0.9630088
precision_naive_bayes_dataset2 <- 0.9598452
precision_decision_tree_dataset2 <- 0.9701704
precision_random_forest_dataset2 <- 0.9701704
precision_knn_dataset2 <- 0.9616331
precision_svm_dataset2 <- 0.9663792
# Create a data frame
precision_data <- data.frame(
  Model = rep(c("Logistic Regression", "Naive Bayes", "Decision Tree", "Random Forest", "KNN", "SVM"), times = 2),
  Dataset = rep(c("Dataset 1", "Dataset 2"), each = 6),
  Precision = c(precision_logistic_regression_dataset1, precision_naive_bayes_dataset1, precision_decision_tree_dataset1, precision_random_forest_dataset1, precision_knn_dataset1, precision_svm_dataset1,
                precision_logistic_regression_dataset2, precision_naive_bayes_dataset2, precision_decision_tree_dataset2, precision_random_forest_dataset2, precision_knn_dataset2, precision_svm_dataset2)
)
#Plot the Precision
ggplot(precision_data, aes(x = Model, y = Precision, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Comparison of Model Precisions", x = "Model", y = "Precision") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#------- RECALL COMPARISON -------

# Recall across different models for Dataset1
recall_logistic_regression_dataset1 <- 0.9910018  # Example values
recall_naive_bayes_dataset1 <- 0.9287796
recall_decision_tree_dataset1 <- 1.0000000
recall_random_forest_dataset1 <- 0.9996357
recall_knn_dataset1 <- 0.9932240
recall_svm_dataset1 <- 0.9980328
# Recall across different models for Dataset2
recall_logistic_regression_dataset2 <- 0.9920219
recall_naive_bayes_dataset2 <- 0.9935883
recall_decision_tree_dataset2 <- 1.0000000
recall_random_forest_dataset2 <- 1.0000000
recall_knn_dataset2 <- 0.9961749
recall_svm_dataset2 <- 1.0000000
# Create a data frame
recall_data <- data.frame(
  Model = rep(c("Logistic Regression", "Naive Bayes", "Decision Tree", "Random Forest", "KNN", "SVM"), times = 2),
  Dataset = rep(c("Dataset 1", "Dataset 2"), each = 6),
  Recall = c(recall_logistic_regression_dataset1, recall_naive_bayes_dataset1, recall_decision_tree_dataset1, recall_random_forest_dataset1, recall_knn_dataset1, recall_svm_dataset1,
             recall_logistic_regression_dataset2, recall_naive_bayes_dataset2, recall_decision_tree_dataset2, recall_random_forest_dataset2, recall_knn_dataset2, recall_svm_dataset2)
)
#Plot the comparison
ggplot(recall_data, aes(x = Model, y = Recall, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Comparison of Model Recalls", x = "Model", y = "Recall") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#------- F1 COMPARISON -------

# F1_Score across different models for Dataset1
f1_logistic_regression_dataset1 <- 0.9784372  # Example values
f1_naive_bayes_dataset1 <- 0.9466083
f1_decision_tree_dataset1 <- 0.9848594
f1_random_forest_dataset1 <- 0.9848539
f1_knn_dataset1 <- 0.9748632
f1_svm_dataset1 <- 0.9800036
# F1_Score across different models for Dataset2
f1_logistic_regression_dataset2 <- 0.9773001
f1_naive_bayes_dataset2 <- 0.9764253
f1_decision_tree_dataset2 <- 0.9848594
f1_random_forest_dataset2 <- 0.9848594
f1_knn_dataset2 <- 0.9785993
f1_svm_dataset2 <- 0.9829022
# Create a data frame to store all the comparison values
f1_data <- data.frame(
  Model = rep(c("Logistic Regression", "Naive Bayes", "Decision Tree", "Random Forest", "KNN", "SVM"), times = 2),
  Dataset = rep(c("Dataset 1", "Dataset 2"), each = 6),
  F1_Score = c(f1_logistic_regression_dataset1, f1_naive_bayes_dataset1, f1_decision_tree_dataset1, f1_random_forest_dataset1, f1_knn_dataset1, f1_svm_dataset1,
               f1_logistic_regression_dataset2, f1_naive_bayes_dataset2, f1_decision_tree_dataset2, f1_random_forest_dataset2, f1_knn_dataset2, f1_svm_dataset2)
)
#Plot the data to compare
ggplot(f1_data, aes(x = Model, y = F1_Score, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Comparison of Model F1 Scores", x = "Model", y = "F1 Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#------- AUC COMPARISON -------

# AUV score across different models for Dataset1
auc_logistic_regression_dataset1 <- 0.9594  # Example values
auc_naive_bayes_dataset1 <- 0.7838
auc_decision_tree_dataset1 <- 0.8345
auc_random_forest_dataset1 <- 0.8363
auc_knn_dataset1 <- 0.7572
auc_svm_dataset1 <- 0.7904
# AUC score across different models for Dataset2
auc_logistic_regression_dataset2 <- 0.9594
auc_naive_bayes_dataset2 <- 0.7731
auc_decision_tree_dataset2 <- 0.8345
auc_random_forest_dataset2 <- 0.8345
auc_knn_dataset2 <- 0.7572
auc_svm_dataset2 <- 0.8127
# Create a data frame
auc_data <- data.frame(
  Model = rep(c("Logistic Regression", "Naive Bayes", "Decision Tree", "Random Forest", "KNN", "SVM"), times = 2),
  Dataset = rep(c("Dataset 1", "Dataset 2"), each = 6),
  AUC = c(auc_logistic_regression_dataset1, auc_naive_bayes_dataset1, auc_decision_tree_dataset1, auc_random_forest_dataset1, auc_knn_dataset1, auc_svm_dataset1,
          auc_logistic_regression_dataset2, auc_naive_bayes_dataset2, auc_decision_tree_dataset2, auc_random_forest_dataset2, auc_knn_dataset2, auc_svm_dataset2)
)
#Plot the comparison data 
ggplot(auc_data, aes(x = Model, y = AUC, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Comparison of Model AUC Scores", x = "Model", y = "AUC") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#------- INDIVIDUAL MODELS AND THEIR COMPARISON AGAINST LOGISTIC REGRESSION (COMMON MODEL) -------


# Logistic Regression vs Decision Tree: Nimisha


#For dataset1
#Define a dataframe to store metrix values for both models
metrics_data <- data.frame(
  Model = rep(c("Logistic Regression", " Decision Tree"), each = 5),
  Metric = rep(c("Accuracy", "Precision", "Recall", "F1 Score", "AUC"), times = 2),
  Value = c(0.9600333, 0.9661872, 0.9910018, 0.9784372, 0.9594,  # Replace with actual values for Model A
            0.9718667, 0.9701704, 1.00000, 0.9848594, 0.8345)  # Replace with actual values for Model B
)
#Plot the comparison
ggplot(metrics_data, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Comparison of Logistic Regression and Decision Tree Metrics", x = "Metric", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#For dataset2
#Define a dataframe to store metrix values for both models
metrics_data1 <- data.frame(
  Model = rep(c("Logistic Regression", " Decision Tree"), each = 5),
  Metric = rep(c("Accuracy", "Precision", "Recall", "F1 Score", "AUC"), times = 2),
  Value = c(0.9578333, 0.9630088, 0.9920219, 0.9773001, 0.9594,  # Replace with actual values for Model A
            0.9718667, 0.9701704, 1.00000, 0.9848594, 0.8345)  # Replace with actual values for Model B
)
#Plot the comparison
ggplot(metrics_data1, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Comparison of Logistic Regression and Decision Tree Metrics", x = "Metric", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Logistic Regression vs Random Forest : Wilson


#For dataset1
#Define a dataframe to store metrix values for both models
metrics_data <- data.frame(
  Model = rep(c("Logistic Regression", "Random Forest"), each = 5),
  Metric = rep(c("Accuracy", "Precision", "Recall", "F1 Score", "AUC"), times = 2),
  Value = c(0.9600333, 0.9661872, 0.9910018, 0.9784372, 0.9594,  # Replace with actual values for Model A
            0.9718000, 0.9704676, 0.9995993, 0.9848180, 0.8361)  # Replace with actual values for Model B
)
#Plot the comparison
ggplot(metrics_data, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Comparison of Logistic Regression and Random Forest Metrics", x = "Metric", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#For dataset2
#Define a dataframe to store metrix values for both models
metrics_data1 <- data.frame(
  Model = rep(c("Logistic Regression", " Random Forest"), each = 5),
  Metric = rep(c("Accuracy", "Precision", "Recall", "F1 Score", "AUC"), times = 2),
  Value = c(0.9600333, 0.9661872, 0.9910018, 0.9784372, 0.9594,  # Replace with actual values for Model A
            0.9718667, 0.9701704, 1.000000, 0.9848594, 0.8345)  # Replace with actual values for Model B
)
#Plot the comparison
ggplot(metrics_data1, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Comparison of Logistic Regression and Random Forest Metrics", x = "Metric", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### Logistic Regression vs K Nearest Neighbour : Vasanth


#For dataset1
#Define a dataframe to store metrix values for both models
metrics_data <- data.frame(
  Model = rep(c("Logistic Regression", "KNN"), each = 5),
  Metric = rep(c("Accuracy", "Precision", "Recall", "F1 Score", "AUC"), times = 2),
  Value = c(0.9600333, 0.9661872, 0.9910018, 0.9784372, 0.9594,  # Replace with actual values for Model A
            0.9531333, 0.9571368, 0.9932605, 0.9748641, 0.7572)  # Replace with actual values for Model B
)
#Plot the comparison
ggplot(metrics_data, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Comparison of Logistic Regression and KNN Metrics", x = "Metric", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#For dataset2
#Define a dataframe to store metrix values for both models
metrics_data <- data.frame(
  Model = rep(c("Logistic Regression", "KNN"), each = 5),
  Metric = rep(c("Accuracy", "Precision", "Recall", "F1 Score", "AUC"), times = 2),
  Value = c(0.9600333, 0.9661872, 0.9910018, 0.9784372, 0.9594,  # Replace with actual values for Model A
            0.9601333, 0.9615358, 0.9962842, 0.9786016, 0.7572)  # Replace with actual values for Model B
)
#Plot the comparison
ggplot(metrics_data, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Comparison of Logistic Regression and KNN Metrics", x = "Metric", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Logistic Regression vs SVM : Jegan


#For dataset1
#Define a dataframe to store metrix values for both models
metrics_data <- data.frame(
  Model = rep(c("Logistic Regression", " SVM"), each = 5),
  Metric = rep(c("Accuracy", "Precision", "Recall", "F1 Score", "AUC"), times = 2),
  Value = c(0.9600333, 0.9661872, 0.9910018, 0.9784372, 0.9594,  # Replace with actual values for Model A
            0.9627333, 0.9626142, 0.9980328, 0.9800036, 0.7904)  # Replace with actual values for Model B
)
#Plot the comparison
ggplot(metrics_data, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Comparison of Logistic Regression and SVM Metrics", x = "Metric", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#For dataset2
#Define a dataframe to store metrix values for both models
metrics_data <- data.frame(
  Model = rep(c("Logistic Regression", "SVM"), each = 5),
  Metric = rep(c("Accuracy", "Precision", "Recall", "F1 Score", "AUC"), times = 2),
  Value = c(0.9600333, 0.9661872, 0.9910018, 0.9784372, 0.9594,  # Replace with actual values for Model A
            0.9681667, 0.9663792, 1.000000, 0.9829022, 0.8127)  # Replace with actual values for Model B
)
#Plot the comparison
ggplot(metrics_data, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Comparison of Logistic Regression and SVM Metrics", x = "Metric", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Logistic Regression vs Naive Bayes : Nayana


#For dataset
#Define a dataframe to store metrix values for both models
metrics_data <- data.frame(
  Model = rep(c("Logistic Regression", "Naive Bayes"), each = 5),
  Metric = rep(c("Accuracy", "Precision", "Recall", "F1 Score", "AUC"), times = 2),
  Value = c(0.9600333, 0.9661872, 0.9910018, 0.9784372, 0.9594,  # Replace with actual values for Model A
            0.9041333, 0.9651348, 0.9287796, 0.9466083, 0.7838)  # Replace with actual values for Model B
)
#Plot the comparison
ggplot(metrics_data, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Comparison of Logistic Regression and Naive Bayes Metrics", x = "Metric", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#For dataset2
#Define a dataframe to store metrix values for both models
metrics_data <- data.frame(
  Model = rep(c("Logistic Regression", "Naive Bayes"), each = 5),
  Metric = rep(c("Accuracy", "Precision", "Recall", "F1 Score", "AUC"), times = 2),
  Value = c(0.9600333, 0.9661872, 0.9910018, 0.9784372, 0.9594,  # Replace with actual values for Model A
            0.9561000, 0.9598452, 0.9935883, 0.9764253, 0.7731)  # Replace with actual values for Model B
)
#Plot the comparison
ggplot(metrics_data, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Comparison of Logistic Regression and Naive Bayes Metrics", x = "Metric", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))