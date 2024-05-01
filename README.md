# Heart-Disease-Decision-Tree
This repository contains code to train a decision tree mode to predict whether an individual has heart disease based on the given features.


#Importing the dataset : You can find the dataset in my github account
heart.data <- read.csv("D:/Datasets/heart.csv")
View(heart.data)
summary(heart.data)

#To familiarize with the dataset.
#To check if there are missing data
sum(is.na(heart.data))
colSums(is.na(heart.data))

#To check the number of #columns and #rows
ncol(heart.data)
nrow(heart.data)

# This next steps involve Splitting the dataset into training and testing sets

# I Set seed for reproducibility
set.seed(123)

# The percentage of data to be used for training (e.g., 70%)
train_percentage <- 0.7

# To generate random indices for splitting
indices <- sample(1:nrow(heart.data))

# Calculating the number of rows for training and testing
num_train <- round(train_percentage * nrow(heart.data))
num_test <- nrow(heart.data) - num_train

# Splitting the dataset into training and testing sets
train_data <- heart.data[indices[1:num_train], ]
test_data <- heart.data[indices[(num_train + 1):nrow(heart.data)], ]

# Output of the dimensions of the training and testing sets
cat("Training set dimensions:", dim(train_data), "\n")
cat("Testing set dimensions:", dim(test_data), "\n")

#The following code is about the Decision tree model
# Load the rpart package
library(rpart)

# Defining the formula
formula <- target ~ age + sex + cp + trestbps + chol + fbs + restecg + thalach + exang + oldpeak + slope + ca + thal

# Building the decision tree model
decision_tree <- rpart(formula, data = train_data, method = "class")

# Print and plotting the decision tree
print(decision_tree)
plot(decision_tree)
text(decision_tree, cex = 1)

# To Make predictions on test data
predictions <- predict(decision_tree, test_data, type = "class")

# To Evaluate model performance
confusion_matrix <- table(predictions, test_data$target)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(confusion_matrix)
print(paste("Accuracy:", accuracy))

