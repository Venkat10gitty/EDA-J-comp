library(tree)
library(ISLR)

# Function calculate_variable_matrix accepts standardized_data_frame which can be either training or test dataset 
# The function adds the intercept in the first column of the matrix
# The function returns the standardized_auto_data_matrix
calculate_variable_matrix <- function(standardized_data_frame){
  variable_frame <- subset(standardized_data_frame, select = -c(mpg,origin))
  Beta_frame <- data.frame("Beta_0" = c(1))
  standardized_auto_data_matrix <- data.matrix(Beta_frame)
  standardized_auto_data_matrix <- as.matrix(merge(standardized_auto_data_matrix,variable_frame))
  return (standardized_auto_data_matrix)
}

# Function calculate_sigmoid calculates the sigmoid value for the training or test dataset
# the function accepts standardized_data_matrix which can be either training or test dataset
# coeffecient_data_matrix_1 holds the initial weights
# label_list_length holds the length of the training or test dataset
calculate_sigmoid <- function(standardized_data_matrix, coefficient_data_matrix_1, label_list_length){
  result_matrix <- standardized_data_matrix%*%coefficient_data_matrix_1
  sigmoid_vector <- vector()
  for(i in 1:label_list_length)
  {
    exp_value = 1+exp(-result_matrix[i])  
    sigmoid_vector[i] <- c(1/exp_value) 
  }
  return (sigmoid_vector)
}

# Function calculate_MSE calculates the mean_squared_error 
# the function accepts sigmoid_vector, prediction_label_list which corresponds the variable High and the length of the training or test set
# the function returns the MSE for the training or the test set
calculate_MSE <- function(sigmoid_vector, prediction_label_list, label_list_length){
  mean_squared_sum <- 0
  mean_squared_error <- 0
  for(i in 1:label_list_length)
  {
    mean_squared_sum <- mean_squared_sum + (prediction_label_list[i]-sigmoid_vector[i])^2
  }
  mean_squared_error <- mean_squared_sum/label_list_length
  return (mean_squared_error)
}

# the function calculate_derivative corresponds to computing the Coeffecients which pertains to step 2 of B) alternate approach
# function accepts the argument label_list_length ~ length of the training or test dataset, sigmoid vector and 
# label_vector holds the variable High
# the function returns the intermediate value derivative_vector
calculate_derivative <- function(label_list_length, sigmoid_vector, label_vector){
  derivative_vector <- vector()
  for(i in 1:label_list_length){
    #arg1 <- label_vector[i]-sigmoid_vector[i]
    #arg2 <- (1 - sigmoid_vector[i])
    derivative_vector[i] <- c(2*(label_vector[i]-sigmoid_vector[i])*sigmoid_vector[i]*(1 - sigmoid_vector[i]))
  }
  return (derivative_vector)
}

# Question No.6 - GD6 - function calculate_gradient_descent runs the logistic regression for the training or test dataset
# The algorithm is run for max_iteration number of times (number of steps)
# function returns the MSE_vector
calculate_gradient_descent <- function(max_iterations, standardized_data_matrix, coefficient_data_matrix, label_list_length, label_list){
  MSE_vector <- vector()
  for(iterator in 1:max_iterations){
    error_sum <- 0
    result_sigmoid_vector <- vector()
    result_sigmoid_vector <- calculate_sigmoid(standardized_data_matrix,coefficient_data_matrix,label_list_length)
    error_sum <- calculate_MSE(result_sigmoid_vector,label_list,label_list_length)
    MSE_vector[iterator] <- c(error_sum)
    #print(error_sum)
    #print(coefficient_data_matrix)
    derivative_matrix <- as.matrix(calculate_derivative(label_list_length, result_sigmoid_vector, label_list))
    for(i in 1:length(coefficient_data_matrix)){
      weight_matrix <- 0
      for(j in 1:length(label_list_length)){
        weight_matrix <- weight_matrix + (derivative_matrix[j]*standardized_data_matrix[j,i])
      }
      coefficient_data_matrix[i] = coefficient_data_matrix[i] + (learning_rate/label_list_length)*weight_matrix
    }
  }
  return (MSE_vector)
}

Auto <- read.table("Auto.data")
Auto <- read.table("Auto.data", header=T, na.strings="?")
Auto <- na.omit(Auto)
attach(Auto)

# Question No.2 - GD2 - Using the Auto Dataset creating a new variable high based on mpg
Auto <- subset(Auto, select = -c(cylinders,displacement,acceleration,name))
High <- ifelse(mpg>=23,1,0)
Auto <- data.frame(Auto,High)
# Question No.2 - GD2 - Predicting High with given horsepower, weight, year and origin
tree.Auto <- tree(High~horsepower+weight+year+origin,Auto)
origin_1 <- ifelse(Auto$origin == 1, 1,0)
origin_2 <- ifelse(Auto$origin == 2, 1,0)

# Question No.2 - GD2 - Adding Dummy variables origin_1 and origin_2 created from origin 
Auto_dummy <- data.frame(Auto,origin_1,origin_2)

# Question No.3 - GD3 - Splitting the Dataset into train and test sets with seed as Date of birth 
set.seed(0701)
train <- sample(1:nrow(Auto_dummy), 196)
train.X <- Auto_dummy[train,]
test.X <- Auto_dummy[-train,]
train.y <- High[train]
test.y <- High[-train]

subset_train <- subset(train.X, select = -c(High))
subset_test <- subset(test.X, select = -c(High))

# Scaling the training and test dataset
standardized.train.X <- scale(subset_train)
standardized.test.X <- scale(subset_test)

# Question No.4 - GD4 - Training the dataset with initial weights in the range of -0.7 to 0.7
random_coefficients <- runif(6, -0.7, 0.7)
coefficients_frame <- data.frame("Coefficients" = c(random_coefficients))
coefficient_data_matrix <- data.matrix(coefficients_frame)

# Finding the length of training and testing set 
train_y_length <- length(train.y)
test_y_length <- length(test.y)

# standardized_train_data_matrix and standardized_test_data_matrix matrices hold the scaled values along with the first column being the intercept
standardized_train_data_matrix <- calculate_variable_matrix(standardized.train.X)
standardized_test_data_matrix  <- calculate_variable_matrix(standardized.test.X)

learning_rate <- 0.1
threshold_value <- 0.5
max_iterations <- 10000

# Calculating Logistic Regression for Training DataSet
train_MSE_vector <- calculate_gradient_descent(max_iterations,standardized_train_data_matrix,coefficient_data_matrix,train_y_length,train.y)
boxplot(train_MSE_vector)

# Calculating Logistic Regression for Test DataSet
test_MSE_vector <- calculate_gradient_descent(max_iterations,standardized_test_data_matrix,coefficient_data_matrix,test_y_length,test.y)
boxplot(test_MSE_vector)

# Question No. 7 - Calculating Gradient Descent for 4 different values of initial weights 

random_coefficients_1 <- runif(6, -0.7, 0.7)
coefficients_frame <- data.frame("Coefficients" = c(random_coefficients_1))
coefficient_data_matrix_1 <- data.matrix(coefficients_frame)

random_coefficients_2 <- runif(6, -0.7, 0.7)
coefficients_frame <- data.frame("Coefficients" = c(random_coefficients_2))
coefficient_data_matrix_2 <- data.matrix(coefficients_frame)

random_coefficients_3 <- runif(6, -0.7, 0.7)
coefficients_frame <- data.frame("Coefficients" = c(random_coefficients_3))
coefficient_data_matrix_3 <- data.matrix(coefficients_frame)

random_coefficients_4 <- runif(6, -0.7, 0.7)
coefficients_frame <- data.frame("Coefficients" = c(random_coefficients_4))
coefficient_data_matrix_4 <- data.matrix(coefficients_frame)

train_MSE_vector_1 <- calculate_gradient_descent(max_iterations,standardized_train_data_matrix,coefficient_data_matrix_1,train_y_length,train.y)
train_MSE_vector_2 <- calculate_gradient_descent(max_iterations,standardized_train_data_matrix,coefficient_data_matrix_2,train_y_length,train.y)
train_MSE_vector_3 <- calculate_gradient_descent(max_iterations,standardized_train_data_matrix,coefficient_data_matrix_3,train_y_length,train.y)
train_MSE_vector_4 <- calculate_gradient_descent(max_iterations,standardized_train_data_matrix,coefficient_data_matrix_4,train_y_length,train.y)

minimum_vector <- vector()
minimum_vector <- c(minimum_vector, min(train_MSE_vector_1))
minimum_vector <- c(minimum_vector, min(train_MSE_vector_2))
minimum_vector <- c(minimum_vector, min(train_MSE_vector_3))
minimum_vector <- c(minimum_vector, min(train_MSE_vector_4))

minimum_MSE <- min(minimum_vector)


