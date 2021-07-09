######################### PROBLEM 1 #################################

# Load the Concrete data as concrete

startup <- read.csv(file.choose())

#label encoding
factors <-  as.factor(startup$State)
startup$State <- as.numeric(factors)

# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame
startup_norm <- as.data.frame(lapply(startup, normalize))

# create training and test data
startup_train <- startup_norm[1:30, ]
startup_test <- startup_norm[31:50, ]

## Training a model on the data ----
# train the neuralnet model
install.packages("neuralnet")
library(neuralnet)

# simple ANN with only a single hidden neuron
startup_model <- neuralnet(formula = Profit ~ R.D.Spend +Administration + Marketing.Spend + State , data = startup_train)


# visualize the network topology
plot(startup_model)

## Evaluating model performance 

# obtain model results
# results_model <- NULL

results_model <- compute(startup_model, startup_test[1:5])
# obtain predicted strength values
str(results_model)
predicted_strength <- results_model$net.result

# examine the correlation between predicted and actual values
cor(predicted_strength, startup_test$Profit)

## Improving model performance ----
# a more complex neural network topology with 5 hidden neurons
startup_model2 <- neuralnet(Profit ~ R.D.Spend +Administration + Marketing.Spend + State , data = startup_train, hidden = 5)


# plot the network
plot(startup_model2)

# evaluate the results as we did before
model_results2 <- compute(startup_model2, startup_test[1:5])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, startup_test$Profit)

############################ PROBLEM 2 ################################
# Load the Concrete data as concrete
library(readr)
forest_data <- read.csv(file.choose())
View(forest_data)
attach(forest_data)
summary(forest_data)
str(forest_data)


#label encoding
factors <-  as.factor(forest_data$month)
forest_data$month <- as.numeric(factors)
fact <- as.factor(forest_data$day)
forest_data$day<-as.numeric(fact)


# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame
forest_data_norm <- as.data.frame(lapply(forest_data, normalize))

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(forest_data_norm), replace = TRUE, prob = c(0.7,0.3))
forest_data_train <- forest_data_norm[ind==1,]
forest_data_test  <-forest_data_norm[ind==2,]


# create training and test data
#forest_data_train <- forest_data_norm[1:300, ]
#forest_data_test <- forest_data_norm[301:517, ]

## Training a model on the data ----
# train the neuralnet model
install.packages("neuralnet")
library(neuralnet)

# simple ANN with only a single hidden neuron
forest_data_model <- neuralnet(area ~., data = forest_data_train)


# visualize the network topology
plot(forest_data_model)

# Evaluating model performance
set.seed(12323)
model_results <- compute(forest_data_model,forest_data_test[-c(11)])
predicted_area <- model_results$net.result

# Predicted profit Vs Actual profit of test data.
cor(predicted_area,forest_data_test$area)

# since the prediction is in Normalized form, we need to de-normalize it 
# to get the actual prediction on profit
str_max <- max(forest_data$area)
str_min <- min(forest_data$area)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

Actualarea_pred <- unnormalize(predicted_area,str_min,str_max)
head(Actualarea_pred)

# Improve the model performance :
set.seed(12345)
forest_data_model2 <- neuralnet(area~.,data = forest_data_train,
                                hidden = 2)
plot(forest_data_model2 ,rep = "best")

summary(forest_data_model2)

# evaluate the results as we did before
model_results2<-compute(forest_data_model2,forest_data_test[-c(11)])
predicted_area2<-model_results2$net.result
cor(predicted_area2,forest_data_test$area)

#plotting
plot(predicted_area2,forest_data_test$area)

install.packages("NeuralNetTools")
library(NeuralNetTools)
par(mar = numeric(4), family = 'serif')
plotnet(forest_data_model2, alpha = 0.6)

################################ PROBLEM 3##############################
# Loading the dataset
library(readr)
concrete_data <- read.csv(file.choose(), header = TRUE)
summary(concrete_data)
str(concrete_data)

#attaching dataframe
attach(concrete_data)

# Exploratory data Analysis :
plot(cement, strength)
plot(fineagg, strength)
plot(coarseagg, strength)
plot(water, strength)

windows()
# Find the correlation between Output (strength) & inputs (R.D Spend, Administration, Marketing, State) - SCATTER DIAGRAM
pairs(concrete_data)

# Correlation coefficient - Strength & Direction of correlation
cor(concrete_data)

# Apply Normalization technique to the whole dataset
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
concrete_norm<-as.data.frame(lapply(concrete_data,FUN=normalize))
summary(concrete_norm$strength) # Normalized form of profit

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(concrete_norm), replace = TRUE, prob = c(0.7,0.3))
concrete_train <- concrete_norm[ind==1,]
concrete_test  <- concrete_norm[ind==2,]

# Creating a neural network model on training data
install.packages("neuralnet")
library(neuralnet)
concrete_model <- neuralnet(strength~.,data = concrete_train)
str(concrete_model)

# visualize the network topology
plot(concrete_model, rep = "best")

# Evaluating model performance
set.seed(12323)
model_results <- compute(concrete_model,concrete_test[1:8])
predicted_strength <- model_results$net.result

# Predicted profit Vs Actual profit of test data.
cor(predicted_strength,concrete_test$strength)

# since the prediction is in Normalized form, we need to de-normalize it 
# to get the actual prediction on profit
str_max <- max(concrete_data$strength)
str_min <- min(concrete_data$strength)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

Actualstrength_pred <- unnormalize(predicted_strength,str_min,str_max)
head(Actualstrength_pred)

# Improve the model performance :
set.seed(12345)
concretes_model2 <- neuralnet(strength~.,data = concrete_train,
                              hidden = 2)
plot(concretes_model2 ,rep = "best")

summary(concretes_model2)

# evaluate the results as we did before
model_results2<-compute(concretes_model2,concrete_test[1:8])
predicted_strength2<-model_results2$net.result
cor(predicted_strength2,concrete_test$strength)

#plotting
plot(predicted_strength2,concrete_test$strength)

install.packages("NeuralNetTools")
library(NeuralNetTools)
par(mar = numeric(4), family = 'serif')
plotnet(concretes_model2, alpha = 0.6)

#########################################Problem 4##################################
# Loading the dataset
library(readr)
RPL_data <- read.csv(file.choose(), header = TRUE)
RPL_data <- RPL_data[,4:14]
summary(RPL_data)
str(RPL_data)

#label encoding the data
install.packages('superml')
library(superml)
label <- LabelEncoder$new()
RPL_data$Geography <- label$fit_transform(RPL_data$Geography)
RPL_data$Gender <- label$fit_transform(RPL_data$Gender)
str(RPL_data)

#attaching dataframe
attach(RPL_data)

# Exploratory data Analysis :
plot(Geography, Exited)
plot(Gender, Exited)

windows()
# Find the correlation between Output (Exited) & inputs (R.D Spend, Administration, Marketing, State) - SCATTER DIAGRAM
pairs(RPL_data)

# Correlation coefficient - Strength & Direction of correlation
cor(RPL_data)

# Apply Normalization technique to the whole dataset
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
RPL_norm<-as.data.frame(lapply(RPL_data,FUN=normalize))
summary(RPL_norm$Exited) # Normalized form of profit

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(RPL_norm), replace = TRUE, prob = c(0.7,0.3))
RPL_train <- RPL_norm[ind==1,]
RPL_test  <- RPL_norm[ind==2,]

# Creating a neural network model on training data
#since output is in binary its classification 
#nnet for classification
install.packages("nnet")
library(nnet)
RPL_model <- nnet(Exited~.,data = RPL_train , size = 5,rang = 0.1 , decay = 5e-2,maxit = 5000)
str(RPL_model)

# visualize the network topology
plotnet(RPL_model)
garson(RPL_model)

# Evaluating model performance
set.seed(12323)
predicted_exited <- predict(RPL_model,RPL_test[-c(11)])

# Predicted profit Vs Actual profit of test data.
cor(predicted_exited,RPL_test$Exited)

# since the prediction is in Normalized form, we need to de-normalize it 
# to get the actual prediction on profit
str_max <- max(RPL_data$Exited)
str_min <- min(RPL_data$Exited)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

Actualexited_pred <- unnormalize(predicted_exited,str_min,str_max)
head(Actualexited_pred)

# Improve the model performance :
set.seed(12345)
RPL_model2 <- nnet(Exited~.,data = RPL_train,
                   hidden = 2 , size = 5)
plotnet(RPL_model2 ,rep = "best")

summary(RPL_model2)

# evaluate the results as we did before
predicted_exited2 <- predict(RPL_model2,RPL_test[-c(11)])

#plotting
plot(predicted_exited2,RPL_test$Exited)

install.packages("NeuralNetTools")
library(NeuralNetTools)
par(mar = numeric(4), family = 'serif')
plotnet(RPL_model2, alpha = 0.6)

##############################END#####################################