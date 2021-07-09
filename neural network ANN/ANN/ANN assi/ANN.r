###################################Problem 1######################
# Loading the dataset
library(readr)
startup_data <- read.csv("C://Users//hp//Desktop//ANN assi//50_Startups (2).csv", header = TRUE)
summary(startup_data)
str(startup_data)

#label encoding state column
library(plyr)
startup_data$State <- as.numeric(revalue(startup_data$State,
                                     c("New York"="0", "California"="1",
                                     "Florida"="2")))
str(startup_data)

#attaching dataframe
startup_data <- as.data.frame(startup_data)
attach(startup_data)

# Exploratory data Analysis :
plot(R.D.Spend, Profit)
plot(Administration, Profit)
plot(Marketing.Spend, Profit)
plot(State, Profit)

windows()
# Find the correlation between Output (Profit) & inputs (R.D Spend, Administration, Marketing, State) - SCATTER DIAGRAM
pairs(startup_data)

# Correlation coefficient - Strength & Direction of correlation
cor(startup_data)

# Apply Normalization technique to the whole dataset
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
Startups_norm<-as.data.frame(lapply(startup_data,FUN=normalize))
summary(Startups_norm$Profit) # Normalized form of profit

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(Startups_norm), replace = TRUE, prob = c(0.7,0.3))
Startups_train <- Startups_norm[ind==1,]
startups_test  <- Startups_norm[ind==2,]


# Creating a neural network model on training data
install.packages("neuralnet")
library(neuralnet)
startups_model <- neuralnet(Profit~R.D.Spend+Administration
                            +Marketing.Spend+State,data = Startups_train)
str(startups_model)

# visualize the network topology
plot(startups_model, rep = "best")

# Evaluating model performance
set.seed(12323)
model_results <- compute(startups_model,startups_test[1:4])
predicted_profit <- model_results$net.result

# Predicted profit Vs Actual profit of test data.
cor(predicted_profit,startups_test$Profit)

# since the prediction is in Normalized form, we need to de-normalize it 
# to get the actual prediction on profit
str_max <- max(startup_data$Profit)
str_min <- min(startup_data$Profit)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualProfit_pred <- unnormalize(predicted_profit,str_min,str_max)
head(ActualProfit_pred)

# Improve the model performance :
set.seed(12345)
Startups_model2 <- neuralnet(Profit~R.D.Spend+Administration
                             +Marketing.Spend+State,data = Startups_train,
                             hidden = 2)
plot(Startups_model2 ,rep = "best")

summary(Startups_model2)

# evaluate the results as we did before
model_results2<-compute(Startups_model2,startups_test[1:4])
predicted_Profit2<-model_results2$net.result
cor(predicted_Profit2,startups_test$Profit)

#plotting
plot(predicted_Profit2,startups_test$Profit)

install.packages("NeuralNetTools")
library(NeuralNetTools)
par(mar = numeric(4), family = 'serif')
plotnet(Startups_model2, alpha = 0.6)

############################Problem 2#######################################
# Loading the dataset
library(readr)
fireforests_data <- read.csv("C://Users//hp//Desktop//ANN assi//fireforests.csv", stringsAsFactors = TRUE, header = TRUE)
summary(fireforests_data)
str(fireforests_data)

#attaching dataframe
attach(fireforests_data)

# Exploratory data Analysis :
plot(month, area)
plot(day, area)
plot(temp, area)


#label encoding the data
install.packages('superml')
library(superml)
label <- LabelEncoder$new()
fireforests_data$month <- label$fit_transform(fireforests_data$month)
fireforests_data$day <- label$fit_transform(fireforests_data$day)

# Correlation coefficient - Strength & Direction of correlation
cor(fireforests_data)

# Apply Normalization technique to the whole dataset
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
fireforests_norm<-as.data.frame(lapply(fireforests_data,FUN=normalize))
summary(fireforests_data$area) # Normalized form of profit

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(fireforests_norm), replace = TRUE, prob = c(0.7,0.3))
fireforests_train <- fireforests_norm[ind==1,]
fireforests_test  <- fireforests_norm[ind==2,]

# Creating a neural network model on training data
install.packages("neuralnet")
library(neuralnet)
fireforests_model <- neuralnet(area~.,data = fireforests_train)
str(fireforests_model)

# visualize the network topology
plot(fireforests_model, rep = "best")

# Evaluating model performance
set.seed(12323)
model_results <- compute(fireforests_model,fireforests_test[-c(11)])
predicted_area <- model_results$net.result

# Predicted profit Vs Actual profit of test data.
cor(predicted_area,fireforests_test$area)

# since the prediction is in Normalized form, we need to de-normalize it 
# to get the actual prediction on profit
str_max <- max(fireforests_data$area)
str_min <- min(fireforests_data$area)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

Actualarea_pred <- unnormalize(predicted_area,str_min,str_max)
head(Actualarea_pred)

# Improve the model performance :
set.seed(12345)
fireforests_model2 <- neuralnet(area~.,data = fireforests_train,
                             hidden = 2)
plot(fireforests_model2 ,rep = "best")

summary(fireforests_model2)

# evaluate the results as we did before
model_results2<-compute(fireforests_model2,fireforests_test[-c(11)])
predicted_area2<-model_results2$net.result
cor(predicted_area2,fireforests_test$area)

#plotting
plot(predicted_area2,fireforests_test$area)

install.packages("NeuralNetTools")
library(NeuralNetTools)
par(mar = numeric(4), family = 'serif')
plotnet(fireforests_model2, alpha = 0.6)

#############################problem 3#####################################
# Loading the dataset
library(readr)
concrete_data <- read.csv("C://Users//hp//Desktop//ANN assi//concrete.csv", header = TRUE)
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
RPL_data <- read.csv("C://Users//hp//Desktop//ANN assi//RPL.csv", header = TRUE)
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