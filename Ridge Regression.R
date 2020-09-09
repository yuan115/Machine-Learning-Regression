#####
#title: "Observing effects of L2 penalty in polynomial regression"
#author: "Guan-Yuan Wang"
#date: "2020/9/9"
#output: html_document
######

### r setup
library(dplyr)
library(glmnet)
data <- read.csv("kc_house_data.csv")

### feature for polynomial regression
get.feature <- function(data, feature, degree) {
  f1 <- 1
  f1.poly <- for(i in 1:degree) {
    temp <- data[, feature] ** i
    f1 <- cbind(f1, matrix(temp))
  }
  return(f1)
}

f <- get.feature(data, feature = "sqft_living", 15)

### observation
y <- data$price

### ridge regression, using library 'glmnet'
ridge_reg <-  glmnet(f, y, nlambda = 1, lambda = 1.5e-5, family = 'gaussian')

### results
ridge_reg$a0
ridge_reg$beta


### fit a 15th degree polynomial on each of the 4 sets
set1 <- read.csv("wk3_kc_house_set_1_data.csv")
set2 <- read.csv("wk3_kc_house_set_2_data.csv")
set3 <- read.csv("wk3_kc_house_set_3_data.csv")
set4 <- read.csv("wk3_kc_house_set_4_data.csv")

### get each set of feartures and observations
f1 <- get.feature(set1, feature = "sqft_living", 15)
f2 <- get.feature(set2, feature = "sqft_living", 15)
f3 <- get.feature(set3, feature = "sqft_living", 15)
f4 <- get.feature(set4, feature = "sqft_living", 15)

y1 <- set1$price
y2 <- set2$price
y3 <- set3$price
y4 <- set4$price

### run ridge regression
ridge_reg1 <-  glmnet(f1, y1, nlambda = 1, lambda = 1e-9, family = 'gaussian')
ridge_reg2 <-  glmnet(f2, y2, nlambda = 1, lambda = 1e-9, family = 'gaussian')
ridge_reg3 <-  glmnet(f3, y3, nlambda = 1, lambda = 1e-9, family = 'gaussian')#####
#title: "Observing effects of L2 penalty in polynomial regression"
#author: "Guan-Yuan Wang"
#date: "2020/9/9"
#output: html_document
######

### r setup
library(dplyr)
library(glmnet)
data <- read.csv("kc_house_data.csv")

### feature for polynomial regression
get.feature <- function(data, feature, degree) {
  f1 <- 1
  f1.poly <- for(i in 1:degree) {
    temp <- data[, feature] ** i
    f1 <- cbind(f1, matrix(temp))
  }
  return(f1)
}

f <- get.feature(data, feature = "sqft_living", 15)

### observation
y <- data$price

### ridge regression, using library 'glmnet'
ridge_reg <-  glmnet(f, y, lambda = 1.5e-5, alpha = 0)

###
#library(caret)
#ridge <- train(
#  f, y, method = "glmnet",
#  tuneGrid = expand.grid(alpha = 0, lambda = 1.5e-5)
#)
#coef(ridge$finalModel, ridge$bestTune$lambda)

### results
coef(ridge_reg)

### fit a 15th degree polynomial on each of the 4 sets
set1 <- read.csv("wk3_kc_house_set_1_data.csv")
set2 <- read.csv("wk3_kc_house_set_2_data.csv")
set3 <- read.csv("wk3_kc_house_set_3_data.csv")
set4 <- read.csv("wk3_kc_house_set_4_data.csv")

### get each set of feartures and observations
f1 <- get.feature(set1, feature = "sqft_living", 15)
f2 <- get.feature(set2, feature = "sqft_living", 15)
f3 <- get.feature(set3, feature = "sqft_living", 15)
f4 <- get.feature(set4, feature = "sqft_living", 15)

y1 <- set1$price
y2 <- set2$price
y3 <- set3$price
y4 <- set4$price

### run ridge regression
ridge_reg1 <-  glmnet(f1, y1, alpha = 0, lambda = 1e-9, family = 'gaussian')
ridge_reg2 <-  glmnet(f2, y2, alpha = 0, lambda = 1e-9, family = 'gaussian')
ridge_reg3 <-  glmnet(f3, y3, alpha = 0, lambda = 1e-9, family = 'gaussian')
ridge_reg4 <-  glmnet(f4, y4, alpha = 0, lambda = 1e-9, family = 'gaussian')

### results
ridge_reg1$beta[2]
ridge_reg2$beta[2]
ridge_reg3$beta[2]
ridge_reg4$beta[2]


dataShuffled <- read.csv("wk3_kc_house_train_valid_shuffled.csv")

###  k-fold cross-validation. 
n <- nrow(dataShuffled)
k <- 10

range <- seq(1, n, round(0.1 * n))
range[length(range)+1] <- n
#range


###  each l2_penalty
seqL2 <- seq(3, 9, 0.5)
l2_penalty <- c()
for (i in 1:length(seqL2)) {
  l2_penalty[i] <- 10 ** seqL2[i]
}
#l2_penalty


featureShuffled <- get.feature(dataShuffled, feature = "sqft_living", 15)

RSS <- data.frame(k = NA, l2_penalty = NA, RSS = NA)

for ( i in 1:(length(range)-1) ){
  
  x_test <- featureShuffled[range[i]:range[i+1], ]
  y_test <- dataShuffled$price[range[i]:range[i+1]]
  x_train <- featureShuffled[-(range[i]:range[i+1]), ]
  y_train <- dataShuffled$price[-(range[i]:range[i+1])]
  
  for (j in 1:(length(seqL2)-1) ) {
    
    ridge_reg.kFold <-  glmnet(x_train, y_train, alpha = 0, lambda = l2_penalty[j], family = 'gaussian')
    pred <- predict.glmnet(ridge_reg.kFold, x_test)
    RSS.temp <- sum((y_test - pred) ** 2)
    temp <- data.frame(k = i, l2_penalty = l2_penalty[j], RSS = RSS.temp)
    RSS <- rbind(RSS, temp)
    
  }
}

RSS <- na.omit(RSS)
RSS


RSS %>%
  group_by(l2_penalty) %>% 
  summarise(mean.RSS = mean(RSS))







ridge_reg4 <-  glmnet(f4, y4, nlambda = 1, lambda = 1e-9, family = 'gaussian')

### results
ridge_reg1$beta[2]
ridge_reg2$beta[2]
ridge_reg3$beta[2]
ridge_reg4$beta[2]


dataShuffled <- read.csv("wk3_kc_house_train_valid_shuffled.csv")

###  k-fold cross-validation. 
n <- nrow(dataShuffled)
k <- 10

range <- seq(1, n, round(0.1 * n))
range[length(range)+1] <- n
#range


###  each l2_penalty
seqL2 <- seq(3, 9, 0.5)
l2_penalty <- c()
for (i in 1:length(seqL2)) {
  l2_penalty[i] <- 10 ** seqL2[i]
}
#l2_penalty


featureShuffled <- get.feature(dataShuffled, feature = "sqft_living", 15)

RSS <- data.frame(k = NA, l2_penalty = NA, RSS = NA)

for ( i in 1:(length(range)-1) ){
  
  x_test <- featureShuffled[range[i]:range[i+1], ]
  y_test <- dataShuffled$price[range[i]:range[i+1]]
  x_train <- featureShuffled[-(range[i]:range[i+1]), ]
  y_train <- dataShuffled$price[-(range[i]:range[i+1])]
  
  for (j in 1:(length(seqL2)-1) ) {
    
    ridge_reg.kFold <-  glmnet(x_train, y_train, nlambda = 1, lambda = l2_penalty[j], family = 'gaussian')
    pred <- predict.glmnet(ridge_reg.kFold, x_test)
    RSS.temp <- sum((y_test - pred) ** 2)
    temp <- data.frame(k = i, l2_penalty = l2_penalty[j], RSS = RSS.temp)
    RSS <- rbind(RSS, temp)
    
  }
}

RSS <- na.omit(RSS)
RSS


RSS %>%
  group_by(l2_penalty) %>% 
  summarise(mean.RSS = mean(RSS))







