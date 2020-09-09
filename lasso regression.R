###
#title: "Using LASSO to select features"
#author: "Guan-Yuan Wang"
#date: "2020/9/9"
###


library(dplyr)
library(glmnet)

data <- read.csv("kc_house_data.csv")

### Create new features 
data <- data %>% 
  mutate(sqft_living_sqrt = sqrt(sqft_living),
         sqft_lot_sqrt = sqrt(sqft_lot),
         bedrooms_square = bedrooms * bedrooms,
         floors_square = floors * floors)

data <- data %>% select(!c(lat, long, zipcode, id, date))

## lasso
x <- as.matrix(select(data, -price))
y <- data$price

lasso <- glmnet(x, y, alpha = 1)
plot(lasso, xvar = "lambda")

lassoTuning <- cv.glmnet(x, y, alpha = 1)
plot(lassoTuning)

lambdas <- 10^seq(2, 6, by = .5)
lasso_reg <- cv.glmnet(x, y, alpha = 1, lambda = lambdas,
                       standardize = TRUE, nfolds = 5)
lambda_best <- lasso_reg$lambda.min 
lambda_best

lassoModel <- glmnet(x, y, alpha = 1, lambda = lambda_best, standardize = TRUE)
lassoModel$beta


## ridge
x <- as.matrix(select(data, -price))
y <- data$price

ridge <- glmnet(x, y, alpha = 0)
plot(lasso, xvar = "lambda")

ridgeTuning <- cv.glmnet(x, y, alpha = 0)
plot(lassoTuning)

lambdas <- 10^seq(2, 6, by = .5)
ridge_reg <- cv.glmnet(x, y, alpha = 0, lambda = lambdas,
                       standardize = TRUE, nfolds = 5)
lambda_best <- ridge_reg$lambda.min 
lambda_best

ridgeModel <- glmnet(x, y, alpha = 0, lambda = lambda_best, standardize = TRUE)
ridgeModel$beta




