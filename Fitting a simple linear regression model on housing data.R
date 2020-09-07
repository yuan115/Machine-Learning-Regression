library(data.table)

data <- fread("kc_house_train_data.csv")
head(data)

lm <- lm(price ~ sqft_living, data)
predict(lm, data.frame(sqft_living = 2650))
summary(lm)

sum((data$price - lm$fitted.values) ** 2)

(800000+47116.079)/281.959 


#lm2 <- lm(sqft_living ~ price, data)
#predict(lm2, data.frame(price = 800000))

test <- fread("kc_house_test_data.csv")
lm3 <- lm(price ~ bedrooms, data)

r1 <- predict(lm, data.table(sqft_living = test$sqft_living))
r2 <- predict(lm3, data.table(bedrooms = test$bedrooms))
sum((test$price - r1) ** 2)
sum((test$price - r2) ** 2)

