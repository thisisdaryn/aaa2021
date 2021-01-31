library(tidyverse)
library(rsample)
library(rpart)
library(rpart.plot)

cars2020 <- read_csv("data/cars2020.csv")

set.seed(1729)
split <- initial_split(cars2020, prop = 0.8, strata = mpg)

train <- training(split) 
test <- testing(split)

ggplot(data = cars2020,
       aes(x = transmission,
           y = mpg)) + geom_boxplot() + theme_minimal()

ggplot(data = cars2020,
       aes(x = disp, 
           y = mpg)) + geom_point(alpha = 0.2) + 
  geom_smooth()


### creating an OLS model with one categorical predictor, transmission

model1 <- lm(mpg~transmission, data = train)
train$model1 <- predict(model1, newdata = train)

summary(model1)

#### creating an OLS model with one numerical predictor,
model2 <- lm(mpg~disp, data = train)

train$model2 <- predict(model2, newdata = train)

#### creating a model with both transmission and displacement
model3 <- lm(mpg~disp+transmission, data = train)

model3
train$model3 <- predict(model3, newdata = train)

### apply these models to the test data 
test <- mutate(test,
               model1 = predict(model1, newdata = test),
               model2 = predict(model2, newdata = test),
               model3 = predict(model3, newdata = test))


#### Using decision trees 

dt_model1 <- rpart(mpg~transmission+disp, data = train)
rpart.plot(dt_model1)

test$dt_model1 <- predict(dt_model1, newdata = test)

dt_model2 <- rpart(log(mpg)~transmission+disp, data = train)
test$dt_model2 <- exp(predict(dt_model2, newdata = test))


test2 <- pivot_longer(test, 15:19, 
                      names_to = "training_model",
                      values_to = "prediction")

ggplot(data = test2,
       aes(x = prediction,
           y = mpg)) + geom_point(shape = 21) + 
  facet_wrap(~training_model) +  
  geom_abline(intercept = 0, slope = 1) + 
  xlim(c(10,60)) + ylim(c(10,60))
























