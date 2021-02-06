library(tidyverse)
library(rpart)
library(rpart.plot)
library(rsample)

install.packages("rsample")

cars2020 <- read_csv("data/cars2020.csv")

set.seed(99)
data_split <- initial_split(cars2020, prop = 0.8)

training_data <- training(data_split)
test_data <- testing(data_split)

### Looking at two variables: disp and transmission 


ggplot(data = training_data,
       aes(x = disp, y = mpg)) + 
  geom_point(shape = 21) + theme_minimal() + 
  geom_smooth(method = "lm") + xlim(c(0,8))

# to make an Ordinary Least Squares linear regression model
model1 <- lm(mpg~disp, data = training_data)
model1

#### looking at transmission 
ggplot(data = training_data,
       aes(x = transmission, y = mpg)) + 
  geom_boxplot() + theme_minimal()


model2 <- lm(mpg~transmission, data = training_data)
model2

### add the predictions to the data
training_data$model1 <- predict(model1, newdata = training_data)
training_data$model2 <- predict(model2, newdata = training_data)


### use a model with both transmission and displacement
model3 <- lm(mpg~disp+transmission, data = training_data)
training_data$model3 <- predict(model3, newdata = training_data)

ggplot(training_data,
       aes(x = model3, y = mpg)) + 
  geom_point(shape = 21) + geom_abline(intercept = 0, slope = 1) + 
  xlim(c(0,60)) + ylim(c(0,60)) + 
  labs(x = "Prediction", y = "Actual mpg") + theme_bw()

### apply the models to the test data 

test_data <- mutate(test_data,
                    model1 = predict(model1, newdata = test_data),
                    model2 = predict(model2, newdata = test_data),
                    model3 = predict(model3, newdata = test_data))

ggplot(test_data,
       aes(x = model3, y = mpg)) + 
  geom_point(shape = 21) + geom_abline(intercept = 0, slope = 1) + 
  xlim(c(0,60)) + ylim(c(0,60)) + 
  labs(title = "Model applied to test data", x = "Prediction", y = "Actual mpg") + 
  theme_bw() + 
  theme(plot.title = element_text(size = 24, colour = "blue"))


### Making a decision tree model

dt_model <- rpart(mpg~transmission+disp, data = training_data)
rpart.plot(dt_model)

dt_model2 <- rpart(mpg~transmission+disp+atvType, data = training_data)
rpart.plot(dt_model2)

test_data <- mutate(test_data, 
                    dt_model2 = predict(dt_model2, newdata = test_data))

ggplot(test_data,
       aes(x = dt_model2, y = mpg)) + 
  geom_point(shape = 21) + geom_abline(intercept = 0, slope = 1) + 
  xlim(c(0,60)) + ylim(c(0,60)) + 
  labs(title = "Model applied to test data", x = "Prediction", y = "Actual mpg") + 
  theme_bw() + 
  theme(plot.title = element_text(size = 24, colour = "blue"))

training_data <- mutate(training_data, 
                    dt_model2 = predict(dt_model2, newdata = training_data))

ggplot(training_data,
       aes(x = dt_model2, y = mpg)) + 
  geom_point(shape = 21) + geom_abline(intercept = 0, slope = 1) + 
  xlim(c(0,60)) + ylim(c(0,60)) + 
  labs(title = "Model applied to test data", x = "Prediction", y = "Actual mpg") + 
  theme_bw() + 
  theme(plot.title = element_text(size = 24, colour = "blue"))

tr2 <- select(training_data, mpg, model3, dt_model2) %>% 
  pivot_longer(model3:dt_model2, names_to = "Model",
               values_to = "Prediction")

ggplot(data = tr2, 
       aes(x = mpg, y = Prediction )) + geom_point() + 
  facet_wrap(~Model) + geom_abline(intercept = 0, slope = 1) + 
  xlim(c(0,60)) + ylim(c(0,60))


