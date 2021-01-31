library(tidyverse)
library(rpart)
library(rpart.plot)
library(rsample)
so <- read_csv("stackoverflow.csv")

count(so, remote)
set.seed(1729)
split <- initial_split(so, prop = 0.8)

train_1 <- training(split) 
test_2 <- testing(split)

count(train_1, remote)

train_1 <- mutate(train_1, 
                  remote_binary = ifelse(remote == "Not remote", 0, 1)) %>%
  relocate(remote_binary, remote, everything())


## creating a decision tree model 
remote_model <- rpart(remote_binary~country+salary+years_coded_job+open_source,
      data = train_1)
rpart.plot(remote_model)

train_1$dt_model <- predict(remote_model, newdata = train_1)

### try to make a more even split

not_remote <- filter(so, remote == "Not remote")
remote <- filter(so, remote == "Remote")

set.seed(99)
sample1 <- sample(718, 359)
sample2 <- sample(6273, 359)
remote_train <- remote[sample1, ]
not_remote_train <- not_remote[sample2, ]

remote_test <- remote[-sample1, ]
not_remote_test <- remote[-sample2, ] 

new_train <- bind_rows(remote_train, not_remote_train)
new_test <- bind_rows(remote_test, not_remote_test)

count(new_train, remote)

new_train <- mutate(new_train, 
       remote_binary = ifelse(remote == "Not remote", 0, 1))

### new dt 
remote_model2 <- rpart(remote_binary~country+salary+years_coded_job+open_source,
      data = new_train)

new_train$dt_model <- predict(remote_model2, newdata = new_train)

rpart.plot(remote_model2)


### do logistic regression using the glm 

log_regression_model <- 
  glm(remote_binary~country+salary+years_coded_job+open_source,
      data = new_train, family = binomial(link = "logit"))
summary(log_regression_model)t


new_train <- mutate(new_train, 
                    log_model_logit = predict(log_regression_model, 
                                              newdata = new_train),
                    log_model_prob = predict(log_regression_model,
                                             newdata = new_train, 
                                             type = "response")) %>%
  relocate(log_model_logit, log_model_prob)




