library(tidyverse)
library(rsample)

so <- read_csv("data/stackoverflow")

set.seed(99)
so_split <- initial_split(so, prop = 0.8)

test_set <- testing(so_split)
training_set <- training(so_split)

## Use glm function to do logistic regression

training_set <- mutate(training_set, 
                       remote2 = ifelse(remote == "Remote", 1, 0)) %>%
  relocate(remote, remote2, everything())

count(training_set, country)

log_reg_model <- glm(remote2~country+salary+years_coded_job, data = training_set,
    family = binomial())

training_set$log_reg_model <- predict(log_reg_model,
                                      newdata = training_set,
                                      type = "response")

count(training_set, remote)


### can we make the training set 50-50?


so <- mutate(so, remote2 = ifelse(remote == "Remote", 1, 0))
not_remote <- filter(so, remote == "Not remote")
remote <- filter(so, remote == "Remote")

set.seed(20)
remote_sample <- sample(718, 359)
nr_sample <- sample(6273, 359)

nr_training <- not_remote[nr_sample, ]
nr_test <- not_remote[-nr_sample, ]

remote_training <- remote[remote_sample, ]
remote_test <- remote[-remote_sample, ]

training_set <- bind_rows(nr_training, remote_training)
test_set <- bind_rows(nr_test, remote_test)

log_reg_model2 <- glm(remote2~country+salary+years_coded_job, data = training_set)
log_reg_model2

training_set$model2 <- predict(log_reg_model2, newdata = training_set)

ggplot(data = training_set,
       aes(x = remote, y = model2)) + geom_boxplot()

test_set$model2 <- predict(log_reg_model2, newdata = test_set)

ggplot(data = test_set,
       aes(x = remote, y = model2)) + geom_boxplot()



