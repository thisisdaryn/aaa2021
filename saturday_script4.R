library(tidyverse)


df <- read_csv("data/cars2020.csv")

ggplot(data = df,
       aes(x = class, y = mpg)) + 
  geom_boxplot() + coord_flip()

ggplot(data = df,
       aes(x = cyl, y = mpg)) + 
  geom_point() + geom_smooth(method = "lm")

ggplot(data = df,
       aes(x = transmission, 
           y = mpg)) + geom_boxplot()


