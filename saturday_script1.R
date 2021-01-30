library(tidyverse)

## tidyverse contains many packages including dplyr, readr, ggplot2, tidyr

cars2020 <- read_csv("data/cars2020.csv")

# How to get all vehicles with manual transmissions

manual <- filter(cars2020, transmission == "Manual")



hist(cars2020$cyl)

count(cars2020, cyl)

## how to get all cars with 6 or more cylinders 

cyl6 <- filter(cars2020, cyl >= 6)


## How do you get all manual transmission cars that have 6 or more cylinders


mancyl6 <- filter(manual, cyl >= 6)
mancyl6_alt <- filter(cyl6, transmission == "Manual")

mancyl6_alt2 <- filter(cars2020, transmission == "Manual",
                       cyl >= 6)


### Question: How many automatic transmission vehicles have less than 6 gears

auto_l6 <- filter(cars2020, transmission == "Automatic",
                  gears < 6)

### Using select to keep only certain columns

cars_narrow <- select(cars2020, mpg, model, cyl, transmission)




## How to drop the startStop and aspiration columns

cars_alt <- select(cars2020, -startStop, -aspiration)


## Using arrange to sort data 

##  sort by mpg

cars_sorted <- arrange(cars2020, mpg)

## to sort in descending order

cars_sorted2 <- arrange(cars2020, desc(mpg))


cars_sorted3 <- arrange(cars2020, transmission, desc(mpg))



cars_99 <- filter(cars2020, transmission == "Automatic") %>% 
  filter(startStop == "N") %>% 
  arrange(desc(mpg)) %>% select(startStop, model, transmission, mpg)

cars_100 <- cars2020 %>% filter(transmission == "Automatic") %>%
  arrange(desc(mpg)) %>% select(make, model, transmission, mpg)

acura <- filter(cars2020, make == "Acura")

acura_manual <- filter(acura, transmission == "Manual")


### mutate to add columns

cars_30 <- mutate(cars2020, 
                  above30 = ifelse(mpg >= 30, TRUE, FALSE))


### summarise

report <- summarise(cars2020, num_cars = n(),
                    avg_mpg = mean(mpg),
                    min_mpg = min(mpg),
                    max_mpg = max(mpg))



grouped_cars <- group_by(cars2020, transmission)

report3 <- summarise(grouped_cars,
                     num_cars = n(),
                     min_mpg = min(mpg),
                     avg_mpg = mean(mpg),
                     max_mpg = max(mpg)
                     )

report4 <- group_by(cars2020, transmission) %>%
  summarise(num_cars = n(),
            min_mpg = min(mpg),
            avg_mpg = mean(mpg),
            max_mpg = max(mpg))


### I do not approve of using the following in practical situations
report5 <- summarise(group_by(cars2020, transmission), 
          num_cars = n(),
          min_mpg = min(mpg),
          avg_mpg = mean(mpg),
          max_mpg = max(mpg))

hist()





























