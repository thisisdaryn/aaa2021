#install.packages("readxl")
library(readxl)
library(tidyverse)
library(lubridate)

df <- read_xlsx("data/SuperstoreData.xlsx")

count(df, Category)
count(df, `Sub-Category`)

## total dollar value in sales in each category? 

df2 <- group_by(df, Category) %>% 
  summarise(total_sales = sum(Sales), 
            total_profit = sum(Profit),
            items = sum(Quantity))


## sales by product category
ggplot(data = df,
       aes(x = Category,
           y = Sales)) + 
  geom_col(fill = "#DC493A") + theme_bw()


## How many different products?
ggplot(data = df,
       aes(x = Category, y = Sales, fill = Category)) + 
  geom_col() + facet_wrap(~Region)

state_lookup <- count(df, Region, State)


### 
order_totals <- group_by(df, `Order ID`, Region) %>%
  summarise(total = sum(Sales))

ggplot(data = order_totals, 
       aes(x = Region, y = total)) + 
  geom_boxplot()

## create a column with the item price

super_df <- mutate(df, item_price = Sales/Quantity,
                   sale_year = year(`Order Date`),
                   sale_month = month(`Order Date`),
                   weekday = weekdays(`Order Date`))

## Comparing item prices across categories

ggplot(data = super_df,
       aes(x = item_price)) + 
  geom_histogram() + 
  facet_wrap(~Category, ncol = 1, scales = "free_y")


## separate expensive items 

## year and month functions are in the lubridate package
super_df <- mutate(df, item_price = Sales/Quantity,
                   sale_year = year(`Order Date`),
                   sale_month = month(`Order Date`),
                   weekday = weekdays(`Order Date`))

expensive <- filter(super_df, item_price > 500)
regular <- filter(super_df, item_price <= 500)

ggplot(data = regular,
       aes(x = item_price)) + 
  geom_histogram(bins = 50, fill = "skyblue",
                 color = "black") + 
  facet_wrap(~Category, ncol = 1,
             scales = "free_y") + theme_minimal()

ggplot(data = regular,
       aes(x = Category, 
           y = item_price)) + 
  geom_boxplot()


df5 <- mutate(df, delay = `Ship Date`-`Order Date`,
              delay_text = as.character(delay),
              delay_in_days = as.numeric(as.character(delay))/86400)

hist(df5$delay_text2, breaks = 7)


ggplot(data = df5,
       aes(x = `Ship Mode`, y = delay_in_days)) + 
  geom_boxplot()








