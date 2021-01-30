
tt_revenue <- read_csv("data/revenue2020.csv")

ggplot(data = tt_revenue,
       aes(x = Category,
       y = Revenue))  + geom_col(fill = "blue") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))



### How to make box plots of body masses by species
library(scales)
ggplot(data = penguins,
       aes(x = species, 
           y = body_mass_g)) + 
  geom_boxplot(fill = "#49AFA3") + theme_bw() + 
  scale_y_continuous(label = comma)

