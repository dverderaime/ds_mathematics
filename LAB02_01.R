#Load mpg data set
library(tidyverse)
data.00 <- mpg
View(data.00)

#1.View the first 6 observations of the data. Do this two different ways.
data.00[1:6,]
slice(data.00, 1:6)
head(data.00)

#2.What is the average of 'cty' for compact cars
summary(data.00$cty)
mean(data.00$cty)
data.00 %>%
  select(class,cty) %>% #subset
  group_by(class) %>%   #collect vehicles of same class
  summarise(mean.cty = mean(cty)) %>% #calculates mean of cty for each class
  filter(class == 'compact')
  

#3.Create a new dataframe that has five variables: manufacturer, class, cty, cyl, mean.hwy, and hwy.per.cyl
#     -mean.hwy should be the average of 'hwy' for each manufacturer's classes of vehicles, by number of cylinders.
#     -Each manufacturer/class/cyl combination should have only one record in the dataframe
new.data <- data.00 %>%
  mutate(hwy.to.cyl = hwy / cyl)%>% # creates and adds a column that is a calculation (ratio) two other columns
  group_by(manufacturer, class, cyl) %>%
  summarise(mean.hwy = mean(hwy), hwy.per.cyl = mean(hwy.to.cyl)) %>% #the average of 'hwy' for each manufacturer's classes of vehicles, by number of cylinders
  select(manufacturer, class, cyl, mean.hwy, hwy.per.cyl)

#4. Plot 'hwy' against 'cty' for all 2008 models
data.08 <- data.00 %>%
  filter(year == '2008') %>%
  select(hwy,cty)

plot(data.08$cty, data.08$hwy, type='p', col='blue',
     main= 'Hwy vs. Cty for 2008 Models')
