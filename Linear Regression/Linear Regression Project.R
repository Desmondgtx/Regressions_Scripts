
### Linear Regression Project ###

## Library and data
library(ggplot2)
library(dplyr)
bike = read.csv('ML/Linear Regression/bikeshare.csv')
head(bike)

## Exploratory Data Analysis - EDA
# Scatter plot of count vs temp
ggplot(bike, aes(x = temp, y = count)) + 
  geom_point(alpha = 0.4, aes(color = temp)) +
  theme_bw()

# Convert the datetime column into POSIXct
bike$datetime = as.POSIXct(bike$datetime)

# Scatter plot count vs datetime w/ color gradient temp
ggplot(bike, aes(x = datetime, y = count)) + 
  geom_point(aes(color = temp), alpha = 0.4) +
  scale_color_continuous(low = '#55D8CE', high = '#FF6E2E') +
  theme_bw()

# Correlation between temp and count
cor(bike[, c('temp', 'count')])

# Boxplot of count and each season
ggplot(bike, aes(factor(season), count)) +
  geom_boxplot(aes(color = factor(season))) +
  theme_bw()

## Feature Engineering
# Hour column from datetime
bike$hour = sapply(bike$datetime, 
                   function(x){
  format(x, "%H")})

# Scatter plot of count versus hour, w/ color of temp
ggplot(filter(bike, workingday==1),
       aes(x = hour, y = count)) +
  geom_point(aes(color = temp),
             position = position_jitter(w=1, h=0),
             alpha = 0.5) +
  scale_color_gradientn(colours = c('dark blue', 'blue',
                                    'light blue', 'light green',
                                    'yellow', 'orange', 'red')) +
  theme_bw()

# Scatter plot for non-working days
ggplot(filter(bike, workingday==0),
       aes(x = hour, y = count)) +
  geom_point(aes(color = temp),
             position = position_jitter(w=1, h=0),
             alpha = 0.5) +
  scale_color_gradientn(colours = c('dark blue', 'blue',
                                    'light blue', 'light green',
                                    'yellow', 'orange', 'red')) +
  theme_bw()

### Building the model ###
## Use lm() to build a model that predicts count based on temp
temp.model = lm(count ~ temp, bike) 
summary(temp.model)

## How many bike rentals would be if the temp was 25°C? 
6.0462 + (9.1705 * 25)
temp.test = data.frame(temp = c(25))
predict(temp.model, temp.test)

## Use sapply() and as.numeric to change the hour column
bike$hour = sapply(bike$hour, as.numeric)
 
## Finally build a model that attempts to predict count 
final.model = lm(count ~ . - casual - registered - datetime - atemp,
                bike)
summary(final.model)


