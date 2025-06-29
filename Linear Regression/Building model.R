
### Building a model ###

## General Form
# The general model of building a linear regression model in R
model = lm(y ~ x1 + x2, data)
model = lm(y ~ . , data) # or to use all the features


## Librería y datos
library(caTools)
df = read.csv('ML/Linear Regression/student-mat.csv', sep = ';')

## Set a random see so your "random" results are the same
set.seed(101) 

## Split up the sample (percent of sample)
sample = sample.split(df$G3, SplitRatio = 0.7) 

## Training Data
train = subset(df, sample == TRUE)

## Testing Data
test = subset(df, sample == FALSE)

## Training model
model = lm(G3 ~ .,train)
summary(model) # Interpret the model

## Visualize the model
# Grab residuals
res = residuals(model)

# Convert to DataFrame for gglpot
res = as.data.frame(res)
head(res)

# Histogram of residuals
ggplot(res,aes(res)) +  
  geom_histogram(fill = 'blue', alpha = 0.5)

# Residuals and regression validation
plot(model)


### Predictions ###
# Predict test set
G3.predictions = predict(model, test)

# Root mean squared error
results = cbind(G3.predictions, test$G3) 
colnames(results) = c('predicted','real')
results = as.data.frame(results)

# Handling negative values
to_zero = function(x){
  if (x < 0){
    return(0)
  }else{
    return(x)
  }
}

# Apply function
results$predicted = sapply(results$predicted,to_zero)

# Mean squared error
mse = mean((results$real-results$predicted)^2)
mse
mse^0.5 # Root MSE

# R-Squared Value
SSE = sum((results$predicted - results$real)^2)
SST = sum((mean(df$G3) - results$real)^2)
R2 = 1 - SSE/SST
R2


