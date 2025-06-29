
### Building a model ###

## Librería y datos
library(ggplot2)
library(Amelia)
library(dplyr)
df.train = read.csv('ML/Logistic Regression/titanic_train.csv')
str(df.train)

## Imputation of age based on class
impute_age <- function(age,class){
  out <- age
  for (i in 1:length(age)){
    if (is.na(age[i])){
      if (class[i] == 1){
        out[i] <- 37
      }else if (class[i] == 2){
        out[i] <- 29
      }else{
        out[i] <- 24
      }
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}

## Replacing values
fixed.ages = impute_age(df.train$Age, df.train$Pclass)
df.train$Age = fixed.ages

## Check the data
missmap(df.train, 
        main="Imputation Check",
        legend=FALSE)

## Select relevant columns
df.train = select(df.train,-PassengerId,-Name,-Ticket,-Cabin)
head(df.train, 3)
str(df.train)

## Factor Columns
{
df.train$Survived = factor(df.train$Survived)
df.train$Pclass = factor(df.train$Pclass)
df.train$Parch = factor(df.train$Parch)
df.train$SibSp = factor(df.train$SibSp)
df.train$Embarked = factor(df.train$Embarked)
}
str(df.train)

### Train the model ###
help('glm') # Logistic Regression Formula
log.model <- glm(formula = Survived ~ . , 
                 family = binomial(link = 'logit'),
                 data = df.train)
summary(log.model)

### Predicting ###
# Library
library(caTools)
set.seed(101)

# Split the data
split = sample.split(df.train$Survived, SplitRatio = 0.70)
final.train = subset(df.train, split == TRUE)
final.test = subset(df.train, split == FALSE)

# Training model on train set
final.log.model <- glm(formula=Survived ~ . , 
                       family = binomial(link = 'logit'),
                       data = final.train)
summary(final.log.model)

# Prediction Accuracy
fitted.probabilities = predict(final.log.model,
                                newdata = final.test,
                                type = 'response')

# Predicted values
fitted.results = ifelse(fitted.probabilities > 0.5,1,0)
misClasificError = mean(fitted.results != final.test$Survived)
print(paste('Accuracy is',1-misClasificError))

# Confussion Matrix
table(final.test$Survived, fitted.probabilities > 0.5)

