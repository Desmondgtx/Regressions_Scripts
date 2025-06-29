
### Logistic Regression ###

## Librería y datos
library(ggplot2)
library(Amelia)
df.train = read.csv('ML/Logistic Regression/titanic_train.csv')
str(df.train)

## Missing data
missmap(df.train, 
        main="Titanic Training Data - Missings Map", 
        legend=FALSE) # col=c("yellow", "black"

## Data Visualization
# Passengers survived
ggplot(df.train,aes(Survived)) + 
  geom_bar()

# Passengers class
ggplot(df.train,aes(Pclass)) + 
  geom_bar(aes(fill = factor(Pclass)), alpha = 0.5)

# Gender
ggplot(df.train,aes(Sex)) + 
  geom_bar(aes(fill = factor(Sex)), alpha = 0.5)

# Age
ggplot(df.train,aes(Age)) + 
  geom_histogram(fill = 'blue', bins = 20, alpha = 0.5)

# Siblings on board
ggplot(df.train,aes(SibSp)) + 
  geom_bar(fill = 'red', alpha = 0.5)

# Fare paid
ggplot(df.train,aes(Fare)) + 
  geom_histogram(fill = 'green', color = 'black', alpha = 0.5)

## Data cleaning
# Average age by passenger class
ggplot(df.train,aes(Pclass,Age)) + 
  geom_boxplot(aes(group=Pclass, fill=factor(Pclass), alpha=0.4)) + 
  scale_y_continuous(breaks = seq(min(0), max(80), by = 2)) +
  theme_bw()

# Imputation of age based on class
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

# Replacing values
fixed.ages = impute_age(df.train$Age, df.train$Pclass)
df.train$Age = fixed.ages

# Check the data
missmap(df.train, 
        main="Imputation Check",
        legend=FALSE)



