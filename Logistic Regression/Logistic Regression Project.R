
### Logistic Regression Project ###

## Librerías
library(ggplot2)
library(Amelia)
library(dplyr)

## Datos
adult = read.csv('ML/Logistic Regression/adult_sal.csv')
adult = select(adult,-X)
head(adult)
str(adult)
summary(adult)

### Data Cleaning ###
## Employer column
table(adult$type_employer)

# Combine 2 smallest group of type_employer
unemp <- function(job){
  job <- as.character(job)
  if (job=='Never-worked' | job=='Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}
adult$type_employer = sapply(adult$type_employer, unemp)
table(adult$type_employer)

# Combine 2 groups of a variable
group_emp <- function(job){
  if (job=='Local-gov' | job=='State-gov'){
    return('SL-gov')
  }else if (job=='Self-emp-inc' | job=='Self-emp-not-inc'){
    return('self-emp')
  }else{
    return(job)
  }
}
adult$type_employer = sapply(adult$type_employer, group_emp)
table(adult$type_employer)

## Marital column
table(adult$marital)

# Reducing groups of marital column
group_marital <- function(mar){
  mar <- as.character(mar)
  
  # Not-Married
  if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
    return('Not-Married')
    
    # Never-Married   
  }else if(mar=='Never-married'){
    return(mar)
    
    #Married
  }else{
    return('Married')
  }
}
adult$marital <- sapply(adult$marital,group_marital)
table(adult$marital)

## Country column
table(adult$country)
levels(adult$country)

# Making groups
{
  Asia <- c('China','Hong','India','Iran','Cambodia','Japan',
            'Laos','Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')
  North.America <- c('Canada','United-States','Puerto-Rico')
  Europe <- c('England' ,'France', 'Germany' ,'Greece',
              'Holand-Netherlands','Hungary','Ireland','Italy',
              'Poland','Portugal','Scotland','Yugoslavia')
  Latin.America <- c('Columbia','Cuba','Dominican-Republic',
                     'Ecuador','El-Salvador','Guatemala',
                     'Haiti','Honduras','Mexico','Nicaragua',
                     'Outlying-US(Guam-USVI-etc)',
                     'Peru','Jamaica','Trinadad&Tobago')
  Other <- c('South')
}

# Combining groups
group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.America){
    return('Latin.America')
  }else{
    return('Other')      
  }
}
adult$country <- sapply(adult$country,group_country)
table(adult$country)

# Factor columns
str(adult)
{ # adult$type_employer <- factor(adult$type_employer) opcional
  adult$type_employer = sapply(adult$type_employer,factor)
  adult$education = sapply(adult$education, factor)
  adult$marital = sapply(adult$marital, factor)
  adult$occupation = sapply(adult$occupation, factor)
  adult$relationship = sapply(adult$relationship, factor)
  adult$race = sapply(adult$race, factor)
  adult$sex = sapply(adult$sex, factor)
  adult$country = sapply(adult$country, factor)
  adult$income = sapply(adult$income, factor)
}
str(adult)


### Missing data ###
# Convert missing values to NA
adult[adult == '?'] <- NA
# Refactor all the columns and run again
table(adult$type_employer)

# Miss data
missmap(adult)
missmap(adult, y.at=c(1),y.labels = c(''))

# Omita the NA data
adult = na.omit(adult)
str(adult)
missmap(adult, y.at=c(1),y.labels = c(''))

### Exploratory data analysis - EDA ###
# Plot the income
ggplot(adult, aes(age)) +
  geom_histogram(aes(fill=income), color='black', binwidth=1) + 
  theme_bw()

# Plot hours worked per week
ggplot(adult,aes(hr_per_week)) + 
  geom_histogram() + 
  theme_bw()

# Rename the country column to region column 
names(adult)[names(adult)=="country"] <- "region"
str(adult)

# Barplot of income by region
ggplot(adult,aes(region)) + 
  geom_bar(aes(fill=income), color='black')+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Building model ###
# Library
library(caTools)
set.seed(101) 

# Split up the sample (SplitRatio = percent of sample==TRUE)
sample = sample.split(adult$income, SplitRatio = 0.70)
train = subset(adult, sample == TRUE)
test = subset(adult, sample == FALSE)

# Training model 
help('glm') # Logistic Regression Formula
model = glm(income ~ ., 
            family = binomial(logit), 
            data = train)
summary(model)

# Step function
help(step) # Function that fix the model fit
new.step.model = step(model) # Delete varaibles not significant
summary(new.step.model)

# Confusion matrix
test$predicted.income = predict(model, newdata=test, 
                                type="response")
table(test$income, test$predicted.income > 0.5)

### Results of the model ###
(6372+1423)/(6372+1423+548+872) # Accuracy
# 00+11/N
6732/(6372+548) # Recall
# 00/00+10
6732/(6372+872) # Precision
# 00/00+01


