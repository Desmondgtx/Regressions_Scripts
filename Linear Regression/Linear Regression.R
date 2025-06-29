
### Linear Regression ###

## Librería y datos
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrplot) # Correlation plots
library(corrgram) # Correlation diagrams
df = read.csv('ML/Linear Regression/student-mat.csv', sep = ';')

## Limpieza de datos ##
# Ver datos
head(df)
any(is.na(df))
str(df)

# Tomar solo las columnas numéricas
num.cols = sapply(df, is.numeric)

# Filtrar las columnas numeras para correlación
cor.data = cor(df[,num.cols])
cor.data

## Visualizar correlación ##
# Correlation plot
help('corrplot')
corrplot(cor.data,method = 'color')

# Correlation diagram
help('corrgram')
corrgram(df)
corrgram(df, order = TRUE, 
         lower.panel = panel.shade,
         upper.panel = panel.pie, 
         text.panel = panel.txt)

# Histogram plot of G3
ggplot(df,aes(x = G3)) + 
  geom_histogram(bins = 20, alpha = 0.5, fill = 'blue') + 
  theme_minimal()


