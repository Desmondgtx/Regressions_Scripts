
### Regresión lineal ### 

## Librerías
library(ggplot2)
library(tidyverse)

## Datos
data("Orange")
head(Orange)
data.frame("Orange")

## Visualización
Orange %>%
  ggplot(aes(x = age,
             y = circumference)) +
  geom_point()

## Ecuación estimada
lm(circumference ~ age, data = Orange)

## Visualización + regresión lineal
Orange %>%
  ggplot(aes(x = age,
             y = circumference)) +
  geom_point() +
  geom_abline(intercept = 17.3,
              slope = 0.1068,
              color = 'blue')

## Regresión + punto de corte
Orange %>%
  ggplot(aes(x = age,
             y = circumference)) +
  geom_point() +
  geom_abline(intercept = 17.3,
              slope = 0.1068,
              color = 'blue')+
  geom_vline(xintercept = 800,
             color='red')


## Resultados
dias <- 800
medida <- 0.1068 * dias + 17.3
print(medida)



