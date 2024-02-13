# Imports das libraries
library(dplyr)
library(tidyverse)

# Datasets utilizados
dataset <- read.csv('dummy_data.csv')
head(dataset)

dt <- dataset |> 
  mutate(tempo_no_dia <- time_spent / 24)

head(dt)

## Homens
homens <- nrow(filter(dt, gender == 'male'))
homens
mens <- filter(dt, gender == 'male')
media_mens <- mean(mens$tempo_no_dia)

x1 <- homens * media_mens
x1

x2 <- filter(dt$gender %in% c('female','male'))

## Mulheres
mulheres <- nrow(filter(dt, gender == 'female'))
mulheres
wmns <- filter(dt, gender == 'female')
media_wmns <- (wmns$tempo_no_dia)


mens$tempo_no_dia
nrow(mens$tempo_no_dia)
# database com/ vetor com mean, não precisa do mean, só vetor embaixo

prop.test(c(mens$tempo_no_dia, wmns$tempo_no_dia), c(homens, mulheres), conf.level = 0.95)
