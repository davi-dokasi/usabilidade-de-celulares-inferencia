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

## Mulheres
mulheres <- nrow(filter(dt, gender == 'female'))
mulheres
wmns <- filter(dt, gender == 'female')
media_wmns <- mean(wmns$tempo_no_dia)

## Teste proporções
# Váriáveis do teste

x2 <- dt |> filter(gender %in% c('female','male')) # Filtragem dos sexos
media_tempo <- mean(x2$tempo_no_dia) # Média do tempo de uso por dia
tamn <- length(x2$tempo_no_dia) # Tamanho do dataset

medi <- ((media_mens * homens) + (media_wmns * mulheres) )
medi/ tamn
dp <- medi/ tamn # Estimativa ponderada -> Pchapéu

# Aplicação da fórmula
z_value <- (media_mens - media_wmns) / (sqrt((dp * (1 - dp)) * ((1/homens + 1/mulheres ))))
z_value

alfa <- 0.05
regiao_critc <- alfa/2
qnorm(regiao_critc)


## Bagunça
mens$tempo_no_dia
nrow(mens$tempo_no_dia)
# database com/ vetor com mean, não precisa do mean, só vetor embaixo
prop.test(c(mens$tempo_no_dia, wmns$tempo_no_dia), c(homens, mulheres), conf.level = 0.95)
