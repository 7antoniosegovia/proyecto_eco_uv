library(rio)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(ggthemes)
library(RColorBrewer)
library(usmap)
library(jtools)
library(gganimate)
library(cowplot)
library(rpivotTable)
library(knitr)
library(kableExtra)
library(DT)


#DATOS

data <- import("./datos/data.csv", setclass = "tibble")

data$Tm <- gsub("NJN", "BRK", data$Tm) #- New Jersey Nets - Brooklyn Nets
data$Tm <- gsub("CHO", "CHA", data$Tm) #- Charlotte Bobcats - Charlotte Hornets
data$Tm <- gsub("VAN", "MEM", data$Tm) #- Vancouver Grizzlies - Memphis Grizzlies
data$Tm <- gsub("NOH", "NOP", data$Tm) #- New Orleans Hornets - New Orleans Pelicans
data$Tm <- gsub("NOK", "NOP", data$Tm) #- New Orleans/Oklahoma City Hornets - New Orleans Pelicans
data$Tm <- gsub("SEA", "OKC", data$Tm) #- Seattle Supersonics - Oklahoma City Thunder

data <- subset(data, Tm != "CHH")

data <- data %>% select(Season, Tm, W, L, FG, FGA, FGp, TP, TPA, TPp, PTS)

data1 <- data %>% filter(Season > 1999)
data <- data %>% filter(Season > 2009)

data <- data %>% mutate(TPpc = TPp * 100)
data <- data %>% mutate(FGpc = FGp * 100)

data <- data %>% mutate(prop = (TPA / FGA) * 100) #- Creamos la variable [0; 100]
data$prop <- format(round(data$prop, 3), nsmall = 3) #- Reducimos los decimales a 3
data$prop <- as.numeric(data$prop)

data <- data %>% group_by(Season) %>% mutate(mprop = mean(prop))


# NUEVO
data1 <-  data %>% separate(Season, "Season", sep = "-")
data1$Season <- as.numeric(data1$Season)

data1$mprop <- format(round(data1$mprop, 3), nsmall = 3)
data1$mprop <- as.numeric(data1$mprop)


data1 %>% ggplot(aes(Season, mprop)) + geom_line() + geom_point() +
    geom_text(aes(label = mprop), size = 3.5, vjust = -1, color = "red") +
    labs(title = "Evolución de la proporción de triples sobre tiros totales")
