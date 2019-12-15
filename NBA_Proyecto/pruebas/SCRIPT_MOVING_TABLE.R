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






#TABLA EN MOVIMIENTOOOOO

data2 <- data1 %>% mutate(
    TPT = case_when(
        W + L == 82 ~ 82*TP,
        W + L != 82 ~ 66*TP
    ))

data2 <-  data2 %>% separate(Season, "Season", sep = "-")

data2$Season <- as.numeric(data2$Season)

data2 <- data2 %>% mutate(Season = Season + 1)



table <- data2 %>%
  filter(Season == 2000) %>%
  select(Season, Tm, TPT)


for (i in 2000:2019) {
  table <- data2 %>%
    filter(Season <= i) %>%
    group_by(Tm) %>%
    summarise(TPTacum = sum(TPT, na.rm = TRUE)) %>%
    mutate(Season = i) %>%
    bind_rows(table)
}

anim_table <- table %>%
  group_by(Season) %>%
  mutate(
    rank = min_rank(-TPTacum) * 1,
    Value_rel = TPTacum / TPTacum[rank == 1],
    Value_lbl = paste0(" ", TPTacum)
  ) %>%
  filter(rank <= 10) %>%
  ungroup()



p <- ggplot(anim_table, aes(rank)) +
  geom_tile(aes(
    y = TPTacum / 2,
    height = TPTacum,
    width = 0.9,
    fill = Tm
  ), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Tm, " ")), size = 5, vjust = 0.2, hjust = 0) +
  geom_text(aes(y = TPTacum, label = Value_lbl, hjust = 0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE)





p <- p +labs(
    title = "Evolución triples anotados desde 2000",
    subtitle = "{closest_state}",
    x = "", y = "Triples totales"
  ) +
  theme(
    plot.title = element_text(color = "darkblue", face = "bold", hjust = 0, size = 15),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  transition_states(Season, transition_length = 4, state_length = 1) +
  ease_aes("cubic-in-out")

animate(p, duration = 20)

