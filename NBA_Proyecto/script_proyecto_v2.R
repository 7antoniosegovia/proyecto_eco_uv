#install.packages("RColorBrewer")
#install.packages("usmap")
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

#- Datos descargados a mano desde basketballreference.com, ya que era la unica base de datos de nba gratis disponible.
data <- import("./datos/data.csv", setclass = "tibble")

#- Filtramos datos para quedarnos solo con a partir de la temporada 2000-2001
data <- data %>% filter(Season > 2000)
data2 <- data %>% filter(Season > 1999) #se usará después


#- Cambiamos nombre de equipos que han tenido varios nombres en el periodo o que se han mudado manteniendo la misma franquicia

data$Tm <- gsub("NJN", "BRK", data$Tm) #- New Jersey Nets - Brooklyn Nets
data$Tm <- gsub("CHO", "CHA", data$Tm) #- Charlotte Bobcats - Charlotte Hornets
data$Tm <- gsub("VAN", "MEM", data$Tm) #- Vancouver Grizzlies - Memphis Grizzlies
data$Tm <- gsub("NOH", "NOP", data$Tm) #- New Orleans Hornets - New Orleans Pelicans
data$Tm <- gsub("NOK", "NOP", data$Tm) #- New Orleans/Oklahoma City Hornets - New Orleans Pelicans
data$Tm <- gsub("SEA", "OKC", data$Tm) #- Seattle Supersonics - Oklahoma City Thunder


#- Eliminamos registros de los Charlotte Hornets de las temporadas 00-01 y 01-02

data <- subset(data, Tm != "CHH")


#Seleccionamos los datos que nos conviene para nuestro estudio

data <- data %>% select(Season, Tm, W, L, FG, FGA, FGp, TP, TPA, TPp, PTS)

#Hacemos una primera visualización de los datos para observar la tendencia.

p <- data %>% ggplot(aes(Season, TPA)) + geom_point(aes(color = Tm)) +
        labs(title = "Triples intentados por partido por cada equipo",
        subtitle = "2000-2019",
        x = "Temporada",
        y = "Intentos por partido") +
        annotate("rect", xmin = "2010-11", xmax = Inf ,ymin = -Inf, ymax = Inf, alpha = 0.33, fill = "yellow")

ggdraw(p) +  draw_image("./imagenes/NBA.png",
               x = 0.25, y = 0.87, hjust = 1, vjust = 1, width = 0.15, height = 0.20)

#- Quiero hacer una visualización animada de la evolución, haré algunas transformaciones

data1 <-  data %>% separate(Season, "Season", sep = "-")
data1$Season <- as.numeric(data1$Season)

#- El grafico animado

data1 %>% ggplot(aes(Tm, TPA)) + geom_point(aes(color = Tm)) +
    labs(title = "Evolución de triples intentados por equipo",
        subtitle = "Año: {frame_time}",
        x = "Equipo",
        y = "Intentos por partido") +
    transition_time(Season) +
    ease_aes("linear")

anim_save("evolucion-tp-equipos.gif")


#- Aunque los datos no se ven con claridad debido a la gran cantidad de equipos, se observa una tendencia claramente ascendente desde 2010 (Rectangulo amarillo en el primer grafico), así que trabajaremos con los datos a partir de dicha temporada

data <- data %>% filter(Season > 2009)

#- En la NBA, los equipos están divididos geográficamente por conferencias, y éstas por divisiones, habiendo un total de 6 divisiones de 5 equipos cada una.

#- Creamos un vector de estados y un bucle para asignar a cada equipo a su estado.

state <- vector("character", nrow(data))
for (i in seq(1, nrow(data))) {
    if(data$Tm[[i]] == "ATL"){
        state[[i]] <- "GA"
    }
    else if(data$Tm[[i]] == "CHA"){
        state[[i]] <- "NC"
    }
    else if(data$Tm[[i]] %in% c("MIA", "ORL")){
        state[[i]] <- "FL"
    }
    else if(data$Tm[[i]] == "WAS"){
        state[[i]] <- "WA"
    }
    else if(data$Tm[[i]] == "BOS"){
        state[[i]] <- "MA"
    }
    else if(data$Tm[[i]] == "BRK"){
        state[[i]] <- "NY"
    }
    else if(data$Tm[[i]] == "NYK"){
        state[[i]] <- "NY"
    }
    else if(data$Tm[[i]] == "PHI"){
        state[[i]] <- "PA"
    }
    else if(data$Tm[[i]] == "CHI"){
        state[[i]] <- "IL"
    }
    else if(data$Tm[[i]] == "CLE"){
        state[[i]] <- "OH"
    }
    else if(data$Tm[[i]] == "DET"){
        state[[i]] <- "MI"
    }
    else if(data$Tm[[i]] == "IND"){
        state[[i]] <- "IN"
    }
    else if(data$Tm[[i]] == "MIL"){
        state[[i]] <- "WI"
    }
    else if(data$Tm[[i]] == "DEN"){
        state[[i]] <- "CO"
    }
    else if(data$Tm[[i]] == "MIN"){
        state[[i]] <- "MN"
    }
    else if(data$Tm[[i]] == "OKC"){
        state[[i]] <- "OK"
    }
    else if(data$Tm[[i]] == "POR"){
        state[[i]] <- "OR"
    }
    else if(data$Tm[[i]] == "UTA"){
        state[[i]] <- "UT"
    }
    else if(data$Tm[[i]] %in% c("LAL", "LAC", "SAC", "GSW")){
        state[[i]] <- "CA"
    }
    else if(data$Tm[[i]] == "PHO"){
        state[[i]] <- "AZ"
    }
    else if(data$Tm[[i]] %in% c("HOU", "DAL", "SAS")){
        state[[i]] <- "TX"
    }
    else if(data$Tm[[i]] == "NOP"){
        state[[i]] <- "LA"
    }
    else if(data$Tm[[i]] == "MEM"){
        state[[i]] <- "TN"
    }
}
data <- cbind(data, state)
#- Creamos un vector de divisiones y un bucle para asignar cada equipo a su division.

div <- vector("character", nrow(data))
for (i in seq(1, nrow(data))) {
    if(data$Tm[[i]] %in% c("ATL", "CHA", "MIA", "ORL", "WAS")){
        div[[i]] <- "SE"
    }
    else if(data$Tm[[i]] %in% c("BOS", "BRK", "NYK", "PHI", "TOR")){
        div[[i]] <- "AT"
    }
    else if(data$Tm[[i]] %in% c("CHI", "CLE", "DET", "IND", "MIL")){
        div[[i]] <- "CEN"
    }
    else if(data$Tm[[i]] %in% c("DEN", "MIN", "OKC", "POR", "UTA")){
        div[[i]] <- "NW"
    }
    else if(data$Tm[[i]] %in% c("GSW", "LAC", "LAL", "PHO", "SAC")){
        div[[i]] <- "PA"
    }
    else{
        div[[i]] <- "SW"
    }
}
data <- cbind(data, div)
data <- data %>% select(Season, Tm, state, div, everything())

#- Visto en el mapa, así quedaría la partición por divisiones según el estado del equipo

plot_usmap(regions = "states",
    data = data, values = "div", labels = F) +
    labs(title = "Separación divisional",
        subtitle = "Así se dividen los equipos por división según su estado")

#- Vamos a representar los triples intentados por partido de cada equipo, separandolos en divisiones para mejor visualización. Haremos también interactivo cada gráfico. Nota: ggplotly aún no acepta ni subtítulos ni leyendas horizontales, así que nos cambiará el gráfico un poco.

plot_se <- data %>% filter(div == "SE") %>%
    ggplot(aes(Season, TPA)) + geom_point(aes(color = Tm)) +
    scale_color_brewer(palette = "Set1") + theme_fivethirtyeight() +
    labs(title = "Triples intentados por partido por cada equipo",
        subtitle = "2010-2019 | División Sudeste",
        x = "Temporada",
        y = "Intentos por partido")

ggdraw(plot_se) +  draw_image("./imagenes/ATL.png",
               x = 0.360, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20) +
    draw_image("./imagenes/CHA.png",
               x = 0.475, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20) +
    draw_image("./imagenes/MIA.png",
               x = 0.595, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20) +
    draw_image("./imagenes/ORL.png",
               x = 0.710, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20) +
    draw_image("./imagenes/WAS.png",
               x = 0.835, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20)

plot_at <- data %>% filter(div == "AT") %>%
    ggplot(aes(Season, TPA)) + geom_point(aes(color = Tm)) +
    scale_color_brewer(palette = "Dark2") + theme_fivethirtyeight() +
    labs(title = "Triples intentados por partido por cada equipo",
        subtitle = "2010-2019 | División Atlántico",
        x = "Temporada",
        y = "Intentos por partido")

ggdraw(plot_at) +  draw_image("./imagenes/BOS.png",
               x = 0.365, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20) +
    draw_image("./imagenes/NJN.png",
               x = 0.485, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20) +
    draw_image("./imagenes/NYK.png",
               x = 0.605, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20) +
    draw_image("./imagenes/PHI.png",
               x = 0.720, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20) +
    draw_image("./imagenes/TOR.png",
               x = 0.8325, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20)

plot_cen <- data %>% filter(div == "CEN") %>%
    ggplot(aes(Season, TPA)) + geom_point(aes(color = Tm)) +
    scale_color_brewer(palette = "Set2") + theme_fivethirtyeight() +
    labs(title = "Triples intentados por partido por cada equipo",
        subtitle = "2010-2019 | División Central",
        x = "Temporada",
        y = "Intentos por partido")

ggdraw(plot_cen) +  draw_image("./imagenes/CHI.png",
               x = 0.370, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20) +
    draw_image("./imagenes/CLE.png",
               x = 0.487, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20) +
    draw_image("./imagenes/DET.png",
               x = 0.6025, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20) +
    draw_image("./imagenes/IND.png",
               x = 0.715, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20) +
    draw_image("./imagenes/MIL.png",
               x = 0.830
    , y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20)


plot_pac <- data %>% filter(div == "PA") %>%
    ggplot(aes(Season, TPA)) + geom_point(aes(color = Tm)) +
    scale_color_brewer(palette = "Accent") + theme_fivethirtyeight() +
    labs(title = "Triples intentados por partido por cada equipo",
        subtitle = "2010-2019 | División Pacífico",
        x = "Temporada",
        y = "Intentos por partido")

ggdraw(plot_pac) +  draw_image("./imagenes/GSW.png",
               x = 0.365, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20) +
    draw_image("./imagenes/LAC.png",
               x = 0.485, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20) +
    draw_image("./imagenes/LAL.png",
               x = 0.600, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20) +
    draw_image("./imagenes/PHO.png",
               x = 0.718, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20) +
    draw_image("./imagenes/SAC.png",
               x = 0.840, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20)


plot_sw <- data %>% filter(div == "SW") %>%
    ggplot(aes(Season, TPA)) + geom_point(aes(color = Tm)) +
    scale_color_brewer(palette = "Spectral") + theme_fivethirtyeight() +
    labs(title = "Triples intentados por partido por cada equipo",
        subtitle = "2010-2019 | División Sudoeste",
        x = "Temporada",
        y = "Intentos por partido")

ggdraw(plot_sw) +  draw_image("./imagenes/DAL.png",
               x = 0.350, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20) +
    draw_image("./imagenes/HOU.png",
               x = 0.475, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20) +
    draw_image("./imagenes/MEM.png",
               x = 0.600, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20) +
    draw_image("./imagenes/NOH.png",
               x = 0.725, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20) +
    draw_image("./imagenes/SAS.png",
               x = 0.850, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20)


plot_nw <- data %>% filter(div == "NW") %>%
    ggplot(aes(Season, TPA)) + geom_point(aes(color = Tm)) +
    scale_color_brewer(palette = "RdBu") + theme_fivethirtyeight() +
    labs(title = "Triples intentados por partido por cada equipo",
        subtitle = "2010-2019 | División Noroeste",
        x = "Temporada",
        y = "Intentos por partido")

ggdraw(plot_nw) +  draw_image("./imagenes/DEN.png",
               x = 0.360, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20) +
    draw_image("./imagenes/MIN.png",
               x = 0.4775, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20) +
    draw_image("./imagenes/OKC.png",
               x = 0.595, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20) +
    draw_image("./imagenes/POR.png",
               x = 0.720, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20) +
    draw_image("./imagenes/UTA.png",
               x = 0.840, y = 0.15, hjust = 1, vjust = 1, width = 0.05, height = 0.20)


#- Ahora, los gráficos interactivos.

ggplotly(plot_se)
ggplotly(plot_at)
ggplotly(plot_cen)
ggplotly(plot_pac)
ggplotly(plot_sw)
ggplotly(plot_nw)



#- Creamos variables que multipliquen porcentajes x 100

data <- data %>% mutate(TPpc = TPp * 100)
data <- data %>% mutate(FGpc = FGp * 100)

#- Creamos variable que mida la proporción de triples intentados / tiros totales intentados

data <- data %>% mutate(prop = (TPA / FGA) * 100)
data$prop <- format(round(data$prop, 3), nsmall = 3)
data$prop <- as.numeric(data$prop)

#- Calculamos la media por temporada de esta proporción

data <- data %>% group_by(Season) %>% mutate(mprop = mean(prop))

#- Vemos como no solo se ha evolucionado hacia tirar más triples, si no también a tirar menos de dos.

data1 <-  data %>% separate(Season, "Season", sep = "-")
data1$Season <- as.numeric(data1$Season)

data1$mprop <- format(round(data1$mprop, 3), nsmall = 3)
data1$mprop <- as.numeric(data1$mprop)


data1 %>% ggplot(aes(Season, mprop)) + geom_line() + geom_point() +
    geom_text(aes(label = mprop), size = 3.5, vjust = -1, color = "red") +
    labs(title = "Evolución de la proporción de triples sobre tiros totales")



#- El hecho de tirar más triples no implica incrementar porcentajes

data %>% group_by(Season) %>% ggplot(aes(TPA, TPpc)) +
    geom_point(aes(color = Season)) + geom_smooth(se = FALSE) +
    labs(title = "Relación triples intentados - porcentaje de aciertos")

modela <- lm(TPpc ~ TPA, data)
summ(modela)

#- Aunque el R2 sea muy bajo, vemos como la influencia de TPA sobre el porcentaje es positiva pero muy baja


#- Relación Tiros intentados con puntos anotados?

data1 <-  data %>% separate(Season, "Season", sep = "-")
data1$Season <- as.numeric(data1$Season)


data1 %>% group_by(Season) %>% ggplot(aes(TPA, PTS)) +
    geom_point(aes(color = Tm)) +
    labs(title = "Relación triples intentados - porcentaje de aciertos",
        subtitle = "Año: {frame_time}") +
    transition_time(Season) +
    ease_aes("linear")

anim_save("evolucion-tp-puntos.gif")

#Vemos desplazamiento diagonal, modelamos con smooth

data %>% group_by(Season) %>% ggplot(aes(TPA, PTS)) +
    geom_point(aes(color = Season)) + geom_smooth(se = FALSE)
    labs(title = "Relación triples intentados - Puntos anotados")


modela <- lm(PTS ~ TPA, data)
summ(modela)

#- Vemos que aquí si que hay una correlación, aunque los porcentajes no hayan aumentado, debido a más velocidad de los partidos

#- Para finalizar, veremos unos datos sobre el rendimiento de los equipos en este periodo desde 2010, a partir de esta "revolución del triple"

options(pillar.sigfig = 4) #- Para que al visualizar se vean 4 digitos

#- Mejores temporadas (más victorias)

data %>% arrange(desc(W)) %>% select(Season, Tm, W) %>% head(5)

#- Peores temporadas (menos victorias)

data %>% arrange(W) %>% select(Season, Tm, W) %>% head(5)

#- Mejores registros de PTS por partido

data %>% arrange(desc(PTS)) %>%  select(Season, Tm, PTS) %>% head(5)

#- Peores registros de PTS por partido

data %>% arrange(PTS) %>% select(Season, Tm, PTS) %>% head(5)

#- Más triples intentados por partido

data %>% arrange(desc(TPA)) %>%  select(Season, Tm, TPA) %>% head(5)

#- Más triples anotados por partido

data %>% arrange(desc(TP)) %>%  select(Season, Tm, TP) %>% head(5)

#- Mayor proporcion de triples sobre tiros totales

data %>% arrange(desc(prop)) %>%  select(Season, Tm, prop) %>% head(5)

#- Mejor porcentaje de triples

data %>% arrange(desc(TPpc)) %>%  select(Season, Tm, TPpc) %>% head(5)

#- Una de estas tablas que tan de moda están con el ranking de triples anotados desde el 2000

data3 <- data2 %>% mutate(
    TPT = case_when(
        W + L == 82 ~ 82*TP,
        W + L != 82 ~ 66*TP
    ))

data3 <-  data3 %>% separate(Season, "Season", sep = "-")

data3$Season <- as.numeric(data3$Season)

data3 <- data3 %>% mutate(Season = Season + 1)



table <- data3 %>%
  filter(Season == 2000) %>%
  select(Season, Tm, TPT)


for (i in 2000:2019) {
  table <- data3 %>%
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

anim_save("ranking.gif")
