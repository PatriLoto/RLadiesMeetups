#Importo librerías

library(extrafont)
loadfonts(dev = "win")
library(tidyverse)
library(ggthemes)
library(wesanderson)
library(DT)
library(maps)
library(plotly)
library(mapproj)

#Lectura de datos
capitulos_rladies <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-26/capitulos_rladies.csv")
eventos_rladies <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-26/eventos_rladies.csv")

#Muestro los datos en formato de tabla con el paquete DT
datatable(capitulos_rladies, rownames = FALSE,
          options = list(pageLength = 10))

#Ordeno por cantidad de miembros para determinar los valores de la escala
datosordenados <-capitulos_rladies %>% arrange(desc(miembros))
View(datosordenados)

# mapa con bordes oscuros
mundo <- ggplot() +
  borders("world", colour = "562457", fill = "gray80") +      #opcion bordes claros:gray85 
  theme_map()
mundo

#colores de las paletas wes anderson
wes_palettes <- names(wesanderson::wes_palettes)
View(wes_palettes)
# extraigo los colores de todas las paletas de WesAnderson con sus correspondientes nombres (lo tomé del código de @committedtotape)
wes_paleta_func <- function(pal) {
  col_df <- tibble(colores = wes_palette(pal), palette = pal)
  }

# genero un dataframe con el nombre de cada paleta y los colores de la misma   
wes_colores <- map_df(wes_palettes, wes_paleta_func)
View(wes_colores) 

#------------------------------------------------------------
# Mapa de Rladies en Twitter
#-----------------------------------------------------------
mapaRladies <- mundo +
  geom_point(aes(x = longitud, y = latitud,
                 text = paste('Ciudad: ', ciudad,
                              '<br /> Miembros : ', miembros,
                              '<br /> Creado : ', creacion),
                 size = miembros), data = capitulos_rladies, colour = "#88398A", alpha = .5)  +     #562457#88398A  #alpha = .5
  scale_size_continuous(range = c(1, 10), breaks = c(250, 500,1000,1500,2000)) +
  # scale_color_continuous(option="purple", trans="log", breaks=c(250, 500, 750, 1000,1250,1500,1750,2000)) +
  theme_void() +
  labs(size = '') +
  theme(legend.position = "left",                
        legend.text = element_text(colour ="#446455" , size = 8),
        legend.title = element_text(colour = "#446455", size = 10),   #actor_colour="#446455"
        legend.title.align = 1,
        legend.background = element_rect(fill = "#D3DDDC", colour =NA),  #colour = #446455=, "#miembros"
        panel.background = element_rect(fill = "#D3DDDC", colour =NA),
        plot.background = element_rect(fill = "#D3DDDC", colour = "#D3DDDC"),
        plot.title = element_text(colour = wes_palette("GrandBudapest1")[2], size = 22, hjust = 0.5, family = "FuturaBT-ExtraBlack", face="bold"),#wes_palette("GrandBudapest1")[2]
        plot.subtitle = element_text(colour = "#446455", size = 14, hjust = 0.5,family = "FuturaBT-ExtraBlack", face="italic"),
        plot.caption = element_text(colour =  wes_palette("GrandBudapest1")[2], size = 10, hjust = 0.5,face="bold", vjust=1))+
  labs(title = toupper("Rladies en el mundo"),
       subtitle = "Cantidad de miembros por capítulo hasta el 25 de junio del 2019",
       caption = "#DatosDeMiercoles por Patricia Loto")

mapaRladies
