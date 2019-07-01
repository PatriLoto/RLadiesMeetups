
library(extrafont)
loadfonts(dev = "win")
loadfonts(dev = "FuturaBT-ExtraBlack")

library(tidyverse)
library(ggforce)  
library(ggthemes)
library(wesanderson)
library(DT)
library(maps)
# install_packages("readr")
capitulos_rladies <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-26/capitulos_rladies.csv")
eventos_rladies <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-26/eventos_rladies.csv")

#trae los datos como en una tabla
datatable(capitulos_rladies, rownames = FALSE,
          options = list(pageLength = 10))


datosordenados <-capitulos_rladies %>% arrange(desc(miembros))
View(datosordenados)

#con grises más claros
world <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +      #otra opcion:gray85 #562457
  theme_map()
world
#con bordes más oscuros
world2 <- ggplot() +
  borders("world", colour = "562457", fill = "gray80") +      #otra opcion:gray85 #562457
  theme_map()
world2

#con bordes y relleno más oscuros
world3 <- ggplot() +
  borders("world", colour = "562457", fill = "gray80") +      #otra opcion:gray85 #562457
  theme_map()
world3

#562457


#colores de wes anderson
wes_palettes <- names(wesanderson::wes_palettes)

# function to extract all colours for palettes along with palette name
wes_pal_func <- function(pal) {
  col_df <- tibble(colours = wes_palette(pal), palette = pal)
}

# create dataframe of all colours and palette names
wes_colours <- map_df(wes_palettes, wes_pal_func)

View(wes_colours)


rladies_Palette <- rev(wes_colours[c(1, 16, 23, 32, 37, 51, 65, 75, 82), ]$colours)
rladies_Palette

#a2e0c3
actor_colour <- wes_colours[47, ]$colours
actor_colour
#--------------------------
#primer version
# -------------------------
map <- world1 +
  geom_point(aes(x = longitud, y = latitud,
                 text = paste('city: ', ciudad,
                              '<br /> created : ', creacion),
                 size = miembros),
             data = capitulos_rladies, colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 8), breaks = c(250, 500, 750, 1000,1250,1500,1750,2000)) +
  labs(size = 'Miembros Meetups')

ggsave("mapaRLadies.png",width = 10, height = 5, dpi = "retina")

#--------------------
# segunda version
#--------------------

map2 <- world +
  geom_point(aes(x = longitud, y = latitud,
                 text = paste('city: ', ciudad,
                              '<br /> created : ', creacion),
                 size = miembros),
             data = capitulos_rladies, colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 8), breaks = c(250, 500, 750, 1000,1250,1500,1750,2000)) +
  labs(size = 'Miembros Meetups') +
  # set theme of graph - use the futura font
  #theme(background = wes_palette("Chevalier1")[3], foreground = NA, base_family = "FuturaBT-BoldCondensed") +
  # set all other themes and labels like any old ggplot
  theme(#legend.text = element_text(colour = actor_colour, face = "bold", size = 12),
        #legend.title = element_text(colour = actor_colour, face = "bold", size = 12),
        legend.title.align = 1,
        #legend.background = element_rect(colour = actor_colour, fill =  wes_palette("Chevalier1")[3]),
        plot.background = element_rect(fill = "#D3DDDC", colour = "#D3DDDC"),
        plot.title = element_text(colour = wes_palette("GrandBudapest1")[2], size = 22, hjust = 0.5, family = "FuturaBT-ExtraBlack"),
        #plot.subtitle = element_text(colour = actor_colour, size = 16, hjust = 0.5),
        plot.caption = element_text(colour =  wes_palette("GrandBudapest1")[2], size = 12, hjust = 0.5),
        plot.margin = margin(0.8, 0.1, 0.5, 0.1, "cm")) +
  labs(title = toupper("Rladies en el mundo"),
       subtitle = "Cantidad de miembros en Meetups hasta el 25 de junio del 2019",
       caption = "@patriloto | Source: rladiesGlobal")

map2
ggsave("mapaRLadies2.png",width = 10, height = 5, dpi = "retina")

#----------------------------------------------------------------------------------------
# 3 ra versión
#-------------------------------------
map3 <- world2 +
  geom_point(aes(x = longitud, y = latitud,
                 text = paste('city: ', ciudad,
                              '<br /> created : ', creacion),
                 size = miembros),
             data = capitulos_rladies, colour = 'purple', alpha = .5) +
  scale_size_continuous(range = c(1, 8), breaks = c(250, 500, 750, 1000,1250,1500,1750,2000)) +
  labs(size = 'Miembros Meetups') +
  # set theme of graph - use the futura font
  #theme(background = wes_palette("Chevalier1")[3], foreground = NA, base_family = "FuturaBT-BoldCondensed") +
  # set all other themes and labels like any old ggplot
  theme(#legend.text = element_text(colour = actor_colour, face = "bold", size = 12),
    #legend.title = element_text(colour = actor_colour, face = "bold", size = 12),
    legend.title.align = 1,
    #legend.background = element_rect(colour = actor_colour, fill =  wes_palette("Chevalier1")[3]),
    plot.background = element_rect(fill = "#D3DDDC", colour = "#D3DDDC"),
    plot.title = element_text(colour = wes_palette("GrandBudapest1")[2], size = 22, hjust = 0.5, family = "FuturaBT-ExtraBlack"),
   # plot.subtitle = element_text(colour = actor_colour, size = 16, hjust = 0.5),
    plot.caption = element_text(colour =  wes_palette("GrandBudapest1")[2], size = 12, hjust = 0.5),
    plot.margin = margin(0.8, 0.1, 0.5, 0.1, "cm")) +
  labs(title = toupper("Rladies en el mundo"),
       subtitle = "Cantidad de miembros en Meetups hasta el 25 de junio del 2019",
       caption = "@patriloto | Source: rladiesGlobal")

map3
ggsave("mapaRLadies3.png",width = 10, height = 5, dpi = "retina")


#--------------------------------------------------------------
# 4ta versión
map4 <- world2 +
  geom_point(aes(x = longitud, y = latitud,
                 text = paste('city: ', ciudad,
                              '<br /> created : ', creacion),
                 size = miembros),
             data = capitulos_rladies, colour = "#88398A", alpha = .5) +     #562457#88398A
  scale_size_continuous(range = c(1, 8), breaks = c(250, 500,1000,1500,2000)) +
  labs(size = 'Miembros Meetups') +
  # set theme of graph - use the futura font
  #theme(background = wes_palette("Chevalier1")[3], foreground = NA, base_family = "FuturaBT-BoldCondensed") +
  # set all other themes and labels like any old ggplot
  theme(#legend.text = element_text(colour = actor_colour, face = "bold", size = 12),
    #legend.title = element_text(colour = actor_colour, face = "bold", size = 12),
    legend.title.align = 1,
    legend.background = element_rect(fill =  wes_palette("Chevalier1")[3]),  #colour = actor_colour, 
    plot.background = element_rect(fill = "#D3DDDC", colour = "#D3DDDC"),
    plot.title = element_text(colour = wes_palette("GrandBudapest1")[2], size = 22, hjust = 0.5, family = "FuturaBT-ExtraBlack", face="bold"),
    #plot.subtitle = element_text(colour = actor_colour, size = 16, hjust = 0.5,family = "FuturaBT-ExtraBlack", face="italic"),
    plot.caption = element_text(colour =  wes_palette("GrandBudapest1")[2], size = 10, hjust = 0.5,face="bold"),
    plot.margin = margin(0.8, 0.1, 0.5, 0.1, "cm")) +
  labs(title = toupper("Rladies en el mundo"),
       subtitle = "Cantidad de miembros en Meetups hasta el 25 de junio del 2019",
       caption = "@patriloto | Source:#DatosdeMiercoles")

map4
ggsave("mapaRLadies5.png",width = 10, height = 5, dpi = "retina")

#------------------------------------------------------------
#5ta version
#-----------------------------------------------------------
map7 <- world2 +
  geom_point(data = capitulos_rladies, aes(x = longitud, y = latitud,
                 text = paste('city: ', ciudad,
                              '<br /> created : ', creacion),
                 size = miembros, alpha = miembros, colour = "#88398A")) +     #562457#88398A  #alpha = .5
  scale_size_continuous(range = c(1, 10), breaks = c(250, 500,1000,1500,2000)) +
 # scale_color_continuous(option="purple", trans="log", breaks=c(250, 500, 750, 1000,1250,1500,1750,2000)) +
  scale_alpha_continuous(trans="log") +
  #theme_void() +
  #ylim(50,59) +
  #coord_map() +
  labs(size = 'Miembros Meetups') +
  theme(legend.position = "left",                ###c(0.85, 0.8),
    legend.text = element_text(colour ="#446455" , size = 8),
    legend.title = element_text(colour = "#446455", size = 10),   #actor_colour="#446455"
    legend.title.align = 1,
    legend.background = element_rect(fill = "#D3DDDC", colour =NA),  #colour = #446455=, "#miembros"
    panel.background = element_rect(fill = "#D3DDDC", colour =NA),
    plot.background = element_rect(fill = "#D3DDDC", colour = "#D3DDDC"),
    plot.title = element_text(colour = wes_palette("GrandBudapest1")[2], size = 22, hjust = 0.5, family = "FuturaBT-ExtraBlack", face="bold"),
    plot.subtitle = element_text(colour = "#446455", size = 16, hjust = 0.5,family = "FuturaBT-ExtraBlack", face="italic"),
    plot.caption = element_text(colour =  wes_palette("GrandBudapest1")[2], size = 10, hjust = 0.5,face="bold", vjust=1))+
    #plot.margin = margin(0.5, 0.1, 0.5, 0.1, "cm")) +
  labs(title = toupper("Rladies en el mundo"),
       subtitle = "Cantidad de miembros en Meetups hasta el 25 de junio del 2019",
       caption = "@patriloto | Source:#DatosdeMiercoles")

map7


#transition_time(importaXP) +
#  ease_aes('linear')+
 #  shadow_mark(alpha = 1, size = 2)
ggsave("mapaRLadies8.png",width = 10, height = 5, dpi = "retina")
