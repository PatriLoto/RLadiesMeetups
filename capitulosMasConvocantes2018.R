extrafont::loadfonts(device = "win")
library(tidyverse)
#library(ggridges)
library(paletteer)
library("LaCroixColoR")
#library(ggbeeswarm)
library(lubridate)
#library(fonts)
library(LaCroixColoR)
#library(cowplot)
#library(viridis)
library(png)
library(grid)
install.packages("ggbeeswarm")
install.packages("ggridges")
install.packages("fonts")
install.packages("themes")
install.packages("cowplot")
eventos_rladies <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-26/eventos_rladies.csv")

datatable(eventos_rladies, rownames = FALSE,
          options = list(pageLength = 10))

#total de asistentes desde su creación al 25 de junio del 2019
asistentesXCap <-eventos_rladies %>% group_by(capitulo)%>% summarise(totalasist=sum(respuesta_asistire))%>% arrange(desc(totalasist))
View(asistentesXCap)

#total de asistentes ordenado por año y capitulo desde su creación al 25 de junio del 2019
asistentesXYearCap <-eventos_rladies %>% group_by(capitulo, anio=year(fecha_local))%>% summarise(totalasist=sum(respuesta_asistire))%>% arrange(anio,totalasist)
View(asistentesXYearCap)
asistentesXYearCap[asistentesXYearCap$capitulo == "Spotkania Entuzjastów R-Warsaw RUG Meetup & R-Ladies Warsaw",1]<-"R-Ladies Warsaw"

#capítulos con más convocatoria durante el 2018
asistentes2018XCap <-filter(asistentesXYearCap, anio==2018)%>%arrange(desc(totalasist))
asistentes2018XCap
View(asistentes2018XCap)
#renombro el capitulo de Warsaw
asistentes2018XCap[asistentes2018XCap$capitulo == "Spotkania Entuzjastów R-Warsaw RUG Meetup & R-Ladies Warsaw",1]<-"R-Ladies Warsaw"

top20convocantes <-asistentes2018XCap %>%arrange(desc(totalasist)) %>% head(20)                 #top_n(-10)
View(top20convocantes)

#total de asistentes ordenado por año y cantidad de asistentes desde su creación al 25 de junio del 2019
asistentesXYearCantidad<-eventos_rladies %>% group_by(capitulo, anio=year(fecha_local))%>% summarise(totalasist=sum(respuesta_asistire))%>% arrange(desc(anio,totalasist))
View(asistentesXYearCantidad)


#grafico twitter
p20 <-ggplot(top20convocantes, aes(reorder(capitulo, totalasist), totalasist, fill=(capitulo))) +
  geom_col(alpha=0.8) +
  scale_fill_manual(values =  pal_2) +
  #geom_text(size = 3, hjust = 1.5,colour = "black")+
  coord_flip() + 
  theme_light() +
  theme(text = element_text(family = "Rockwell"), 
        legend.position = "NA",
        panel.grid = element_blank(),
        plot.subtitle = element_text(color = "black", hjust = 0.5),
        plot.caption = element_text(color = "black"),
        plot.title = element_text(size = 20, hjust = 0.5,colour = "#562457",face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +  #colour = "#88398A",face = "bold"
  labs(x = "",
       y = "Asistentes",
       title = "Capítulos con mayor convocatoria durante 2018",
       subtitle = "Dos son de latinoamérica",
       caption =
         "Data: #DatosDeMiercoles \n por Patricia Loto")
p20
# inserto logo
img <- readPNG("D:/Patri/RLADIES-RCIA-CTES/LOGOS_FLYER/rladies.PNG", FALSE)
marca <- rasterGrob(img, interpolate=F,height=unit(3, "cm"),hjust=-2.2, vjust=1.5)
pmarca <-p20 + annotation_custom(marca,xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
ggsave("blueberry6.png",width = 10, height = 5, dpi = "retina")



# en breve terminaré la animacion
library(gganimate)
pmarca+
  transition_time(totalasist) +
  ease_aes('linear')+
  shadow_mark(alpha = 1, size = 2)


# con paleta lacroix_palette
pal_2 <- lacroix_palette("Tangerine", n = 20, type = "continuous")

ggplot(top20convocantes, aes(reorder(totalasist), x =(capitulo), y = totalasist, fill=(capitulo))) +
  geom_col(alpha=0.8) +
  #scale_color_manual(values = pal_2)+
  scale_fill_manual(values =  pal_2) +
  #scale_fill_viridis_c(option = "B") +
  coord_flip() + 
  theme_light() +
  theme(text = element_text(family = "Rockwell"),
        legend.position = "NA",
        panel.grid = element_blank(),
        plot.subtitle = element_text(color = "black", hjust = 0.5),
        plot.caption = element_text(color = "black"),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
  ) +
  labs(x = "",
       y = "Cantidad asistentes",
       title = "Capítulos con mayor convocatoria durante 2018",
       subtitle = "Dos son de latinoamérica",
       caption =
         "Data: #DatosDeMiercoles \n por Patricia Loto")
ggsave("Tangerine.png",width = 10, height = 5, dpi = "retina")
# con paleta lacroix_palette
pal_2 <- lacroix_palette("Berry", n = 20, type = "continuous")
pal_2
#reorder(nombreProducto, importaXP), importaXP








p <-asistentesXYearCap%>%arrange(anio,totalasist)%>%
ggplot(aes(x = reorder(anio, totalasist), fill = capitulo))+
  geom_col(show.legend = TRUE) + 
  scale_size(range = c(2, 1500))+
  geom_text(
    aes(label = totalasist),
    #data = lealtades,
    color = "grey",
    angle=90,
    size=3,
    alpha = 0.80)+
  #scale_size_continuous(range = c(1, 8), breaks = c(250, 500, 750, 1000,1250,1500,1750,2000)) +
  scale_color_manual(values = pal_1) +
  scale_fill_manual(values = pal_1) +
 coord_flip() + 
  labs(x = "",
       y = "",
       title = "Asistentes por Año en Meeetups de RLadies",
       subtitle = "Período: 2012-2019",
       caption = "DatosDeMiercoles por Patricia Loto ")+
  #theme(legend.position = "bottom")+
  #coord_flip() +
  theme_light() +
  theme(legend.position = "bottom",text = element_text(family = " Vivaldi"),
        legend.position = "NA",
        panel.grid = element_blank(),
        plot.title = element_text(colour = "#562457", size = 22, hjust = 0.5, family = "Palatino Linotype",face="bold"),
        plot.subtitle = element_text(colour = "grey", size = 16, hjust = 0.5),
        plot.caption = element_text(colour =  wes_palette("Darjeeling2")[2], size = 8, hjust = 0.5, face="bold"),
       # plot.subtitle = element_text(color = "black", hjust = 0.5),
       # plot.caption = element_text(color = "black"),
        #plot.title = element_text(size = 18, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
  )
ggsave("assitentesAlAnio.png",width = 10, height = 5, dpi = "retina")
p
























