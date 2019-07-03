extrafont::loadfonts(device = "win")
library(tidyverse)
library(paletteer)
library("LaCroixColoR")
library(lubridate)
library(LaCroixColoR)
library(png)
library(grid)


eventos_rladies <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-26/eventos_rladies.csv")

datatable(eventos_rladies, rownames = FALSE,
          options = list(pageLength = 10))

#total de asistentes desde su creación al 25 de junio del 2019
asistentesXCap <-eventos_rladies %>% group_by(capitulo)%>% summarise(totalasist=sum(respuesta_asistire))%>% arrange(desc(totalasist))
View(asistentesXCap)

#total de asistentes ordenado por año y cantidad de asistentes desde su creación al 25 de junio del 2019
asistentesXYearCantidad<-eventos_rladies %>% group_by(capitulo, anio=year(fecha_local))%>% summarise(totalasist=sum(respuesta_asistire))%>% arrange(desc(anio,totalasist))
View(asistentesXYearCantidad)

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

#grafico twitter
# con paleta lacroix_palette - Berry
pal_1 <- lacroix_palette("Berry", n = 20, type = "continuous")
pal_1

p20 <-ggplot(top20convocantes, aes(reorder(capitulo, totalasist), totalasist, fill=(capitulo))) +
  geom_col(alpha=0.8) +
  scale_fill_manual(values =  pal_1) +
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


# con paleta lacroix_palette - Tangerine
pal_2 <- lacroix_palette("Tangerine", n = 20, type = "continuous")

ggplot(top20convocantes, aes(reorder(capitulo, totalasist), totalasist, fill=(capitulo))) +
  geom_col(alpha=0.8) +
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



























