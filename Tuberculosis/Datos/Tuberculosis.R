library(tidyverse)
library(ggridges)
library(inspectdf)
library(viridisLite)
library(viridis)
library(extrafont)
library(ggthemes)

loadfonts(device = "win")

tuberculosis_oms <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-22/tuberculosis_oms.csv")


#### Exploramos los datos
str(tuberculosis_oms)
inspect_na(tuberculosis_oms) %>% show_plot() ###wow que nivel de NAs


##DATA WRANGLING
tuber<- tuberculosis_oms %>% 
  gather(nuevos_fpp_h014:nuevosrecaida_m65,key= "key", value= "casos", na.rm = T) %>% 
  mutate(key= str_replace(key, "nuevosrecaida", "nuevos_recaida")) %>% 
  separate(key,c("nuevo", "tipo", "sexoedad"), sep = "_") %>% 
  separate(sexoedad, c("genero", "edad"), sep=1)


#FILTRADO

tuber_ca <- tuber %>% 
  filter(pais %in% c("Nicaragua", "Costa Rica", "Guatemala", "Honduras", "Panamá", "El Salvador",
                     "Belice"),
         tipo== "recaida")

#Visualización

p1 <- ggplot(aes(y=reorder(pais, desc(pais)),x=casos,fill=pais, group=pais) ,data= tuber_ca)+
  geom_density_ridges2(scale=1.5, show.legend = F)+
  stat_density_ridges(quantile_lines = T, show.legend = F, quantiles = 2)+
  theme_wsj()+
  labs(title = "Distribucción de # Casos de Recaidas de Tubercolosis en Centro America",
       subtitle = "Para R4DS_es",
       caption = "Source: Organización Mundial de la Salud datos del año 2013/
       Visualization by M.Ortiz (twitter: @miangeltiz20)")+
  theme(plot.title = element_text(family = "Rockwell",size = 22),
        plot.subtitle = element_text(family = "Rockwell", size=18),
        plot.caption = element_text(family= "Rockwell", size=18))+
  scale_x_continuous(breaks = seq(0,500,100))


## Note errores en la clasificacion de las edades, vamos a solucionarlo.
tuber_ca <-  tuber_ca %>% 
  mutate(edad= as.factor(edad),
         edad= case_when(
           tuber_ca$edad=="3524"~"3544",
           TRUE~tuber_ca$edad
         ),
         genero= case_when(
           tuber_ca$genero=="h"~"Hombre",
           tuber_ca$genero=="m"~"Mujer",
           TRUE~tuber_ca$genero
         ))

p2 <- ggplot(aes(x= edad, y= casos, fill=pais), data= tuber_ca)+
  geom_col(position = "dodge", color="black")+
  theme_minimal()+
  scale_x_discrete(limits=c("014","1524","2534","3544","4554","5564", "65"),
                   labels=c("De 0 a 14", "De 15 a 24","De 25 a 34", "De 35 a 44",
                            "De 45 a 54","De 55 a 64","De 65 a mas"))+
  labs(x="",y="", title = "Casos de Recaida de Tuberculosis por Rango de Edad en Centro America",
       subtitle = "Para R4DS_es", caption = "Source: Organización Mundial de la Salud/Datos del año 2013/
       Visualization by Miguel Ortiz (twitter: @miangeltiz20)")+
  theme(plot.title = element_text(family = "Rockwell", size= 22),
        plot.subtitle = element_text(family = "Rockwell", size=16),
        plot.caption = element_text(family = "Rockwell", size = 16),
        legend.title = element_text(family = "Rockwell"),
        strip.text = element_text(family = "Rockwell", size = 12))+
  facet_wrap(~genero)

p1
p2



#Un placer participar para R4DS_es 


