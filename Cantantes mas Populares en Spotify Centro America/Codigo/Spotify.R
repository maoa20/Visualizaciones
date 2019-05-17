#Miercoles de datos 15/05/2019 API SPOTIFY

library(Rspotify)
library(tidyverse)
library(ggthemes)
library(extrafont)
library(viridis)
library(viridisLite)
library(patchwork)

loadfonts(device = "win")



## La importación del data set es obtenido de las instrucciones del github @R4DS_es para acceder al API
## de Spotify



##Grafico de los 10 artistas con mas canciones en el Top 50 de Nicaragua

centroamericanos <- dataset_spotify %>% 
  filter(top_pais %in% c("El Top 50 de Nicaragua", "El Top 50 de Costa Rica", 
                         "El Top 50 de Honduras", "El Top 50 de El Salvador",
                         "El Top 50 de Panamá", "El Top 50 de Guatemala")) 


p4 <- ggplot(aes(x=as.factor(modo), y=bailabilidad, fill=as.factor(top_pais)),
             data = centroamericanos)+
  geom_boxplot()+
  theme_excel()+
  labs(y="", title = "Grado de Bailabilidad por Modo Musical ")+
  scale_x_discrete(limits=c("0","1"),
                   labels=c("Mayor", "Menor"))+
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(family = "Rockwell"))

###Grafico compartido de TOP 5 artistas centroamericanos con su grado de bailabilidad

centroamericanos2 <- centroamericanos %>% 
  group_by(artista, top_pais) %>% 
  summarise(count=n()) %>% 
  group_by(top_pais) %>% 
  mutate(rank=min_rank(desc(count))) %>% 
  filter(rank %in% c(1:5),
         artista %in% c("Bad Bunny", "Anuel Aa", "Ozuna", "Becky G", "Piso 21")) %>% 
  arrange(rank) %>% 
  rename(conteo=count) 

p3 <- ggplot(aes(x=reorder(artista,conteo), y=conteo, fill=reorder(top_pais, conteo)), 
             data = centroamericanos2)+
  theme_economist()+
  geom_col(position = "dodge", color="black", size=1)+
  labs(title = "Top 5 Artistas con Mayor Número de Canciones 
en el Top 50 de Canciones mas Populares de Spotify en Centro America",
       subtitle = "Para R4DS_es",
       caption = "Source: API de Spotify/ Visualization by M.Ortiz @miangeltiz20")+
  scale_fill_viridis(option = "inferno", direction = -1, discrete = T)+
  theme(legend.title = element_blank(),
        legend.position = "right",
        axis.title = element_blank(),
        plot.title = element_text(family = "Rockwell"),
        plot.subtitle = element_text(family = "Rockwell", size=12),
        plot.caption = element_text(family = "Rockwell", size=15))
coord_flip()

##Unión de graficos

p3+p4+plot_layout(ncol = 1)


#Un placer participar en Miercoles de datos para R4DS_es
