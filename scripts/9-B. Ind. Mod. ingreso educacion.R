# PCA variables continuas:
#Edad
#Años de escolaridad
#Ingreso
#Horas trabajadas
#Antiguedad en el puesto
#Subconjunto: 
#Mayores de 18, 
#ocupados (asalariados)
#Exceptuando educacion especial (CH12< 9) 
rm(list=ls())
##################### Preprocesamiento de datos #####################
library(tidyverse)
library(readxl)
library(ca)
library(FactoMineR)
library(ggord)
library(ggbiplot)
library(ggthemes)
library(dplyr)
# library("devtools")
# install_github("kassambara/factoextra")
#devtools::install_github('fawda123/ggord')

script.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
dir <- paste0(dirname(script.dir),"/")
data.dir <- paste0(dir,"bases/")
resultados.dir <- paste0(dir,"resultados/Individual/PCA ingreso educ/")
dir.create(resultados.dir)

usu_individual_t216 <-  read_delim(paste0(data.dir, "usu_individual_t216.txt"),delim = ";")

variables <- c('CODUSU',      #Id
               'ANO4',        #Id
               'TRIMESTRE',   #Id
               'COMPONENTE',  #Id
               'CH04',        #SEXO
               'CH06' ,       #Edad
               'CH10' ,       #Asistencia educativa
               'CH12' ,       #Nivel educ más alto
               'CH13' ,       #Finalizacion
               'CH14' ,       #ultimo año aprobado
               'P21'  ,       #Ingreso ocupación ppal
               'TOT_P12',     # Ingreso otras ocupaciones
               'ESTADO',      # Condición de actividad
               'CAT_OCUP',    # Categoría ocupacional
               'PP3E_TOT')    #Horas trabajadas

datos <- usu_individual_t216 %>% 
  select(., one_of(variables)) 

table(datos$CH04)

####### Creacion variable de años de escolaridad ##########
datos <- datos %>%
  filter(CH06>=18,CH12< 9 ) %>% 
  mutate(CH14 = as.numeric(paste(CH14)))
datos_completo <- datos %>%  
  filter(CH13== 1)
datos_incompleto <- datos %>% 
  filter(CH13 %in% c(0,2))

datos_incompleto$CH14[datos_incompleto$CH14 == 99 |
                        datos_incompleto$CH14 == 98 |
                        is.na(datos_incompleto$CH14) ] <- 0
# A los de nivel completo, les creo la variable años de escolaridad, con su valor
# En orden: nunca asistio, jardin, primario, EGB, secundario, polimodal, terciario,
# universitario, posgrado
datos_completo$anios_de_escolaridad <- NA
datos_completo$anios_de_escolaridad[datos_completo$CH10== 3] <- 0  
datos_completo$anios_de_escolaridad[datos_completo$CH12== 1] <- 0  
datos_completo$anios_de_escolaridad[datos_completo$CH12== 2] <- 7
datos_completo$anios_de_escolaridad[datos_completo$CH12== 3] <- 9  
datos_completo$anios_de_escolaridad[datos_completo$CH12== 4] <- 12 
datos_completo$anios_de_escolaridad[datos_completo$CH12== 5] <- 12 
datos_completo$anios_de_escolaridad[datos_completo$CH12== 6] <- 15 
datos_completo$anios_de_escolaridad[datos_completo$CH12== 7] <- 17 
datos_completo$anios_de_escolaridad[datos_completo$CH12== 8] <- 20

# A los de nivel incompleto, les asigno el valor del nivel anterior completado,
# y se les suma la cantidad de años aprobados del incompleto.
# En orden: nunca asistio, jardin, primaria, EGB, secundario, polimodal, terciario,
# universitario, posgrado
# En el valor a sumar se vuelve a especificar CH12 para que coincidan las dimensiones,
# asi el programa reconoce que recorte realizar
datos_incompleto$anios_de_escolaridad <- NA
datos_incompleto$anios_de_escolaridad[datos_incompleto$CH10== 3] <- 0  
datos_incompleto$anios_de_escolaridad[datos_incompleto$CH12== 1] <- 0 
datos_incompleto$anios_de_escolaridad[datos_incompleto$CH12== 2] <- 0 + datos_incompleto$CH14[datos_incompleto$CH12== 2]
datos_incompleto$anios_de_escolaridad[datos_incompleto$CH12== 3] <- 0 + datos_incompleto$CH14[datos_incompleto$CH12== 3]
datos_incompleto$anios_de_escolaridad[datos_incompleto$CH12== 4] <- 7 + datos_incompleto$CH14[datos_incompleto$CH12== 4]
datos_incompleto$anios_de_escolaridad[datos_incompleto$CH12== 5] <- 9 + datos_incompleto$CH14[datos_incompleto$CH12== 5]
datos_incompleto$anios_de_escolaridad[datos_incompleto$CH12== 6] <- 12 + datos_incompleto$CH14[datos_incompleto$CH12== 6]
datos_incompleto$anios_de_escolaridad[datos_incompleto$CH12== 7] <- 12 + datos_incompleto$CH14[datos_incompleto$CH12== 7]
datos_incompleto$anios_de_escolaridad[datos_incompleto$CH12== 8] <- 17 + datos_incompleto$CH14[datos_incompleto$CH12== 8]

# Vuelvo a atar las filas de los completos y los incompletos, la llamo adultos
# Atencion: adultos, ademas de no tener menores, tampoco tiene gente de ed. especial

adultos <- bind_rows(datos_completo,datos_incompleto)

adultos$CH10 <- adultos$CH12 <- adultos$CH13 <- adultos$CH14 <- NULL

#### Creación variable antiguedad ####
## no creo la variable antiguedad porque las variables Año, Mes día estan sólo para trabajadores
## independientes y servicio doméstico

# Me quedo con los asalariados que respondieron sobre su ingreso
 adultos <- adultos %>% 
   filter(PP3E_TOT   !=999,
          ESTADO      == 1,
          P21         !=-9)  
#   mutate(Antiguedad = PP04B3_ANO*365 +
#                       PP04B3_MES*30  +
#                       PP04B3_DIA)

#PP04B3_ANO es servicio doméstico
#### PCA ####
#Eliminamos los que tienen antiguedad 0, y nos quedamos con las variables continuas
asalariados <- adultos %>% 
  select(-CODUSU,-ANO4,-TRIMESTRE,-COMPONENTE,-ESTADO, -CAT_OCUP, 
         sexo= CH04, edad = CH06, ingreso.ppal =P21, ingreso.otros = TOT_P12,
         hrs.trab = PP3E_TOT, educ =anios_de_escolaridad) %>%
  mutate(sexo     = factor(sexo))

table(asalariados$sexo)


pca <- princomp(asalariados %>% select( -c(sexo)),cor=T,scores = T)
summary(pca)
#Me quedo con los primeros tres, que explican más del 20% cada uno
loads <- as.data.frame(pca$loadings[,1:3]) %>% 
  mutate(variable = row.names(.)) %>% 
  gather(.,componente,carga,1:3)

#grafico los loadings
a <- ggplot(loads,aes(variable, carga, fill= variable, group= variable))+ 
  geom_col()+
  labs(title="Cargas",
       subtitle = "primeras tres componentes")+
  facet_grid(componente~., scales = "free_x")+
  theme_tufte()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=15, angle=15))
a
ggsave(paste0(resultados.dir,"cargas.PNG"))
saveRDS(a, paste0(resultados.dir,"cargas.RDS"))

#biplot
a <- ggbiplot(pca,
         groups = asalariados$sexo,
         choices = c(1,2),
         obs.scale = .05, 
         var.scale = 1,
         circle = TRUE, 
         alpha = 0.05,
         varname.size=5,
         varname.adjust =1.5)+
  labs(title="PCA",
       subtitle = "componentes uno y dos")+
  xlim(-4,4)+ylim(-4,4)+
  theme_tufte()+
  theme(legend.position = "bottom",
        text = element_text(size=15))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  scale_color_gdocs(labels=c("Hombre", "Mujer"), name = 'Sexo')
a
#rojo 1, turquesa 2
ggsave(paste0(resultados.dir,"PC1,PC2 según sexo.PNG"), scale=2)
saveRDS(a, paste0(resultados.dir,"PC1,PC2 según sexo.RDS"))

a <- ggbiplot(pca,
              choices = c(1,2),
              obs.scale = .05, 
              var.scale = 1,
              circle = TRUE, 
              alpha = 0.01,
              varname.size=5,
              varname.adjust =1.5)+
  labs(title="PCA",
       subtitle = "componentes uno y dos")+
  xlim(-4,4)+ylim(-4,4)+
  theme_tufte()+
  theme(legend.position = "bottom",
        text = element_text(size=15))+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  scale_color_gdocs(labels=c("Hombre", "Mujer"), name = 'Sexo')
a
ggsave(paste0(resultados.dir,"PC1,PC2.PNG"), scale=2)
saveRDS(a, paste0(resultados.dir,"PC1,PC2.RDS"))

a <- ggbiplot(pca,
              choices = c(1,3),
              obs.scale = .05, 
              var.scale = 1,
              circle = TRUE, 
              alpha = 0.01,
              varname.size=5,
              varname.adjust =1.5)+
  labs(title="PCA",
       subtitle = "componentes uno y tres")+
  xlim(-4,4)+ylim(-4,4)+
  theme_tufte()+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))+
  scale_color_gdocs(labels=c("Hombre", "Mujer"), name = 'Sexo')

a
ggsave(paste0(resultados.dir,"PC1,PC3.PNG"),scale=2)
saveRDS(a, paste0(resultados.dir,"PC1,PC3.RDS"))


a <- ggbiplot(pca,
              choices = c(2,3),
              obs.scale = .05, 
              var.scale = 1,
              circle = TRUE, 
              alpha = 0.01,
              varname.size=5,
              varname.adjust =1.5)+
  labs(title="PCA",
       subtitle = "componentes dos y tres")+
  xlim(-4,4)+ylim(-4,4)+
  theme_tufte()

a
ggsave(paste0(resultados.dir,"PC2,PC3.PNG"),scale=2)
saveRDS(a, paste0(resultados.dir,"PC2,PC3.RDS"))



