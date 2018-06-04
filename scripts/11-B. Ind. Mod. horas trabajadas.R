# PCA variables continuas:
#Variables de ingreso laboral y no laboral
rm(list=ls())
##################### Preprocesamiento de datos #####################
# hay conflicto entre ggbiplot, que invoca a plyr y dplyr
library(tidyverse)
library(readxl)
library(ca)
library(FactoMineR)
library(ggord)
library(ggbiplot)
library(dplyr)
library(ggthemes)
library(pROC)
library(popbio)

script.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
dir <- paste0(dirname(script.dir),"/")
data.dir <- paste0(dir,"bases/")
resultados.dir <- paste0(dir,"resultados/Individual/logit_subocupacion/")
dir.create(resultados.dir)
usu_individual_t216 <-  read_delim(paste0(data.dir, "usu_individual_t216.txt"),delim = ";")

variables <- c('CH04',        # Sexo
               'PP3E_TOT',    # Horas trabajadas
               'PP03G')       # La semana pasada, ¿quería trabajar más horas? 1-Si 2-No

datos <- usu_individual_t216 %>% 
  select(., one_of(variables))

# Me quedo con los asalariados que respondieron sobre su ingreso
datos1 <- datos %>% 
   filter(!is.na(PP3E_TOT),
          !is.na(PP03G),
          !PP3E_TOT %in% c(0,999),
          PP03G    !=9) %>%
  mutate(PP03G= case_when(PP03G==1 ~ 0,
                          PP03G==2 ~ 1),
         Sexo = case_when(CH04==1 ~'Varon',
                          CH04==2 ~'Mujer')) %>% 
  select(-CH04)

subempleo_y_genero <- ggplot(datos1, aes(x=PP3E_TOT, y=PP03G, color= Sexo, group=Sexo))+
  geom_jitter(alpha=0.03,width = 0,height = 0.01)+ 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=T)+
  geom_vline(xintercept = 35, linetype= 'dashed')+
  labs(x = "Total de Horas trabajadas en la semana en la ocupación principal",
       y ="¿Quería trabajar más horas?",
       title= "Probabilidad de querer trabajar más horas según género")+
  theme_tufte()+
  theme(legend.position = "bottom",
        text = element_text(size=15))+
  scale_color_gdocs()+
  scale_y_continuous(breaks = c(0,.5,1),labels = c("Si","50%","No"))+
  scale_x_continuous(limits = c(0,100))
subempleo_y_genero

ggsave(paste0(resultados.dir,"subempleo y genero.PNG"))
saveRDS(subempleo_y_genero, paste0(resultados.dir,"subempleo y genero.RDS"))
  