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
library(corrplot)

# library("devtools")
# install_github("kassambara/factoextra")
#devtools::install_github('fawda123/ggord')

script.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
dir <- paste0(dirname(script.dir),"/")
data.dir <- paste0(dir,"bases/")
resultados.dir <- paste0(dir,"resultados/Individual/PCA ingresos diversos/")
dir.create(resultados.dir)
# bases <- list.files(data.dir)[endsWith(list.files(data.dir),suffix = ".txt")]
# bases <- substr(bases,0,nchar(bases)-4)
# 
# for(base in bases){assign(base, read_delim(paste0(data.dir, paste(base),".txt"),delim = ";"))}
cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

usu_individual_t216 <-  read_delim(paste0(data.dir, "usu_individual_t216.txt"),delim = ";")

#### Primer enfoque ####

variables <- c('P21'  ,       #Ingreso ocupación ppal
               'TOT_P12',     # Ingreso otras ocupaciones
               'V2_M',        # Monto del ingreso por JUBILACION O PENSION
#               'V3_M',        # Monto del ingreso por INDEMNIZACION POR DESPIDO
#               'V4_M',        # Monto del ingreso por SEGURO DE DESEMPLEO
               'V5_M',        # Monto del ingreso por SUBSIDIO O AYUDA SOCIAL (EN DINERO) DEL GOBIERNO, IGLESIAS, ETC.
               'V8_M',        # Monto del ingreso por ALQUILER (VIVIENDA , TERRENO, OFICINA, ETC.) DE SU PROPIEDAD.
#               'V9_M',        # Monto del ingreso por GANANCIAS DE ALGUN NEGOCIO EN EL QUE NO TRABAJÓ.
#               'V10_M',       # Monto del ingreso por INTERESES O RENTAS POR PLAZOS FIJOS/INVERSIONES.
               'V11_M',       # Monto del ingreso por BECA DE ESTUDIO.
               'V12_M',       # Monto del ingreso por CUOTAS DE ALIMENTOS O AYUDA EN DINERO DE PERSONAS QUE NO VIVEN EN EL HOGAR.
#               'V18_M',       # Monto del ingreso por OTROS INGRESOS EN EFECTIVO (LIMOSNAS, JUEGOS DE AZAR, ETC.)
#               'V19_AM',      # Monto del ingreso por TRABAJO DE MENORES DE 10 AÑOS.
#               'V21_M',       # Monto del ingreso por aguinaldo.
               'ESTADO',      # Condición de actividad
               'CAT_OCUP',    # Categoría ocupacional
               'PP3E_TOT')    #Horas trabajadas
               

datos <- usu_individual_t216 %>% 
  select(., one_of(variables)) 

# Me quedo con los asalariados que respondieron sobre su ingreso
datos <- datos %>% 
   filter(PP3E_TOT   !=999
          )  
#### PCA ####
#Eliminamos los que tienen Antiguedad 0, y nos quedamos con las variables continuas
asalariados <- datos %>% 
  filter(CAT_OCUP ==3,
         P21 != -9,
         TOT_P12 != -9, V2_M !=-9, V5_M !=-9, V8_M !=-9, V11_M !=-9, V12_M !=-9 )  %>% 
  mutate(CAT_OCUP = factor(CAT_OCUP),
         ESTADO   = factor(ESTADO))

table(asalariados$CAT_OCUP)


#Tiene sentido hacer PCA??


M <- cor(asalariados %>% select( -c(ESTADO, CAT_OCUP)))

res1 <- cor.mtest(asalariados %>% select( -c(ESTADO, CAT_OCUP))  ,0.95)

corrplot(M,method = "square", p.mat = res1[[1]], sig.level=0.05, order = 'hclust',
         cl.pos="b", tl.pos="d", tl.srt = 60,tl.col = "black")

### La correlación entre las variables es muy baja, no tiene sentido hacer PCA


#### Segundo enfoque ####

variables <- c('PP08D4',       # tickets
               'PP08F1',       # Comisión 
               'PP08F2',       # Propinas
               'PP08J1',       # aguinaldo
               'PP08J2',       # otras bonificaciones no habituales
               'PP08J3',       # retroactivos
               'TOT_P12',     # Ingreso otras ocupaciones
               'PP04C',       # ¿Cuántas personas, incluido...trabajan allí en total? (ordinal)
               'PP07A',        #¿Cuánto tiempo hace que está trabajando en ese empleo en forma continua? (ordinal)
               'V2_M',        # Monto del ingreso por JUBILACION O PENSION
               'V5_M',        # Monto del ingreso por SUBSIDIO O AYUDA SOCIAL (EN DINERO) DEL GOBIERNO, IGLESIAS, ETC.
               'ESTADO',      # Condición de actividad
               'CAT_OCUP',    # Categoría ocupacional
               'PP3E_TOT')    #Horas trabajadas


datos <- usu_individual_t216 %>% 
  select(., one_of(variables)) 

# Me quedo con los asalariados que respondieron sobre su ingreso
asalariados <- datos %>% 
  filter(CAT_OCUP ==3,PP3E_TOT   !=999, ESTADO ==1,
         PP08D4 != -9, PP08F1 != -9, PP08F2 !=-9, PP08J1 !=-9, PP08J2 !=-9, PP08J3 !=-9,
         TOT_P12 !=-9, PP04C != 99, PP07A != 9, V2_M != -9, V5_M != -9)  %>% 
  mutate(CAT_OCUP = factor(CAT_OCUP),
         ESTADO   = factor(ESTADO))

#Tiene sentido hacer PCA??

M <- cor(asalariados %>% select( -c(ESTADO, CAT_OCUP)))

res1 <- cor.mtest(asalariados %>% select( -c(ESTADO, CAT_OCUP))  ,0.95)

corrplot(M,method = "square", p.mat = res1[[1]], sig.level=0.05, order = 'hclust',
         cl.pos="b", tl.pos="d", tl.srt = 60,tl.col = "black")
#En el enfoque 1 y 2 la alta dispersión de los datos hace que haya una muy baja correlación y no
#tiene sentido hacer PCA. Vamos a reducir a unas pocas variables con mayor presencia

##### Tercer enfoque #####

variables <- c('P21'  ,       #Ingreso ocupación ppal
               'TOT_P12',     # Ingreso otras ocupaciones
               'T_VI',        # MONTO TOTAL DE INGRESOS NO LABORALES
               'PP04C',       # ¿Cuántas personas, incluido...trabajan allí en total? (ordinal)
               'PP07A',        #¿Cuánto tiempo hace que está trabajando en ese empleo en forma continua? (ordinal)
               'ESTADO',      # Condición de actividad
               'CAT_OCUP',    # Categoría ocupacional
               'PP3E_TOT')    #Horas trabajadas


datos <- usu_individual_t216 %>% 
  select(., one_of(variables)) 

asalariados <- datos %>% 
  select(CAT_OCUP, I.O.Ppal = P21, I.O.noPpal = TOT_P12, I.No.Laboral = T_VI,
         Tam.estab = PP04C, Antiguedad = PP07A, Horas.trab = PP3E_TOT) %>% 
  filter(CAT_OCUP ==3,
         I.O.Ppal != -9,
         I.O.noPpal != -9, I.No.Laboral !=-9,Tam.estab != 99, Antiguedad != 9,Horas.trab !=999 )  %>% 
  mutate(CAT_OCUP = factor(CAT_OCUP))

#Tiene sentido hacer PCA??

M <- cor(asalariados %>% select( -c(CAT_OCUP)))

res1 <- cor.mtest(asalariados %>% select( -c(CAT_OCUP))  ,0.95)

corrplot(M,method = "square", p.mat = res1[[1]], sig.level=0.05, order = 'hclust',
         cl.pos="b", tl.pos="d", tl.srt = 60,tl.col = "black")

#### PCA #### 
pca <- princomp(asalariados %>% select( -c(CAT_OCUP)),cor=T,scores = T)
summary(pca)
plot(pca, type='l')

#Me quedo con los primeros 2,ya que su autovalor asociado>1
loads <- as.data.frame(pca$loadings[,1:2]) %>% 
  mutate(variable = row.names(.)) %>% 
  gather(.,componente,carga,1:2)

a <- ggplot(loads,aes(variable, carga, fill= variable, group= variable))+ 
  geom_col()+
  labs(title="Cargas",
       subtitle = "primeras dos componentes")+
  facet_grid(componente~., scales = "free")+
  theme_tufte()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=15, angle=15))
a
ggsave(paste0(resultados.dir,"cargas.PNG"))
saveRDS(a, paste0(resultados.dir,"cargas.RDS"))

## El componente 1  divide entre los ingresos no laborales y el resto, 
## El componente 2 divide entre las horas trabajadas y el resto. 


a <- ggbiplot(pca,
         choices = c(1,2),
         obs.scale = .05, 
         var.scale = 1,
         circle = TRUE, 
         alpha = 0.001,
         varname.size=5,
         varname.adjust =1.5)+
  xlim(-3,3)+ylim(-3,3)+
labs(title="PCA",
     subtitle = "componentes uno y dos")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))
a
ggsave(paste0(resultados.dir,"PC1,PC2.PNG"), scale = 2)
saveRDS(a, paste0(resultados.dir,"PC1,PC2.RDS"))

ggbiplot(pca,
         choices = c(1,3),
         obs.scale = .05, 
         var.scale = 1,
         circle = TRUE, 
         alpha = 0.001,
         varname.size=5,
         varname.adjust =1.5)+
  xlim(-3,3)+ylim(-3,3)+
  labs(title="PCA",subtitle = "componentes uno y dos")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))
#ggsave(paste0(resultados.dir,"PC1,PC3.PNG"))

ggbiplot(pca,
         choices = c(2,3),
         obs.scale = .05, 
         var.scale = 1,
         circle = TRUE, 
         alpha = 0.001,
         varname.size=5,
         varname.adjust =1.5)+
  xlim(-3,3)+ylim(-3,3)+
  labs(title="PCA",
       subtitle = "componentes uno y dos")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))
#ggsave(paste0(resultados.dir,"PC2,PC3.PNG"))

ggbiplot(pca,
         choices = c(1,4),
         obs.scale = .05, 
         var.scale = 1,
         circle = TRUE, 
         alpha = 0.001,
         varname.size=5,
         varname.adjust =1.5)+
  xlim(-3,3)+ylim(-3,3)+
  labs(title="PCA",
       subtitle = "componentes uno y dos")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))
#ggsave(paste0(resultados.dir,"PC1,PC4.PNG"))

ggbiplot(pca,
         choices = c(2,4),
         obs.scale = .05, 
         var.scale = 1,
         circle = TRUE, 
         alpha = 0.001,
         varname.size=5,
         varname.adjust =1.5)+
  xlim(-3,3)+ylim(-3,3)+
  labs(title="PCA",
       subtitle = "componentes uno y dos")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))
#ggsave(paste0(resultados.dir,"PC2,PC4.PNG"))




