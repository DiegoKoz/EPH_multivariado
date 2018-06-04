rm(list=ls())
##################### Preprocesamiento de datos #####################
#librerías
library(tidyverse)
library(readxl)
library(ca)
library(FactoMineR)
library(ggthemes)
#Directorios
script.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
dir <- paste0(dirname(script.dir),"/")
data.dir <- paste0(dir,"bases/")
resultados.dir <- paste0(dir,"resultados/Hogar/Vivienda/alternativa/")
dir.create(resultados.dir)

# Funciones Propias
colapsador <- function(base,anular){
  a <- base %>% 
    mutate(id = rownames(base)) %>% 
    mutate_if(is.factor, as.character) %>% 
    gather(.,Variable, Valor, c(1:(ncol(base))))  %>% 
    left_join(.,anular, by = c("Variable", "Valor"))
  
  a$Valor[a$anular=="anular"] <- 0
  a[['anular']] <- NULL
  a <- spread(data = a,key = Variable,Valor)
  a[['id']] <- NULL
  a <- a %>% 
    mutate_if(is.character, as.factor)
}

#Bases

# bases <- list.files(data.dir)[endsWith(list.files(data.dir),suffix = ".txt")]
# bases <- substr(bases,0,nchar(bases)-4)
# 
# for(base in bases){assign(base, read_delim(paste0(data.dir, paste(base),".txt"),delim = ";"))}
usu_hogar_t216 <-  read_delim(paste0(data.dir, "usu_hogar_t216.txt"),delim = ";")

#Selección de variables
### Características de la vivienda###
variables <- c("IV1", "IV2", "IV3", "IV4", "IV5", "IV6", "IV7","IV8",
               "IV9", "IV10",  "IV11", "IV12_3")

usu_hogar_t216_strat <- usu_hogar_t216 %>% 
  select(., one_of(variables)) %>% 
  mutate_all(funs(as.factor(.)))  

#Elimino no respuestas
usu_hogar_t216_strat <- usu_hogar_t216_strat[apply(usu_hogar_t216_strat[,1:ncol(usu_hogar_t216_strat)], 1, function(x) all(x !=9) ),]
usu_hogar_t216_strat <- usu_hogar_t216_strat[apply(usu_hogar_t216_strat[,1:ncol(usu_hogar_t216_strat)], 1, function(x) all(x !=0) ),]

#ajusto el MCA
mca.fit<- MCA(usu_hogar_t216_strat,graph = FALSE)
coordenadas_sinCol <- as.data.frame(mca.fit$var$coord[,1:2])
coordenadas_sinCol$variable <- row.names(coordenadas_sinCol)
a <- ggplot(coordenadas_sinCol, aes(x = variable,`Dim 1`, fill = variable))+
  geom_col()+
  labs(x="",y="",title="" )+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15),
        axis.text.x = element_text(size=7, angle = 45))
a
ggsave(paste0(resultados.dir,"coordenadas sin colapsar Dim 1.png"),scale=2)
saveRDS(a,paste0(resultados.dir,"coordenadas sin colapsar Dim 1.RDS"))

#Colapso las variables menos relevantes

# Elimino las variable IV12_1 y IV12_2 a mano
# Elijo matar todo lo que tiene un valor absoluto menor a 3 cuartos
# ese límite debe variar en función del gráfico de coordenadas

anular <- coordenadas_sinCol %>%
  mutate(nombre = row.names(coordenadas_sinCol)) %>%
  arrange(abs(`Dim 1`)) %>%
  filter(abs(`Dim 1`)<=summary(abs(coordenadas_sinCol$`Dim 1`))["3rd Qu."]) %>%
  arrange(nombre) %>%  
  filter(nombre != "IV12_3_2") %>% 
  separate(nombre, into = c('Variable', 'Valor'), sep = '_') %>% 
  select(Variable, Valor) %>% 
  mutate(anular='anular')
  
base_reduc <- colapsador(base = usu_hogar_t216_strat, anular = anular)

#####base reducida ######

mca.fit<- MCA(base_reduc,graph = FALSE)

head(mca.fit$eig,10)

coordenadas <- as.data.frame(mca.fit$var$coord[,1:2]) %>% 
  mutate(varname = row.names(.)) %>% 
  gather(., dimension, carga,1:2)


a <- ggplot(coordenadas %>% filter(abs(carga)>0.25), aes(x = varname,carga, fill = varname ))+
  geom_col()+
  labs(x=paste0("Categoría"),
       y="",
       title="Cargas" )+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15),
        axis.text.x = element_text(size=7, angle = 45))+
  facet_grid(dimension~., scales = "free")

a

ggsave(paste0(resultados.dir,"coordenadas_colapsadas.png"),scale=2)
saveRDS(a,paste0(resultados.dir,"coordenadas_colapsadas.RDS"))

a <- ggplot(coordenadas %>% filter(abs(carga)>0.25, dimension == "Dim 1"),
            aes(x = varname,carga, fill = varname ))+
  geom_col()+
  labs(x="",
       y="",
       title="" )+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15),
        axis.text.x = element_text(size=7, angle = 45))

a

ggsave(paste0(resultados.dir,"coordenadas_colapsadas_comparacion.png"),scale=2)
saveRDS(a,paste0(resultados.dir,"coordenadas_colapsadas_comparacion.RDS"))




# data frame with variable coordinates
mca1_vars_df = data.frame(mca.fit$var$coord)
mca1_vars_df$Variable <- row.names(mca1_vars_df)

# data frame with observation coordinates
mca1_obs_df = data.frame(mca.fit$ind$coord)

#Clusters de variables
mca_cluster <- kmeans(mca1_obs_df[,1:2], centers = 3,iter.max = 50, nstart = 10)
mca1_obs_df$cluster <- as.factor(mca_cluster$cluster)

# plot of variable categories
a <- ggplot(data=mca1_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  labs(x=paste0("Dim 1- Inercia: ", round(mca.fit$eig$`percentage of variance`[1],2),"%"),
       y=paste0("Dim 2- Inercia: ", round(mca.fit$eig$`percentage of variance`[2],2),"%"),
       title="MCA características de la vivienda",
       subtitle = "Variables")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))
a
ggsave(paste0(resultados.dir,"vivienda_2_vars_D1D2.png"),scale=2)
saveRDS(a,paste0(resultados.dir,"vivienda_2_vars_D1D2.RDS"))

a <- ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(aes(color = cluster), alpha = 0.5) +
  scale_colour_discrete(name = "Variable")+
  labs(x=paste0("Dim 1- Inercia: ", round(mca.fit$eig$`percentage of variance`[1],2),"%"),
       y=paste0("Dim 2- Inercia: ", round(mca.fit$eig$`percentage of variance`[2],2),"%"),
       title="MCA características de la vivienda",
       subtitle = "Observaciones")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))
a
ggsave(paste0(resultados.dir,"Vivienda_2_obs_D1D2.png"),scale=2)
saveRDS(a,paste0(resultados.dir,"Vivienda_2_obs_D1D2.RDS"))

### Dim 1 contra Dim 3

# plot of variable categories
a <- ggplot(data=mca1_vars_df, 
       aes(x = Dim.1, y = Dim.3, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  labs(x=paste0("Dim 1- Inercia: ", round(mca.fit$eig$`percentage of variance`[1],2),"%"),
       y=paste0("Dim 3- Inercia: ", round(mca.fit$eig$`percentage of variance`[3],2),"%"),
       title="MCA características de la vivienda",
       subtitle = "Variables")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))
a
ggsave(paste0(resultados.dir,"vivienda_2_vars_D1D3.png"),scale=2)
saveRDS(a, paste0(resultados.dir,"vivienda_2_vars_D1D3.RDS"))

# MCA plot of observations and categories
a <- ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.3)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(aes(colour = cluster), alpha = 0.5) +
  scale_colour_discrete(name = "Variable")+
  labs(x=paste0("Dim 1- Inercia: ", round(mca.fit$eig$`percentage of variance`[1],2),"%"),
       y=paste0("Dim 3- Inercia: ", round(mca.fit$eig$`percentage of variance`[3],2),"%"),
       title="MCA características de la vivienda",
       subtitle = "Observaciones")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))
a
ggsave(paste0(resultados.dir,"Vivienda_2_obs_D1D3.png"),scale=2)
saveRDS(a, paste0(resultados.dir,"Vivienda_2_obs_D1D3.RDS"))

a <- ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) + 
  geom_hline(yintercept = 0,colour = "gray70") + 
  geom_vline(xintercept = 0, colour = "gray70") + 
  geom_point(aes(colour =cluster),alpha = 0.5) + 
  geom_text(data = mca1_vars_df %>% filter(abs(Dim.1)>0.5 |abs(Dim.2)>0.5), 
            aes(x = Dim.1, y = Dim.2, label = Variable)) + 
  labs(x=paste0("Dim 1- Inercia: ", round(mca.fit$eig$`percentage of variance`[1],2),"%"),
       y=paste0("Dim 2- Inercia: ", round(mca.fit$eig$`percentage of variance`[2],2),"%"),
       title="MCA características de la vivienda",
       subtitle = "Observaciones y variables")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))
a
ggsave(paste0(resultados.dir,"vivienda_2_var_obs.png"),scale=2)
saveRDS(a,paste0(resultados.dir,"vivienda_2_var_obs.RDS"))



