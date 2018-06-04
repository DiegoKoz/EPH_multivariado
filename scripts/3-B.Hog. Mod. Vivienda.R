rm(list=ls())
##################### Preprocesamiento de datos #####################
library(tidyverse)
library(readxl)
library(ca)
library(FactoMineR)
library(ggthemes)
script.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
dir <- paste0(dirname(script.dir),"/")

data.dir <- paste0(dir,"bases/")
resultados.dir <- paste0(dir,"resultados/Hogar/Vivienda/")
dir.create(resultados.dir)

# bases <- list.files(data.dir)[endsWith(list.files(data.dir),suffix = ".txt")]
# bases <- substr(bases,0,nchar(bases)-4)
# 
# for(base in bases){assign(base, read_delim(paste0(data.dir, paste(base),".txt"),delim = ";"))}
usu_hogar_t216 <-  read_delim(paste0(data.dir, "usu_hogar_t216.txt"),delim = ";")

### Características de la vivienda###
variables <- c("IV1", "IV2", "IV3", "IV4", "IV5", "IV6", "IV7","IV8",
               "IV9", "IV10",  "IV11", "IV12_1",  "IV12_2", "IV12_3")

usu_hogar_t216_strat <- usu_hogar_t216 %>% 
  select(., one_of(variables)) %>% 
  mutate_all(funs(as.factor(.)))  

usu_hogar_t216_strat <- usu_hogar_t216_strat[apply(usu_hogar_t216_strat[,1:14], 1, function(x) all(x !=9) ),]
usu_hogar_t216_strat <- usu_hogar_t216_strat[apply(usu_hogar_t216_strat[,1:14], 1, function(x) all(x !=0) ),]

mca.fit<- MCA(usu_hogar_t216_strat,graph = FALSE)

head(mca.fit$eig,10)

coordenadas <- as.data.frame(mca.fit$var$coord[,1:2])

a <- ggplot(coordenadas, aes(x = row.names(coordenadas),`Dim 1`, fill = row.names(coordenadas)))+
  geom_col()+
  labs(x=paste0("Categoría"),
                  y="",
                  title="Cargas primera dimension")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15),
        axis.text.x = element_text(size=7, angle = 45))
a
ggsave(paste0(resultados.dir,"Coordenadas Dim 1- Sin ponderar.png"),scale=2)
saveRDS(a,paste0(resultados.dir,"Coordenadas Dim 1- Sin ponderar.RDS"))


a <- ggplot(coordenadas, aes(x = row.names(coordenadas),`Dim 2`, fill = row.names(coordenadas)))+
  geom_col()+
  labs(x=paste0("Categoría"),
       y="",
       title="Cargas segunda dimension")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15),
        axis.text.x = element_text(size=7, angle = 45))

a
ggsave(paste0(resultados.dir,"Coordenadas Dim 2- Sin ponderar.png"),scale=2)
saveRDS(a,paste0(resultados.dir,"Coordenadas Dim 2- Sin ponderar.RDS"))

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
       aes(x = Dim.1, y = Dim.2, label = Variable)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  labs(x=paste0("Dim 1- Inercia: ", round(mca.fit$eig$`percentage of variance`[1],2),"%"),
       y=paste0("Dim 2- Inercia: ", round(mca.fit$eig$`percentage of variance`[2],2),"%"),
       title="Sin ponderar")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))
a
ggsave(paste0(resultados.dir,"características de la vivienda, sin ponderar_vars_D1D2.png"),scale=2)
saveRDS(a,paste0(resultados.dir,"características de la vivienda, sin ponderar_vars_D1D2.RDS"))


### Dimensión 1 y 3
# plot of variable categories
a <- ggplot(data=mca1_vars_df, 
       aes(x = Dim.1, y = Dim.3, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  labs(x=paste0("Dim 1- Inercia: ", round(mca.fit$eig$`percentage of variance`[1],2),"%"),
       y=paste0("Dim 3- Inercia: ", round(mca.fit$eig$`percentage of variance`[3],2),"%"),
       title="MCA características de la vivienda")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))
a
ggsave(paste0(resultados.dir,"características de la vivienda, sin ponderar_vars_D1D3.png"),scale=2)
saveRDS(a,paste0(resultados.dir,"características de la vivienda, sin ponderar_vars_D1D3.RDS"))

# MCA plot of observations and categories
a <- ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.3)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(aes(colour = cluster), alpha = 0.5) +
  scale_colour_discrete(name = "Variable")+
  labs(x=paste0("Dim 1- Inercia: ", round(mca.fit$eig$`percentage of variance`[1],2),"%"),
       y=paste0("Dim 3- Inercia: ", round(mca.fit$eig$`percentage of variance`[3],2),"%"),
       title="MCA características de la vivienda")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))

a
ggsave(paste0(resultados.dir,"características de la vivienda, sin ponderar_obs_D1D3.png"),scale=2)
saveRDS(a,paste0(resultados.dir,"características de la vivienda, sin ponderar_obs_D1D3.RDS"))



#### ponderando por PONDERA ####

variables <- c("IV1", "IV2", "IV3", "IV4", "IV5", "IV6", "IV7","IV8",
               "IV9", "IV10",  "IV11", "IV12_1",  "IV12_2", "IV12_3", "PONDERA")        

usu_hogar_t216_strat_pond <- usu_hogar_t216 %>% 
  select(., one_of(variables))%>% 
  mutate_all(funs(as.factor(.))) %>% 
  mutate(PONDERA = as.numeric(PONDERA))

## Análisis de correspondencia ##
usu_hogar_t216_strat_pond <- usu_hogar_t216_strat_pond[apply(usu_hogar_t216_strat_pond[,1:15], 1, function(x) all(x !=9) ),]
usu_hogar_t216_strat_pond <- usu_hogar_t216_strat_pond[apply(usu_hogar_t216_strat_pond[,1:15], 1, function(x) all(x !=0) ),]

mca.fit<- MCA(usu_hogar_t216_strat_pond[1:14], row.w = usu_hogar_t216_strat_pond$PONDERA,
              graph = FALSE)

head(mca.fit$eig,10)


coordenadas <- as.data.frame(mca.fit$var$coord[,1:2])

a <- ggplot(coordenadas, aes(x = row.names(coordenadas),`Dim 1`, fill = row.names(coordenadas)))+
  geom_col()+
  labs(x=paste0("Categoría"),
       y="",
       title="Cargas primera dimension",
       subtitle = "ponderado")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15),
        axis.text= element_text(size=7, angle = 45))
a
ggsave(paste0(resultados.dir,"Coordenadas Dim 1- Ponderado.png"),scale=2)
saveRDS(a,paste0(resultados.dir,"Coordenadas Dim 1- Ponderado.RDS"))


a <- ggplot(coordenadas, aes(x = row.names(coordenadas),`Dim 2`, fill = row.names(coordenadas)))+
  geom_col()+
  labs(x=paste0("Categoría"),
       y="",
       title="Cargas segunda dimension",
       subtitle = "ponderado")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15),
        axis.text= element_text(size=7, angle = 45))
a
ggsave(paste0(resultados.dir,"Coordenadas Dim 2- Ponderado.png"),scale=2)
saveRDS(a,paste0(resultados.dir,"Coordenadas Dim 2- Ponderado.RDS"))


#mca.fit.df <- data.frame(mca.fit$cs)
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
       aes(x = Dim.1, y = Dim.2, label = Variable)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  labs(x=paste0("Dim 1- Inercia: ", round(mca.fit$eig$`percentage of variance`[1],2),"%"),
       y=paste0("Dim 2- Inercia: ", round(mca.fit$eig$`percentage of variance`[2],2),"%"),
       title="Ponderado")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))
a
ggsave(paste0(resultados.dir,"características de la vivienda, sin ponderado_vars.png"),scale=2)
saveRDS(a,paste0(resultados.dir,"características de la vivienda, sin ponderado_vars.RDS"))

# MCA plot of observations and categories
a <- ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(aes(colour = cluster), alpha = 0.5) +
  scale_colour_discrete(name = "Variable")+
  labs(x=paste0("Dim 1- Inercia: ", round(mca.fit$eig$`percentage of variance`[1],2),"%"),
       y=paste0("Dim 2- Inercia: ", round(mca.fit$eig$`percentage of variance`[2],2),"%"),
       title="MCA características de la vivienda",
       subtitle = "Observaciones, ponderado")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))

a

ggsave(paste0(resultados.dir,"características de la vivienda, ponderado_obs.png"),scale=2)
saveRDS(a,paste0(resultados.dir,"características de la vivienda, ponderado_obs.RDS"))


######## Ponderado y normalizado

variables <- c("IV1", "IV2", "IV3", "IV4", "IV5", "IV6", "IV7","IV8",
               "IV9", "IV10",  "IV11", "IV12_1",  "IV12_2", "IV12_3", "PONDERA")        

usu_hogar_t216_strat_pond <- usu_hogar_t216 %>% 
  select(., one_of(variables))%>% 
  mutate_all(funs(as.factor(.))) %>% 
  mutate(PONDERA = as.numeric(PONDERA))

## Análisis de correspondencia ##
usu_hogar_t216_strat_pond <- usu_hogar_t216_strat_pond[apply(usu_hogar_t216_strat_pond[,1:15], 1, function(x) all(x !=9) ),]
usu_hogar_t216_strat_pond <- usu_hogar_t216_strat_pond[apply(usu_hogar_t216_strat_pond[,1:15], 1, function(x) all(x !=0) ),]

mca.fit<- MCA(usu_hogar_t216_strat_pond[1:14], row.w = usu_hogar_t216_strat_pond$PONDERA,
              graph = FALSE)

head(mca.fit$eig,10)


coordenadas <- as.data.frame(mca.fit$var$coord[,1:2])

a <- ggplot(coordenadas, aes(x = row.names(coordenadas),`Dim 1`, fill = row.names(coordenadas)))+
  geom_col()+
  labs(x=paste0("Categoría"),
       y="",
       title="Cargas primera dimension",
       subtitle = "ponderado")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15),
        axis.text= element_text(size=7, angle = 45))
a
ggsave(paste0(resultados.dir,"Coordenadas Dim 1- Ponderado.png"),scale=2)
saveRDS(a,paste0(resultados.dir,"Coordenadas Dim 1- Ponderado.RDS"))


a <- ggplot(coordenadas, aes(x = row.names(coordenadas),`Dim 2`, fill = row.names(coordenadas)))+
  geom_col()+
  labs(x=paste0("Categoría"),
       y="",
       title="Cargas segunda dimension",
       subtitle = "ponderado")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15),
        axis.text= element_text(size=7, angle = 45))
a
ggsave(paste0(resultados.dir,"Coordenadas Dim 2- Ponderado.png"),scale=2)
saveRDS(a,paste0(resultados.dir,"Coordenadas Dim 2- Ponderado.RDS"))


#mca.fit.df <- data.frame(mca.fit$cs)
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
            aes(x = Dim.1, y = Dim.2, label = Variable)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  labs(x=paste0("Dim 1- Inercia: ", round(mca.fit$eig$`percentage of variance`[1],2),"%"),
       y=paste0("Dim 2- Inercia: ", round(mca.fit$eig$`percentage of variance`[2],2),"%"),
       title="Ponderado")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))
a
ggsave(paste0(resultados.dir,"características de la vivienda, sin ponderado_vars.png"),scale=2)
saveRDS(a,paste0(resultados.dir,"características de la vivienda, sin ponderado_vars.RDS"))

# MCA plot of observations and categories
a <- ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(aes(colour = cluster), alpha = 0.5) +
  scale_colour_discrete(name = "Variable")+
  labs(x=paste0("Dim 1- Inercia: ", round(mca.fit$eig$`percentage of variance`[1],2),"%"),
       y=paste0("Dim 2- Inercia: ", round(mca.fit$eig$`percentage of variance`[2],2),"%"),
       title="MCA características de la vivienda",
       subtitle = "Observaciones, ponderado")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))

a

ggsave(paste0(resultados.dir,"características de la vivienda, ponderado_obs.png"),scale=2)
saveRDS(a,paste0(resultados.dir,"características de la vivienda, ponderado_obs.RDS"))

