rm(list=ls())
##################### Preprocesamiento de datos #####################
library(tidyverse)
library(readxl)
library(ca)
library(FactoMineR)
script.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
dir <- paste0(dirname(script.dir),"/")
data.dir <- paste0(dir,"bases/")
resultados.dir <- paste0(dir,"resultados/Individual/Caracteristicas_miembros/")
dir.create(resultados.dir)
# bases <- list.files(data.dir)[endsWith(list.files(data.dir),suffix = ".txt")]
# bases <- substr(bases,0,nchar(bases)-4)
# 
# for(base in bases){assign(base, read_delim(paste0(data.dir, paste(base),".txt"),delim = ";"))}

usu_individual_t216 <-  read_delim(paste0(data.dir, "usu_individual_t216.txt"),delim = ";")

table(sapply(usu_individual_t216, class))

#### MCA sin ponderar####

variables_continuas <- c("CH06")  #edad
variables_discretas <- c("CH03", #Relación de parentesco
                         "CH04", #sexo
                         "CH08", #cobertura médica
                         "CH09", #lectocomprensión
                         "CH15", #lugar de nacimiento
                         "NIVEL_ED",
                         "ESTADO",
                         "CAT_OCUP",
                         "CAT_INAC")                         
                         
                         
ind_t216_var_grales <- usu_individual_t216 %>% 
#agrego un filtro para outliers, para ajustar la escala. 
  select(., one_of(variables_discretas)) %>% 
  mutate_all(funs(as.factor(.)))  

# ind_t216_var_grales <- ind_t216_var_grales[apply(ind_t216_var_grales[,1:ncol(ind_t216_var_grales)], 1, function(x) all(x !=9) ),]
# ind_t216_var_grales <- ind_t216_var_grales[apply(ind_t216_var_grales[,1:ncol(ind_t216_var_grales)], 1, function(x) all(x !=0) ),]

mca.fit<- MCA(ind_t216_var_grales,graph = FALSE)

head(mca.fit$eig,10)

coordenadas <- as.data.frame(mca.fit$var$coord[,1:2]) %>% 
  mutate(varname = row.names(.)) %>% 
  gather(., dimension, carga,1:2)


a <- ggplot(coordenadas %>% filter(abs(carga)>0.25), aes(x = varname,carga, fill = varname ))+
  geom_col()+
  labs(x=paste0("Categoría"),
       y="",
       title="Coordenadas" )+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15),
        axis.text.x = element_text(size=7, angle = 45))+
  facet_grid(dimension~., scales = "free")

a
ggsave(paste0(resultados.dir,"Coordenadas no ponderado.png"),scale=2)
saveRDS(a, paste0(resultados.dir,"Coordenadas no ponderado.RDS"))



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
       title="No Ponderado")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))
  
a 
ggsave(paste0(resultados.dir,"variables generales no ponderador_vars.png"),scale=2)
saveRDS(a, paste0(resultados.dir,"variables generales no ponderador_vars.RDS"))

# MCA plot of observations and categories
a <- ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(aes(colour = cluster), alpha = 0.5) +
  scale_colour_discrete(name = "Variable")+
  labs(x=paste0("Dim 1- Inercia: ", round(mca.fit$eig$`percentage of variance`[1],2),"%"),
       y=paste0("Dim 2- Inercia: ", round(mca.fit$eig$`percentage of variance`[2],2),"%"),
       title="MCA variables generales",
       subtitle = "Observaciones, no ponderado")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))
  
a
ggsave(paste0(resultados.dir,"variables generales, no ponderador_obs.png"),scale=2)
saveRDS(a, paste0(resultados.dir,"variables generales, no ponderador_obs.RDS"))


#### MCA ponderando ####

variables_discretas <- c("CH03", #Relación de parentesco
                         "CH04", #sexo
                         "CH08", #cobertura médica
                         "CH09", #lectocomprensión
                         "CH15", #lugar de nacimiento
                         "NIVEL_ED",
                         "ESTADO",
                         "CAT_OCUP",
                         "CAT_INAC",
                         "PONDERA")                         

ind_t216_var_grales_pond <- usu_individual_t216 %>% 
#agrego un filtro para outliers, para ajustar la escala
  select(., one_of(variables_discretas))%>% 
  mutate_all(funs(as.factor(.))) %>% 
  mutate(PONDERA = as.numeric(PONDERA))

## Análisis de correspondencia ##
# ind_t216_var_grales_pond <- ind_t216_var_grales_pond[apply(ind_t216_var_grales_pond[,1:(ncol(ind_t216_var_grales_pond)-1)], 1, function(x) all(x !=9) ),]
# ind_t216_var_grales_pond <- ind_t216_var_grales_pond[apply(ind_t216_var_grales_pond[,1:(ncol(ind_t216_var_grales_pond)-1)], 1, function(x) all(x !=0) ),]

mca.fit<- MCA(ind_t216_var_grales_pond[1:(ncol(ind_t216_var_grales_pond)-1)], row.w = ind_t216_var_grales_pond$PONDERA,
              graph = FALSE)

head(mca.fit$eig,10)


coordenadas <- as.data.frame(mca.fit$var$coord[,1:2])

a <- ggplot(coordenadas, aes(x = row.names(coordenadas),`Dim 1`, fill = row.names(coordenadas)))+
  geom_col()+
  labs(x=paste0("Categoría"),
       y="",
       title="Cargas primera dimension",
       subtitle = "no ponderado")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15),
        axis.text.x = element_text(size=7, angle = 45))
  
a
ggsave(paste0(resultados.dir,"Coordenadas Dim 1- Ponderado.png"),scale=2)
saveRDS(a, paste0(resultados.dir,"Coordenadas Dim 1- Ponderado.RDS"))


a <- ggplot(coordenadas, aes(x = row.names(coordenadas),`Dim 2`, fill = row.names(coordenadas)))+
  geom_col()+
  labs(x=paste0("Categoría"),
       y="",
       title="Cargas segunda dimension",
       subtitle = "no ponderado")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15),
        axis.text.x = element_text(size=7, angle = 45))
  
a
ggsave(paste0(resultados.dir,"Coordenadas Dim 2- Ponderado.png"),scale=2)
saveRDS(a, paste0(resultados.dir,"Coordenadas Dim 2- Ponderado.RDS"))


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
       title="Ponderado",
       subtitle = "sin normalizar")+
   theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))
  
a
ggsave(paste0(resultados.dir,"variables generales, ponderado_vars.png"),scale=2)
saveRDS(a, paste0(resultados.dir,"variables generales, ponderado_vars.RDS"))

# MCA plot of observations and categories
a <- ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(aes(colour = cluster), alpha = 0.5) +
  scale_colour_discrete(name = "Variable")+
  labs(x=paste0("Dim 1- Inercia: ", round(mca.fit$eig$`percentage of variance`[1],2),"%"),
       y=paste0("Dim 2- Inercia: ", round(mca.fit$eig$`percentage of variance`[2],2),"%"),
       title="MCA variables generales",
       subtitle = "Observaciones, ponderado")+
   theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))
  
a
ggsave(paste0(resultados.dir,"variables generales, ponderado_obs.png"),scale=2)
saveRDS(a, paste0(resultados.dir,"variables generales, ponderado_obs.RDS"))

#### MCA ponderando normalizado ####

variables_discretas <- c("CH03", #Relación de parentesco
                         "CH04", #sexo
                         "CH08", #cobertura médica
                         "CH09", #lectocomprensión
                         "CH15", #lugar de nacimiento
                         "NIVEL_ED",
                         "ESTADO",
                         "CAT_OCUP",
                         "CAT_INAC",
                         "PONDERA")                         

ind_t216_var_grales_pond <- usu_individual_t216 %>% 
#agrego un filtro para outliers, para ajustar la escala
  select(., one_of(variables_discretas))%>% 
  mutate_all(funs(as.factor(.))) %>% 
  mutate(PONDERA = as.numeric(PONDERA)) %>% 
  mutate(PONDERA_norm = PONDERA/sum(PONDERA))

## Análisis de correspondencia ##
# ind_t216_var_grales_pond <- ind_t216_var_grales_pond[apply(ind_t216_var_grales_pond[,1:(ncol(ind_t216_var_grales_pond)-1)], 1, function(x) all(x !=9) ),]
# ind_t216_var_grales_pond <- ind_t216_var_grales_pond[apply(ind_t216_var_grales_pond[,1:(ncol(ind_t216_var_grales_pond)-1)], 1, function(x) all(x !=0) ),]

mca.fit<- MCA(ind_t216_var_grales_pond[1:(ncol(ind_t216_var_grales_pond)-2)], row.w = ind_t216_var_grales_pond$PONDERA_norm,
              graph = FALSE)

head(mca.fit$eig,10)


coordenadas <- as.data.frame(mca.fit$var$coord[,1:2])

a <- ggplot(coordenadas, aes(x = row.names(coordenadas),`Dim 1`, fill = row.names(coordenadas)))+
  geom_col()+
  labs(x=paste0("Categoría"),
       y="",
       title="Cargas primera dimension",
       subtitle = "no ponderado")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15),
        axis.text.x = element_text(size=7, angle = 45))

a
ggsave(paste0(resultados.dir,"Coordenadas Dim 1- Pond_norm.png"),scale=2)
saveRDS(a, paste0(resultados.dir,"Coordenadas Dim 1- Pond_norm.RDS"))


a <- ggplot(coordenadas, aes(x = row.names(coordenadas),`Dim 2`, fill = row.names(coordenadas)))+
  geom_col()+
  labs(x=paste0("Categoría"),
       y="",
       title="Cargas segunda dimension",
       subtitle = "no ponderado")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15),
        axis.text.x = element_text(size=7, angle = 45))

a
ggsave(paste0(resultados.dir,"Coordenadas Dim 2- Pond_norm.png"),scale=2)
saveRDS(a, paste0(resultados.dir,"Coordenadas Dim 2- Pond_norm.RDS"))


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
       title="Ponderado", 
       subtitle= "normalizado")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))

a
ggsave(paste0(resultados.dir,"variables generales, Pond_norm_vars.png"),scale=2)
saveRDS(a, paste0(resultados.dir,"variables generales, Pond_norm_vars.RDS"))

# MCA plot of observations and categories
a <- ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(aes(colour = cluster), alpha = 0.5) +
  scale_colour_discrete(name = "Variable")+
  labs(x=paste0("Dim 1- Inercia: ", round(mca.fit$eig$`percentage of variance`[1],2),"%"),
       y=paste0("Dim 2- Inercia: ", round(mca.fit$eig$`percentage of variance`[2],2),"%"),
       title="MCA variables generales",
       subtitle = "Observaciones, ponderado")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))

a
ggsave(paste0(resultados.dir,"variables generales, Pond_norm_obs.png"),scale=2)
saveRDS(a, paste0(resultados.dir,"variables generales, Pond_norm_obs.RDS"))
