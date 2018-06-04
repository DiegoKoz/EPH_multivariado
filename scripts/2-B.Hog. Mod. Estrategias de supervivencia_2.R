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
resultados.dir <- paste0(dir,"resultados/Hogar/Estrategias/alternativa/")
dir.create(dirname(resultados.dir))
dir.create(resultados.dir)

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

usu_hogar_t216 <-  read_delim(paste0(data.dir, "usu_hogar_t216.txt"),delim = ";")

### Estrategias del hogar  y Organización del hogar###
variables <- c(paste0("V",1:18), "V19_A","V19_B","V21","V22")        
               
usu_hogar_t216_strat <- usu_hogar_t216 %>% 
  select(., one_of(variables), mnr.10.trb = V19_A, mnr.10.pid = V19_B)%>% 
  mutate_all(funs(as.factor(.)))

## Análisis de correspondencia ##

usu_hogar_t216_strat <- usu_hogar_t216_strat[apply(usu_hogar_t216_strat[,1:22], 1, function(x) all(x !=9) ),]

mca.fit<- MCA(usu_hogar_t216_strat,graph = FALSE)

coordenadas <- as.data.frame(mca.fit$var$coord[,1:2]) %>% 
  mutate(varname = row.names(.)) %>% 
  gather(., dimension, carga,1:2)


a <- ggplot(coordenadas, aes(x = varname,carga, fill = varname ))+
  geom_col()+
  labs(x=paste0("Categoría"),
       y="",
       title="Coordenadas" )+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15),
        axis.text.x = element_text(angle = 45))+
  facet_grid(dimension~. )

a
ggsave(paste0(resultados.dir,"coordenadas sin colapsar Dim 1.png"),scale=2)
saveRDS(a,paste0(resultados.dir,"coordenadas sin colapsar Dim 1.RDS"))

#Colapso las variables menos relevantes

# V19_B la quiero conservar por la segunda dimensión
# Elijo matar todo lo que tiene un valor absoluto menor a 3 cuartos

anular <- as.data.frame(mca.fit$var$coord[,1:2]) %>%
  mutate(nombre = row.names(.)) %>%
  arrange(abs(`Dim 1`)) %>%
  filter(abs(`Dim 1`)<=summary(abs(mca.fit$var$coord[,1]))["3rd Qu."]) %>%
  arrange(nombre) %>%  
  filter(nombre != "mnr.10.pid_1") %>% 
  separate(nombre, into = c('Variable', 'Valor'), sep = '_') %>% 
  select(Variable, Valor) %>% 
  mutate(anular='anular')

#mata las variables 1,2,5,6,7,11,12,16,18. RARO

base_reduc <- colapsador(base = usu_hogar_t216_strat, anular = anular)

mca.fit<- MCA(base_reduc,graph = FALSE)

coordenadas <- as.data.frame(mca.fit$var$coord[,1:2]) %>% 
  mutate(varname = row.names(.)) %>% 
  gather(., dimension, carga,1:2)


a <- ggplot(coordenadas %>% filter(abs(carga)>1), aes(x=varname,y= carga, fill = varname, group=varname ))+
  geom_col()+
  labs(x=paste0("Categoría"),
       y="",
       title="Cargas" )+
  theme_minimal()+
  theme(legend.position = "none",
        text = element_text(size=15))+
  facet_grid(dimension~., scales = "free" )

a
ggsave(paste0(resultados.dir,"coordenadas colapsado Dim 1.png"),scale=2)
saveRDS(a,paste0(resultados.dir,"coordenadas colapsado Dim 1.RDS"))


# data frame with variable coordinates
mca1_vars_df = data.frame(mca.fit$var$coord)
mca1_vars_df$Variable <- row.names(mca1_vars_df)

# data frame with observation coordinates
mca1_obs_df = data.frame(mca.fit$ind$coord)

#Clusters de variables
mca_cluster <- kmeans(mca1_obs_df[,1:2], centers = 2,iter.max = 50, nstart = 10)
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
ggsave(paste0(resultados.dir,"supervivencia_2_vars_D1D2.png"),scale=2)
saveRDS(a,paste0(resultados.dir,"supervivencia_2_vars_D1D2.RDS"))

a <- ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(aes(color = cluster), alpha = 0.5) +
  scale_colour_discrete(name = "Variable")+
  labs(x=paste0("Dim 1- Inercia: ", round(mca.fit$eig$`percentage of variance`[1],2),"%"),
       y=paste0("Dim 2- Inercia: ", round(mca.fit$eig$`percentage of variance`[2],2),"%"),
       title="MCA características de la vivienda",
       subtitle = "Observaciones y variables")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))
a
ggsave(paste0(resultados.dir,"supervivencia_2_obs_D1D2.png"),scale=2)
saveRDS(a,paste0(resultados.dir,"supervivencia_2_obs_D1D2.RDS"))

#var y obs

a <- ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) + 
  geom_hline(yintercept = 0,colour = "gray70") + 
  geom_vline(xintercept = 0, colour = "gray70") + 
  geom_point(aes(colour =cluster),alpha = 0.5) + 
  geom_text(data = mca1_vars_df %>% filter(abs(Dim.1)>0.5 |abs(Dim.2)>0.5), 
            aes(x = Dim.1, y = Dim.2, label = Variable)) + 
  labs(x=paste0("Dim 1- Inercia: ", round(mca.fit$eig$`percentage of variance`[1],2),"%"),
       y=paste0("Dim 2- Inercia: ", round(mca.fit$eig$`percentage of variance`[2],2),"%"),
       title="MCA supervivencia del hogar",
       subtitle = "Observaciones y variables")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))
a
ggsave(paste0(resultados.dir,"supervivencia_2_var_obs.png"),scale=2)
saveRDS(a,paste0(resultados.dir,"supervivencia_2_var_obs.RDS"))

