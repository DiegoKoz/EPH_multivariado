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
resultados.dir <- paste0(dir,"resultados/Hogar/Estrategias/")
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

# bases <- list.files(data.dir)[endsWith(list.files(data.dir),suffix = ".txt")]
# bases <- substr(bases,0,nchar(bases)-4)
# 
# for(base in bases){assign(base, read_delim(paste0(data.dir, paste(base),".txt"),delim = ";"))}

usu_hogar_t216 <-  read_delim(paste0(data.dir, "usu_hogar_t216.txt"),delim = ";")

table(sapply(usu_hogar_t216, class))
### Estrategias del hogar  y Organización del hogar###
variables <- c(paste0("V",1:18), "V19_A","V19_B","V21","V22")        
               
usu_hogar_t216_strat <- usu_hogar_t216 %>% 
  select(., one_of(variables))%>% 
  mutate_all(funs(as.factor(.)))

## Análisis de correspondencia ##
usu_hogar_t216_strat2 <-usu_hogar_t216_strat  

usu_hogar_t216_strat <- usu_hogar_t216_strat2[apply(usu_hogar_t216_strat2[,1:22], 1, function(x) all(x !=9) ),]

mca.fit<- MCA(usu_hogar_t216_strat,graph = FALSE)

head(mca.fit$eig,10)

#mca.fit.df <- data.frame(mca.fit$cs)
# data frame with variable coordinates
mca1_vars_df = data.frame(mca.fit$var$coord)
mca1_vars_df$Variable <- row.names(mca1_vars_df)

# data frame with observation coordinates
mca1_obs_df = data.frame(mca.fit$ind$coord)

# plot of variable categories
ggplot(data=mca1_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  ggtitle("MCA variables supervivencia")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))

# MCA plot of observations and categories
a <- ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.1) +
  geom_density2d(colour = "gray80") +
  geom_text(data = mca1_vars_df, 
            aes(x = Dim.1, y = Dim.2, 
                label = rownames(mca1_vars_df), colour = Variable)) +
  labs(x=paste0("Dim 1- Inercia: ", round(mca.fit$eig$`percentage of variance`[1],2),"%"),
       y=paste0("Dim 2- Inercia: ", round(mca.fit$eig$`percentage of variance`[2],2),"%"),
       title="MCA variables supervivencia")+
  scale_colour_discrete(name = "Variable")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))

ggsave(paste0(resultados.dir,"MCA variables supervivencia sin ponderar.png"),scale=2)
saveRDS(a, paste0(resultados.dir,"MCA variables supervivencia sin ponderar.RDS"))

#### ponderando por PONDERA ####

variables <- c(paste0("V",1:18), "V19_A","V19_B","V21","V22", "PONDERA")        

usu_hogar_t216_strat_pond <- usu_hogar_t216 %>% 
  select(., one_of(variables))

usu_hogar_t216_strat_pond <-usu_hogar_t216_strat_pond %>% 
  mutate_all(funs(as.factor(.)))
usu_hogar_t216_strat_pond$PONDERA <- as.numeric(usu_hogar_t216_strat_pond$PONDERA)
## Análisis de correspondencia ##
usu_hogar_t216_strat_pond <- usu_hogar_t216_strat_pond[apply(usu_hogar_t216_strat_pond[,1:22], 1, function(x) all(x !=9) ),]

mca.fit<- MCA(usu_hogar_t216_strat_pond[1:22], row.w = usu_hogar_t216_strat_pond$PONDERA,
              graph = FALSE)

head(mca.fit$eig,10)

#mca.fit.df <- data.frame(mca.fit$cs)
# data frame with variable coordinates
mca1_vars_df = data.frame(mca.fit$var$coord)
mca1_vars_df$Variable <- row.names(mca1_vars_df)

# data frame with observation coordinates
mca1_obs_df = data.frame(mca.fit$ind$coord)

# plot of variable categories
ggplot(data=mca1_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca1_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(aes(colour=Variable)) +
  ggtitle("MCA variables supervivencia")+
  theme(legend.position = "none",
        text = element_text(size=15))

# MCA plot of observations and categories
a <- ggplot(data = mca1_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  geom_text(data = mca1_vars_df, 
            aes(x = Dim.1, y = Dim.2, 
                label = rownames(mca1_vars_df), colour = Variable)) +
  labs(x=paste0("Dim 1- Inercia: ", round(mca.fit$eig$`percentage of variance`[1],2),"%"),
       y=paste0("Dim 2- Inercia: ", round(mca.fit$eig$`percentage of variance`[2],2),"%"),
       title="MCA variables supervivencia",
       subtitle = "Variables, Ponderado")+
  theme_tufte()+
  theme(legend.position = "none",
        text = element_text(size=15))
a
ggsave(paste0(resultados.dir,"MCA variables supervivencia ponderado.png"),scale=2)
saveRDS(a, paste0(resultados.dir,"MCA variables supervivencia ponderado.RDS"))


