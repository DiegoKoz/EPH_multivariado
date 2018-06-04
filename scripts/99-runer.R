#script para correr los demás scripts. 
rm(list=ls())
script.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
scripts <- list.files(script.dir)
scripts <- list.files(script.dir)[endsWith(list.files(script.dir),suffix = ".R")]
scripts <- scripts[-length(scripts)]

for(i in scripts){ 

  source(i,echo = TRUE,local = T,encoding = "UTF-8") 
  }
