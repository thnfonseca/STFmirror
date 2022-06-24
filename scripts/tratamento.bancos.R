####
# download.processos
####

# O objetivo deste script é converter as informações processuais, 
# presentes nas listas, em bancos de dados.
# Os bancos serão inseridos na pasta `outputs`


# Se alguns dos pacotes necessários não estiverem instalados, instalamos:
x <- installed.packages() 
if(sum(grepl('tidyverse', names(data.frame(x)[, 1])))==0){installed.packages(tidyverse)}
rm(x)

# Carregamos os pacotes:
library(tidyverse) #

# Definimos a pasta onde se encontram as listas, que foram baixadas pelo script `download.processos.R`
pasta <- file.path(getwd(), "outputs")
pasta <- 'G:/Meu Drive/projeto.stf'

# Obtemos o nome dos arquivos que contém as listas
arquivos <- list.files(pasta) %>% .[grepl('lista.processos', .)] 

# Para obter o nome dos arquivos-listas presentes na pasta `outputs`
arquivos <- data.frame(arquivos=arquivos) %>% 
  mutate(ordem=str_extract(arquivos, "\\d+"),
         ordem=as.numeric(ordem)) %>%
  arrange(ordem)

# Para saber o número total de processos presentes nas listas
load(file = file.path(pasta, arquivos$arquivos[nrow(arquivos)]))
total <- length(lista.processos)

# Carregamos a primeira lista
load(file = file.path(pasta, arquivos$arquivos[k]))



# Selecionamos 
n.bancos <- names(lista.processos[[1]]) %>% 
  grep('banco.andamento|banco.deslocamento|banco.introducao', ., invert = T)

i <- 1
for(i in i:total){
  print(i)
  
  if(i==1){
    banco.introducao<-lista.processos[[i]]$banco.introducao[0, ] %>% as_tibble()
    banco.andamento<-lista.processos[[i]]$banco.andamento[0, ] %>% as_tibble()
    banco.informacoes<-lista.processos[[i]]$informacoes[0, ] %>% as_tibble()
    banco.partes<-lista.processos[[i]]$banco.partes[0, ] %>% as_tibble()
    banco.deslocamento<-lista.processos[[i]]$banco.deslocamento[0, ] %>% as_tibble()
    banco.decisoes<-lista.processos[[i]]$banco.decisoes[0, ] %>% as_tibble()
    banco.peticoes<-lista.processos[[i]]$banco.peticoes[0, ] %>% as_tibble()
    banco.recurso<-lista.processos[[i]]$banco.recurso[0, ] %>% as_tibble()
  }
  
  if(i==31){banco.pauta<-lista.processos[[i]]$banco.pauta[0, ] %>% as_tibble()}
  
    for(j in n.bancos){
      
      if(class(lista.processos[[i]][[j]])=="data.frame"){
        x <- lista.processos[[i]][[j]] %>% 
          mutate(n.proc=lista.processos[[i]][[1]])
      }else{x <- lista.processos[[i]][[j]]}
    
      # Se houver um data frame, agregamos aos demais bancos.
      if(class(x)=='data.frame' & j==2){banco.introducao <- rbind(banco.introducao, as_tibble(x))}
      if(class(x)=='data.frame' & j==3){banco.andamento <- rbind(banco.andamento, as_tibble(x))}
      if(class(x)=='data.frame' & j==4){banco.informacoes <- rbind(banco.informacoes, as_tibble(x))}
      if(class(x)=='data.frame' & j==5){banco.partes <- rbind(banco.partes, as_tibble(x))}
      if(class(x)=='data.frame' & j==6){banco.deslocamento <- rbind(banco.deslocamento, as_tibble(x))}
      if(class(x)=='data.frame' & j==7){banco.decisoes <- rbind(banco.decisoes, as_tibble(x))}
      if(class(x)=='data.frame' & j==8){banco.peticoes <- rbind(banco.peticoes, as_tibble(x))}
      if(class(x)=='data.frame' & j==9){banco.recurso <- rbind(banco.recurso, as_tibble(x))}
      if(class(x)=='data.frame' & j==10){banco.pauta <- rbind(banco.pauta, as_tibble(x))}

  }

  # Se terminarmos um dos arquivos, carregaremos o próximo
  if(i%%10000==0){
    k <- k +1
    load(file = file.path(pasta, arquivos$arquivos[k]))
    }

  # Salvamos os Bancos em RData e csv: 
  if(i==total){
    for(j in n.bancos){
      
    if(j==2){save(banco.introducao, file=file.path(getwd(), 'outputs', 'banco.introducao.RData'))}
    if(j==3){save(banco.andamento, file=file.path(getwd(), 'outputs', 'banco.andamento.RData'))}
    if(j==4){save(banco.informacoes, file=file.path(getwd(), 'outputs', 'banco.informacoes.RData'))}
    if(j==5){save(banco.partes, file=file.path(getwd(), 'outputs', 'banco.partes.RData'))}
    if(j==6){save(banco.deslocamento, file=file.path(getwd(), 'outputs', 'banco.deslocamento.RData'))}
    if(j==7){save(banco.decisoes, file=file.path(getwd(), 'outputs', 'banco.decisoes.RData'))}
    if(j==8){save(banco.peticoes, file=file.path(getwd(), 'outputs', 'banco.peticoes.RData'))}
    if(j==9){save(banco.recurso, file=file.path(getwd(), 'outputs', 'banco.recurso.RData'))}
    if(j==10){save(banco.pauta, file=file.path(getwd(), 'outputs', 'banco.pauta.RData'))}
    
    if(j==2){write.csv2(banco.introducao, file=file.path(getwd(), 'outputs', 'banco.introducao.csv'))}
    if(j==3){write.csv2(banco.andamento, file=file.path(getwd(), 'outputs', 'banco.andamento.csv'))}
    if(j==4){write.csv2(banco.informacoes, file=file.path(getwd(), 'outputs', 'banco.informacoes.csv'))}
    if(j==5){write.csv2(banco.partes, file=file.path(getwd(), 'outputs', 'banco.partes.csv'))}
    if(j==6){write.csv2(banco.deslocamento, file=file.path(getwd(), 'outputs', 'banco.deslocamento.csv'))}
    if(j==7){write.csv2(banco.decisoes, file=file.path(getwd(), 'outputs', 'banco.decisoes.csv'))}
    if(j==8){write.csv2(banco.peticoes, file=file.path(getwd(), 'outputs', 'banco.peticoes.csv'))}
    if(j==9){write.csv2(banco.recurso, file=file.path(getwd(), 'outputs', 'banco.recurso.csv'))}
    if(j==10){write.csv2(banco.pauta, file=file.path(getwd(), 'outputs', 'banco.pauta.csv'))}
    }
  }
    
}



