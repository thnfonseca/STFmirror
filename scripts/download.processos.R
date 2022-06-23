####
# download.processos
####

# O objetivo deste script é obter as informações de todos os processos 
# a partir do `código url` (denominação nossa) obtido por meio do script `download.urls`.
# O presente script retorna várias listas, cada qual representando um processo.
# Em cada uma das listas, estão inseridos vários bancos, cada um representando um tipo de informação. 

# Se alguns dos pacotes necessários não estiverem instalados, instalamos:
x <- installed.packages() 
if(sum(grepl('tidyverse', names(data.frame(x)[, 1])))==0){installed.packages(tidyverse)}
if(sum(grepl('rvest', names(data.frame(x)[, 1])))==0){installed.packages(rvest)}
if(sum(grepl('RSelenium', names(data.frame(x)[, 1])))==0){installed.packages(RSelenium)}
rm(x)

# Carregamos os pacotes:
library(tidyverse) #
library(rvest)
library(RSelenium)

# A página do STF está em https, ou seja, precisamos simular um browser para baixar os dados.
# Por isso utilizaremos o pacote `RSelenium`.
# Acessamos o browser para baixar os dados:
driver<- rsDriver(browser = "chrome", port = 4444L, chromever = "72.0.3626.7") #verbose = FALSE,port=4567L
remDr <- driver[["client"]]

#binman::list_versions("chromedriver")


# Carregamos o banco com `código url` (denominação nossa) de cada processo.
# Este banco foi obtido pelo script `download.url.R`.
load(file.path(getwd(), 'outputs', 'stf.urls.RData'))

# Criamos a lista para os processos:
lista.processos <- vector("list", 1)



##################
#i<- 1724634
#n.salve<-173

# Para informar ao looping que iniciaremos desde a primeira url
i <- 1

repeat{
  # Inserimos o looping completo em um looping infinito. 
  # Caso o primeiro seja interrompido por instabilidade de acesso, 
  # fechamos o browser, abrimos outro, esperamos alguns segundos e 
  # repetimos de onde parou.
  
  tryCatch({
    remDr$close()
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  remDr$open()
  Sys.sleep(4*30)

  # O looping principal está dentro de uma função tryCatch para que o código continue 
  # executando de onde parou se houver erro.
  tryCatch({
    
    # Iniciamos a função:
    for(i in i:nrow(stf.urls)){
      #  for(i in i:42000){
      
      print(paste(i,"de",nrow(stf.urls),"- baixando"))
      
      # Definimos a página de busca dos processos
      url <- gsub("CODURL", stf.urls$cod.url[i], "http://portal.stf.jus.br/processos/detalhe.asp?incidente=CODURL")
      
      # Navegamos na página do processo:
      remDr$navigate(url)
      Sys.sleep(1.5)
      
      # Criamos uma lista com espaço para 10 bancos por processo.
      lista.stf <- vector("list",10) 
      names(lista.stf) <- c("n.processo", "banco.introducao", "banco.andamento",
                            "informacoes", "banco.partes", "banco.deslocamento",
                            "banco.decisoes", "banco.peticoes", "banco.recurso", 
                            "banco.pautas")
      
      # Bloco 1: Conferir e coleta as informações iniciais do processo
      ##########################################################################
      print(paste(i,"de",nrow(stf.urls),"- baixando n.processo (1)"))
      
      webElems<-NULL
      repeat{
        tryCatch({
          webElems <- remDr$getPageSource(using = 'xpath', '//*[@id="texto-pagina-interna"]')[[1]]
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
        if(length(webElems)>0){break}
      }
      
      processo <- data.frame(num=NA, num.unico=NA, info=NA)
      
      processo$num <- webElems %>% read_html() %>% 
        html_nodes(xpath = '//*[@id="texto-pagina-interna"]/div/div/div/div[1]/div[1]/div[1]/text()') %>%
        html_text() %>% .[[1]] %>% strsplit('\n') %>% unlist() %>% .[[2]] %>% gsub('  ', "", x=.)
      
      processo$num.unico <- webElems %>% read_html() %>% 
        html_nodes(xpath = '//*[@id="texto-pagina-interna"]/div/div/div/div[1]/div[1]/div[2]') %>%
        html_text() %>% gsub('Número Único: ', '', .)
      
      processo$info <- webElems %>% read_html() %>% 
        html_nodes(xpath = '//*[@id="texto-pagina-interna"]/div/div/div/div[1]/div[1]/div[1]/div') %>%
        html_text() %>% gsub('  ', '', .)
      
      lista.stf[["n.processo"]] <- processo %>% as_tibble()
      if(nchar(processo)==1){identificador<-1}else{identificador<-0}
      rm(processo)
      ##########################################################################
      
      # Bloco 2: Variáveis introdutórias ligadas ao processo e às partes
      ##########################################################################
      print(paste(i,"de",nrow(stf.urls),"- baixando banco.introducao (2)"))
      
      # o objeto `fonte` se refere a todo o conteúdo da página, a partir do qual extraímos os bancos 
      fonte <- webElems %>% read_html() %>% 
        html_nodes('#texto-pagina-interna > div > div > div > div.card-conteudo.row.m-0.p-0 > div.card-processo > div')
      
      
      banco.introducao <- data.frame(variavel='tipo.processo', valor=NA)[1:3,] %>%
        mutate(variavel=as.character(variavel), valor=as.character(valor))
      
      banco.introducao$valor[1] <- fonte %>% html_nodes('div.processo-classe.m-t-8.p-l-16') %>% html_text()
      banco.introducao$variavel[2] <- fonte %>% 
        html_nodes(xpath = '//*[@id="texto-pagina-interna"]/div/div/div/div[2]/div[1]/div/div[2]/text()') %>%
        html_text() %>% gsub(x=., ": ","") %>% as.character()
      banco.introducao$valor[2] <- fonte %>% html_nodes('#descricao-procedencia') %>%
        html_text() %>% unique() %>% gsub(x=., "\n","") %>% gsub(x=., "  ","") 
      banco.introducao[3,] <- fonte %>% html_nodes(xpath = '//*[@id="texto-pagina-interna"]/div/div/div/div[2]/div[1]/div/div[3]') %>%
        html_text() %>% strsplit(': ') %>% unlist()
      
      linhas <- fonte %>% html_nodes(xpath = '//*[@id="partes-resumidas"]') %>% html_children() %>% length()
      if(linhas>0){
        banco.introducao <- banco.introducao[1:(3+linhas),]
        row.names(banco.introducao) <- NULL
        
        for(j in 1:linhas){
          banco.introducao$variavel[3+j] <- fonte %>% 
            html_nodes(xpath = gsub("JJJ",j,'//*[@id="partes-resumidas"]/div[JJJ]/div[1]')) %>% html_text() %>% as.character()
          banco.introducao$valor[3+j] <- fonte %>% 
            html_nodes(xpath = gsub("JJJ",j,'//*[@id="partes-resumidas"]/div[JJJ]/div[2]')) %>% html_text()
        }
        lista.stf[["banco.introducao"]] <- banco.introducao %>% as_tibble()
      }
      
      if(linhas==0){
        lista.stf[["banco.introducao"]] <- NA
      }
      rm(banco.introducao, introducao)
      
      ##########################################################################
      
      # Bloco 3: Andamentos
      ##########################################################################
      print(paste(i,"de",nrow(stf.urls),"- baixando banco.andamento (3)"))
      
      linhas <- fonte %>% html_nodes(xpath = '//*[@id="andamentos"]/div/ul') %>%
        html_children() %>% length()
      
      if(linhas>0){
        banco.andamento <- data.frame(data.andamento=NA, tipo.andamento=NA, 
                                      detalhe.andamento=NA, ator.andamento=NA)[1:linhas, ]
        row.names(banco.andamento) <- NULL
        for(j in 1:linhas){
          caminho <- fonte %>% 
            html_nodes(xpath = '//*[@id="texto-pagina-interna"]/div/div/div/div[2]/div[5]') %>%
            html_nodes(gsub("JJJ", j,'#andamentos > div > ul > li:nth-child(JJJ) > div > div > div > div'))
          banco.andamento$data.andamento[j] <- caminho %>% html_nodes('div.col-md-3.p-l-0 > div') %>% html_text()
          banco.andamento$tipo.andamento[j] <- caminho %>% html_nodes('div.col-md-5.p-l-0 > h5') %>% html_text()
          tryCatch({
            banco.andamento$detalhe.andamento[j] <- caminho %>% html_nodes('div.col-md-9.p-0') %>% html_text()
          }, error=function(e){cat()})
          tryCatch({
            banco.andamento$ator.andamento[j] <- caminho %>% html_nodes('div.col-md-3.p-0 > span') %>% html_text()
          }, error=function(e){cat()})
        }
        banco.andamento <- banco.andamento %>% mutate_all(as.character) %>% as_tibble()
        
        lista.stf[["banco.andamento"]] <- banco.andamento
      }
      
      if(linhas==0){lista.stf[["banco.andamento"]] <- NA}
      rm(banco.andamento)
      
      ##########################################################################
      
      # Bloco 4: Informações
      ##########################################################################
      print(paste(i,"de",nrow(stf.urls),"- baixando informacoes (4)"))
      
      informacoes <- data.frame(assunto=NA, data.protocolo=NA, orgao.origem=NA, uf.origem=NA, no.origem=NA,
                                volumes=NA, folhas=NA, apensos=NA)
      
      tryCatch({
        informacoes$assunto[1] <- fonte %>% 
          html_nodes(xpath = '//*[@id="informacoes-completas"]/div[1]/div[2]/div[2]') %>%
          html_text() %>% gsub("\n", "", x=.) %>% gsub("  ", "", x=.)
      }, error=function(e){cat()})
      tryCatch({
        informacoes$data.protocolo[1] <- fonte %>% 
          html_nodes(xpath = '//*[@id="informacoes-completas"]/div[2]/div[1]/div[2]/div[2]') %>%
          html_text() %>% gsub("\n", "", x=.) %>% gsub("  ", "", x=.)
      }, error=function(e){cat()})
      tryCatch({
        informacoes$orgao.origem[1] <- fonte %>% 
          html_nodes(xpath = '//*[@id="informacoes-completas"]/div[2]/div[1]/div[2]/div[4]') %>%
          html_text() %>% gsub("\n", "", x=.) %>% gsub("  ", "", x=.)
      }, error=function(e){cat()})
      tryCatch({
        informacoes$uf.origem[1] <- fonte %>% 
          html_nodes(xpath = '//*[@id="informacoes-completas"]/div[2]/div[1]/div[2]/div[6]') %>%
          html_text() %>% gsub("\n", "", x=.) %>% gsub("  ", "", x=.)
      }, error=function(e){cat()})
      tryCatch({
        informacoes$no.origem[1] <- fonte %>% 
          html_nodes(xpath = '//*[@id="informacoes-completas"]/div[2]/div[1]/div[2]/div[8]') %>%
          html_text() %>% gsub("\n", "", x=.) %>% gsub("  ", "", x=.)
      }, error=function(e){cat()})
      tryCatch({
        informacoes$volumes[1] <- fonte %>% 
          html_nodes(xpath = '//*[@id="informacoes-completas"]/div[2]/div[2]/div[1]/div[1]') %>%
          html_text() %>% gsub("\n", "", x=.) %>% gsub("  ", "", x=.)
      }, error=function(e){cat()})
      tryCatch({
        informacoes$folhas[1] <- fonte %>% 
          html_nodes(xpath = '//*[@id="informacoes-completas"]/div[2]/div[2]/div[2]/div[1]') %>%
          html_text() %>% gsub("\n", "", x=.) %>% gsub("  ", "", x=.)
      }, error=function(e){cat()})
      tryCatch({
        informacoes$apensos[1] <- fonte %>% 
          html_nodes(xpath = '//*[@id="informacoes-completas"]/div[2]/div[2]/div[3]/div[1]') %>%
          html_text() %>% gsub("\n", "", x=.) %>% gsub("  ", "", x=.)
      }, error=function(e){cat()})
      
      informacoes <- informacoes %>% mutate_all(as.character) %>% as_tibble()
      
      lista.stf[["informacoes"]] <- informacoes
      
      #Se a página não fornecer informações, vamos para o próximo processo i
      if(informacoes$data.protocolo=='' & identificador==1){next}
      rm(informacoes)
      
      ##########################################################################
      
      # Bloco 5: Partes
      ##########################################################################
      print(paste(i,"de",nrow(stf.urls),"- baixando banco.partes (5)"))
      
      linhas <- fonte %>% html_nodes(xpath = '//*[@id="todas-partes"]') %>%
        html_children() %>% length()
      if(linhas>0){
        banco.partes <- data.frame(tipo=NA, nome=NA)[1:linhas, ]
        for(j in 1:linhas){
          banco.partes$tipo[j] <- fonte %>% html_nodes(xpath = gsub('JJJ', j, '//*[@id="todas-partes"]/div[JJJ]/div[1]')) %>%
            html_text()
          banco.partes$nome[j] <- fonte %>% html_nodes(xpath = gsub('JJJ', j, '//*[@id="todas-partes"]/div[JJJ]/div[2]')) %>%
            html_text()
        }
        lista.stf[["banco.partes"]] <- banco.partes  %>% as_tibble()
        
      }
      
      if(linhas==0){
        lista.stf[["banco.partes"]] <- NA
      }
      rm(banco.partes)
      
      ##########################################################################
      
      # Bloco 6: Deslocamentos
      ##########################################################################
      
      print(paste(i,"de",nrow(stf.urls),"- baixando banco.deslocamento (6)"))
      
      linhas <- fonte %>% html_nodes(xpath = '//*[@id="deslocamentos"]') %>%
        html_children() %>% length()
      
      if(linhas>0){
        banco.deslocamento <- data.frame(data.deslocamento=NA, tipo.deslocamento=NA,
                                         detalhe.deslocamento=NA, guia.deslocamento=NA)[1:linhas, ]
        for(j in 1:linhas){
          banco.deslocamento$tipo.deslocamento[j] <- fonte %>% 
            html_nodes(xpath = gsub('JJJ', j, '//*[@id="deslocamentos"]/div[JJJ]/div[1]/div[2]')) %>%
            html_text() %>% gsub(x=., "\n", "") %>% gsub(x=., "  ", "")
          banco.deslocamento$detalhe.deslocamento[j] <- fonte %>% 
            html_nodes(xpath = gsub('JJJ', j, '//*[@id="deslocamentos"]/div[JJJ]/div[1]/div[3]')) %>%
            html_text() %>% gsub(x=., "\n", "") %>% gsub(x=., "  ", "")
          banco.deslocamento$guia.deslocamento[j] <- fonte %>% 
            html_nodes(xpath = gsub('JJJ', j, '//*[@id="deslocamentos"]/div[JJJ]/div[2]/div[1]')) %>%
            html_text() %>% gsub(x=., "\n", "") %>% gsub(x=., "  ", "")
          banco.deslocamento$data.deslocamento[j] <- fonte %>% 
            html_nodes(xpath = gsub('JJJ', j, '//*[@id="deslocamentos"]/div[JJJ]/div[2]/div[2]')) %>%
            html_text() %>% gsub(x=., "\n", "") %>% gsub(x=., "  ", "")
        }
        row.names(banco.deslocamento) <- NULL
        
        lista.stf[["banco.deslocamento"]] <- banco.deslocamento %>% as_tibble()
      }
      
      if(linhas==0){lista.stf[["banco.deslocamento"]] <- NA}
      rm(banco.deslocamento)
      
      ##########################################################################
      
      # Bloco 7: Decisões
      ##########################################################################
      print(paste(i,"de",nrow(stf.urls),"- baixando banco.decisoes (7)"))
      
      linhas <- fonte %>% html_nodes(xpath = '//*[@id="decisoes"]/div') %>%
        html_children() %>% length()
      if(linhas>0){
        banco.decisoes <- data.frame(data.decisao=NA, tipo.decisao=NA,
                                     conteudo.decisao=NA, colegiado=NA)[1:linhas,]
        codigo.lista <- 
          fonte %>% html_nodes(xpath = '//*[@id="decisoes"]/div') %>%
          html_children() %>% as.character() %>% strsplit('id="') %>% unlist() %>% 
          substr(1,5) %>% stringr::str_match_all("[0-9]+") %>% unlist() %>% as.numeric()
        codigo.lista <- codigo.lista[!is.na(codigo.lista)]
        
        for(j in 1:linhas){
          banco.decisoes$data.decisao[j] <- fonte %>% 
            html_nodes(xpath = gsub('JJJ', codigo.lista[j], '//*[@id="JJJ"]/div/div/div/div[1]')) %>%
            html_text() %>% gsub(x=., '\n', "") %>% gsub(x=., '  ', "")
          banco.decisoes$tipo.decisao[j] <- fonte %>% 
            html_nodes(xpath = gsub('JJJ', codigo.lista[j], '//*[@id="JJJ"]/div/div/div/div[2]')) %>%
            html_text() %>% gsub(x=., '\n', "") %>% gsub(x=., '  ', "")
          banco.decisoes$colegiado[j] <- fonte %>% 
            html_nodes(xpath = gsub('JJJ', codigo.lista[j], '//*[@id="JJJ"]/div/div/div/div[5]')) %>%
            html_text() %>% gsub(x=., '\n', "") %>% gsub(x=., '  ', "")
          banco.decisoes$conteudo.decisao[j] <- fonte %>% 
            html_nodes(xpath = gsub('JJJ', codigo.lista[j], '//*[@id="JJJ"]/div/div/div/div[6]')) %>%
            html_text() %>% gsub(x=., '\n', "") %>% gsub(x=., '  ', "")
        }
        row.names(banco.decisoes) <- NULL 
        
        lista.stf[["banco.decisoes"]] <- banco.decisoes %>% as_tibble()
      }
      
      if(linhas==0){lista.stf[["banco.decisoes"]] <- NA}
      rm(banco.decisoes)
      
      ##########################################################################
      
      # Bloco 8: Peticoes
      ##########################################################################
      print(paste(i,"de",nrow(stf.urls),"- baixando banco.peticoes (8)"))
      
      linhas <- fonte %>% html_nodes(xpath = '//*[@id="peticoes"]') %>% html_children() %>% length()
      
      if(linhas>0){
        banco.peticoes <- data.frame(no.peticao=NA, data.peticao=NA, detalhe.peticao=NA)[1:linhas, ]
        for(j in 1:linhas){
          banco.peticoes$no.peticao[j] <- fonte %>% html_nodes(xpath = gsub('JJJ', j, '//*[@id="peticoes"]/div[JJJ]/div[1]/span[1]')) %>% html_text()
          banco.peticoes$data.peticao[j] <- fonte %>% html_nodes(xpath = gsub('JJJ', j, '//*[@id="peticoes"]/div[JJJ]/div[1]/span[2]')) %>% html_text()
          banco.peticoes$detalhe.peticao[j] <- fonte %>% html_nodes(xpath = gsub('JJJ', j, '//*[@id="peticoes"]/div[JJJ]/div[2]/span')) %>% html_text()
        }
        lista.stf[["banco.peticoes"]] <- banco.peticoes %>% as_tibble()
      }
      
      if(linhas==0){lista.stf[["banco.peticoes"]] <- NA}
      rm(banco.peticoes)
      
      ##########################################################################
      
      # Bloco 9: Recursos
      ##########################################################################
      print(paste(i,"de",nrow(stf.urls),"- baixando recursos (9)"))
      
      banco.recurso <- 
        fonte %>% html_nodes(xpath = '//*[@id="recursos"]') %>% 
        html_text() %>% strsplit('\n    \n') %>% unlist() %>% 
        gsub('\n', '', x=.) %>% gsub('  ', '', x=.)
      banco.recurso <- data.frame(recurso=banco.recurso)  %>% as_tibble()
      
      lista.stf[["banco.recurso"]] <- banco.recurso
      rm(banco.recurso)
      ##########################################################################
      
      # Bloco 10: Pautas
      ##########################################################################
      print(paste(i,"de",nrow(stf.urls),"- baixando banco.pautas (10)"))
      
      linhas <- fonte %>% html_nodes(xpath = '//*[@id="pautas"]/div') %>% 
        html_children() %>% length()
      if(linhas>0){
        banco.pautas <- data.frame(no.pauta=NA, data.pauta=NA, colegiado=NA, detalhe.data.pauta=NA)[1:linhas, ]
        for(j in 1:linhas){
          banco.pautas$data.pauta[j] <- fonte %>% html_nodes(xpath = '//*[@id="pautas"]/div/div/div/div/div/div[1]/div') %>% 
            html_text()  
          banco.pautas$detalhe.data.pauta[j] <- fonte %>% html_nodes(xpath = '//*[@id="pautas"]/div/div/div/div/div/div[2]/h5') %>% 
            html_text()  
          banco.pautas$colegiado[j] <- fonte %>% html_nodes(xpath = '//*[@id="pautas"]/div/div/div/div/div/div[5]') %>% 
            html_text()  
          banco.pautas$no.pauta[j] <- fonte %>% html_nodes(xpath = '//*[@id="pautas"]/div/div/div/div/div/div[6]') %>% 
            html_text()  
        }
        lista.stf[["banco.pautas"]] <- banco.pautas %>% as_tibble()
      }
      if(linhas==0){lista.stf[["banco.pautas"]] <- NA}
      rm(data.pautas)
      
      ##########################################################################
      
      # Salvamos os bancos em listas
      ##########################################################################
      print(paste(i,"de",nrow(stf.urls),"- salvando"))
      
      # Adicionamos a lista de informações do processo i na lista de todos os processos:
      lista.processos[[i]] <- lista.stf
      names(lista.processos)[i] <- stf.urls$Processo[i]
      
      # Salvamos a cada 500 processos acessados e dividimos as listas a cada 10000 processos.
      # A divisão serve para evitar problemas de capacidade de hardware.
      if(i%%500==0){save(lista.processos, file = gsub("NUM", n.salve, file.path(getwd(), 'outputs.updated-06.22', 'lista.processos.NUM.RData')))}
      if(i%%10000==0){n.salve<-n.salve+1}
      if(i%%10000==0){lista.processos <- vector("list", 1)}
      ##########################################################################
      
    }
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
  if(i==nrow(stf.urls)){print('Processo encerrado')}
  
}


