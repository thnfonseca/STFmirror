####
# download.urls
####

# O objetivo deste script � obter o `c�digo url` (denomina��o nossa) de cada processo do STF
# Sem este c�digo n�o � poss�vel acessar informa��es do respectivo processo.

# Se alguns dos pacotes necess�rios n�o estiver instalado, instalamos:
x <- installed.packages() 
if(sum(grepl('tidyverse', names(data.frame(x)[, 1])))==0){installed.packages(tidyverse)}
if(sum(grepl('rvest', names(data.frame(x)[, 1])))==0){installed.packages(rvest)}
if(sum(grepl('RSelenium', names(data.frame(x)[, 1])))==0){installed.packages(RSelenium)}
rm(x)

# Carregamos os pacotes:
library(tidyverse) 
library(rvest)
library(RSelenium)

# A p�gina do STF est� em https, ou seja, precisamos simular um browser para baixar os dados.
# Por isso utilizaremos o pacote `RSelenium`.
# Acessamos o browser para baixar os dados:
driver<- rsDriver(browser = "chrome", port = 4444L, chromever = "72.0.3626.7") #verbose = FALSE,port=4567L
remDr <- driver[["client"]]

# Obs: em alguns dispositivos, talvez seja necess�rio ao acesso via Docker.

# Definimos a p�gina de busca dos processos
url <- "http://www.stf.jus.br/portal/processo/listarProcesso.asp"

# Navegamos para a url
#remDr$open()
remDr$navigate(url)

# Definimos o numero m�ximo de processos que uma classe processual pode ter:
# (ou seja, nenhuma classe processual tem mais de `numero.max` processos
numero.max <- 13892300
    # Obs: Este valor foi identificado qualitativamente na p�gina `http://www.stf.jus.br/portal/processo/listarProcesso.asp`

# O looping a seguir coleta os links dos processos, do n�mmero 1 de cada classe processual 
# at� o �ltimo processo de cada classe processual
i<-1
repeat{
  
  # Inserimos o looping dentro da fun��o tryCatch para continuarmos a coletar informa��es, 
  #caso ocorra algum erro de acesso � p�gina do STF:
  tryCatch({  

    # Iniciamos o looping at� o n�mero 13892300, ou seja, o �timo processo da corte    
    for(i in i:numero.max){
      print(paste(i, 'de', numero.max))
      if(i%%100000==0){next}
      
      # A cada 350 coletas esperamos 3 minutos para a p�gina do STF n�o bloquear o acesso
      if(i%%350==0){print('Em espera para a p�gina do STF n�o bloquear')}
      if(i%%350==0){Sys.sleep(60*3)}
      
      # Identifica a tag para preenchimento
      fill <- remDr$findElement(using = "xpath", value = '//*[@id="numero"]')
      # Preennchemos o formul�rio com o n�mero do processo.
      fill$sendKeysToElement(list(as.character(i)))
      
      # Identifica a tag para clicar
      click <- remDr$findElement(using = "xpath", value = '//*[@id="dropmsgform"]/div[5]/input[2]')
      # Submetemos a busca. Como resultado, iremos obter informa��es dos processos:
      click$clickElement()
      
      
      a <- 0
      while(a<=10){ # Inserimos em um loopinh infinito at� confirmar que realmente houve o retorno da busca 
        # Faz a leitura da p�gina
        webElems <- remDr$getPageSource(using = 'xpath', '//*[@id="divImpressao"]/div')[[1]]
        # Identifica se a tabela j� retornou
        verificador <- webElems %>% read_html() %>% html_node(css = '#divImpressao > div > p') %>% html_text() %>%
          stringr::str_extract('\\d+')
        if(verificador==as.character(i)){break}
        
        a <- a + 1
        if(verificador==as.character(i)){break} # Depois de 10 tentativas, desistimos
      }
      
      # Em caso de sucesso, obtemos a tabela
      tab <- webElems %>% read_html() %>% html_table() %>% .[[1]]
      
      # Coletamos o o c�digo das urls que cont�m informa��es particulares de cada processo
      # Posteriormente, o c�digo dever� ser acrescentado no final do endere�o:
      # 'http://portal.stf.jus.br/processos/detalhe.asp?incidente='
      urls <- webElems %>% read_html() %>% 
        html_nodes("table") %>%
        html_nodes('td:nth-child(1) > a') %>% 
        html_attr('href') %>% stringr::str_extract('\\d+') 
      # Inserimos o c�digo das urls na tabela:
      tab$cod.url <- urls
      
      # Juntamos as tabelas no decorrer do looping
      if(i==1){stf.urls.v2 <- tab}else{stf.urls.v2 <- rbind(stf.urls.v2, tab)}
      # Salvamos a cada 1000 coletas
      if(i%%1000){save(stf.urls.v2, file = file.path(getwd(), 'outputs/stf.urls.RData'))}
    }
  }, error=function(e){cat("ERROR: ", conditionMessage(e), "\n")})
  
  # Se der erro devido a problemas e instabilidades na p�gina do STF, esperamos alguns minutos:
  # Como sabemos disso? Tentativa e erro ao baixar os dados pela primeira vez.
  Sys.sleep(3*60)
  
  # Se chegarmos ao n�mero m�ximo de processos, interrompemos a repeti��o
  if(i==numero.max){print("Fim da coleta!")}
  if(i==numero.max){break}
}

# Encerramos a simula��o do browser:
remDr$close()
remDr$quit()
remDr$closeServer()
