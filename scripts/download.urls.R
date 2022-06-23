####
# download.urls
####

# O objetivo deste script é obter o `código url` (denominação nossa) de cada processo do STF
# Sem este código não é possível acessar informações do respectivo processo.

# Se alguns dos pacotes necessários não estiver instalado, instalamos:
x <- installed.packages() 
if(sum(grepl('tidyverse', names(data.frame(x)[, 1])))==0){installed.packages(tidyverse)}
if(sum(grepl('rvest', names(data.frame(x)[, 1])))==0){installed.packages(rvest)}
if(sum(grepl('RSelenium', names(data.frame(x)[, 1])))==0){installed.packages(RSelenium)}
rm(x)

# Carregamos os pacotes:
library(tidyverse) 
library(rvest)
library(RSelenium)

# A página do STF está em https, ou seja, precisamos simular um browser para baixar os dados.
# Por isso utilizaremos o pacote `RSelenium`.
# Acessamos o browser para baixar os dados:
driver<- rsDriver(browser = "chrome", port = 4444L, chromever = "72.0.3626.7") #verbose = FALSE,port=4567L
remDr <- driver[["client"]]

# Obs: em alguns dispositivos, talvez seja necessário ao acesso via Docker.

# Definimos a página de busca dos processos
url <- "http://www.stf.jus.br/portal/processo/listarProcesso.asp"

# Navegamos para a url
#remDr$open()
remDr$navigate(url)

# Definimos o numero máximo de processos que uma classe processual pode ter:
# (ou seja, nenhuma classe processual tem mais de `numero.max` processos
numero.max <- 13892300
    # Obs: Este valor foi identificado qualitativamente na página `http://www.stf.jus.br/portal/processo/listarProcesso.asp`

# O looping a seguir coleta os links dos processos, do númmero 1 de cada classe processual 
# até o último processo de cada classe processual
i<-1
repeat{
  
  # Inserimos o looping dentro da função tryCatch para continuarmos a coletar informações, 
  #caso ocorra algum erro de acesso à página do STF:
  tryCatch({  

    # Iniciamos o looping até o número 13892300, ou seja, o útimo processo da corte    
    for(i in i:numero.max){
      print(paste(i, 'de', numero.max))
      if(i%%100000==0){next}
      
      # A cada 350 coletas esperamos 3 minutos para a página do STF não bloquear o acesso
      if(i%%350==0){print('Em espera para a página do STF não bloquear')}
      if(i%%350==0){Sys.sleep(60*3)}
      
      # Identifica a tag para preenchimento
      fill <- remDr$findElement(using = "xpath", value = '//*[@id="numero"]')
      # Preennchemos o formulário com o número do processo.
      fill$sendKeysToElement(list(as.character(i)))
      
      # Identifica a tag para clicar
      click <- remDr$findElement(using = "xpath", value = '//*[@id="dropmsgform"]/div[5]/input[2]')
      # Submetemos a busca. Como resultado, iremos obter informações dos processos:
      click$clickElement()
      
      
      a <- 0
      while(a<=10){ # Inserimos em um loopinh infinito até confirmar que realmente houve o retorno da busca 
        # Faz a leitura da página
        webElems <- remDr$getPageSource(using = 'xpath', '//*[@id="divImpressao"]/div')[[1]]
        # Identifica se a tabela já retornou
        verificador <- webElems %>% read_html() %>% html_node(css = '#divImpressao > div > p') %>% html_text() %>%
          stringr::str_extract('\\d+')
        if(verificador==as.character(i)){break}
        
        a <- a + 1
        if(verificador==as.character(i)){break} # Depois de 10 tentativas, desistimos
      }
      
      # Em caso de sucesso, obtemos a tabela
      tab <- webElems %>% read_html() %>% html_table() %>% .[[1]]
      
      # Coletamos o o código das urls que contém informações particulares de cada processo
      # Posteriormente, o código deverá ser acrescentado no final do endereço:
      # 'http://portal.stf.jus.br/processos/detalhe.asp?incidente='
      urls <- webElems %>% read_html() %>% 
        html_nodes("table") %>%
        html_nodes('td:nth-child(1) > a') %>% 
        html_attr('href') %>% stringr::str_extract('\\d+') 
      # Inserimos o código das urls na tabela:
      tab$cod.url <- urls
      
      # Juntamos as tabelas no decorrer do looping
      if(i==1){stf.urls.v2 <- tab}else{stf.urls.v2 <- rbind(stf.urls.v2, tab)}
      # Salvamos a cada 1000 coletas
      if(i%%1000){save(stf.urls.v2, file = file.path(getwd(), 'outputs/stf.urls.RData'))}
    }
  }, error=function(e){cat("ERROR: ", conditionMessage(e), "\n")})
  
  # Se der erro devido a problemas e instabilidades na página do STF, esperamos alguns minutos:
  # Como sabemos disso? Tentativa e erro ao baixar os dados pela primeira vez.
  Sys.sleep(3*60)
  
  # Se chegarmos ao número máximo de processos, interrompemos a repetição
  if(i==numero.max){print("Fim da coleta!")}
  if(i==numero.max){break}
}

# Encerramos a simulação do browser:
remDr$close()
remDr$quit()
remDr$closeServer()
