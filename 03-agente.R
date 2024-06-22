library(httr)
library(stringr)
library(glue)

source("02-funcoes.R")

options(readr.show_col_types = FALSE)


# testes das funcoes -----------------------------------------------------------
topico <- "machine learning"
obter_resposta_chatgpt(glue("é facil aprender {topico}?"))

obter_performance(nome_asset = "PETR4",
                  data_inicio = dmy("05/06/24"),
                  data_fim = dmy("07/06/24"))

gerar_e_salvar_analise(nome_asset = "PETR4",
                       data_inicio = dmy("05/06/24"),
                       data_fim = dmy("07/06/24"))

# geracao das analises ---------------------------------------------------------
assets <- c("PETR4", "VALE3")

n_dias <- 4

datas <- c(dmy("03/06/24"),
           dmy("10/06/24"))

for(asset in assets){
  for(i in 1:length(datas)){
    inicio <- datas[i]
    fim <- inicio + days(n_dias)
    print(paste(asset, inicio, "e", fim))
    
    resultado <- tryCatch({
      gerar_e_salvar_analise(nome_asset = asset,
                             data_inicio = inicio,
                             data_fim = fim)
    }, warning = function(w) {
      print(paste("Aviso:", w))
    }, error = function(e){
      print(paste("Erro:", e))
    })
  }
}


analises <- read_csv("data/analises.csv")
View(analises)

analises$analise[1]
analises$analise[2]
analises$analise[3]
