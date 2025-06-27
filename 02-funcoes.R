library(tidyverse)
library(lubridate)

obter_performance <- function(nome_asset, data_inicio, data_fim) {
  dados <- read_csv("data/daily.csv")
  
  if(class(data_inicio) != "Date"){
    stop("formato invalido para data_inicio")
  }
  
  if(class(data_fim) != "Date"){
    stop("formato invalido para data_fim")
  }
    
  df_fim <- dados %>% 
    filter(asset == nome_asset,
           data == data_fim)
  
  df_inicio <- dados %>% 
    filter(asset == nome_asset,
           data == data_inicio)

  resultado <- tibble(asset = df_fim$asset,
                      empresa = df_fim$empresa,
                      performance = round((df_fim$fechamento - df_inicio$fechamento)/df_inicio$fechamento * 100, 
                                          2))
  if(nrow(resultado)==0)
    stop(paste("Performance não encontrada para", nome_asset, 
               "entre", data_inicio, "e", data_fim))
  
  return(resultado)
}

obter_noticias <- function(nome_asset, data_inicio, data_fim) {
  if(class(data_inicio) != "Date"){
    stop("formato invalido para data_inicio")
  }
  
  if(class(data_fim) != "Date"){
    stop("formato invalido para data_fim")
  }
  
  noticias <- read_csv("data/news.csv") %>% 
    filter(asset == nome_asset,
           data >= data_inicio,
           data <= data_fim) %>% 
    arrange(data)
  
  return(noticias)
}

formatar_noticias <- function(noticias) {
  resultado <- noticias %>% 
    mutate(data = format(data, "%d/%m"),
           noticia_dia = str_c(data, ": ", titulo_noticia)) %>% 
    pull(noticia_dia) %>% 
    paste(collapse = " \n ")
  
  return(resultado)
}

obter_resposta_chatgpt <- function(prompt) {
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions", 
    add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo",
      messages = list(list(
        role = "user", 
        content = prompt
      ))
    ),
    timeout(240),
    config(connecttimeout = 60),
    config(low_speed_time = 30)
  )
  
  if(status_code(response)>200) {
    stop(content(response)$error$message)
  }
  
  return(str_trim(content(response)$choices[[1]]$message$content))
}

gerar_e_salvar_analise <- function(nome_asset, data_inicio, data_fim){
  if(class(data_inicio) != "Date"){
    stop("formato invalido para data_inicio")
  }
  
  if(class(data_fim) != "Date"){
    stop("formato invalido para data_fim")
  }
  
  performance <- obter_performance(nome_asset, data_inicio, data_fim)
  noticias <- obter_noticias(nome_asset, data_inicio, data_fim)
  
  noticias_formatadas <- formatar_noticias(noticias) %>% enc2utf8()
  
  n_dias <- as.integer(data_fim - data_inicio)
  empresa <- performance$empresa
  
  prompt <- glue("
Ativo: {nome_asset}
Empresa: {empresa}

Lista de títulos de notícias:
                 {noticias_formatadas}

Você está atuando como analista financeiro em {data_fim}, 
avaliando a performance recente do ativo {nome_asset}, 
que registrou uma variação de {performance$performance}% 
nos últimos {n_dias} dias. Sua tarefa é realizar uma 
análise focada nos eventos dos últimos {n_dias} dias apresentados na 
lista acima, fornecendo uma perspectiva 
fundamentada sobre a situação atual da empresa {empresa}.

Por favor, siga estas diretrizes:

Inicie a análise com uma visão geral clara da situação.
Mencione a performance do ativo no período.
Destaque os eventos mais recentes e relevantes.
Enfatize os contrastes entre diferentes eventos e conclua com uma síntese ponderada.
Inclua exclusivamente informações que estão presentes neste prompt.
Evite redundâncias e repetições na resposta.
Estruture a análise em um parágrafo único.

Modelo da análise: 'Com base nas notícias dos últimos {n_dias} dias... (análise)...
Portanto, (conclusão).'
")
  
  if(nchar(noticias_formatadas) == 0){
    analise <- "Não foi possível criar análise. Não há notícias disponíveis no período."
  }else{
    analise <- obter_resposta_chatgpt(as.character(prompt))
  }
  
  resultado <- tibble(asset = nome_asset,
                      data_inicio = data_inicio, 
                      data_fim = data_fim,
                      n_dias = as.integer(data_fim - data_inicio),
                      prompt = prompt,
                      analise = analise)
  
  write_csv(resultado, 
            file = "data/analises.csv",
            append = TRUE)
}
