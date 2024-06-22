library(shiny)
library(tidyverse)

# Server
shinyServer(function(input, output, session) {
  
  # Carrega os dados no servidor
  noticias <- read_csv("../data/news.csv")
  analises <- read_csv("../data/analises.csv")
  
  # Atualiza os ativos disponíveis no dropdown
  observe({
    updateSelectInput(session, "ativo_selecionado", choices = unique(noticias$asset))
  })
  
  # Filtra os dados e exibe a análise e notícias correspondentes
  observeEvent(input$mostrar_analise, {
    ativo_selecionado <- input$ativo_selecionado
    data_inicio_selecionada <- input$data_inicio
    data_fim_selecionada <- input$data_fim
    
    # Filtra as análises
    analise_selecionada <- analises %>%
      filter(asset == ativo_selecionado,
             data_inicio == data_inicio_selecionada,
             data_fim == data_fim_selecionada) %>%
      select(analise, prompt)
    
    # Exibe a análise
    if (nrow(analise_selecionada) > 0) {
      texto_analise <- analise_selecionada$analise
      texto_prompt <- analise_selecionada$prompt
    } else {
      texto_analise <- "Nenhuma análise disponível para os critérios selecionados."
      texto_prompt <- "Nenhum prompt disponível para os critérios selecionados."
    }
    
    output$output_analise <- renderUI({
      tags$textarea(id = "texto_analise", rows = 10, style = "width: 100%;", texto_analise)
    })
    
    output$output_prompt <- renderUI({
      tags$textarea(id = "texto_prompt", rows = 10, style = "width: 100%;", texto_prompt)
    })
    
    # Filtra as notícias
    noticias_filtradas <- noticias %>%
      filter(asset == ativo_selecionado,
             data >= data_inicio_selecionada,
             data <= data_fim_selecionada) %>%
      mutate(data = format(data, "%d/%m/%Y"),
             fonte = paste0('<a href="', fonte, '" target="_blank">', fonte, '</a>')) %>% 
      select("Título da Notícia" = titulo_noticia, "Data" = data, "Fonte" = fonte) 
    
    # Exibe a tabela de notícias
    output$output_noticias <- renderUI({
      tableOutput("tabela_noticias")
    })
    
    output$tabela_noticias <- renderTable({
      noticias_filtradas
    }, sanitize.text.function = function(x) x)
  })
})
