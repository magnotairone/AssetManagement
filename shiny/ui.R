library(shiny)
library(dplyr)

# UI
shinyUI(fluidPage(
  titlePanel("Análise de Ativos"),
  sidebarLayout(
    sidebarPanel(
      selectInput("ativo_selecionado", "Selecione o Ativo:", choices = NULL),
      dateInput("data_inicio", "Data de Início:", value = Sys.Date() - 5),
      dateInput("data_fim", "Data de Fim:", value = Sys.Date()),
      actionButton("mostrar_analise", "Mostrar Análise")
    ),
    mainPanel(
      uiOutput("output_analise"),
      uiOutput("output_prompt"),
      uiOutput("output_noticias")
    )
  )
))
