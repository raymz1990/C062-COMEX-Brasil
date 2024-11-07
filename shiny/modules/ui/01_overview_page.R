# 1ª Pagina (Overview) ----

# Função para a página Overview
overview_page <- function() {
  tagList(
    fluidRow(
      column(
        width = 2,
        selectInput(
          inputId = "year_filter_overview",
          label = "Ano:",
          choices = c("Todos", sort(2011:2020, decreasing = TRUE)),
          width = '100%'
        )
      ),
      column(
        width = 10,
        fluidRow(
          create_value_box4("Exportações (em $)", "money-bill", "KPI_VALUE"),
          create_value_box4("Toneladas Exportadas", "weight", "KPI_WEIGHT"),
          create_value_box4("Municípios Brasileiros", "city", "KPI_CITIES"),
          create_value_box4("Destinos de Exportação", "globe", "KPI_COUNTRIES")
        )
      )
    ),
    
    fluidRow(
      tabBox(
        title = "Visão Geral Exportação por Região (2011-2020)",
        width = 12,
        status = "danger",
        closable = FALSE,
        maximizable = FALSE,
        collapsible = TRUE,
        tabPanel(
          "por Região de Estado",
          radioButtons(
            inputId = "measure_select_region",
            choices = list("Valor ($)" = "value", 
                           "Peso (Toneladas)" = "weight"), 
            label = NULL,
            selected = "value",
            inline = TRUE
          ),
          highchartOutput("line_chart_region")
        ),
        tabPanel(
          "por Bloco Econômico",
          radioButtons(
            inputId = "measure_select_block",
            choices = list("Valor ($)" = "value",
                           "Peso (Toneladas)" = "weight"),
            label = NULL,
            selected = "value",
            inline = TRUE
          ),
          highchartOutput("line_chart_block")
        )
      )
    ),
    
    fluidRow(
      box(
        title = "Mapa do Destino das Exportações",
        width = 12,
        status = "danger",
        closable = FALSE,
        maximizable = FALSE,
        collapsible = TRUE,
        highchartOutput("map", height = 600)
      )
    )
  )
}