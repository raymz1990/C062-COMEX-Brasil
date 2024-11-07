# 3ª Pagina (por Destino) ----

# Função para a página export
export_page <- function() {
  tagList(
    fluidRow(
      
      ### Seleção exibição de valor ---- 
      column(
        width = 2,
        radioButtons(
          inputId = "measure_select_export",
          label = NULL,
          choices = list("Valor ($)" = "value", "Peso (Toneladas)" = "weight"),
          selected = "value",
          inline = TRUE
        )
      ),
      
      ### Seleção de ano ----
      column(
        width = 2,
        selectInput(
          inputId = "year_filter_export",
          label = "Ano:",
          choices = c("Todos", sort(2011:2020, decreasing = TRUE)),
          # selected = 2020,
          width = '100%'
        )
      ),
      
      ### seleção do bloco econômmico ----
      column(
        width = 2,
        selectInput(
          inputId = "block_filter",
          label = "Bloco Econômico:",
          choices = c(
            "Todos",
            "América do Sul",
            "América Central",
            "América do Norte",
            "Europa",
            "Ásia",
            "África",
            "Oriente Médio",
            "Oceania"
          ),
          selected = "América do Sul",
          width = '100%'
        )
      ),
      
      ## Indicadores ----
      column(
        width = 6,
        fluidRow(
          create_value_box3("Exportações (em $)", "money-bill", "KPI_VALUE_EXPORT"),
          create_value_box3("Toneladas Exportadas", "weight", "KPI_WEIGHT_EXPORT"),
          create_value_box3("Destinos de Exportação", "globe", "KPI_DESTINATION_EXPORT")
        )
      )
    ), 
    
    ## Gráficos ----
    
    fluidRow(
      ### Grafico de barras de países exportadores -----
      box(
        # title = "Top 10 Países Exportadores",
        status = "primary",
        closable = FALSE,
        maximizable = FALSE,
        collapsible = TRUE,
        width = 6,
        highchartOutput("bar_export", height = 400)
      ),
      
      ### Grafico de rosca ----
      box(
        # title = "Representatividade por Região",
        status = "primary",
        closable = FALSE,
        maximizable = FALSE,
        collapsible = TRUE,
        width = 6,
        highchartOutput("donut_export", height = 400)
      )
    )
  )
}