# 2ª Pagina (Region) ----

# Função para a página region
region_page <- function() {
  tagList(
    fluidRow(
      
      ### Seleção exibição de valor ---- 
      column(
        width = 2,
        radioButtons(
          inputId = "measure_filter_region",
          label = NULL,
          choices = list("Valor ($)" = "value", "Peso (Toneladas)" = "weight"),
          selected = "value",
          inline = TRUE
        )
      ),
      
      #### Seleção de ano ----
      column(
        width = 2,
        selectInput(
          inputId = "year_filter_region",
          label = "Ano:",
          choices = c("Todos", sort(2011:2020, decreasing = TRUE)),
          # selected = 2020,
          width = '100%'
        )
      ),
      
      #### Seleção da região ----
      column(
        width = 2,
        selectInput(
          inputId = "region_filter",
          label = "Região:",
          choices = c("Todos", "Sul", "Sudeste", "Centro Oeste", "Norte", "Nordeste"),
          selected = "Sul",
          width = '100%'
        )
      ), 
      
      ### Indicadores ----
      column(
        width = 6,
        fluidRow(
          create_value_box2("Exportações (em $)", "money-bill", "KPI_VALUE_REGION"),
          create_value_box2("Toneladas Exportadas", "weight", "KPI_WEIGHT_REGION")
        )
      )
    ), 
    
    # wheelchair
    fluidRow(
      box(
        # title = "Região",  
        status = "primary",
        closable = FALSE,
        maximizable = TRUE,
        collapsible = TRUE,
        width = 6,
        highchartOutput("wheel_region", height = 400)  
      ),
      
      # Gráfico de rosca para a região
      box(
        # title = "Representatividade da Região no Ano por US$",  
        status = "primary",
        closable = FALSE,
        maximizable = FALSE,
        collapsible = TRUE,
        width = 6,
        highchartOutput("donut_region", height = 400)  
      )
    )
  )
}
    