# 4ª Pagina (por Produto) ----

# Função para a página export
product_page <- function() {
  tagList(
    fluidRow(
      
      ### Seleção exibição de valor ---- 
      column(
        width = 2,
        radioButtons(
          inputId = "measure_select_product",
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
          inputId = "year_filter_product",
          label = "Ano:",
          choices = c("Todos", sort(2011:2020, decreasing = TRUE)),
          # selected = 2020,
          width = '100%'
        )
      ),
      
      ### Seleção de tipo de produto ----
      column(
        width = 2,
        selectInput(
          inputId = "product_filter",
          label = "Atividade Econômica:",
          choices = c("Todos", 
                      "Agropecuária",
                      "Indústria Extrativa",
                      "Indústria de Transformação",
                      "Outros Produtos"),
          selected = "Todos",
          width = '100%'
        )
      ), 
      
      ## Indicadores ----
      column(
        width = 6,
        fluidRow(
          create_value_box3("Exportações (em $)", "money-bill", "KPI_VALUE_PRODUCT"),
          create_value_box3("Toneladas Exportadas", "weight", "KPI_WEIGHT_PRODUCT"),
          create_value_box3("Preço Médio (US$ / Ton)", "calculator", "KPI_PRICE_PRODUCT")
        )
      )
    ), 
    
    ## Gráficos ----
    
    fluidRow(
      
      ### Grafico de barras de produtos -----
      tabBox(
        id = "tabset_product",
        # title = "Top 10 Regiões por Produtos",
        status = "primary",
        closable = FALSE,
        maximizable = FALSE,
        collapsible = TRUE,
        width = 6,
        tabPanel(
          "por Região de Estado",
          highchartOutput("bar_product_region", height = 400)
        ),
        tabPanel(
          "por Bloco Econômico",
          highchartOutput("bar_product_block", height = 400)
        )
      ), 
      
      
      ### Grafico de treemap ----
      box(
        # title = "Representatividade por Região",
        status = "primary",
        closable = FALSE,
        maximizable = FALSE,
        collapsible = TRUE,
        width = 6,
        highchartOutput("treemap_product", height = 400)
      )
    )
  )
}