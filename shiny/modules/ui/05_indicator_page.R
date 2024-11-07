# 5ª Pagina ----

# Função para a página indicator
indicator_page <- function() {
  tagList(
    ## Tabsets ----
    tabsetPanel(id = "main_tab",
                
                ### Municípios ----
                tabPanel(
                  "Municípios",
                  
                  controlbarMenu(
                    id = "controlbarmenu_city",
                    vertical = TRUE,
                    type = "pills",
                    side = "left",
                    
                    #### Mapa Cloroplético ----
                    # controlbarItem(
                    #   title = "Mapa Cloroplético",
                    #   
                    #   fluidRow(
                    #     column(
                    #       width = 3,
                    #       radioButtons(
                    #         inputId = "value_cloropletic_city",
                    #         label = "Métrica:",
                    #         choices = list("Valor ($)" = "value", "Peso (Toneladas)" = "weight"),
                    #         selected = "value",
                    #         inline = TRUE
                    #       )
                    #     ),
                    #     column(
                    #       width = 3,
                    #       selectInput(
                    #         inputId = "year_cloropletic_city", 
                    #         label = "Ano:",
                    #         choices = c("Todos", sort(2011:2020, decreasing = TRUE)),
                    #         selected = 2020,
                    #         width = '100%'
                    #       )
                    #     ),
                    #     column(
                    #       width = 4,
                    #       selectInput(
                    #         inputId = "isic_cloropletic_city",
                    #         label = "Atividade Econômica:",
                    #         choices = c("Todos", 
                    #                     "Agropecuária",
                    #                     "Indústria Extrativa",
                    #                     "Indústria de Transformação",
                    #                     "Outros Produtos"),
                    #         selected = "Todos",
                    #         width = '100%'
                    #       )
                    #     )
                    #   ),
                    #   fluidRow(
                    #     box(
                    #       title = "Mapa de Exportações por Município",
                    #       sidebar = boxSidebar(
                    #         icon = icon("question"),
                    #         id = "info_cloropletic_city",
                    #         background = "#3d9970",
                    #         h5("O ", strong("Mapa Cloroplético"), " exibe a distribuição geográfica das exportações dos municípios brasileiros.
                    #            Espaços em brancos representam municípios que não tiveram movimentações para o exterior no período."),
                    #         h5("Como utilizar?"),
                    #         p(strong("Métrica"), ": Escolha entre 'Valor' (US$) ou 'Peso' (Toneladas) para visualizar as exportações em uma das duas unidades"),
                    #         p(strong("Ano"), ": Selecione o ano desejado ou 'Todos' para visualizar o período completo de 2011 a 2020."),
                    #         p(strong("Atividade Econômica"), ": Selecione uma das atividades econômicas para filtrar as exportações com base no setor")
                    #       ),
                    #       status = "primary",
                    #       # closable = FALSE,
                    #       maximizable = FALSE,
                    #       # collapsible = TRUE,
                    #       width = 12,
                    #       highchartOutput("cloropletic_city", height = 500)
                    #     )
                    #   )
                    # ),
                    
                    #### Mapa de Conexão (Origem-Destino) ----
                    controlbarItem(
                      title = "Mapa de Conexão",
                      
                      fluidRow(
                        column(
                          width = 3,
                          radioButtons(
                            inputId = "value_conection_city",
                            label = "Métrica:",
                            choices = list("Valor ($)" = "value", "Peso (Toneladas)" = "weight"),
                            selected = "value",
                            inline = TRUE
                          )
                        ),
                        column(
                          width = 3,
                          selectInput(
                            inputId = "year_conection_city", 
                            label = "Ano:",
                            choices = c("Todos", sort(2011:2020, decreasing = TRUE)),
                            # selected = 2020,
                            width = '100%'
                          )
                        ),
                        column(
                          width = 6,
                          selectizeInput(
                            inputId = "town_conection_city",
                            label = "Município:", 
                            choices = NULL,
                            selected = "",
                            options = list(placeholder = 'Selecione um município'),
                            multiple = FALSE,
                            width = '100%'
                          )
                        )
                      ),
                      fluidRow(
                        box(
                          title = "Fluxo de Exportação por Município",
                          sidebar = boxSidebar(
                            icon = icon("question"),
                            id = "info_conection_city",
                            background = "#3d9970",
                            h5("O ", strong("Mapa de Conexão"), " mostra as principais rotas de exportação entre um município e seus países de destino."),
                            h5("Como utilizar?"),
                            p(strong("Métrica"), ": Escolha entre 'Valor' (US$) ou 'Peso' (Toneladas) para visualizar as exportações em uma das duas unidades"),
                            p(strong("Ano"), ": Selecione o ano desejado ou 'Todos' para visualizar o período completo de 2011 a 2020."),
                            p(strong("Município"), ": Escolha o município.")
                            
                          ),
                          status = "primary",
                          closable = FALSE,
                          maximizable = FALSE,
                          collapsible = TRUE,
                          width = 12,
                          highchartOutput("conection_city", height = 500)
                        )
                      )
                    ),
                    
                    #### Sankey (Origem-ISIC-Destino) ----
                    controlbarItem(
                      title = "Sankey",
                      
                      fluidRow(
                        column(
                          width = 3,
                          radioButtons(
                            inputId = "value_sankey_city",
                            label = "Métrica:",
                            choices = list("Valor ($)" = "value", "Peso (Toneladas)" = "weight"),
                            selected = "value",
                            inline = TRUE
                          )
                        ),
                        column(
                          width = 3,
                          selectInput(
                            inputId = "year_sankey_city", 
                            label = "Ano:",
                            choices = c("Todos", sort(2011:2020, decreasing = TRUE)),
                            # selected = "Todos",
                            width = '100%'
                          )
                        ),
                        column(
                          width = 6,
                          selectizeInput(
                            inputId = "town_sankey_city",
                            label = "Município:", 
                            choices = NULL,
                            selected = "",
                            options = list(placeholder = 'Selecione um município'),
                            multiple = FALSE,
                            width = '100%'
                          )
                        )
                      ),
                      fluidRow(
                        box(
                          title = "Fluxo de Exportações por Município e Atividade Econômica",
                          sidebar = boxSidebar(
                            icon = icon("question"),
                            id = "info_csankey_city",
                            background = "#3d9970",
                            h5("O gráfico ", strong("Sankey"), " mostra o fluxo de exportações entre municípios, atividades econômicas e destinos."),
                            h5("Como utilizar?"),
                            p(strong("Métrica"), ": Escolha entre 'Valor' (US$) ou 'Peso' (Toneladas) para visualizar as exportações em uma das duas unidades"),
                            p(strong("Ano"), ": Selecione o ano desejado ou 'Todos' para visualizar o período completo de 2011 a 2020."),
                            p(strong("Município"), ": Escolha um município.")
                            
                          ),
                          status = "primary",
                          closable = FALSE,
                          maximizable = FALSE,
                          collapsible = TRUE,
                          width = 12,
                          highchartOutput("sankey_city", height = 500)
                        )
                      )
                    )
                    
                  )
                ),
                
                ### Estados ----
                tabPanel(
                  "Estados",
                  
                  controlbarMenu(
                    id = "controlbarmenu_state",
                    vertical = TRUE,
                    type = "pills",
                    side = "left",
                    
                    #### Mapa Cloroplético ----
                    controlbarItem(
                      title = "Mapa Cloroplético",
                      
                      fluidRow(
                        column(
                          width = 3,
                          radioButtons(
                            inputId = "value_cloropletic_state",
                            label = "Métrica:",
                            choices = list("Valor ($)" = "value", "Peso (Toneladas)" = "weight"),
                            selected = "value",
                            inline = TRUE
                          )
                        ),
                        column(
                          width = 3,
                          selectInput(
                            inputId = "year_cloropletic_state", 
                            label = "Ano:",
                            choices = c("Todos", sort(2011:2020, decreasing = TRUE)),
                            # selected = 2020,
                            width = '100%'
                          )
                        ),
                        column(
                          width = 6,
                          selectInput(
                            inputId = "product_cloropletic_state",
                            label = "Atividade Econômica:",
                            choices = c("Todos", 
                                        "Agropecuária",
                                        "Indústria Extrativa",
                                        "Indústria de Transformação",
                                        "Outros Produtos"),
                            selected = "Todos",
                            width = '100%'
                          )
                        )
                      ),
                      fluidRow(
                        box(
                          title = "Mapa Exportação por Estado",
                          sidebar = boxSidebar(
                            icon = icon("question"),
                            id = "info_cloropletic_state",
                            background = "#3d9970",
                            h5("O ", strong("Mapa Cloroplético"), " exibe a distribuição geográfica das exportações dos estados brasileiros."),
                            h5("Como utilizar?"),
                            p(strong("Métrica"), ": Escolha entre 'Valor' (US$) ou 'Peso' (Toneladas) para visualizar as exportações em uma das duas unidades"),
                            p(strong("Ano"), ": Selecione o ano desejado ou 'Todos' para visualizar o período completo de 2011 a 2020."),
                            p(strong("Atividade Econômica"), ": Selecione a atividade econômica para filtrar os dados de exportação por setor.")
                          ),
                          status = "primary",
                          closable = FALSE,
                          maximizable = FALSE,
                          collapsible = TRUE,
                          width = 12,
                          highchartOutput("cloropletic_state", height = 500)
                        )
                      )
                    ),
                    
                    #### Sunburst ----
                    controlbarItem(
                      title = "Sunburst",
                      
                      fluidRow(
                        column(
                          width = 3,
                          radioButtons(
                            inputId = "value_sunburst_state",
                            label = "Métrica:",
                            choices = list("Valor ($)" = "value", "Peso (Toneladas)" = "weight"),
                            selected = "value",
                            inline = TRUE
                          )
                        ),
                        column(
                          width = 3,
                          selectInput(
                            inputId = "year_sunburst_state", 
                            label = "Ano:",
                            choices = c("Todos", sort(2011:2020, decreasing = TRUE)),
                            # selected = 2020,
                            width = '100%'
                          )
                        ),
                        column(
                          width = 6,
                          selectizeInput(
                            inputId = "state_sunburst_state",
                            label = "Estado:", 
                            choices = NULL,
                            selected = "",
                            options = list(placeholder = 'Selecione um estado'),
                            multiple = FALSE,
                            width = '100%'
                          )
                        )
                      ),
                      fluidRow(
                        box(
                          title = "Exportações por Estado e Categoria de Produto",
                          sidebar = boxSidebar(
                            icon = icon("question"),
                            id = "info_sunburst_state",
                            background = "#3d9970",
                            h5("O gráfico ", strong("Sunburst"), " permite visualizar a hierarquia das exportações de um estado, desde as atividades econômicas até os produtos exportados."),
                            h5("Como utilizar?"),
                            p(strong("Métrica"), ": Escolha entre 'Valor' (US$) ou 'Peso' (Toneladas) para visualizar as exportações em uma das duas unidades"),
                            p(strong("Ano"), ": Selecione o ano desejado ou 'Todos' para visualizar o período completo de 2011 a 2020."),
                            p(strong("Estado"), ": Escolha o estado que deseja analisar para ver a distribuição das suas exportações.")
                            
                          ),
                          status = "primary",
                          closable = FALSE,
                          maximizable = FALSE,
                          collapsible = TRUE,
                          width = 12,
                          highchartOutput("sunburst_state", height = 500)
                        )
                      )
                    ) 
                  )
                ),
                
                ### Países ----
                tabPanel(
                  "Países",
                  
                  controlbarMenu(
                    id = "controlbarmenu_countries",
                    vertical = TRUE,
                    type = "pills",
                    side = "left",
                    
                    #### Racebar ----
                    # controlbarItem(
                    #   title = "Evolução no Tempo",
                    #   
                    #   fluidRow(
                    #     box(
                    #       title = "Top 10 Países Destinatários de Exportações (2011 - 2020)",
                    #       sidebar = boxSidebar(
                    #         icon = icon("question"),
                    #         id = "info_racebar_countries",
                    #         background = "#3d9970",
                    #         h5("O gráfico de ", strong("barras"), " mostra o ranking dos 10 principais destinos de exportações brasileiras no período de 2011 à 2020."),
                    #         h5("Como utilizar?"),
                    #         p(strong("Métrica"), ": Escolha entre 'Valor' (US$) ou 'Peso' (Toneladas) para visualizar as exportações em uma das duas unidades"),
                    #         p(strong("Atividade Econômica"), ": Selecione uma das atividades econômicas para filtrar as exportações com base no setor")
                    #       ),
                    #       status = "primary",
                    #       closable = FALSE,
                    #       maximizable = FALSE,
                    #       collapsible = TRUE,
                    #       width = 12,
                    #       imageOutput("bar_race", height = 500)
                    #     )
                    #   )
                    # ),
                    
                    #### Treemap ----
                    controlbarItem(
                      title = "Treemap",
                      
                      fluidRow(
                        column(
                          width = 3,
                          radioButtons(
                            inputId = "value_treemap_countries",
                            label = "Métrica:",
                            choices = list("Valor ($)" = "value", "Peso (Toneladas)" = "weight"),
                            selected = "value",
                            inline = TRUE
                          )
                        ),
                        column(
                          width = 3,
                          selectInput(
                            inputId = "year_treemap_countries", 
                            label = "Ano:",
                            choices = c("Todos", sort(2011:2020, decreasing = TRUE)),
                            # selected = 2020,
                            width = '100%'
                          )
                        ),
                        column(
                          width = 6,
                          selectInput(
                            inputId = "product_treemap_countries",
                            label = "Atividade Econômica:",
                            choices = c(
                              "Todos",
                              "Agropecuária",
                              "Indústria Extrativa",
                              "Indústria de Transformação",
                              "Outros Produtos"
                            ),
                            selected = "Todos",
                            width = '100%'
                          )
                        )
                      ),
                      fluidRow(
                        box(
                          title = "Exportações por País, Categoria e Produto",
                          sidebar = boxSidebar(
                            icon = icon("question"),
                            id = "info_treemap_countries",
                            background = "#3d9970",
                            h5("O ", strong("mapa de Árvore"), " apresenta os dados hierárquicos, exibindo as exportações brasileiras organizadas por Bloco Econômico e País."),
                            h5("Como utilizar?"),
                            p(strong("Métrica"), ": Escolha entre 'Valor' (US$) ou 'Peso' (Toneladas) para visualizar as exportações em uma das duas unidades"),
                            p(strong("Ano"), ": Selecione o ano desejado ou 'Todos' para visualizar o período completo de 2011 a 2020."),
                            p(strong("País"), ": Escolha o país que deseja analisar para ver a distribuição das suas exportações.")
                          ),
                          status = "primary",
                          closable = FALSE,
                          maximizable = FALSE,
                          collapsible = TRUE,
                          width = 12,
                          highchartOutput("treemap_countries", height = 500)
                        )
                      )
                    ),
                    
                    #### Gráfico de Bolhas ----
                    controlbarItem(
                      title = "Gráfico de Bolhas",
                      
                      fluidRow(
                        column(
                          width = 3,
                          radioButtons(
                            inputId = "value_bubble_countries",
                            label = "Métrica:",
                            choices = list("Valor ($)" = "value", "Peso (Toneladas)" = "weight"),
                            selected = "value",
                            inline = TRUE
                          )
                        ),
                        column(
                          width = 3,
                          selectInput(
                            inputId = "year_bubble_countries", 
                            label = "Ano:",
                            choices = c("Todos", sort(2011:2020, decreasing = TRUE)),
                            # selected = 2020,
                            width = '100%'
                          )
                        ),
                        column(
                          width = 6,
                          selectInput(
                            inputId = "product_bubble_countries",
                            label = "Atividade Econômica:",
                            choices = c("Todos", 
                                        "Agropecuária",
                                        "Indústria Extrativa",
                                        "Indústria de Transformação",
                                        "Outros Produtos"),
                            selected = "Todos",
                            width = '100%'
                          )
                        )
                      ),
                      fluidRow(
                        box(
                          title = "Exportações e Representatividade dos Países",
                          sidebar = boxSidebar(
                            icon = icon("question"),
                            id = "info_bubble_countries",
                            background = "#3d9970",
                            h5("O ", strong("Gráfico de Bolhas"), " mostra a representatividade de cada país em relação ao volume total de exportações brasileiras."),
                            h5("Como utilizar?"),
                            p(strong("Métrica"), ": Escolha entre 'Valor' (US$) ou 'Peso' (Toneladas) para visualizar as exportações em uma das duas unidades"),
                            p(strong("Ano"), ": Selecione o ano desejado ou 'Todos' para visualizar o período completo de 2011 a 2020."),
                            p(strong("Produto"), ": Selecione o produto para detalhar as exportações por estado.")
                            
                          ),
                          status = "primary",
                          closable = FALSE,
                          maximizable = FALSE,
                          collapsible = TRUE,
                          width = 12,
                          highchartOutput("bubble_countries", height = 500)
                        )
                      )
                    )
                    
                  )
                ),
                
                ### Produtos ----
                tabPanel(
                  "Produtos",
                  
                  controlbarMenu(
                    id = "controlbarmenu_products",
                    vertical = TRUE,
                    type = "pills",
                    side = "left",
                    
                    #### Scatterplot ----
                    controlbarItem(
                      title = "Scatterplot",
                      
                      fluidRow(
                        # column(
                        #   width = 3,
                        #   radioButtons(
                        #     inputId = "value_scatterplot_products",
                        #     label = "Métrica:",
                        #     choices = list("Valor ($)" = "value", "Peso (Toneladas)" = "weight"),
                        #     selected = "value",
                        #     inline = TRUE
                        #   )
                        # ),
                        column(
                          width = 3,
                          selectInput(
                            inputId = "year_scatterplot_products", 
                            label = "Ano:",
                            choices = c("Todos", sort(2011:2020, decreasing = TRUE)),
                            # selected = 2020,
                            width = '100%'
                          )
                        ),
                        column(
                          width = 9,
                          selectInput(
                            inputId = "product_scatterplot_products",
                            label = "Atividade Econômica:",
                            choices = c("Todos", 
                                        "Agropecuária",
                                        "Indústria Extrativa",
                                        "Indústria de Transformação",
                                        "Outros Produtos"),
                            selected = "Todos",
                            width = '100%'
                          )
                        )
                      ),
                      fluidRow(
                        box(
                          title = "Relação Valor x Peso Exportado por Município",
                          sidebar = boxSidebar(
                            icon = icon("question"),
                            id = "info_scatterplot_products",
                            background = "#3d9970",
                            h5("O gráfico de ", strong("Dispersão"), " mostra a relação entre o valor e peso exportado para diferentes atividade econômicas em uma dimensão como um conjunto de pontos, categorizados por estado."),
                            h5("Como utilizar?"),
                            # p(strong("Métrica"), ": Escolha entre 'Valor' (US$) ou 'Peso' (Toneladas) para visualizar as exportações em uma das duas unidades"),
                            p(strong("Ano"), ": Selecione o ano desejado ou 'Todos' para visualizar o período completo de 2011 a 2020."),
                            p(strong("Atividade Econômica"), ": Selecione uma das atividades econômicas para filtrar as exportações com base no setor")
                          ),
                          status = "primary",
                          closable = FALSE,
                          maximizable = FALSE,
                          collapsible = TRUE,
                          width = 12,
                          highchartOutput("scatterplot_products", height = 500)
                        )
                      )
                    ),
                    
                    #### Área ----
                    controlbarItem(
                      title = "Área",
                      
                      fluidRow(
                        column(
                          width = 3,
                          radioButtons(
                            inputId = "value_area_products",
                            label = "Métrica:",
                            choices = list("Valor ($)" = "value", "Peso (Toneladas)" = "weight"),
                            selected = "value",
                            inline = TRUE
                          )
                        ),
                        column(
                          width = 4,
                          selectizeInput(
                            inputId = "state_area_products", 
                            label = "Estado:",
                            choices = NULL,
                            selected = "Todos",
                            options = list(placeholder = 'Selecione um estado'),
                            multiple = FALSE
                          )
                        ),
                        column(
                          width = 5,
                          selectizeInput(
                            inputId = "country_area_products", 
                            label = "País:",
                            choices = NULL,
                            selected = "Todos",
                            options = list(placeholder = 'Selecione um país'),
                            multiple = FALSE,
                            width = '100%'
                          )
                        )
                      ),
                      fluidRow(
                        box(
                          title = "Evolução das Exportações por Atividade Econômica",
                          sidebar = boxSidebar(
                            icon = icon("question"),
                            id = "info_area_products",
                            background = "#3d9970",
                            h5("O gráfico de ", strong("Área"), " exibe a evolução das exportações ao longo dos anos, segmentadas por atividade econômica."),
                            h5("Como utilizar?"),
                            p(strong("Métrica"), ": Escolha entre 'Valor' (US$) ou 'Peso' (Toneladas) para visualizar as exportações em uma das duas unidades"),
                            p(strong("Estado"), ": Escolha o estado que deseja analisar para ver a distribuição das suas exportações."),
                            p(strong("País"), ": Escolha o país que deseja analisar para ver a distribuição das suas exportações.")
                          ),
                          status = "primary",
                          closable = FALSE,
                          maximizable = FALSE,
                          collapsible = TRUE,
                          width = 12,
                          highchartOutput("area_products", height = 500)
                        )
                      )
                    )
                    
                    #### Dependency Wheel ----
                    # controlbarItem(
                    #   title = "Gráfico de Roda",
                    #   
                    #   fluidRow(
                    #     column(
                    #       width = 3,
                    #       radioButtons(
                    #         inputId = "value_wheel_products",
                    #         label = "Métrica:",
                    #         choices = list("Valor ($)" = "value", "Peso (Toneladas)" = "weight"),
                    #         selected = "value",
                    #         inline = TRUE
                    #       )
                    #     ),
                    #     column(
                    #       width = 3,
                    #       selectInput(
                    #         inputId = "year_wheel_products", 
                    #         label = "Ano:",
                    #         choices = c("Todos", sort(2011:2020, decreasing = TRUE)),
                    #         selected = 2020,
                    #         width = '100%'
                    #       )
                    #     ),
                    #     # column(
                    #     #   width = 7,
                    #     #   selectInput(
                    #     #     inputId = "product_wheel_products",
                    #     #     label = "Produto:", 
                    #     #     choices = "aaaa", 
                    #     #     selected = "aaaa",
                    #     #     width = '100%'
                    #     #   )
                    #     # )
                    #   ),
                    #   fluidRow(
                    #     box(
                    #       title = "Relações de Exportações entre Atividade Econômica e Regiões",
                    #       sidebar = boxSidebar(
                    #         icon = icon("question"),
                    #         id = "info_wheel_products_1",
                    #         background = "#3d9970",
                    #         h5("O gráfico de ", strong("Roda"), " visualiza as conexões entre as atividades econômicas e as regiões ou blocos econômicos que recebem os produtos exportados."),
                    #         h5("Como utilizar?"),
                    #         p(strong("Métrica"), ": Escolha entre 'Valor' (US$) ou 'Peso' (Toneladas) para visualizar as exportações em uma das duas unidades"),
                    #         p(strong("Ano"), ": Selecione o ano desejado ou 'Todos' para visualizar o período completo de 2011 a 2020.")
                    #       ),
                    #       status = "primary",
                    #       closable = FALSE,
                    #       maximizable = FALSE,
                    #       collapsible = TRUE,
                    #       width = 12,
                    #       highchartOutput("wheel_products_1", height = 500)
                    #     )
                    #     # box(
                    #     #   title = "Relações de Exportações entre Atividade Econômica e Blocos",
                    #     #   sidebar = boxSidebar(
                    #     #     icon = icon("question"),
                    #     #     id = "info_wheel_products_2",
                    #     #     background = "#3d9970",
                    #     #     h5("O ", strong("mapa de conexão"), " mostra os principais destinos de exportações por cidade."),
                    #     #     h5("Como utilizar?"),
                    #     #     p(strong("Métrica"), ": Escolha entre 'Valor' (US$) ou 'Peso' (Toneladas) para visualizar as exportações em uma das duas unidades"),
                    #     #     p(strong("Ano"), ": Selecione o ano desejado ou 'Todos' para visualizar o período completo de 2011 a 2020.")
                    #     #     
                    #     #   ),
                    #     #   status = "primary",
                    #     #   closable = FALSE,
                    #     #   maximizable = FALSE,
                    #     #   collapsible = TRUE,
                    #     #   width = 6,
                    #     #   highchartOutput("wheel_products_2", height = 500)
                    #     # )
                    #   )
                    # )
                    
                  )
                ),
                
                ### Comparativo ----
                tabPanel(
                  "Comparativo",
                  
                  controlbarMenu(
                    id = "controlbarmenu_analytics",
                    vertical = TRUE,
                    type = "pills",
                    side = "left",
                    
                    #### por Cidade ----
                    controlbarItem(
                      title = "por Cidade",

                      fluidRow(
                        column(
                          width = 3,
                          radioButtons(
                            inputId = "value_city_analytics",
                            label = "Métrica:",
                            choices = list("Valor ($)" = "value", "Peso (Toneladas)" = "weight"),
                            selected = "value",
                            inline = TRUE
                          )
                        ),
                        column(
                          width = 9,
                          selectInput(
                            inputId = "city_city_analytics",
                            label = "Cidades:",
                            choices = NULL,
                            multiple = TRUE,
                            # options = list(title = "Selecione uma ou mais cidades"),
                            # options = list(placeholder = 'Selecione uma cidade'),
                            width = '100%'
                          )
                        )
                      ),
                      fluidRow(
                        box(
                          title = "Comparativo Temporal de Exportações por Município",
                          sidebar = boxSidebar(
                            icon = icon("question"),
                            id = "info_city_analytics",
                            background = "#3d9970",
                            h5("O gráfico de ", strong("linhas temporais"), " compara as exportações entre diferentes municípios ao longo do tempo."),
                            h5("Como utilizar?"),
                            p(strong("Métrica"), ": Escolha entre 'Valor' (US$) ou 'Peso' (Toneladas) para visualizar as exportações em uma das duas unidades"),
                            p(strong("Cidade"), ": Selecione um ou mais municípios para comparar suas exportações.")
                          ),
                          status = "primary",
                          closable = FALSE,
                          maximizable = FALSE,
                          collapsible = TRUE,
                          width = 12,
                          highchartOutput("city_analytics", height = 500)
                        )
                      )
                    ),
                    
                    #### por Estado ----
                    controlbarItem(
                      title = "por Estado",
                      
                      fluidRow(
                        column(
                          width = 3,
                          radioButtons(
                            inputId = "value_state_analytics",
                            label = "Métrica:",
                            choices = list("Valor ($)" = "value", "Peso (Toneladas)" = "weight"),
                            selected = "value",
                            inline = TRUE
                          )
                        ),
                        column(
                          width = 9,
                          selectInput(
                            inputId = "state_state_analytics",
                            label = "Estados:",
                            choices = NULL,
                            multiple = TRUE,
                            # options = list(title = "Selecione uma ou mais cidades"),
                            # options = list(placeholder = 'Selecione uma cidade'),
                            width = '100%'
                          )
                        )
                      ),
                      fluidRow(
                        box(
                          title = "Comparativo Temporal de Exportações por Estado",
                          sidebar = boxSidebar(
                            icon = icon("question"),
                            id = "info_state_analytics",
                            background = "#3d9970",
                            h5("O gráfico de ", strong("linhas temporais"), " compara as exportações entre diferentes estados ao longo do tempo."),
                            h5("Como utilizar?"),
                            p(strong("Métrica"), ": Escolha entre 'Valor' (US$) ou 'Peso' (Toneladas) para visualizar as exportações em uma das duas unidades"),
                            p(strong("Estado"), ": Selecione um ou mais estados para comparar suas exportações.")
                          ),
                          status = "primary",
                          closable = FALSE,
                          maximizable = FALSE,
                          collapsible = TRUE,
                          width = 12,
                          highchartOutput("state_analytics", height = 500)
                        )
                      )
                    ),
                    
                    #### por País ----
                    controlbarItem(
                      title = "por País",
                      
                      fluidRow(
                        column(
                          width = 3,
                          radioButtons(
                            inputId = "value_country_analytics",
                            label = "Métrica:",
                            choices = list("Valor ($)" = "value", "Peso (Toneladas)" = "weight"),
                            selected = "value",
                            inline = TRUE
                          )
                        ),
                        column(
                          width = 9,
                          selectInput(
                            inputId = "country_country_analytics",
                            label = "País:",
                            choices = NULL,
                            multiple = TRUE,
                            # options = list(title = "Selecione uma ou mais cidades"),
                            # options = list(placeholder = 'Selecione uma cidade'),
                            width = '100%'
                          )
                        )
                      ),
                      fluidRow(
                        box(
                          title = "Comparativo Temporal de Exportações por País",
                          sidebar = boxSidebar(
                            icon = icon("question"),
                            id = "info_country_analytics",
                            background = "#3d9970",
                            h5("O gráfico de ", strong("linhas temporais"), " compara as exportações entre diferentes países ao longo do tempo."),
                            h5("Como utilizar?"),
                            p(strong("Métrica"), ": Escolha entre 'Valor' (US$) ou 'Peso' (Toneladas) para visualizar as exportações em uma das duas unidades"),
                            p(strong("País"), ": Selecione um ou mais países para comparar suas exportações.")
                          ),
                          status = "primary",
                          closable = FALSE,
                          maximizable = FALSE,
                          collapsible = TRUE,
                          width = 12,
                          highchartOutput("country_analytics", height = 500)
                        )
                      )
                    )
                    
                  )
                ),
                
                ### Análise ----
                tabPanel(
                  "Análise",

                  controlbarMenu(
                    id = "controlbarmenu_analytics2",
                    vertical = TRUE,
                    type = "pills",
                    side = "left",

                    #### Heatmap ----
                    # controlbarItem(
                    #   title = "Mapa do Calor",
                    #
                    #   fluidRow(
                    #     column(
                    #       width = 3,
                    #       radioButtons(
                    #         inputId = "value_heatmap_analytics2",
                    #         label = "Métrica:",
                    #         choices = list("Valor ($)" = "value", "Peso (Toneladas)" = "weight"),
                    #         selected = "value",
                    #         inline = TRUE
                    #       )
                    #     ),
                    #     column(
                    #       width = 3,
                    #       selectInput(
                    #         inputId = "year_heatmap_analytics2",
                    #         label = "Ano:",
                    #         choices = c("Todos", sort(2011:2020, decreasing = TRUE)),
                    #         selected = 2020,
                    #         width = '100%'
                    #       )
                    #     ),
                    #     column(
                    #       width = 4,
                    #       selectInput(
                    #         inputId = "isic_heatmap_analytics2",
                    #         label = "Atividade Econômica:",
                    #         choices = c("Todos",
                    #                     "Agropecuária",
                    #                     "Indústria Extrativa",
                    #                     "Indústria de Transformação",
                    #                     "Outros Produtos"),
                    #         selected = "Todos",
                    #         width = '100%'
                    #       )
                    #     )
                    #   ),
                    #   fluidRow(
                    #     box(
                    #       title = "Mapa Exportação por Estado",
                    #       sidebar = boxSidebar(
                    #         icon = icon("question"),
                    #         id = "info_heatmap_analytics2",
                    #         background = "#3d9970",
                    #         h5("O ", strong("Mapa de Calor"), " mostra a variação das exportações ao longo do ano, identificando padrões trimestrais."),
                    #         h5("Como utilizar?"),
                    #         p(strong("Métrica"), ": Escolha entre 'Valor' (US$) ou 'Peso' (Toneladas) para visualizar as exportações em uma das duas unidades"),
                    #         p(strong("Ano"), ": Selecione o ano desejado ou 'Todos' para visualizar o período completo de 2011 a 2020."),
                    #         p(strong("Atividade Econômica"), ": Selecione uma das atividades econômicas para filtrar as exportações com base no setor")
                    #       ),
                    #       status = "primary",
                    #       closable = FALSE,
                    #       maximizable = FALSE,
                    #       collapsible = TRUE,
                    #       width = 12,
                    #       highchartOutput("heatmap_analytics2", height = 500)
                    #     )
                    #   )
                    # ),

                    #### Distribuição ----
                    controlbarItem(
                      title = "Distribuição de Exportações",

                      fluidRow(
                        column(
                          width = 3,
                          radioButtons(
                            inputId = "value_pareto_analytics2",
                            label = "Métrica:",
                            choices = list("Valor ($)" = "value", "Peso (Toneladas)" = "weight"),
                            selected = "value",
                            inline = TRUE
                          )
                        ),
                        column(
                          width = 3,
                          selectInput(
                            inputId = "year_pareto_analytics2",
                            label = "Ano:",
                            choices = c("Todos", sort(2011:2020, decreasing = TRUE)),
                            selected = 2020,
                            width = '100%'
                          )
                        ),
                        column(
                          width = 6,
                          selectInput(
                            inputId = "states_pareto_analytics2",
                            label = "Estado:",
                            choices = NULL,
                            multiple = FALSE,
                            # options = list(title = "Selecione uma ou mais cidades"),
                            # options = list(placeholder = 'Selecione uma cidade'),
                            width = '100%'
                          )
                        )
                      ),
                      fluidRow(
                        box(
                          title = "Distribuição Acumulada das Exportações",
                          sidebar = boxSidebar(
                            icon = icon("question"),
                            id = "info_pareto_analytics2",
                            background = "#3d9970",
                            h5("O gráfico de ", strong("Pareto"), " revela a concentração das exportações, destacando os principais contribuintes do maior para o menor.
                               <br> Este gráfico é uma ferramenta da análise e controle de qualidade dos processos."),
                            h5("Como utilizar?"),
                            p(strong("Métrica"), ": Escolha entre 'Valor' (US$) ou 'Peso' (Toneladas) para visualizar as exportações em uma das duas unidades"),
                            p(strong("Ano"), ": Selecione o ano desejado ou 'Todos' para visualizar o período completo de 2011 a 2020."),
                            p(strong("Estado"), ": Selecione o estado de interesse para analisar a distribuição interna de suas exportações.")

                          ),
                          status = "primary",
                          closable = FALSE,
                          maximizable = FALSE,
                          collapsible = TRUE,
                          width = 12,
                          highchartOutput("pareto_analytics2", height = 500)
                        )
                      )
                    ),

                    #### Gráfico de Radar ----
                    controlbarItem(
                      title = "Gráfico de Radar",

                      fluidRow(
                        column(
                          width = 3,
                          radioButtons(
                            inputId = "value_radar_analytics2",
                            label = "Métrica:",
                            choices = list("Valor ($)" = "value",
                                           "Peso (Toneladas)" = "weight"),
                            selected = "value",
                            inline = TRUE
                          )
                        ),
                        column(
                          width = 9,
                          selectInput(
                            inputId = "sh2_radar_analytics2",
                            label = "Produto:",
                            choices = NULL,
                            multiple = FALSE,
                            # options = list(title = "Selecione uma ou mais cidades"),
                            # options = list(placeholder = 'Selecione uma cidade'),
                            width = '100%'
                          )
                        )
                      ),
                      fluidRow(
                        box(
                          title = "Análise de Anual dos Produtos por Região",
                          sidebar = boxSidebar(
                            icon = icon("question"),
                            id = "info_radar_analytics2",
                            background = "#3d9970",
                            h5("O ", strong("Gráfico de Radar"), " permite a comparação de valores e medir variações,
                                         identificando discrepâncias, semelhanças e o desempenho dos dados."),
                            h5("Como utilizar?"),
                            p(strong("Métrica"), ": Escolha entre 'Valor' (US$) ou 'Peso' (Toneladas) para visualizar as exportações em uma das duas unidades"),
                            p(strong("Produto"), ": Selecione o produto para detalhar as exportações por região.")

                          ),
                          status = "primary",
                          closable = FALSE,
                          # maximizable = FALSE,
                          collapsible = TRUE,
                          width = 12,
                          highchartOutput("radar_analytics2", height = 500)
                        )
                      )
                    )

                  )
                )
                
    )
  )
}