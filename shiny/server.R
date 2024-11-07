
shinyServer(function(input, output, session) {
  
  # Configurações Iniciais ----
  
  ## Filtros ----
  filter_data <-
    function(data,
             year_filter = NULL,
             region_filter = NULL,
             block_filter = NULL,
             product_filter = NULL,
             city_filter = NULL,
             state_filter = NULL,
             country_filter = NULL) {
      if (!is.null(year_filter) &&
          year_filter != "Todos" && !is.na(year_filter)) {
        data <- data %>% filter(Ano == as.numeric(year_filter))
      }
      
      if (!is.null(region_filter) &&
          region_filter != "Todos" && !is.na(region_filter)) {
        data <- data %>% filter(Nome_Regiao == region_filter)
      }
      
      if (!is.null(block_filter) &&
          block_filter != "Todos" && !is.na(block_filter)) {
        data <- data %>% filter(Nome_Bloco == block_filter)
      }
      
      if (!is.null(product_filter) &&
          product_filter != "Todos" && !is.na(product_filter)) {
        data <- data %>% filter(Nome_ISIC_Secao == product_filter)
      }
      
      if (!is.null(city_filter) &&
          city_filter != "Todos" && !is.na(city_filter)) {
        data <- data %>% filter(Cidade == city_filter)
      }
      
      if (!is.null(state_filter) &&
          state_filter != "Todos" && !is.na(state_filter)) {
        data <- data %>% filter(Nome_Estado == state_filter)
      }
      
      if (!is.null(country_filter) &&
          country_filter != "Todos" && !is.na(country_filter)) {
        data <- data %>% filter(Nome_Pais == country_filter)
      }
      
      return(data)
    }
  
  ## Reatividade entre cards ----
  
  ### Ano ----
  selected_year <- reactiveVal("Todos")
  
  # Observadores para sincronizar o ano entre as páginas
  observeEvent(input$year_filter_overview, {
    selected_year(input$year_filter_overview)
  })
  
  observeEvent(input$year_filter_region, {
    selected_year(input$year_filter_region)
  })
  
  observeEvent(input$year_filter_export, {
    selected_year(input$year_filter_export)
  })
  
  observeEvent(input$year_filter_product, {
    selected_year(input$year_filter_product)
  })
  
  observeEvent(input$year_conection_city, {
    selected_year(input$year_conection_city)
  })
  
  observeEvent(input$year_sankey_city, {
    selected_year(input$year_sankey_city)
  })
  
  observeEvent(input$year_cloropletic_state, {
    selected_year(input$year_cloropletic_state)
  })
  
  observeEvent(input$year_sunburst_state, {
    selected_year(input$year_sunburst_state)
  })
  
  observeEvent(input$year_treemap_countries, {
    selected_year(input$year_treemap_countries)
  })
  
  observeEvent(input$year_bubble_countries, {
    selected_year(input$year_bubble_countries)
  })
  
  observeEvent(input$year_scatterplot_products, {
    selected_year(input$year_scatterplot_products)
  })
  
  observeEvent(input$year_bubble_countries, {
    selected_year(input$year_wheel_products)
  })
  
  # Sincronize os inputs de ano em ambos os filtros quando `selected_year` mudar
  observe({
    updateSelectInput(session, "year_filter_overview", selected = selected_year())
    updateSelectInput(session, "year_filter_region", selected = selected_year())
    updateSelectInput(session, "year_filter_export", selected = selected_year())
    updateSelectInput(session, "year_filter_product", selected = selected_year())
    updateSelectInput(session, "year_conection_city", selected = selected_year())
    updateSelectInput(session, "year_sankey_city", selected = selected_year())
    updateSelectInput(session, "year_cloropletic_state", selected = selected_year())
    updateSelectInput(session, "year_sunburst_state", selected = selected_year())
    updateSelectInput(session, "year_treemap_countries", selected = selected_year())
    updateSelectInput(session, "year_bubble_countries", selected = selected_year())
    updateSelectInput(session, "year_scatterplot_products", selected = selected_year())
    updateSelectInput(session, "year_wheel_products", selected = selected_year())
  })
  
  # Página 1 ----
  
  ## Filtros
  filtered_overview <- reactive({
    filter_data(data1, year_filter = selected_year())
  })
  
  ## Indicadores ----
  
  ### Valor US$ 
  output$KPI_VALUE <- renderUI({
    formatted_value <-
      format_functions$value(sum(filtered_overview()$US_FOB, na.rm = TRUE))
    HTML(paste0('<span>', formatted_value, '</span>'))
  })
  
  ### Peso Líquido em Toneladas 
  output$KPI_WEIGHT <- renderUI({
    formatted_weight <-
      format_functions$weight(sum(filtered_overview()$Peso_Liquido, na.rm = TRUE))
    HTML(paste0('<span>', formatted_weight, '</span>'))
  })
  
  ### Quantidade Cidades
  output$KPI_CITIES <- renderUI({
    n_distinct(filtered_overview()$CO_Municipio, na.rm = TRUE)
  })
  
  ### Quantidade Destinos
  output$KPI_COUNTRIES <- renderUI({
    n_distinct(filtered_overview()$CO_Pais_ISOA3, na.rm = TRUE)
  })
  
  ## Gráficos ----
  
  ### Gráfico de Exportações ----
  
  #### por Valor ----
  output$line_chart_region <- renderHighchart({
    generate_region_evolution_plot(g_111a, input$measure_select_region)
  })
  
  #### por Peso ----
  output$line_chart_block <- renderHighchart({
    generate_region_evolution_plot(g_111b, input$measure_select_block)
  })
  
  ### Mapa de exportações ----
  output$map <- renderHighchart({
    generate_export_map(g_121)
  })
  
  # Página 2 ----
  
  ## Indicadores ----
  
  ### Filtros
  filtered_kpi_region <- reactive({
    filter_data(
      data2,
      year_filter = selected_year(),
      region_filter = input$region_filter
    )
  })
  
  ### Valor em US$ 
  output$KPI_VALUE_REGION <- renderUI({
    total_value <- sum(filtered_kpi_region()$US_FOB, na.rm = TRUE)
    formatted_value <- format_functions$value(total_value)
    HTML(paste0('<span>', formatted_value, '</span>'))
  })
  
  ### Peso Líquido em Toneladas 
  output$KPI_WEIGHT_REGION <- renderUI({
    total_weight <- sum(filtered_kpi_region()$Peso_Liquido, na.rm = TRUE)
    formatted_weight <- format_functions$weight(total_weight)
    HTML(paste0('<span>', formatted_weight, '</span>'))
  })
  
  ## Gráficos ----
  
  ### Gráfico Wheelchair ----
  
  filtered_wheel_region <- reactive({
    filter_data(g_211,
                year_filter = input$year_filter_region,
                region_filter = input$region_filter)
  })
  
  output$wheel_region <- renderHighchart({
    generate_wheel_region(
      data = filtered_wheel_region(),
      measure = input$measure_filter_region,
      year_filter = input$year_filter_region,
      region_filter = input$region_filter
    )
  })
  
  ### Gráfico de Rosca ----
  
  #### Filtros para Gráfico de Rosca
  filtered_donut_region <- reactive({
    filter_data(g_212,
                year_filter = input$year_filter_region,
                region_filter = input$region_filter)
  })
  
  output$donut_region <- renderHighchart({
    generate_donut_region(
      data = filtered_donut_region(),
      measure = input$measure_filter_region,
      year_filter = input$year_filter_region,
      region_filter = input$region_filter
    )
  })
  
  # Página 3 ----
  
  ## Indicadores ----
  
  ## Fitros 
  filtered_kpi_export <- reactive({
    filter_data(
      data3,
      year_filter = selected_year(),
      block_filter = input$block_filter
    )
  })
  
  ### Valor em US$
  output$KPI_VALUE_EXPORT <- renderUI({
    total_value <- sum(filtered_kpi_export()$US_FOB, na.rm = TRUE)
    formatted_value <- format_functions$value(total_value)
    HTML(paste0('<span>', formatted_value, '</span>'))
  })
  
  ### Peso Líquido em Toneladas
  output$KPI_WEIGHT_EXPORT <- renderUI({
    total_weight <- sum(filtered_kpi_export()$Peso_Liquido, na.rm = TRUE)
    formatted_weight <- format_functions$weight(total_weight)
    HTML(paste0('<span>', formatted_weight, '</span>'))
  })
  
  ### Destinos
  output$KPI_DESTINATION_EXPORT <- renderUI({
    total_destinations <- n_distinct(filtered_kpi_export()$Nome_Pais)
    HTML(paste0('<span>', total_destinations, '</span>'))
  })
  
  ## Gráficos ----
  
  ### Gráfico de barras ----
  
  #### Filtros
  filtered_bar_export <- reactive({
    filter_data(g_311,
                year_filter = input$year_filter_export, 
                block_filter = input$block_filter)
  })
    
  output$bar_export <- renderHighchart({
    generate_bar_export(
      data = filtered_bar_export(),
      measure = input$measure_select_export,
      year_filter = input$year_filter_export,
      block_filter = input$block_filter)
  })
  
  ### Gráfico de Rosca ----
  filtered_donut_export<- reactive({
    filter_data(g_312,
                year_filter = input$year_filter_export,
                block_filter = input$block_filter)
  })
  
  output$donut_export <- renderHighchart({
    generate_donut_export(
      data = filtered_donut_export(),
      measure = input$measure_select_export,
      year_filter = input$year_filter_export,
      block_filter = input$block_filter
    )
  })
  
  # Página 4 ----
  
  ## Indicadores ----
  
  ### Filtros
  filtered_kpi_product <- reactive({
    filter_data(
      data4,
      year_filter = selected_year(),
      product_filter = input$product_filter
    )
  })
  
  ### Valor em US$
  output$KPI_VALUE_PRODUCT <- renderUI({
    total_value <- sum(filtered_kpi_product()$US_FOB, na.rm = TRUE)
    formatted_value <- format_functions$value(total_value)
    HTML(paste0('<span>', formatted_value, '</span>'))
  })
  
  ### Peso Líquido em Toneladas
  output$KPI_WEIGHT_PRODUCT <- renderUI({
    total_weight <- sum(filtered_kpi_product()$Peso_Liquido, na.rm = TRUE)
    formatted_weight <- format_functions$weight(total_weight)
    HTML(paste0('<span>', formatted_weight, '</span>'))
  })
  
  ### Preço Médio por Tonelada
  output$KPI_PRICE_PRODUCT <- renderUI({
    total_value <- sum(filtered_kpi_product()$US_FOB, na.rm = TRUE)
    total_weight <- sum(filtered_kpi_product()$Peso_Liquido, na.rm = TRUE)
    price_ton <- total_value / total_weight
    formatted_price <- format_functions$price(price_ton)
    HTML(paste0('<span>', formatted_price, '</span>'))
  })
  
  ## Gráficos ----
  
  ### Gráfico de barras ----
  
  #### por regiao ----
  
  #### Filtros
  filtered_bar_product <- reactive({
    filter_data(
      g_411,
      year_filter = input$year_filter_product,
      product_filter = input$product_filter
    )
  })
  
  output$bar_product_region <- renderHighchart({
    generate_bar_product(
      data = filtered_bar_product(),
      measure = input$measure_select_product,
      year = input$year_filter_product,
      product_filter = input$product_filter
    )
  })
  
  #### por bloco ----
  
  #### Filtros
  filtered_bar_product_block <- reactive({
    filter_data(
      g_412,
      year_filter = input$year_filter_product,
      product_filter = input$product_filter
    )
  })
  
  output$bar_product_block <- renderHighchart({
    generate_bar_product(
      data = filtered_bar_product_block(),
      measure = input$measure_select_product,
      year = input$year_filter_product,
      product_filter = input$product_filter
    )
  })
  
  ### Gráfico Treemap ----
  
  #### Filtros
  filtered_treemap_product <- reactive({
    filter_data(
      g_413,
      year_filter = input$year_filter_product)
  })
  
  output$treemap_product <- renderHighchart({
    generate_treemap_product(
      data = filtered_treemap_product(),
      measure = input$measure_select_product,
      year = input$year_filter_product
    )
  })
  
  ## Página 5 ----
  
  ### Municípios ----
  
  #### Mapa Cloroplético ----
  
  #### Mapa de Conexão ----
  filtered_conection_city <- reactive({
    req(input$year_conection_city, input$town_conection_city, input$value_conection_city)
    
    filter_data(
      g_512,
      year_filter = input$year_conection_city,
      city_filter = input$town_conection_city
    )
  })
  
  # Renderização do gráfico de conexão
  output$conection_city <- renderHighchart({
    data_filtered <- filtered_conection_city()
    
    generate_conection_city(
      data_filtered,
      measure = input$value_conection_city,
      year = input$year_conection_city,
      city = input$town_conection_city
    )
  })
  
  # Atualiza as opções de cidade para `town_conection_city`
  observe({
    unique_city <- unique(g_512$Cidade)
    
    updateSelectizeInput(
      session,
      inputId = "town_conection_city",
      choices = unique_city,
      selected = "",
      server = TRUE
    )
  })
  
  #### Sincronização das Cidades 
  observeEvent(input$town_conection_city, {
    updateSelectizeInput(
      session,
      inputId = "town_sankey_city",
      selected = input$town_conection_city
    )
  })
  
  observeEvent(input$town_sankey_city, {
    updateSelectizeInput(
      session,
      inputId = "town_conection_city",
      selected = input$town_sankey_city
    )
  })
  
  #### Sankey ----
  filtered_sankey_city <- reactive({
    req(input$year_sankey_city, input$town_sankey_city, input$value_sankey_city)
    
    filter_data(
      g_513,
      year_filter = input$year_sankey_city,
      city_filter = input$town_sankey_city
    )
  })
  
  # Renderização do gráfico Sankey
  output$sankey_city <- renderHighchart({
    generate_sankey_city(
      data = filtered_sankey_city(),
      measure = input$value_sankey_city,
      year = input$year_sankey_city,
      city = input$town_sankey_city
    )
  })
  
  # Atualiza as opções de cidades para `town_sankey_city`
  observe({
    unique_cities <- unique(g_513$Cidade)
    
    updateSelectizeInput(
      session,
      inputId = "town_sankey_city",
      choices = unique_cities,
      selected = "",
      server = TRUE
    )
  })
  
  ### Estados ----
  
  #### Mapa Cloroplético ----
  filtered_cloropletic_state <- reactive({
    req(
      input$year_cloropletic_state,
      input$product_cloropletic_state,
      input$value_cloropletic_state
    )
    
    filter_data(
      g_521,
      year_filter = input$year_cloropletic_state,
      product_filter = input$product_cloropletic_state
    )
  })
  
  # Renderização do gráfico
  output$cloropletic_state <- renderHighchart({
    generate_cloropletic_state(
      data = filtered_cloropletic_state(),
      measure = input$value_cloropletic_state,
      year = input$year_cloropletic_state,
      product_filter = input$product_cloropletic_state
    )
  })
  
  #### Sunburst ----
  filtered_sunburst_state <- reactive({
    req(
      input$year_sunburst_state,
      input$state_sunburst_state,
      input$value_sunburst_state
    )
    
    filter_data(
      g_522,
      year_filter = input$year_sunburst_state,
      state_filter = input$state_sunburst_state
    )
  })
  
  # Renderização do gráfico
  output$sunburst_state <- renderHighchart({
    generate_sunburst_state(
      data = filtered_sunburst_state(),
      measure = input$value_sunburst_state,
      year = input$year_sunburst_state,
      state_filter = input$state_sunburst_state
    )
  })
  
  # Filtro de seleção de estado
  observe({
    unique_states <- unique(g_522$Nome_Estado)
    
    updateSelectizeInput(
      session,
      inputId = "state_sunburst_state",
      choices = unique_states,
      selected = "",
      server = TRUE
    )
  })
  
  
  #### Arvore Hierarquica ----
  # filtered_data_tree_state <- reactive({
  #   req(input$year_telegraph_state, input$value_telegraph_state)
  #   
  #   data_filtered <- g_523  
  #   
  #   if (input$year_telegraph_state != "Todos") {
  #     data_filtered <- data_filtered %>%
  #       filter(Ano == input$year_telegraph_state)
  #   }
  #   
  #   return(data_filtered)
  # })
  # 
  # 
  # output$telegraph_state <- renderHighchart({
  #   data_filtered <- filtered_data_tree_state()
  #   
  #   generate_hierarchical_tree(
  #     data_filtered,
  #     input$value_telegraph_state,
  #     input$year_telegraph_state,
  #     input$product_telegraph_state
  #   )
  # })
  
  
  # # Preencher as opções de estados com valores únicos
  # observe({
  #   unique_products <- unique(g_523$Nome_SH2)
  #   
  #   updateSelectizeInput(
  #     session,
  #     inputId = "product_telegraph_state", 
  #     choices = unique_products,
  #     selected = "",
  #     options = list(placeholder = 'Selecione um produto'),
  #     server = TRUE
  #   )
  # })
  
  ### País ----
  
  # output$racebar <- renderHighchart({
  #   generate_racebar(
  #     g_531,
  #     input$value_racebar,       
  #     input$isic_filter_racebar  
  #   )
  # })
  
  # #### Racebar ----
  # race_data <- reactive({
  #   prepare_race_data(g_531)
  # })
  # 
  # # Gerar e salvar o gráfico como .gif
  # observe({
  #   generate_bar_race(race_data())
  # })
  # 
  # # Renderizar o .gif no UI
  # output$bar_race <- renderImage({
  #   list(
  #     src = "www/bar_race.gif",
  #     contentType = "image/gif",
  #     width = 800,
  #     height = 600
  #   )
  # }, deleteFile = FALSE)

  
  #### Treemap ----
  # Filtros
  filtered_treemap_country <- reactive({
    req(
      input$year_treemap_countries,
      input$product_treemap_countries,
      input$value_treemap_countries
    )
    
    filter_data(
      g_532,
      year_filter = input$year_treemap_countries,
      product_filter = input$product_treemap_countries 
    )
  })
  
  # Renderização do gráfico
  output$treemap_countries <- renderHighchart({
    generate_treemap_country(
      data = filtered_treemap_country(),
      measure = input$value_treemap_countries,
      year = input$year_treemap_countries,
      product_filter = input$product_treemap_countries
    )
  })
  
  #### Packed Bubble ----
  # Filtros
  filtered_bubble_country <- reactive({
    req(
      input$year_bubble_countries,
      input$product_bubble_countries,
      input$value_bubble_countries
    )
    
    filter_data(
      g_533,
      year_filter = input$year_bubble_countries,
      product_filter = input$product_bubble_countries
    )
  })
  
  # Renderização do gráfico
  output$bubble_countries <- renderHighchart({
    generate_bubble_country(
      data = filtered_bubble_country(),
      measure = input$value_bubble_countries,
      year = input$year_bubble_countries,
      product_filter = input$product_bubble_countries
    )
  })
  
  ### por Produto ----
  
  #### Scatterplot ----
  
  # Filtros
  filtered_scatter_product <- reactive({
    req(input$year_scatterplot_products,
        input$product_scatterplot_products)
    
    filter_data(
      g_541,
      year_filter = input$year_scatterplot_products,
      product_filter = input$product_scatterplot_products
    )
  })
  
  # Renderização do gráfico
  output$scatterplot_products <- renderHighchart({
    generate_scatterplot_product(
      data = filtered_scatter_product(),
      year = input$year_scatterplot_products,
      product_filter = input$product_scatterplot_products 
    )
  })
  
  #### areachart ----
  
  # Filtros
  filtered_area_product <- reactive({
    req(
      input$value_area_products,
      input$state_area_products,
      input$country_area_products
    )
    
    filter_data(
      g_542,
      state_filter = input$state_area_products,
      country_filter = input$country_area_products
    )
  })
  
  # Renderização do gráfico
  output$area_products <- renderHighchart({
    generate_area_product(
      data = filtered_area_product(),
      measure = input$value_area_products,
      state_filter = input$state_area_products,
      country_filter = input$country_area_products
    )
  })
  
  # Filtro de seleção de estado e país
  observe({
    unique_states <- unique(g_542$Nome_Estado)
    unique_countries <- unique(g_542$Nome_Pais)
    
    updateSelectizeInput(
      session,
      inputId = "state_area_products",
      choices = c("Todos", unique_states),
      selected = "Todos",
      server = TRUE
    )
    
    updateSelectizeInput(
      session,
      inputId = "country_area_products",
      choices = c("Todos", unique_countries),
      selected = "Todos",
      server = TRUE
    )
  })
  
  # #### Dependecy Wheel ----
  # 
  # ##### por Região 
  # 
  # # Filtros
  # filtered_wheel_region <- reactive({
  #   req(input$year_wheel_products, input$value_wheel_products)
  #   
  #   filter_data(
  #     g_543a,
  #     year_filter = input$year_wheel_products
  #   )
  # })
  # 
  # # Renderização do gráfico
  # output$wheel_products_1 <- renderHighchart({
  #   generate_wheel_region(
  #     data = filtered_wheel_region(),
  #     measure = input$value_wheel_products,
  #     year = input$year_wheel_products)
  # })
  
  # ##### por Bloco
  # 
  # # Filtros
  # filtered_wheel_block <- reactive({
  #   req(input$year_wheel_products, input$value_wheel_products)
  #   
  #   filter_data(
  #     g_543b,
  #     year_filter = input$year_wheel_products
  #   )
  # })
  # 
  # # Renderização do gráfico
  # output$wheel_products_2 <- renderHighchart({
  #   generate_wheel_block(
  #     data = filtered_wheel_block(),
  #     measure = input$value_wheel_products,
  #     year = input$year_wheel_products)
  # })
  # 
  ### Comparabilidade ----
  
  #### por Cidade ----
  
  # Filtros
  filtered_city_analytics <- reactive({
    req(input$value_city_analytics, input$city_city_analytics)

    data_filtered <- g_551

    if (!is.null(input$city_city_analytics)) {
      data_filtered <- data_filtered %>%
        filter(Cidade %in% input$city_city_analytics)
    }

    return(data_filtered)
  })

  # Renderização do gráfico
  output$city_analytics <- renderHighchart({
    data_filtered <- filtered_city_analytics()

    generate_city_analytics(data_filtered,
                            input$value_city_analytics,
                            input$city_city_analytics)
  })

  # Filtro de seleção de cidades
  observe({
    unique_cities <- unique(g_551$Cidade)

    updateSelectInput(
      session,
      inputId = "city_city_analytics",
      choices = unique_cities,
      selected = NULL)
  })
  
  #### por Estado ----
  
  # Filtros
  filtered_data_state_analytics <- reactive({
    req(input$value_state_analytics, input$state_state_analytics)
    
    data_filtered <- g_552
    
    if (!is.null(input$state_state_analytics)) {
      data_filtered <- data_filtered %>%
        filter(Nome_Estado %in% input$state_state_analytics)
    }
    
    return(data_filtered)
  })
  
  # Renderização do gráfico
  output$state_analytics <- renderHighchart({
    data_filtered <- filtered_data_state_analytics()
    
    generate_state_analytics(
      data_filtered,
      input$value_state_analytics,
      input$state_state_analytics)
  })
  
  # Filtro de seleção de estado
  observe({
    unique_states <- unique(g_552$Nome_Estado)
    
    updateSelectInput(
      session,
      inputId = "state_state_analytics",
      choices = unique_states,
      selected = NULL)
  })
  
  #### por País ----
  
  # Filtros
  filtered_data_country_analytics <- reactive({
    req(input$value_country_analytics,
        input$country_country_analytics)
    
    data_filtered <- g_553
    
    if (!is.null(input$country_country_analytics)) {
      data_filtered <- data_filtered %>%
        filter(Nome_Pais %in% input$country_country_analytics)
    }
    
    return(data_filtered)
  })
  
  # Renderização do gráfico
  output$country_analytics <- renderHighchart({
    data_filtered <- filtered_data_country_analytics()
    
    generate_country_analytics(
      data_filtered,
      input$value_country_analytics,
      input$country_country_analytics)
  })
  
  # Filtro de seleção de país
  observe({
    unique_countries <- unique(g_553$Nome_Pais)
    
    updateSelectInput(session,
                      inputId = "country_country_analytics",
                      choices = unique_countries,
                      selected = NULL)
    })
  
  ### Análise ----
  
    #### Pareto ----
  filtered_pareto_analytics2 <- reactive({
    req(
      input$year_pareto_analytics2,
      input$states_pareto_analytics2,
      input$value_pareto_analytics2
    )
    
    data_filtered <- g_562
    
    if (input$year_pareto_analytics2 != "Todos") {
      data_filtered <- data_filtered %>%
        filter(Ano == input$year_pareto_analytics2)
    }
    
    if (input$states_pareto_analytics2 != "Todos") {
      data_filtered <- data_filtered %>%
        filter(Nome_Estado == input$states_pareto_analytics2)
    }
    
    return(data_filtered)
  })
  
  # Renderização do grafico
  output$pareto_analytics2 <- renderHighchart({
    data_filtered <- filtered_pareto_analytics2()
    
    generate_pareto_analytics2(
      data_filtered,
      input$value_pareto_analytics2,
      input$year_pareto_analytics2,
      input$states_pareto_analytics2
    )
  })
  
  observe({
    unique_states <- unique(g_562$Nome_Estado)
    
    updateSelectInput(
      session,
      inputId = "states_pareto_analytics2",
      choices = c("Todos", unique_states),
      selected = "Todos"
    )
  })
  
  #### Gráfico de Radar ----
  filtered_radar_analytics2 <- reactive({
    req(input$sh2_radar_analytics2, input$value_radar_analytics2)
    
    data_filtered <- g_563
    
    if (input$sh2_radar_analytics2 != "Todos") {
      data_filtered <- data_filtered %>%
        filter(Nome_SH2 == input$sh2_radar_analytics2)
    }
    
    return(data_filtered)
  })
  
  # Renderizar do gráfico
  output$radar_analytics2 <- renderHighchart({
    data_filtered <- filtered_radar_analytics2()
    
    generate_radar_analytics2(data_filtered,
                              input$value_radar_analytics2,
                              input$sh2_radar_analytics2)
  })
  
  observe({
    unique_sh2 <- unique(g_563$Nome_SH2)
    
    updateSelectInput(
      session,
      inputId = "sh2_radar_analytics2",
      choices = c("Todos", unique_sh2),
      selected = "Todos"
    )
  })

  library(pryr)
  
    observe({
      cat("Memória usada:", mem_used(), "\n")
    })
  
})
