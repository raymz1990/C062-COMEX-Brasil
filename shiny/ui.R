# Carregando Biblioteca ----
source("./R/00.library.R")                  # Carrega pacotes
source("./R/theme_utils.R")                 # Carrega função para temas
# source("./R/ui_components.R")               # Funções auxiliares de componentes UI
source("./modules/ui/01_overview_page.R")   # Página Overview
source("./modules/ui/02_region_page.R")     # Página Região
source("./modules/ui/03_export_page.R")     # Página Exportação
source("./modules/ui/04_product_page.R")    # Página Produtos
source("./modules/ui/05_indicator_page.R")  # Página Indicadores
# source("./modules/ui/06_database_page.R")   # Página Indicadores
source("./modules/ui/07_about_page.R")      # Página Indicadores

# Tema do painel
# theme <- create_theme(bs4dash_status(primary = "#3d9970", danger = "#3d9970"))

# Customização Página ----
ui <- dashboardPage(
  dark = TRUE,                            # Tema dark como default
  freshTheme = theme,
  help = NULL,                            # Remove o controle switch extra que não faz nada
  fullscreen = TRUE,
  # scrollToTop = TRUE,
  
  # Cabeçalho ----
  dashboardHeader(
    title = dashboardBrand(
      title = "Painel de Navegação",
      color = "olive",
      href = "https://www.gov.br/mdic/pt-br",
      image = "https://www.gov.br/mdic/++theme++padrao_govbr/favicons/favicon-48x48.png",
      opacity = 0.8
    ),
    fixed = TRUE,     # Fixar o cabeçalho no topo
    rightUi = tagList(
      
      # LinkedIn
      tags$li(
        class = "nav-item dropdown",
        style = "list-style-type: none; display: inline-block;
                  margin-right: 10px; margin-top: 7px;",
        tags$a(
          href = "https://www.linkedin.com/in/raymundopilz/",
          target = "_blank",
          icon("linkedin-in", class = "fa-lg text-gray icon-hover")
        )
      ),
      
      # Github
      tags$li(
        class = "nav-item dropdown",
        style = "list-style-type: none; display: inline-block;
                  margin-right: 10px; margin-top: 7px;",
        tags$a(
          href = "https://github.com/raymz1990",
          target = "_blank",
          icon("github", class = "fa-lg text-gray icon-hover")
        )
      ),
      
      # BuyMeACoffee
      tags$li(
        class = "nav-item dropdown",
        style = "list-style-type: none; display: inline-block;
                  margin-right: 10px; margin-top: 7px;",
        tags$a(
          href = "https://ko-fi.com/raymundopilz",
          target = "_blank",
          icon("mug-hot", class = "fa-lg text-gray icon-hover")
        )
      ),
      
      # e-mail
      tags$li(
        class = "nav-item dropdown",
        style = "list-style-type: none; display: inline-block;
                  margin-top: 7px;",
        tags$a(
          href = "mailto:raymundopilz@gmail.com",
          target = "_blank",
          icon("envelope", class = "fa-lg text-gray icon-hover")
        )
      )
    ),
    "COMÉRCIO INTERNACIONAL BRASILEIRO: EXPORTAÇÕES DE 2011 À 2020" #----
  ), 
  
  # Barra Lateral ----
  dashboardSidebar(
    fixed = TRUE,
    skin = "light",
    # status = "primary",
    id = "sidebar",
    sidebarMenu(
      id = "current_tab",
      flat = FALSE,
      compact = FALSE,
      childIndent = TRUE,
      menuItem("Geral",
               tabName = "OVERVIEW",
               icon = icon("dashboard")),
      menuItem("Por Região",
               tabName = "REGION",
               icon = icon("globe-americas")),
      menuItem("Destinos Internacionais",
               tabName = "EXPORT",
               icon = icon("globe")),
      menuItem("Produtos",
               tabName = "PRODUCT",
               icon = icon("boxes")),
      menuItem("Indicadores",
               tabName = "INDICATOR",
               icon = icon("chart-line")),
      # menuItem("Base de Dados",
      #          tabName = "DATABASE",
      #          icon = icon("database")),
      menuItem("Sobre",
               tabName = "ABOUT",
               icon = icon("info-circle"))
    )
  ),
  
  # Painéis ----
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      # tags$link(rel = "stylesheet", type = "text/css", href = "theme_light.css"),
      # tags$link(rel = "stylesheet", type = "text/css", href = "theme_dark.css")
    ),
    
    # Definição das Páginas
    tabItems(
      tabItem(tabName = "OVERVIEW", overview_page()),
      tabItem(tabName = "REGION", region_page()),
      tabItem(tabName = "EXPORT", export_page()),
      tabItem(tabName = "PRODUCT", product_page()),
      tabItem(tabName = "INDICATOR", indicator_page()),
      # tabItem(tabName = "DATABASE", database_page()),
      tabItem(tabName = "ABOUT", about_page())
    )
  )
)