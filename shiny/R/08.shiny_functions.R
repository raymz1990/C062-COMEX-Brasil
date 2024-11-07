# Carregando bibliotecas ----
# source("./R/00.library.R")

# Funções -----

# Função para criar 4 valueBoxes width = 3
create_value_box4 <- function(subtitle, icon_name, output_id) {
  valueBox(
    subtitle = subtitle,
    color = "danger",
    icon = icon(icon_name),
    value = tags$div(class = "info-box-number", htmlOutput(output_id)),
    width = 3
  )
}

# Função  para criar 3 valueBoxes width = 4
create_value_box3 <- function(subtitle, icon_name, output_id) {
  valueBox(
    subtitle = subtitle,
    color = "danger",
    icon = icon(icon_name),
    value = tags$div(class = "info-box-number", htmlOutput(output_id)),
    width = 4
  )
}

# Função  para criar 2 valueBoxes width = 12
create_value_box2 <- function(subtitle, icon_name, output_id) {
  valueBox(
    subtitle = subtitle,
    color = "danger",
    icon = icon(icon_name),
    value = tags$div(class = "info-box-number", htmlOutput(output_id)),
    width = 6
  )
}

# Função para formatar os valores em US$ e Toneladas
# Lista de funções de formatação
format_functions <- list(
  value = function(value) {
    sapply(value, function(v) {
      if (v >= 1e12) {
        formatted_value <- paste0(round(v / 1e12, 1), " tri")
      } else if (v >= 1e9) {
        formatted_value <- paste0(round(v / 1e9, 1), " bi")
      } else if (v >= 1e6) {
        formatted_value <- paste0(round(v / 1e6, 1), " mi")
      } else if (v >= 1e3) {
        formatted_value <- paste0(round(v / 1e3, 1), " mil")
      } else {
        formatted_value <- as.character(round(v, 0))
      }
      return(formatted_value)
    })
  },
  
  weight = function(weight) {
    sapply(weight, function(w) {
      if (w >= 1e12) {
        formatted_weight <- paste0(round(w / 1e12, 1), " tri")
      } else if (w >= 1e9) {
        formatted_weight <- paste0(round(w / 1e9, 1), " bi")
      } else if (w >= 1e6) {
        formatted_weight <- paste0(round(w / 1e6, 1), " mi")
      } else if (w >= 1e3) {
        formatted_weight <- paste0(round(w / 1e3, 1), " mil")
      } else {
        formatted_weight <- as.character(round(w, 0))
      }
      return(formatted_weight)
    })
  },
  
  price = function(price) {
    sapply(price, function(p) {
      return(as.character(round(p, 2)))  
    })
  }
)

# Função para retornar as cores de acordo com a região
get_region_color <- function(region_name) {
  region_colors <- list(
    "Nordeste" = "#0C3661",
    "Norte" = "#24033D",
    "Centro Oeste" = "#613F00",
    "Sudeste" = "#5C0010",
    "Sul" = "#5C5C5C",
    "América Central" = "#FF5733",
    "América do Norte" = "#00A36C",
    "América do Sul" = "#FFCC00",
    "Europa" = "#8E44AD",
    "Oceania" = "#2874A6",
    "Oriente Médio" = "#C0392B",
    "África" = "#7D6608",
    "Ásia" = "#154360"
  )
  
  return(region_colors[[region_name]])
}

get_color <- function(category_type, category_name) {
  category_colors <- list(
    "region" = list(
      "Nordeste" = "#0C3661",
      "Norte" = "#24033D",
      "Centro Oeste" = "#613F00",
      "Sudeste" = "#5C0010",
      "Sul" = "#5C5C5C",
      "América Central" = "#FF5733",
      "América do Norte" = "#1F618D",
      "América do Sul" = "#FFCC00",
      "Europa" = "#8E44AD",
      "Oceania" = "#00A36C",
      "Oriente Médio" = "#D35400",
      "África" = "#3F6129",
      "Ásia" = "#154360"
    ),
    "isic" = list(
      "Agropecuária" = "#898C30",
      "Indústria Extrativa" = "#171C26",
      "Indústria de Transformação" = "#8C504A",
      "Outros Produtos" = "#119FBF"
    )
  )
  
  return(category_colors[[category_type]][[category_name]])
}