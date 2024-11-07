# Carregando bibliotecas ----
# source("./R/00.library.R")

# Funções -----

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