
# 01.data.R ----

## Função para criação de subconjuntos de dados por bloco econômico ----
process_economic_block <- function(data, block_name) {
  data %>%
    filter(`Economic Block` == block_name) %>%
    group_by(Country, `Economic Block`) %>%
    summarise(
      US = sum(`US$ FOB`),
      Weight = sum(`Net Weight`),
      .groups = 'drop'
    )
}

# 03.data_modeling.R ----
## Função para contagem de linhas ----
qtde_linhas <- function(df, mensagem = NULL) {
  num_linhas <- nrow(df)
  if (!is.null(mensagem)) {
    cat(mensagem, num_linhas, "\n")
  } else {
    cat("Número de linhas:", num_linhas, "\n")
  }
  return(num_linhas)
}

## Função para verificar valores NA ----
verificar_na <- function(tb, coluna) {
  cat("NA em", coluna, ":", any(is.na(tb[[coluna]])), "\n")
}

## Função para renomear colunas ----
renomear_colunas <- function(tb, novos_nomes) {
  colnames(tb) <- novos_nomes
  return(tb)
}

## Função para remover duplicatas e contar linhas ----
remover_duplicatas <- function(df) {
  df <- df %>% distinct()
  qtde_linhas(df)  # Conta o número de linhas após a remoção de duplicatas
  return(df)
}
