# Carregando bibliotecas ----
source("./R/00.library.R")

## Extração dos arquivos ----

## Carregamento da tabela auxiliar disponivel em 
### https://www.gov.br/mdic/pt-br/assuntos/comercio-exterior/estatisticas/base-de-dados-bruta#Municipio
tb_auxiliar <- "./data/TABELAS_AUXILIARES.xlsx"

lista_abas <-
  excel_sheets(tb_auxiliar) 

print(lista_abas) 

for (aba in lista_abas) {
  assign(paste0("tb_", aba), read_excel(tb_auxiliar, sheet = aba))
} 

ls(pattern = "tb_")

## tb_1 ----
### SH - Sistema Harmonizado
### Códigos e descrições do Sistema Harmonizado (Seções, Capítulos-SH2, Posições-SH4 e Subposições-SH6).
head(tb_1)
colnames(tb_1)

### Quantidade de linhas
cat("Quantidade de linhas únicas em tb_1 (CO_NCM):", length(unique(tb_1$CO_NCM)), "\n")

### Criação da base de dados ----
# tb_product <- select(
#   tb_1, CO_SH4, NO_SH4_POR, NO_SH4_ING, CO_SH2, NO_SH2_POR, NO_SH2_ING)

tb_product <-  tb_1 %>%
  select(
  CO_NCM, NO_NCM_POR, CO_SH6, NO_SH6_POR, 
  CO_SH4, NO_SH4_POR, CO_SH2, NO_SH2_POR)

## tb_2 ----
### CUCI - Classificação Uniforme para Comércio Internacional
### Códigos e descrições dos níveis da classificação CUCI (Revisão 4). Pode ser utilizada conjuntamente com ISIC.
head(tb_2)
colnames(tb_2)

## tb_3 ----
### CGCE - Classificação por Grandes Categorias Econômicas
### Códigos e descrições dos níveis da classificação CGCE.
head(tb_3)
colnames(tb_3)

## tb_4 ----
### ISIC - International Standard Industrial Classification (Setores Industriais)
### Códigos e descrições da classificação ISIC (Revisão 4).
head(tb_4)
colnames(tb_4)

### Verficando colunas ----
unique(tb_4$NO_ISIC_CLASSE)   # descarta
unique(tb_4$NO_ISIC_GRUPO)    # descarta
unique(tb_4$NO_ISIC_DIVISAO)  # descarta
unique(tb_4$NO_ISIC_SECAO)

### Criação da base de dados ----
tb_divisao <- tb_4 %>%
  select(
  CO_NCM, NO_NCM_POR, 
  CO_ISIC_SECAO, NO_ISIC_SECAO)

## tb_5 ----
### SIIT - Setores Industriais por Intensidade Tecnológica
### Códigos e descrições da classificação SIIT.
head(tb_5)
colnames(tb_5)

## tb_6 ----
### Unidade Estatística da NCM
### Códigos e descrições das unidades estatísticas das NCMs.
head(tb_6)
colnames(tb_6)

## tb_7 ----
### Fator Agregado da NCM
### Classificação própria da SECEX - Códigos e descrições de Fator Agregado das NCMs. Pode ser utilizada conjuntamente com a tabela de PPI ou PPE.
head(tb_7)
colnames(tb_7)

## tb_8 ----
### Pauta de Produtos Importados
### Classificação própria da SECEX - Códigos e descrições da Pauta de Produtos Importados. DEVE SER UTILIZADA APENAS PARA IMPORTAÇÃO.
head(tb_8)
colnames(tb_8)

## tb_9 ----
### Pauta de Produtos Exportados
### Classificação própria da SECEX - Códigos e descrições da Pauta de Produtos Exportados. DEVE SER UTILIZADA APENAS PARA EXPORTAÇÃO.
head(tb_9)
colnames(tb_9)

## tb_10 ----
### Países
### Códigos e descrições de países.
head(tb_10)
colnames(tb_10)

### Criação da base de dados ----
tb_country <- tb_10 %>%
  select(CO_PAIS, CO_PAIS_ISON3, CO_PAIS_ISOA3, NO_PAIS, NO_PAIS_ING)

## tb_11 ----
### Blocos de Países
### Códigos e descrições das principais agregações de países em blocos. Deve ser usada em cojunto com a tabela de países.
head(tb_11)
colnames(tb_11)

### Criação da base de dados ----
tb_block <- tb_11 %>%
  select(CO_PAIS, CO_BLOCO, NO_BLOCO, NO_BLOCO_ING)

## tb_12 ----
### Unidades da Federação
### Códigos e nome das unidades da federação (estados) do Brasil.
head(tb_12)
colnames(tb_12)

### Exclusão de outras regiões
unique(tb_12$NO_REGIAO)
tb_12 <- tb_12 %>%
  filter(!(
    NO_REGIAO %in% c(
      "REGIAO NAO DECLARADA",
      "CONSUMO DE BORDO",
      "MERCADORIA NACIONALIZADA",
      "REEXPORTACAO"
    )
  ))

### Criação da base de dados ----
tb_state <- tb_12

## tb_13 ----
### Municípios
### Códigos e nome dos municípios brasileiros. Pode ser utilizada em conjunto com a tabela de UF. Fundamental para utilização junto com o arquivo de dados brutos por municípios domicílio fiscal das empresas
head(tb_13)
colnames(tb_13)

### Exclusão de outras regiões
unique(tb_13$SG_UF)
tb_13 <- tb_13 %>%
  filter(!(SG_UF %in% c("EX", "ND"))) %>%
  select(CO_MUN_GEO, NO_MUN_MIN, SG_UF)

### Criação da base de dados ----
tb_city <- select(
  tb_13, CO_MUN_GEO, NO_MUN_MIN, SG_UF)

## tb_14 ----
### Via
### Código e descrição da via (modal) de transporte"
head(tb_14)
colnames(tb_14)

## tb_15 ----
### Urf 
### Código e descrição da Unidade da Receita Federal (embarque/despacho) mais detalhes em: http://idg.receita.fazenda.gov.br/orientacao/aduaneira/importacao-e-exportacao/recinto-alfandegados

head(tb_15)
colnames(tb_15)

## tb_16 ----
### ISIC Seção x CUCI Grupo 
### Códigos e descrições dos níveis ISIC e CUCI usados na coletiva de apresentação da balança comercial brasileira
head(tb_16)
colnames(tb_16)

## tb_ÍNDICE ----head(tb_16)
colnames(tb_ÍNDICE)

## Limpeza do ambiente ----
rm(list = ls(pattern = "tb_[1-9]|tb_1[0-6]|tb_ÍNDICE"))
