# Carregando dados anteriores ----
source("./R/03.data_modeling.R")

# salvando em formato .rds
saveRDS(data, file = "./files/data_files/data.rds")                       # data
saveRDS(tb_city, file = "./files/data_files/tb_city.rds")                 # tb_city
saveRDS(tb_state, file = "./files/data_files/tb_state.rds")               # tb_state
saveRDS(tb_country, file = "./files/data_files/tb_country.rds")           # tb_country
saveRDS(tb_block, file = "./files/data_files/tb_block.rds")               # tb_block
saveRDS(tb_region, file = "./files/data_files/tb_region")                 # tb_region
saveRDS(tb_product, file = "./files/data_files/tb_product.rds")           # tb_product
saveRDS(tb_ncm, file = "./files/data_files/tb_ncm.rds")                   # tb_ncm
saveRDS(tb_sh2, file = "./files/data_files/tb_sh2.rds")                   # tb_sh2
saveRDS(tb_sh4, file = "./files/data_files/tb_sh4.rds")                   # tb_sh4
saveRDS(tb_sh6, file = "./files/data_files/tb_sh6.rds")                   # tb_sh6
saveRDS(tb_divisao, file = "./files/data_files/tb_divisao.rds")           # tb_divisao
saveRDS(tb_section, file = "./files/data_files/tb_section.rds")           # tb_section

data <- data %>% 
  mutate(Peso_Liquido = Peso_Liquido / 1000)
# salvando em formato .csv
write.csv(data, file = "./files/csv_files/data.csv", row.names = FALSE, fileEncoding = "UTF-8")                       # data
write.csv(tb_city, file = "./files/csv_files/tb_city.csv", row.names = FALSE, fileEncoding = "UTF-8")                 # tb_city
write.csv(tb_state, file = "./files/csv_files/tb_state.csv", row.names = FALSE, fileEncoding = "UTF-8")               # tb_state
write.csv(tb_country, file = "./files/csv_files/tb_country.csv", row.names = FALSE, fileEncoding = "UTF-8")           # tb_country
write.csv(tb_block, file = "./files/csv_files/tb_block.csv", row.names = FALSE, fileEncoding = "UTF-8")               # tb_block
write.csv(tb_region, file = "./files/csv_files/tb_region.csv", row.names = FALSE, fileEncoding = "UTF-8")                 # tb_region
write.csv(tb_product, file = "./files/csv_files/tb_product.csv", row.names = FALSE, fileEncoding = "UTF-8")           # tb_product
write.csv(tb_ncm, file = "./files/csv_files/tb_ncm.csv", row.names = FALSE, fileEncoding = "UTF-8")                   # tb_ncm
write.csv(tb_sh2, file = "./files/csv_files/tb_sh2.csv", row.names = FALSE, fileEncoding = "UTF-8")                   # tb_sh2
write.csv(tb_sh4, file = "./files/csv_files/tb_sh4.csv", row.names = FALSE, fileEncoding = "UTF-8")                   # tb_sh4
write.csv(tb_sh6, file = "./files/csv_files/tb_sh6.csv", row.names = FALSE, fileEncoding = "UTF-8")                   # tb_sh6
write.csv(tb_divisao, file = "./files/csv_files/tb_divisao.csv", row.names = FALSE, fileEncoding = "UTF-8")           # tb_divisao
write.csv(tb_section, file = "./files/csv_files/tb_section.csv", row.names = FALSE, fileEncoding = "UTF-8")           # tb_section

