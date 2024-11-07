# Carregando bibliotecas ----
# source("./R/00.library.R")

# Carregando arquivos -----
# source("./R/00.functions.R")

data            <- readRDS("./files/data_files/data.rds")                
tb_city         <- readRDS("./files/data_files/tb_city.rds")          
tb_state        <- readRDS("./files/data_files/tb_state.rds")        
tb_country      <- readRDS("./files/data_files/tb_country.rds")    
tb_block        <- readRDS("./files/data_files/tb_block.rds")        
# tb_region       <- readRDS("./files/data_files/tb_region.rds")      
tb_product      <- readRDS("./files/data_files/tb_product.rds")    
tb_ncm          <- readRDS("./files/data_files/tb_ncm.rds")            
tb_sh2          <- readRDS("./files/data_files/tb_sh2.rds")            
tb_sh4          <- readRDS("./files/data_files/tb_sh4.rds")
#tb_sh6          <- readRDS("./files/data_files/tb_sh6.rds")            
tb_divisao      <- readRDS("./files/data_files/tb_divisao.rds")    
tb_section      <- readRDS("./files/data_files/tb_section.rds")    
