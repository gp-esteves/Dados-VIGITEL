library(tidyverse); library(survey)

xls_files <- list.files(path = getwd(), pattern = "\\.xls$|\\.xlsx$", full.names = FALSE)

data_list <- lapply(xls_files[5:17], readxl::read_xls)
data_list[[14]] <- readxl::read_xlsx(xls_files[18])

# arrumando chave

data_list[[9]]$chave <- as.character(data_list[[9]]$chave)
data_list[[10]]$chave <- as.character(data_list[[10]]$chave)
data_list[[11]]$chave <- as.character(data_list[[11]]$chave)
data_list[[12]]$chave <- as.character(data_list[[12]]$chave)
data_list[[13]]$chave <- as.character(data_list[[13]]$chave)
data_list[[14]]$chave <- as.character(data_list[[14]]$chave)

# criando var edição

data_list[[1]]$ano_edicao <- 2009 
data_list[[2]]$ano_edicao <- 2010
data_list[[3]]$ano_edicao <- 2011
data_list[[4]]$ano_edicao <- 2012
data_list[[5]]$ano_edicao <- 2013
data_list[[6]]$ano_edicao <- 2014
data_list[[7]]$ano_edicao <- 2015
data_list[[8]]$ano_edicao <- 2016
data_list[[9]]$ano_edicao <- 2017
data_list[[10]]$ano_edicao <- 2018
data_list[[11]]$ano_edicao <- 2019
data_list[[12]]$ano_edicao <- 2020
data_list[[13]]$ano_edicao <- 2021
data_list[[14]]$ano_edicao <- 2023

# combinando

combined_data <- reduce(data_list, bind_rows)

print(combined_data)

# salvando essa versão

# save(combined_data, file="banco_completo.Rdata")
# load("banco_completo.Rdata")

# selecionando variaveis relevantes

vigitel <- combined_data |> select(c(ano, ano_edicao, cidade, fesc, q6, q7, civil, q8a, q8b, q8_anos, 
                          r128a, q9, q11, q42, q43a, q44, q45, q46, q47, q48, 
                          q49, r147, r148_hh, r148_mm, q50, q51, q52, q53, q54, 
                          q55, q56, r149, r150_hh, r150_mm, q59a, q59b, q59c, q60, pesorake,
                          q6, q7, fet, q8a, q9, q11, q42, q47, q48, q49, q50, 
                          q51, q52, q60, q64, q69, q74, q75, q76, q78, q88))

# save(vigitel, file="banco_reduzido.Rdata")
