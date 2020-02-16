library(readxl)
library(stringr)
library(magrittr)
library(tidyverse)
library(tidyr)
library(dplyr)
library(lubridate)

setwd("C:/Users/João/OneDrive/Dissertação/Base de Dados ANEEL/")

#---- Le tabelas RAP 
le_tabela_rap <- function(nome, ...) {
tabela_rap <- read_excel(nome, sheet = 1, ...) 
  #cria variáveis que somam a rap de cada lote (contrato de concessão)
# ----------------------- Seção renomear_colunas ---------------------------
if(nome == "./RAP_2019.xlsx") {
    names(tabela_rap)[1:31] <- c("edificacao", "modulo","id_mdl", "classificacao", 
                           "funcao_transmissao","concessionaria_do_modulo",
                           "contrato_do_modulo", "grupo_equipamento", "tipo_uso",
                           "id_agente", "situacao_modulo", "id_rct", 
                           "concessionaria_da_raceita", "contrato_da_receita", 
                           "atualizacao_da_pa", "indice", "tipo_receita", "ato_da_rap",
                           "rap_ft_ato_legal", "data_ref_rap", "descricao_da_receita",
                           "inicio_de_vigencia", "fim_de_vigencia", "data_de_previsao",
                           "operacao_comercial", "rap_ciclo", "ref_do_ciclo", 
                           "situacao_da_rap", "id_agente_2", "usuario", "tipo_do_usuario")
}
if(nome == "./RAP_2018.xlsx") {
names(tabela_rap)[1:30] <-  c("edificacao", "modulo","id_mdl", "classificacao", 
                            "funcao_transmissao","concessionaria_do_modulo",
                            "contrato_do_modulo", "grupo_equipamento", "tipo_uso",
                            "situacao_modulo", "id_rct", "concessionaria_da_raceita", 
                            "contrato_da_receita", "atualizacao_da_pa", "indice", "tipo_receita", 
                            "ato_da_rap","rap_ft_ato_legal", "data_ref_rap", "descricao_da_receita",
                            "inicio_de_vigencia", "fim_de_vigencia", "data_de_previsao",
                            "operacao_comercial", "rap_ciclo", "ref_do_ciclo", 
                            "situacao_da_rap", "id_agente_2", "usuario", "tipo_do_usuario")
}
if(nome == "./RAP_2017.xlsx") {
names(tabela_rap)[1:32] <-  c("edificacao", "modulo","id_mdl", "classificacao", 
                            "funcao_transmissao","concessionaria_do_modulo",
                            "contrato_do_modulo", "grupo_equipamento", "tipo_uso",
                            "id_agente", "situacao_modulo", "id_rct", 
                            "concessionaria_da_raceita", "contrato_da_receita", 
                            "atualizacao_da_pa", "indice", "pis_cofins", "tipo_receita", "ato_da_rap",
                            "rap_ft_ato_legal", "data_ref_rap", "descricao_da_receita",
                            "inicio_de_vigencia", "fim_de_vigencia", "data_de_previsao",
                            "operacao_comercial", "rap_ciclo", "ref_do_ciclo", 
                            "situacao_da_rap", "id_agente_2", "usuario", "tipo_do_usuario")
}
if(nome == "./RAP_2016.xlsx") {
names(tabela_rap)[1:30] <-  c("edificacao", "modulo","id_mdl", "classificacao", 
                            "funcao_transmissao","concessionaria_do_modulo",
                            "contrato_do_modulo", "grupo_equipamento", "tipo_uso",
                            "situacao_modulo", "id_rct", 
                            "concessionaria_da_raceita", "contrato_da_receita", 
                            "atualizacao_da_pa", "indice", "pis_cofins", "tipo_receita", "ato_da_rap",
                            "rap_ft_ato_legal", "data_ref_rap", "descricao_da_receita",
                            "inicio_de_vigencia", "fim_de_vigencia", "data_de_previsao",
                            "operacao_comercial", "rap_ciclo", "ref_do_ciclo", 
                            "situacao_da_rap", "usuario", "tipo_do_usuario")
}
if(nome == "./RAP_2015.xlsx") {
names(tabela_rap)[1:31] <-  c("edificacao", "modulo","id_mdl", "classificacao", 
                            "funcao_transmissao","concessionaria_do_modulo",
                            "contrato_do_modulo", "grupo_equipamento", "tipo_uso",
                            "situacao_modulo", "id_rct", 
                            "concessionaria_da_raceita", "contrato_da_receita", 
                            "atualizacao_da_pa", "indice", "pis_cofins", "tipo_receita", "ato_da_rap",
                            "rap_ft_ato_legal", "data_ref_rap", "descricao_da_receita",
                            "inicio_de_vigencia", "fim_de_vigencia", "data_de_previsao",
                            "operacao_comercial_tlp", "operacao_comercial_tld",
                            "rap_ciclo", "ref_do_ciclo", 
                            "situacao_da_rap_inicio_ciclo", "usuario", "tipo_do_usuario")
}
if(nome == "./RAP_2014.xlsx") {
names(tabela_rap)[1:31] <-  c("edificacao", "modulo","id_mdl", "classificacao", 
                            "funcao_transmissao","concessionaria_do_modulo",
                            "contrato_do_modulo", "grupo_equipamento", "tipo_uso",
                            "situacao_modulo", "id_rct", 
                            "concessionaria_da_raceita", "contrato_da_receita", 
                            "atualizacao_da_pa", "indice", "pis_cofins", "tipo_receita", "ato_da_rap",
                            "rap_ft_ato_legal", "data_ref_rap", "descricao_da_receita",
                            "inicio_de_vigencia", "fim_de_vigencia", "data_de_previsao",
                            "operacao_comercial_tlp", "operacao_comercial_tld",
                            "rap_ciclo", "ref_do_ciclo", 
                            "situacao_da_rap_inicio_ciclo", "usuario", "tipo_do_usuario")
}
if(nome == "./RAP_2013.xls") {
names(tabela_rap)[1:29] <-  c("edificacao", "modulo","id_mdl", "classificacao", 
                            "situacao_modulo","concessionaria_do_modulo",
                            "contrato_do_modulo", "concessionaria_da_raceita", "contrato_da_receita", 
                            "indice","id_rct", "tipo_receita", "ato_da_rap", "descricao_da_receita",
                            "rap_ft_ato_legal", "data_ref_rap",
                            "grupo_equipamento", "tipo_uso",
                            "inicio_de_vigencia", "fim_de_vigencia",
                            "operacao_comercial_tlp", "operacao_comercial_tld",
                            "rap_ciclo", "ref_do_ciclo", "situacao_da_rap_inicio_ciclo", 
                            "pis_cofins",  "usuario", "tipo_do_usuario", "atualizacao_da_pa")
}
if(nome == "./RAP_2012.xlsx") {
names(tabela_rap)[1:26] <-  c("edificacao", "modulo","id_mdl", "classificacao", 
                            "situacao_modulo","concessionaria_do_modulo",
                            "contrato_do_modulo", "concessionaria_da_raceita", "contrato_da_receita", 
                            "indice","id_rct", "tipo_receita", "ato_da_rap", "descricao_da_receita",
                            "rap_ft_ato_legal", "data_ref_rap",
                            "grupo_equipamento", "tipo_uso",
                            "inicio_de_vigencia", "fim_de_vigencia",
                            "operacao_comercial_tlp", "operacao_comercial_tld",
                            "rap_ciclo", "ref_do_ciclo", "situacao_da_rap_inicio_ciclo", 
                            "pis_cofins")
}

  tabela_rap %>%
  group_by(contrato_do_modulo) %>%
  mutate(rap_total_ato_Legal = sum(rap_ft_ato_legal), 
         rap_lote_hj = sum(rap_ciclo),
         rap_percent_hj = rap_ciclo/rap_lote_hj)
}

RAP_2019 <- le_tabela_rap("./RAP_2019.xlsx")
RAP_2018 <- le_tabela_rap("./RAP_2018.xlsx")
RAP_2017 <- le_tabela_rap("./RAP_2017.xlsx")
RAP_2016 <- le_tabela_rap("./RAP_2016.xlsx")
RAP_2015 <- le_tabela_rap("./RAP_2015.xlsx")
RAP_2014 <- le_tabela_rap("./RAP_2014.xlsx")
RAP_2013 <- le_tabela_rap("./RAP_2013.xls")
RAP_2012 <- le_tabela_rap("./RAP_2012.xlsx", skip = 2)




# ====================Seção join tabelas (tCompleta) ==================================
  nome_colunas_join <- c( "id_rct", "id_mdl")
                         # "concessionaria_do_modulo","contrato_do_modulo",
                         # "situacao_modulo", "concessionaria_da_raceita","contrato_da_receita",
                         # "indice", "tipo_receita", "edificacao" , "classificacao","id_mdl")
 
  filtra_DIT_ICG_IEG <- function (tabela) {
    tabela %>% filter(!classificacao %in% c("DIT","ICG","IEG"))
  }
  
#Avaliar full_join para verificação de perda de dados
  
  tCompleta <- filtra_DIT_ICG_IEG(RAP_2019)  %>%
               left_join(RAP_2018, by = nome_colunas_join,
                         suffix = c("_2019", "_2018"))%>%
               left_join(RAP_2017, by = nome_colunas_join,
                           suffix = c("","_2017")) %>%
               left_join(RAP_2016, by = nome_colunas_join,
                          suffix = c("","_2016")) %>%
               left_join(RAP_2015, by = nome_colunas_join,
                          suffix = c("","_2015")) %>%
               left_join(RAP_2014, by = nome_colunas_join,
                          suffix = c("","_2014")) %>%
               left_join(RAP_2013, by =nome_colunas_join,
                          suffix = c("","_2013")) %>%
               left_join(RAP_2012, by = nome_colunas_join,
                        suffix = c("","_2012"))

  ############    
############Conferência se falta algum dado nas tabelas que não foram adicionado pelo left_join
  # confere_2018 <- filtra_DIT_ICG_IEG(RAP_2018) %>%
  #                 anti_join(tCompleta, by = nome_colunas_join) %>%
  #                 select(modulo, id_rct, id_mdl, descricao_da_receita)
  #  
  # confere_2017 <- filtra_DIT_ICG_IEG(RAP_2017) %>%
  #                 anti_join(tCompleta, by = nome_colunas_join) %>%
  #                 select(modulo, id_rct, id_mdl, descricao_da_receita)
  # 
  # confere_2016 <- filtra_DIT_ICG_IEG(RAP_2016) %>%
  #                 anti_join(tCompleta, by = nome_colunas_join) %>%
  #                 select(modulo, id_rct, id_mdl, descricao_da_receita)
  # 
  # confere_2015 <- filtra_DIT_ICG_IEG(RAP_2015) %>%
  #                 anti_join(tCompleta, by = nome_colunas_join) %>%
  #                 select(modulo, id_rct, id_mdl, descricao_da_receita)
  # print(paste(nrow(confere_2018), nrow(confere_2017),  nrow(confere_2016),  nrow(confere_2015)))
 #--------------------   

  
#Função que trata os dados do leilão
ftrata_dadosLeilao <- function(nome_excel){  
   tabela <-read_excel(nome_excel, sheet = 1)
    
    tabela[,17:20] <- NULL
    names(tabela)[1:16] <- c("leilao","lote", "data", "empreendimentos","transmissora", 
                             "contrato_de_concessao", "final_da_concessao",
                             "UF", "extensao_km", "prazo_meses", "edital_rap", 
                             "proposta_RAP", "desagio_percent", "invest_edital", 
                             "invest_contrato", "concessionaria")
    
    remove <-  which(is.na(tabela$contrato_de_concessao))
    tabela <- tabela[-remove,]
    remove <- which(!str_detect(tabela$contrato_de_concessao, "/"))
    tabela <- tabela[-remove,]
    
    #Ajusta o padrão dos códigos dos contratos
    tabela$contrato_de_concessao %<>% str_pad(8,pad = "0", side = c("left"))
    
    remove <- which(str_detect(tabela$empreendimentos, "REDE BÁSICA"))
    tabela_a <- tabela[-remove,]
    
    #remove empreendimento caducados
    tabela_a %<>% filter(concessionaria != "CADUCADO") 
    
    #Ajusta formato da data do leilão preenche informações faltantes
    tabela_a$data <- ymd(tabela_a$data)
    tabela_a$data[tabela_a$leilao == "005/2016"] <- ymd("2017-04-24")
    tabela_a$data[tabela_a$leilao == "002/2017"] <- ymd("2017-12-15")
    tabela_a$data[tabela_a$leilao == "002/2018"] <- ymd("2018-06-28")
    tabela_a$leilao[tabela_a$leilao == "04/2018"] <- "004/2018"
    tabela_a$data[tabela_a$leilao == "004/2018"] <- ymd("2018-12-20")
    
    #Ajusta formato datas referentes ao final da concessão
    tabela_a$final_da_concessao <- ymd(tabela_a$final_da_concessao)
    
    return (tabela_a)
  }

  
t_leilao <- ftrata_dadosLeilao("./RESUMO_GERAL_LEILAO_v1.xlsx") %>%
            filter(!str_detect(contrato_de_concessao, "2015|2016|2017|2018|2019"))
  
t <- tCompleta %>%
      select(modulo, funcao_transmissao, starts_with("rap_ft_ato_legal"), starts_with("rap_ciclo"),
             starts_with("rap_total_ato_Legal"), starts_with("rap_lote_hj"),
             id_mdl, edificacao, contrato_do_modulo, contrato_da_receita, concessionaria_do_modulo)

# uni as tabelas referente às RAPs ciclo 2019 e compilado dos leiões (que possui as 
#informações dos investimentos estimados pela ANEEL e em alguns casos o valor das propostas)
t1 <- t %>%
      filter(!str_detect(contrato_da_receita, "2015|2016|2017|2018|2019")) %>%
      left_join(t_leilao, by = c("contrato_da_receita" = "contrato_de_concessao"),
               suffix = c("_trap","_tleilao"))  %>%
        select(leilao, lote, contrato_do_modulo, proposta_RAP, starts_with("rap_total_ato_Legal"), edital_rap,
                rap_ciclo,modulo, everything()) 


a <- unique(t1$contrato_da_receita)
b <- unique(t_leilao$contrato_de_concessao)

dif <- setdiff(a,b)

t2 <- t1 %>% filter(!contrato_da_receita %in% dif) %>%
        group_by(contrato_da_receita, lote, data) %>%
        summarize(rap_ato_legal_2019 = mean(rap_total_ato_Legal_2019),
                  rap_ato_legal_2018 = mean(rap_total_ato_Legal_2018),
                  rap_ato_legal_2017 = mean(rap_total_ato_Legal),
                  rap_ato_legal_2016 = mean(rap_total_ato_Legal_2016),
                  proposta_RAP = mean(proposta_RAP),
                  Dif_pu_2019 = (rap_ato_legal_2019 - proposta_RAP)/proposta_RAP,
                  Dif_pu_2018 = (rap_ato_legal_2018 - proposta_RAP)/proposta_RAP,
                  Dif_pu_2017 = (rap_ato_legal_2017 - proposta_RAP)/proposta_RAP,
                  Dif_pu_2016 = (rap_ato_legal_2016 - proposta_RAP)/proposta_RAP)
         

t2 %<>% filter(Dif_pu_2019 < 90)
ggplot(t2, aes(x = data, y = Dif_pu_2019)) + geom_point()
ggplot(t2, aes(x = data, y = Dif_pu_2018)) + geom_point()
ggplot(t2, aes(x = data, y = Dif_pu_2017)) + geom_point()
ggplot(t2, aes(x = data, y = Dif_pu_2016)) + geom_point()

c <- unique(t1$contrato_da_receita)
  
# t_1999_2013 <- t %>% filter(str_detect(contrato_do_modulo, "2013|2012|2011|2010|2009
#                                   |2008|2007|2006|2005|2004|2003|2002|2001|2000|1999"))
# LT <- t %>%
#       filter(str_detect(funcao_transmissao, "FT LT")) #|FT EL|FT RTL
 


 
  # select(edificacao,modulo, id_mdl, classificacao, funcao_transmissao, concessionaria_do_modulo,
  #        contrato_do_modulo, id_agente, situacao_modulo, id_rct, 
  #        concessionaria_da_raceita,contrato_da_receita, indice, tipo_receita, 
  #        ato_da_rap, rap_ft_ato_legal, data_ref_rap,descricao_da_receita, 
  #        inicio_de_vigencia,fim_de_vigencia, data_de_previsao, operacao_comercial,
  #        rap_ciclo, ref_do_ciclo, situacao_da_rap, usuario, tipo_do_usuario)