library(readxl)

#Função que trata os dados do leil?o
ftrata_dadosLeilao <- function(tabela){  

library(magrittr)
library(tidyverse)
library("tidyr")
library(dplyr)
library(lubridate)
library(RColorBrewer)
    
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

#Ajusta o padr?o dos c?digos dos contratos
tabela$contrato_de_concessao %<>% str_pad(8,pad = "0", side = c("left"))

#remove empreendimento caducados
tabela_a <- tabela %>% filter(concessionaria != "CADUCADO") 

#Ajusta formato da data do leil?o preenche informa??es faltantes
tabela_a$data <- ymd(tabela_a$data)
tabela_a$data[tabela_a$leilao == "005/2016"] <- ymd("2017-04-24")
tabela_a$data[tabela_a$leilao == "002/2017"] <- ymd("2017-12-15")
tabela_a$data[tabela_a$leilao == "002/2018"] <- ymd("2018-06-28")
tabela_a$leilao[tabela_a$leilao == "04/2018"] <- "004/2018"
tabela_a$data[tabela_a$leilao == "004/2018"] <- ymd("2018-12-20")

#Ajusta formato datas referentes ao final da concess?o
tabela_a$final_da_concessao <- ymd(tabela_a$final_da_concessao)

# Exlclu? a palavra lote da vari?vel lote
tabela_a$lote %<>% 
  str_replace("LOTE", "") %>%
  str_replace("Lota", "") %>%
  str_trim()

return (tabela_a)
}


#Funcao que trata os dados da tabela rap 2019 e faz o inner_join com a tabela com os dados do leil?o
fjoin_rap_leilao <- function(tabela_rap) {
  
  names(tabela_rap)[1:31] <- c("edificacao", "modulo","id_mdl", "classificacao", 
                               "funcao_transmissao","concessionaria_do_modulo",
                               "contrato_do_modulo", "grupo_equipamento", "tipo_uso",
                               "id_agente", "situacao_modulo", "id_rct", 
                               "concessionaria_da_raceita", "contrato_da_receita", 
                               "atualizacao_da_pa", "indice", "tipo_receita", "ato_da_rap",
                               "rap_equip_atolegal", "data_ref_rap", "descricao_da_receita",
                               "inicio_de_vigencia", "fim_de_vigencia", "data_de_previsao",
                               "operacao_comercial", "rap_ciclo", "ref_do_ciclo", 
                               "situacao_da_rap", "id_agente_2", "usuario", "tipo_do_usuario")
  
  
  #Selciona empreendimentos da rede b?sica e
  #cria a vari?vel FT somente com as fun??es de transmiss?o isoladas
  tabela_rap_RB <- tabela_rap  %>%
    #exclui empreendimentos fora da RB
    filter(classificacao != "DIT", classificacao != "ICG",
           classificacao != "IEG") %>% 
    #separa a FT em uma variavel
    mutate(ft_especifico = (str_split(funcao_transmissao, " ")),
           ft_especifico = lapply(ft_especifico, 
                                  function(e){unlist(e[2])})) %>% 
    #cria variaveis que somam a rap de cada lote (contrato de concess?o)
    group_by(contrato_do_modulo) %>%
    mutate(rap_total_atoLegal = sum(rap_equip_atolegal), 
           rap_lote_hj = sum(rap_ciclo),
           rap_percent_hj = rap_ciclo/rap_lote_hj) %>%
    #ordena algumas colunas para facilitar a vizualizacao - view()
    select(contrato_do_modulo, funcao_transmissao, rap_total_atoLegal, rap_equip_atolegal,
           rap_lote_hj,rap_ciclo,rap_percent_hj,modulo, everything())
    
  # uni as tabelas referente as RAPs ciclo 2019 e compilado dos leiloes (que possui as 
  #informacoes dos investimentos estimados pela ANEEL e em alguns casos o valor das propostas)
  tcompleta <- tabela_rap_RB %>%
    inner_join(tabela_x, by = c("contrato_da_receita" = "contrato_de_concessao"),
               suffix = c("_trap","_tleilao"))  %>%
    select(contrato_da_receita, funcao_transmissao, rap_total_atoLegal, proposta_RAP, edital_rap,
           rap_lote_hj, rap_equip_atolegal, rap_ciclo,rap_percent_hj,modulo, everything())
  
  #Adiciona variáveis para serem utilizadas no fluxo de caixa descontado
  tcompleta <- tcompleta %>%
                mutate(anos_vigencia = year(fim_de_vigencia) - year(inicio_de_vigencia),
                       ano_inicio = year(inicio_de_vigencia) - year(data))
  
  
  #conversão para número forçada, para evitar erros posteriores
  tcompleta$invest_contrato = round(as.numeric(tcompleta$invest_contrato), digits = 2)
  tcompleta$proposta_RAP <- as.numeric(tcompleta$proposta_RAP)
  return(tcompleta)
  
}




tabela_x <- read_excel("C:/Users/João/OneDrive/Dissertação/Base de Dados ANEEL/RESUMO_GERAL_LEILAO_v1.xlsx",
                       sheet = 1)

tabela_x <- ftrata_dadosLeilao(tabela_x)

tabela_rap <- read_excel("C:/Users/João/OneDrive/Dissertação/Base de Dados ANEEL/RAP_2019.xlsx",
                         sheet = 1)

tcompleta <- fjoin_rap_leilao(tabela_rap)

#Situações para reflexão
#A data de entrada em operação é diferente do início da vigência do contrato, 
#isso pode sinalizar um avaliação de expectativa de antencipação da empresa.
# tt  <- filter(tcompleta, operacao_comercial != inicio_de_vigencia)

#Entrada em operação = a data do leilão


t <-  tcompleta %>%
              # mutate(invest_contrato = round(as.numeric(invest_contrato,digits = 2))) %>%
              group_by(contrato_da_receita, leilao, lote, data, concessionaria_da_raceita, 
                        proposta_RAP, invest_contrato, edital_rap) %>%
              filter(is.na(invest_contrato) == FALSE) %>%
              summarize("Rap (MR$)" = sum(rap_equip_atolegal)/1E6,
                          rap_leilao = sum(proposta_RAP)/n(),
                          dif_rap_percent = (1E6*`Rap (MR$)` - rap_leilao)/rap_leilao*100,
                          taxa_juros = 0.044,
                          ano_inicio = 5)


t1 <- tcompleta %>% filter(ft_especifico == "LT") #observar a existencia de modulos EL e RTL inclusos





##### Plota gráfico de Barra Volume RAP de todos os leiloes ####
# ggplot(t, aes(x = t$data, y = t$dif_rap_percent)) + geom_point()
# 
# library(ggplot2)
# 
# t1 <- arrange(t,data)
# Leilao <- paste(t1$data, t1$leilao, sep = " ")
# t1_plot <-t1 %>%
# ggplot(aes(x = Leilao ,y = `Rap (MilhoesR$)`, fill = factor(lote)))
# colourCount = length(unique(t1$lote))
# getPalette = colorRampPalette(brewer.pal(9, "Set1"))
# t1_plot +
#   geom_bar(stat="identity", colour = "black") +
#   scale_fill_manual(values = getPalette(colourCount)) +
#     theme(legend.direction = "horizontal",
#           legend.box = "vertical",
#           legend.position = c(0.005,0.99),
#           legend.justification = c(0, 1),
#           axis.text.x = element_text(angle=90, hjust=1))



 # # geom_point(aes(x = t$data, y = t$dif_rap_percent))

  # + geom_text(aes(label=proposta_RAP, y=y.pos.ctr), colour="white")

# ############Confere se existe algum elemento que ficou exclu?do pelo inner_join ###########
# sum(tcompleta$rap_ciclo)/1E9
# sum(tabela_x$proposta_RAP)/1E9
# sum(tabela_x$edital_rap)/1E9
# sum(tcompleta$rap_equip_atolegal)/1E9
# sum(tcompleta$rap_percent_hj)
# t <- tabela_x %>%
#       anti_join(tcompleta, by = c("contrato_de_concessao" = "contrato_do_modulo")) %>%
#       filter(concessionaria != "CADUCADO")
# t <- tcompleta %>%
#   anti_join(tabela_x, by = c("contrato_do_modulo" = "contrato_de_concessao"))
##############################################################################################


############### C?dito para separa LTs e SEs #####################
# LT <- which(str_detect(tabela_x2$empreendimentos_sep, "LT "))
# # SE <- which(str_detect(tabela_x2$empreendimentos_sep, "SE "))
# # 
# tabela_x3 <- tabela_x2[LT,]
# tabela_x3 %<>% mutate_all(tabela_x2$empreendimentos_sep, funs = toupper)
#  tabela_x4 <- tabela_x2[-SE,]
# tabela_x5 <- tabela_x3[-SE,]
# SE <- which(str_detect(tabela_x5$empreendimentos_sep, "SE "))
# tabela_x5 <- tabela_x5[-SE,]
#################################################################


                  
# tabela_rap_LT_ft <-  tabela_rap_RB %>%
#                      filter(ft_especifico == "LT") %>%
#                      mutate(ft_modulo = (str_split(modulo, " ")),
#                                           ft_modulo = lapply(ft_modulo, 
#                                           function(e){unlist(e[1])}))
# 
# tabela_rap_LT <- tabela_rap_LT_ft %>%
#                   group_by(funcao_transmissao) %>%
#                   summarize(total_RAP_leilao = sum(rap_do_ato_legal),
#                             total_RAP_2019 = sum(rap_do_ato_legal))



# unlist(unique(tabela_rap_LT$ft_modulo))





