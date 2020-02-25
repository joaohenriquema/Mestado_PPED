#Scrip para execução das rotinas de cálculo do fluxo de caixa
#Objetivo: Calcular os principais indicadores finaceiros: TIR do Proejto, Indíce de cobertura

library(magrittr )
library(tidyverse)
library("tidyr")
library(dplyr)
library(lubridate)
library(reshape2)
library(FinancialMath)

# 
# fCash_Flow <- function(t_in) {
  #Recebe a tabela derivada t do scrip Preparação base de dados, agrupados pelo nome do contrato
  #são discriminados os empreendimentos com obras com prazos diferentes
  t_in <- tcompleta %>% 
    filter(contrato_da_receita == "001/2007")  %>%
    mutate(taxa_juros = 0.044)                    

  
# Investimento  - alor estimado dos investimentos R$
# Os investimentos são discriminados por grupo de equipamentos com mesmo tempo de inicio e fim do contrato
# Os investimentos são distribuidos no tempo igualmente por simplifciação  
  vec1 <- matrix(nrow = length(t_in$rap_equip_atolegal), ncol = max(t_in$ano_inicio))
  colnames(vec1) <- sprintf("Ano %d",seq(1:max(t_in$ano_inicio)))
  rownames(vec1) <- sprintf("Equip. %d",seq(1:length(t_in$rap_equip_atolegal)))
  vec1 <- apply(vec1, c(1, 2), function(x) 0)
  for (i in 1:length(t_in$rap_equip_atolegal)) {
    vec1[i, 1:t_in$ano_inicio[i]] <-
      t_in$rap_equip_atolegal[i]/sum(t_in$rap_equip_atolegal)*t_in$invest_contrato[i]/t_in$ano_inicio[i]
  }
  investimento <- (colSums(vec1)) 

# receita_bruta  #Valor da RAP estimado em R$
  vec2 <- matrix(nrow = length(t_in$rap_equip_atolegal), ncol = max(t_in$anos_vigencia))
  colnames(vec2) <- sprintf("Ano %d",seq(1:max(t_in$anos_vigencia)))
  rownames(vec2) <- sprintf("Equip. %d",seq(1:length(t_in$rap_equip_atolegal)))
  vec2 <- apply(vec2, c(1, 2), function(x) 0)
  for (i in 1:length(t_in$rap_equip_atolegal)) {
    vec2[i, t_in$ano_inicio[i]:t_in$anos_vigencia[i]] <-  t_in$rap_equip_atolegal[i]
  }
  receita_bruta <- (colSums(vec2))
  #taxa_real #Estimativa de juros real 
  taxa_real <- as.numeric(t_in$taxa_juros[i])
  
  #Ajusta o comprimento dos vetores 
  length(investimento) <- max(length(receita_bruta), length(investimento))                      
  length(receita_bruta) <- max(length(receita_bruta), length(investimento)) 
  
  rm(vec1,vec2)
  
  investimento <- replace_na(investimento,0)
  receita_bruta <- replace_na(receita_bruta,0)
  tt <- rbind(investimento, receita_bruta) %>% replace_na(0)
  
  rm(tt)
##### Dados parametrizados para estimativa dos Custos Fixos ####
impostos <- 0.09  #Estimativa da taxa dos impostos totais, exceto IR (9%)

###### Estimativa percentual dos custos fixos anuais em relação ao total investido da vida útil do equipamento (AnosFim - AnoInicio) ####
# administrativos -> taxa fixa de 0,46%  - vec2
# depreciacao -> taxa fixa de 0,56716% - vec2
# Manutencao -> taxa fixa de 0,4%  - vec3
# Treinamento -> taxa fixa de 0,14%  - vec4
# Mao de Obra -> taxa fixa de 0,5%  - vec5
# Servico Publico -> taxa fixa de 0,5%  - vec5
vec1 <- matrix(nrow = length(t_in$rap_equip_atolegal), ncol = max(t_in$anos_vigencia))
vec2 <- matrix(nrow = length(t_in$rap_equip_atolegal), ncol = max(t_in$anos_vigencia))
vec3 <- matrix(nrow = length(t_in$rap_equip_atolegal), ncol = max(t_in$anos_vigencia))
vec4 <- matrix(nrow = length(t_in$rap_equip_atolegal), ncol = max(t_in$anos_vigencia))
vec5 <- matrix(nrow = length(t_in$rap_equip_atolegal), ncol = max(t_in$anos_vigencia))
vec6 <- matrix(nrow = length(t_in$rap_equip_atolegal), ncol = max(t_in$anos_vigencia))
colnames(vec1) <- sprintf("Ano %d",seq(1:max(t_in$anos_vigencia)))
rownames(vec1) <- sprintf("Equip. %d",seq(1:length(t_in$rap_equip_atolegal)))
vec1 <- apply(vec2, c(1, 2), function(x) 0)
vec2 <- apply(vec2, c(1, 2), function(x) 0)
vec3 <- apply(vec2, c(1, 2), function(x) 0)
vec4 <- apply(vec2, c(1, 2), function(x) 0)
vec5 <- apply(vec2, c(1, 2), function(x) 0)
vec6 <- apply(vec2, c(1, 2), function(x) 0)

for (i in 1:length(t_in$rap_equip_atolegal)) {
  vec1[i, t_in$ano_inicio[i]:t_in$anos_vigencia[i]] <- 
    0.0046*(t_in$rap_equip_atolegal[i]/sum(t_in$rap_equip_atolegal))*t_in$invest_contrato[i]
  
  vec2[i, t_in$ano_inicio[i]:t_in$anos_vigencia[i]] <- 
    0.0056716*(t_in$rap_equip_atolegal[i]/sum(t_in$rap_equip_atolegal))*t_in$invest_contrato[i]
  
  vec3[i, t_in$ano_inicio[i]:t_in$anos_vigencia[i]] <- 
    0.004*(t_in$rap_equip_atolegal[i]/sum(t_in$rap_equip_atolegal))*t_in$invest_contrato[i]
  
  vec4[i, t_in$ano_inicio[i]:t_in$anos_vigencia[i]] <- 
    0.0014*(t_in$rap_equip_atolegal[i]/sum(t_in$rap_equip_atolegal))*t_in$invest_contrato[i]
  
  vec5[i, t_in$ano_inicio[i]:t_in$anos_vigencia[i]] <- 
    0.005*(t_in$rap_equip_atolegal[i]/sum(t_in$rap_equip_atolegal))*t_in$invest_contrato[i]
  
  vec6[i, t_in$ano_inicio[i]:t_in$anos_vigencia[i]] <- 
    0.005*(t_in$rap_equip_atolegal[i]/sum(t_in$rap_equip_atolegal))*t_in$invest_contrato[i]
}

custos_admin <- (colSums(vec1))
custo_fixo_depreciacao <- colSums(vec2)
manutencao <- colSums(vec3) 
treinamento <- colSums(vec4)
mao_de_obra <- colSums(vec5)
servico_publico <- colSums(vec5)

rm(vec1,vec2, vec3, vec4, vec5, vec6)

##### Imposto de renda estimado em 15% ##################
imposto_de_renda <- 0.15
total_Invest <- sum(investimento)

##### Dados parametrizados para estimativa do funding ####
recursos_proprios <- .3 #Fração dos investimentos com recursos próprios

####Criação da tabela de dados####
var_DRE <- c("Receita_Bruta", "Impostos", "Receita_Liquida", "Custos_Fixos_Admin",
             "Custos_Fixos_Depreciacao", "Custos_Fixos_Manutencao","Custos_Fixos_Treinamento",
             "Custos_Var_Mao_de_obra", "Custos_Var_Servicos_Publicos", "Total_Custos",
             "Lucro_Bruto","Imposto_de_Renda", "Lucro_Liquido", "EBDA","Investimentos",
             "Recursos_Proprios","Recursos_de_Terceiros", "Saldo_Inicial","Saldo_Devedor",
             "Pagamento_Juros", "Pagamento_de_Principal", "Fluxo_de_Caixa_TIR", "Fluxo_de_Caixa",
             "Fluxo_de_Caixa_Acumulado", "ICSD_Caixa_Anual", "ICSD_Caixa_Acumulado",
             "Fluxo_Caixa_TIR_Acionista")
receita <- matrix(nrow = length(var_DRE), ncol = max(t_in$anos_vigencia))
rownames(receita)<- var_DRE
colnames(receita) <- sprintf("Ano %d",seq(1:max(t_in$anos_vigencia)))
df_DRE <- as.data.frame(receita)
df_DRE[1:nrow(receita),1:ncol(receita)] <- 0
rm(receita)

df_DRE["Receita_Bruta",] <- receita_bruta
df_DRE["Investimentos", ] <- investimento
df_DRE["Impostos",] <- impostos*df_DRE["Receita_Bruta",]
df_DRE["Receita_Liquida",] <- df_DRE["Receita_Bruta",]-df_DRE["Impostos",]



# ####Alocação custos fixos#####
df_DRE["Custos_Fixos_Admin",] <- custos_admin
df_DRE["Custos_Fixos_Depreciacao",] <- custo_fixo_depreciacao
df_DRE["Custos_Fixos_Manutencao",] <- manutencao
df_DRE["Custos_Fixos_Treinamento",] <- treinamento
# ####Alocação custos variáveis #####
df_DRE["Custos_Var_Mao_de_obra",] <- mao_de_obra
df_DRE["Custos_Var_Servicos_Publicos",] <- servico_publico
# #### Custos e Receitas Totais ####
df_DRE["Total_Custos",] <- colSums(df_DRE[c("Custos_Fixos_Admin", "Custos_Fixos_Depreciacao","Custos_Fixos_Treinamento",
                                            "Custos_Fixos_Manutencao", "Custos_Var_Mao_de_obra", "Custos_Var_Servicos_Publicos"),],na.rm = TRUE)

df_DRE["Lucro_Bruto",] <- df_DRE["Receita_Liquida",] - df_DRE["Total_Custos",]
df_DRE["Imposto_de_Renda",] <- df_DRE["Lucro_Bruto",]*imposto_de_renda
df_DRE["Lucro_Liquido",] <- df_DRE["Lucro_Bruto",] - df_DRE["Imposto_de_Renda",]
df_DRE["EBDA",] <- df_DRE["Lucro_Liquido",] + df_DRE["Custos_Fixos_Depreciacao",]

####### Quadro de usos e fontes #  Substituido pela alacoção feita discretizada por equipamento ####
# aux <- anoInicio-1
# fracao_invest <- c(1:aux)
#   if(aux == 3) { fracao_invest <- c(0.1, 0.5, 0.4)
#   } else if (aux == 4) { fracao_invest <- c(0.1, 0.4, 0.4, 0.1)
#   } else if (aux == 5) { fracao_invest<- c(0.1, 0.3, 0.4, 0.1, 0.1)
#   } else {fracao_invest <- 1/aux}
####################
df_DRE["Investimentos", ] <- -df_DRE["Investimentos", ]
df_DRE["Fluxo_de_Caixa_TIR",] <- colSums(df_DRE[c("Investimentos", "EBDA"),],
                                         na.rm = TRUE)
df_DRE["Recursos_Proprios",] <- -df_DRE["Investimentos",]*recursos_proprios
df_DRE["Recursos_de_Terceiros",] <- -df_DRE["Investimentos",]*(1-recursos_proprios)

# ##### Montagem do Fluxo de Caixa ####
aux <- anoInicio - 1
for(i in 1:anos_Concessao) {
  if (i == 1) {
      df_DRE["Saldo_Inicial",1] <- df_DRE["Recursos_de_Terceiros",1]
      df_DRE["Pagamento_Juros",1] <- taxa_real*df_DRE["Saldo_Inicial",1]
      df_DRE["Saldo_Devedor",1] <- df_DRE["Saldo_Inicial",1] + df_DRE["Pagamento_Juros",1]
  } else if (i>1 & i <= aux) {
  df_DRE["Saldo_Inicial", i] <- df_DRE["Saldo_Devedor", i-1] + df_DRE["Recursos_de_Terceiros", i]
  df_DRE["Pagamento_Juros", i] <- taxa_real*df_DRE["Saldo_Inicial",i]
  df_DRE["Saldo_Devedor",i] <- df_DRE["Saldo_Inicial", i] + df_DRE["Pagamento_Juros", i]
  } else if (i > aux) {
    df_DRE["Saldo_Inicial", i] <- df_DRE["Saldo_Devedor", i-1]
    df_DRE["Pagamento_de_Principal",c(i:anos_Concessao)] <-
              df_DRE["Saldo_Inicial", anoInicio]/(anos_Concessao-aux)
    df_DRE["Pagamento_Juros", i] <- taxa_real*df_DRE["Saldo_Inicial",i]
    df_DRE["Saldo_Devedor", i] <- df_DRE["Saldo_Inicial", i] - df_DRE["Pagamento_de_Principal",i]
  }
}

df_DRE["Fluxo_de_Caixa_TIR",] <- df_DRE["EBDA",] + df_DRE["Investimentos",]
df_DRE["Fluxo_de_Caixa",c(anoInicio:anos_Concessao)] <-  df_DRE["Fluxo_de_Caixa_TIR",c(anoInicio:anos_Concessao)] +
                df_DRE["Recursos_Proprios", c(anoInicio:anos_Concessao)] - df_DRE["Recursos_de_Terceiros",c(anoInicio:anos_Concessao)] -
                df_DRE["Pagamento_Juros",c(anoInicio:anos_Concessao)] - df_DRE["Pagamento_de_Principal",c(anoInicio:anos_Concessao)]


df_DRE["Fluxo_de_Caixa_Acumulado",] <- lag(df_DRE["Fluxo_de_Caixa_Acumulado",],1) + df_DRE["Fluxo_de_Caixa",]
df_DRE["Fluxo_de_Caixa_Acumulado",1] <- df_DRE["Fluxo_de_Caixa",1]
#
# ##### Cálculo dos Índices de Cobertura #####
#   df_DRE["ICSD_Caixa_Anual",] <- df_DRE["EBDA",]/( df_DRE["Pagamento_Juros",]+ df_DRE["Pagamento_de_Principal",])
#   df_DRE["ICSD_Caixa_Acumulado",c(anoInicio:anos_Concessao)] <-
#     (df_DRE["Fluxo_de_Caixa_TIR",c(anoInicio:anos_Concessao)] +
#        lag(df_DRE["Fluxo_de_Caixa_Acumulado",c(anoInicio:anos_Concessao)], 1))/
#     ( df_DRE["Pagamento_Juros",c(anoInicio:anos_Concessao)]+
#         df_DRE["Pagamento_de_Principal",c(anoInicio:anos_Concessao)])
#   df_DRE["ICSD_Caixa_Acumulado",anoInicio] <- df_DRE["ICSD_Caixa_Anual",anoInicio]
#
#
# ##### Fluxo de caixa para os Acionistas #####
#   df_DRE["Fluxo_Caixa_TIR_Acionista",c(1:anoInicio)] <- -df_DRE["Recursos_Proprios",c(1:anoInicio)]
#   df_DRE["Fluxo_Caixa_TIR_Acionista",c(anoInicio:anos_Concessao)] <- df_DRE["Fluxo_de_Caixa_TIR",c(anoInicio:anos_Concessao)] -
#   df_DRE["Pagamento_Juros",c(anoInicio:anos_Concessao)] - df_DRE["Pagamento_de_Principal",c(anoInicio:anos_Concessao)]
#
# ##### Cálculo da TIR ####
#   vetorTIR <- as.vector(as.numeric(df_DRE["Fluxo_de_Caixa_TIR",]))
#   vetorTIR_Acionista <- as.vector(as.numeric(df_DRE["Fluxo_Caixa_TIR_Acionista",]))
#   TIR = IRR(cf0 = 0, cf=vetorTIR,times = c(1:anos_Concessao), plot = FALSE)
#   TIR_ACIONISTA <- IRR(cf0 = 0, cf=vetorTIR_Acionista,times = c(1:anos_Concessao), plot = FALSE)
#   Saida <- list(Lote = nome, TIR = TIR[1], TIR_ACIONISTA = TIR_ACIONISTA[1],
#                 Indice_Cobertura = df_DRE["ICSD_Caixa_Anual",anoInicio],
#                 AnoInicio = anoInicio, Juros = taxa_real)
#   return(Saida)
# }


