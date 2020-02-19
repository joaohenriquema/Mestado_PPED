#Scrip para execução das rotinas de cálculo do fluxo de caixa
#Objetivo: Calcular os principais indicadores finaceiros: TIR do Proejto, Indíce de cobertura

library(magrittr )
library(tidyverse)
library("tidyr")
library(dplyr)
library(lubridate)
library(reshape2)
library(FinancialMath)

fCash_Flow <- function(nome, investimento, taxa_real, anoInicio, receita_bruta) {

##### Dados de Entrada dos Empreendimentos
nome <- nome 
investimento <- investimento  #Valor estimado dos investimentos R$
taxa_real <- taxa_real #Estimativa de juros real 
anoInicio <-  anoInicio #Ano de entrada em operação após o leilão

receita_bruta <- receita_bruta  #Valor da RAP estimado em R$

##### Dados parametrizados para estimativa dos Custos Fixos ####
impostos <- 0.09  #Estimativa da taxa dos impostos totais, exceto IR (9%)
custos_admin <- 0.0046 #Estimativa percentual dos custos fixos administrativos anuais
custo_fixo_depreciacao <- 0.056716 #Estimativa percentual dos custos fixos anuais relativo à depreciação
manutencao <- 0.004 #Estimativa percentual dos custos fixos anuais
treinamento <- 0.0014 #Estimativa percentual dos custos fixos anuais

##### Dados parametrizados para estimativa dos Custos Variáveis ##### 
mao_de_obra <- 0.005 #Estimativa percentual dos custos variáveis anuais
servico_publico <- 0.005 #Estimativa percentual dos custos variáveis anual
imposto_de_renda <- 0.15

##### Dados parametrizados para estimativa do funding ####
recursos_proprios <- .3 #Fração dos investimentos com recursos próprios
anos_Concessao <- 30 # Período de concessão, normalmente 30 anos para LTs e Equipamentos

####Criação da tabela de dados####
var_DRE <- c("Receita_Bruta", "Impostos", "Receita_Liquida", "Custos_Fixos_Admin",
             "Custos_Fixos_Depreciacao", "Custos_Fixos_Manutencao","Custos_Fixos_Treinamento",
             "Custos_Var_Mao_de_obra", "Custos_Var_Servicos_Publicos", "Total_Custos",
             "Lucro_Bruto","Imposto_de_Renda", "Lucro_Liquido", "EBDA","Investimentos", 
             "Recursos_Proprios","Recursos_de_Terceiros", "Saldo_Inicial","Saldo_Devedor",
             "Pagamento_Juros", "Pagamento_de_Principal", "Fluxo_de_Caixa_TIR", "Fluxo_de_Caixa",
             "Fluxo_de_Caixa_Acumulado", "ICSD_Caixa_Anual", "ICSD_Caixa_Acumulado",
             "Fluxo_Caixa_TIR_Acionista")
receita <- matrix(nrow = length(var_DRE), ncol = anos_Concessao)
rownames(receita)<- var_DRE
colnames(receita) <- c(1:anos_Concessao)
df_DRE <- as.data.frame(receita)
for (i in 1:anos_Concessao) {df_DRE[,i] <- df_DRE[,i] %>% replace_na(0)}
rm(receita)

df_DRE["Receita_Bruta", c(anoInicio:anos_Concessao)] <- receita_bruta
df_DRE["Impostos",] <- impostos*df_DRE["Receita_Bruta",]
df_DRE["Receita_Liquida",] <- df_DRE["Receita_Bruta",]-df_DRE["Impostos",]

####Alocação custos fixos #####
df_DRE["Custos_Fixos_Admin",c(anoInicio:anos_Concessao)] <- custos_admin*investimento
df_DRE["Custos_Fixos_Depreciacao",c(anoInicio:anos_Concessao)] <- custo_fixo_depreciacao*investimento
df_DRE["Custos_Fixos_Manutencao",c(anoInicio:anos_Concessao)] <- manutencao*investimento
df_DRE["Custos_Fixos_Treinamento",c(anoInicio:anos_Concessao)] <- treinamento*investimento
####Alocação custos variáveis #####
df_DRE["Custos_Var_Mao_de_obra",c(anoInicio:anos_Concessao)] <- mao_de_obra*investimento
df_DRE["Custos_Var_Servicos_Publicos",c(anoInicio:anos_Concessao)] <- servico_publico*investimento
#### Custos e Receitas Totais ####
df_DRE["Total_Custos",] <- colSums(df_DRE[c("Custos_Fixos_Admin", "Custos_Fixos_Depreciacao","Custos_Fixos_Treinamento",
                                            "Custos_Fixos_Manutencao", "Custos_Var_Mao_de_obra", "Custos_Var_Servicos_Publicos"),],na.rm = TRUE)

df_DRE["Lucro_Bruto",] <- df_DRE["Receita_Liquida",] - df_DRE["Total_Custos",]
df_DRE["Imposto_de_Renda",] <- df_DRE["Lucro_Bruto",]*imposto_de_renda
df_DRE["Lucro_Liquido",] <- df_DRE["Lucro_Bruto",] - df_DRE["Imposto_de_Renda",]
df_DRE["EBDA",] <- df_DRE["Lucro_Liquido",] + df_DRE["Custos_Fixos_Depreciacao",]

#### Quadro de usos e fontes ####
aux <- anoInicio-1
fracao_invest <- c(1:aux)
  if(aux == 3) { fracao_invest <- c(0.1, 0.5, 0.4) 
  } else if (aux == 4) { fracao_invest <- c(0.1, 0.4, 0.4, 0.1)
  } else if (aux == 5) { fracao_invest<- c(0.1, 0.3, 0.4, 0.1, 0.1)
  } else {fracao_invest <- 1/aux}
df_DRE["Investimentos",c(1:aux)] <- -investimento*fracao_invest
df_DRE["Fluxo_de_Caixa_TIR",] <- colSums(df_DRE[c("Investimentos", "EBDA"),],
                                         na.rm = TRUE)
df_DRE["Recursos_Proprios",] <- -df_DRE["Investimentos",]*recursos_proprios
df_DRE["Recursos_de_Terceiros",] <- -df_DRE["Investimentos",]*(1-recursos_proprios)

##### Montagem do Fluxo de Caixa ####
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

df_DRE["Fluxo_de_Caixa_Acumulado",] <- lag(df_DRE["Fluxo_de_Caixa_Acumulado",],1) + df_DRE["Fluxo_de_Caixa",i]

##### Cálculo dos Índices de Cobertura #####
df_DRE["ICSD_Caixa_Anual",] <- df_DRE["EBDA",]/( df_DRE["Pagamento_Juros",]+ df_DRE["Pagamento_de_Principal",])
df_DRE["ICSD_Caixa_Acumulado",c(anoInicio:anos_Concessao)] <- 
  (df_DRE["Fluxo_de_Caixa_TIR",c(anoInicio:anos_Concessao)] + 
     lag(df_DRE["Fluxo_de_Caixa_Acumulado",c(anoInicio:anos_Concessao)], 1))/
  ( df_DRE["Pagamento_Juros",c(anoInicio:anos_Concessao)]+ 
      df_DRE["Pagamento_de_Principal",c(anoInicio:anos_Concessao)])
df_DRE["ICSD_Caixa_Acumulado",anoInicio] <- df_DRE["ICSD_Caixa_Anual",anoInicio]


##### Fluxo de caixa para os Acionistas #####
df_DRE["Fluxo_Caixa_TIR_Acionista",c(1:anoInicio)] <- -df_DRE["Recursos_Proprios",c(1:anoInicio)]
df_DRE["Fluxo_Caixa_TIR_Acionista",c(anoInicio:anos_Concessao)] <- df_DRE["Fluxo_de_Caixa_TIR",c(anoInicio:anos_Concessao)] - 
  df_DRE["Pagamento_Juros",c(anoInicio:anos_Concessao)] - df_DRE["Pagamento_de_Principal",c(anoInicio:anos_Concessao)]

##### Cálculo da TIR ####
vetorTIR <- as.vector(as.numeric(df_DRE["Fluxo_de_Caixa_TIR",]))
vetorTIR_Acionista <- as.vector(as.numeric(df_DRE["Fluxo_Caixa_TIR_Acionista",]))
TIR = IRR(cf0 = 0, cf=vetorTIR,times = c(1:anos_Concessao), plot = TRUE)
TIR_ACIONISTA <- IRR(cf0 = 0, cf=vetorTIR_Acionista,times = c(1:anos_Concessao), plot = TRUE)
str(TIR_ACIONISTA)
Saida <- list(TIR = TIR[1], TIR_ACIONISTA = TIR_ACIONISTA[1], Indice_Cobertura = df_DRE["ICSD_Caixa_Anual",anoInicio])
return(Saida)
}


Lote15_a <- fCash_Flow("teste", 560497000, 0.044, 6, 61630000) 
# Lote15_b <- fCash_Flow("teste", 560497000, 0.044, 5, 61630000) 
Lote16 <- fCash_Flow("teste", 64146090.61, 0.044, 6, 5800000) 
Lote18 <- fCash_Flow("teste", 57436000, 0.044, 6, 7800000) 
tt <- list(Lote15, Lote16, Lote18)
tt
