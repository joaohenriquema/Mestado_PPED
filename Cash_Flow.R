#Scrip para execução das rotinas de cálculo do fluxo de caixa
#Objetivo: Calcular os principais indicadores finaceiros: TIR do Proejto, Indíce de cobertura

library(magrittr )
library(tidyverse)
library("tidyr")
library(dplyr)
library(lubridate)
library(reshape2)

Empreendimento <- list(nome = character(),investimento = numeric(), receita_bruta = numeric()
                             , taxa_real= numeric(), recursos_proprios = numeric(),
                             rap_Max = numeric(), rap_Vencedora = numeric(), impostos = numeric(),
                       custo_fixo_admin = numeric(), custo_fixo_depreciacao = numeric(), 
                       anoInicio = numeric(), manutencao = numeric(), treinamento = numeric(),
                       mao_de_obra = numeric(), servico_publico = numeric(), imposto_de_renda = numeric())
Empreendimento$nome <- "Lote_15"
Empreendimento$investimento <- 560497000 #Valor estimado dos investimentos (R$)
Empreendimento$receita_bruta <- 91197289 #Valor estimado em R$
Empreendimento$impostos <- 0.09 #Estimativa da taxa dos impostos totais, exceto IR
Empreendimento$taxa_real <- 0.044 #Estimativa de juros real
Empreendimento$anoInicio <-  6 #Ano de entrada em operação após o leilão
#**** Custos Fixos****
Empreendimento$custos_admin <- 0.0046 #Estimativa percentual dos custos fixos administrativos anuais
Empreendimento$custo_fixo_depreciacao <- 0.056716 #Estimativa percentual dos custos fixos anuais relativo à depreciação
Empreendimento$manutencao <- 0.004 #Estimativa percentual dos custos fixos anuais
Empreendimento$treinamento <- 0.0014 #Estimativa percentual dos custos fixos anuais
#**** Custos Variáveis****
Empreendimento$mao_de_obra <- 0.005 #Estimativa percentual dos custos variáveis anuais
Empreendimento$servico_publico <- 0.005 #Estimativa percentual dos custos variáveis anual

Empreendimento$imposto_de_renda <- 0.15

Empreendimento$recursos_proprios <- .3 #Fração dos investimentos com recursos próprios
Empreendimento$rap_Max <- 133273885.72
Empreendimento$rap_Vencedora <- 52510000

####Criação da tabela de dados####
var_DRE <- c("Receita_Bruta", "Impostos", "Receita_Liquida", 
             "Custos_Fixos_Admin","Custos_Fixos_Depreciacao",
             "Custos_Fixos_Manutencao","Custos_Fixos_Treinamento",
             "Custos_Var_Mao_de_obra", "Custos_Var_Servicos_Publicos",
             "Total_Custos","Lucro_Bruto","Imposto_de_Renda", 
             "Lucro_Liquido", "EBDA","Investimentos", 
             "Reinvestimento", "Valor_Residual", "Recursos_Proprios",
             "Recursos_de_Terceiros", "Saldo_Inicial","Saldo_Devedor","Pagamento_Juros",
             "Pagamento_de_Principal", "Fluxo_de_Caixa_TIR", "Fluxo_de_Caixa",
             "Fluxo_de_Caixa_Acumulado", "Fluxo_de_Caixa_TIR_Acionista", "ICSD_Caixa_Anual",
             "ICSD_Caixa_Acumulado","TIR_Projeto", "Fluxo_Caixa_TIR_Acionista",
             "TIR_Acionista")
receita <- matrix(nrow = length(var_DRE), ncol = 30)
rownames(receita)<- var_DRE
colnames(receita) <- c(1:30)
df_DRE <- as.data.frame(receita)
i <- is.numeric(i)
for (i in 1:30) {df_DRE[,i] <- df_DRE[,i] %>% replace_na(0)}
rm(receita)

df_DRE["Receita_Bruta", c(Empreendimento$anoInicio:30)] <- Empreendimento$receita_bruta
df_DRE["Impostos",] <- Empreendimento$impostos*df_DRE["Receita_Bruta",]
df_DRE["Receita_Liquida",] <- df_DRE["Receita_Bruta",]-df_DRE["Impostos",]

####Alocação custos fixos #####
df_DRE["Custos_Fixos_Admin",c(Empreendimento$anoInicio:30)] <- Empreendimento$custos_admin*Empreendimento$investimento
df_DRE["Custos_Fixos_Depreciacao",c(Empreendimento$anoInicio:30)] <- Empreendimento$custo_fixo_depreciacao*Empreendimento$investimento
df_DRE["Custos_Fixos_Manutencao",c(Empreendimento$anoInicio:30)] <- Empreendimento$manutencao*Empreendimento$investimento
df_DRE["Custos_Fixos_Treinamento",c(Empreendimento$anoInicio:30)] <- Empreendimento$treinamento*Empreendimento$investimento
####Alocação custos variáveis #####
df_DRE["Custos_Var_Mao_de_obra",c(Empreendimento$anoInicio:30)] <- Empreendimento$mao_de_obra*Empreendimento$investimento
df_DRE["Custos_Var_Servicos_Publicos",c(Empreendimento$anoInicio:30)] <- Empreendimento$servico_publico*Empreendimento$investimento
#### Custos e Receitas Totais ####
df_DRE["Total_Custos",] <- colSums(df_DRE[c("Custos_Fixos_Admin", "Custos_Fixos_Depreciacao","Custos_Fixos_Treinamento",
                                            "Custos_Fixos_Manutencao", "Custos_Var_Mao_de_obra", "Custos_Var_Servicos_Publicos"),],na.rm = TRUE)

df_DRE["Lucro_Bruto",] <- df_DRE["Receita_Liquida",] - df_DRE["Total_Custos",]
df_DRE["Imposto_de_Renda",] <- df_DRE["Lucro_Bruto",]*Empreendimento$imposto_de_renda
df_DRE["Lucro_Liquido",] <- df_DRE["Lucro_Bruto",] - df_DRE["Imposto_de_Renda",]
df_DRE["EBDA",] <- df_DRE["Lucro_Liquido",] + df_DRE["Custos_Fixos_Depreciacao",]

#### Quadro de usos e fontes ####
aux <- Empreendimento$anoInicio-1
fracao_invest <- c(1:aux)
  if(aux == 3) { fracao_invest <- c(0.1, 0.5, 0.4) 
  } else if (aux == 4) { fracao_invest <- c(0.1, 0.4, 0.4, 0.1)
  } else if (aux == 5) { fracao_invest<- c(0.1, 0.3, 0.4, 0.1, 0.1)
  } else {fracao_invest <- 1/aux}
df_DRE["Investimentos",c(1:aux)] <- -Empreendimento$investimento*fracao_invest
df_DRE["Fluxo_de_Caixa_TIR",] <- colSums(df_DRE[c("Investimentos", "EBDA"),],
                                         na.rm = TRUE)
df_DRE["Recursos_Proprios",] <- -df_DRE["Investimentos",]*Empreendimento$recursos_proprios
df_DRE["Recursos_de_Terceiros",] <- -df_DRE["Investimentos",]*(1-Empreendimento$recursos_proprios)

##### Montagem do Fluxo de Caixa ####
for(i in 1:30) {
  if (i == 1) {
      df_DRE["Saldo_Inicial",1] <- df_DRE["Recursos_de_Terceiros",1] 
      df_DRE["Pagamento_Juros",1] <- Empreendimento$taxa_real*df_DRE["Saldo_Inicial",1]
      df_DRE["Saldo_Devedor",1] <- df_DRE["Saldo_Inicial",1] + df_DRE["Pagamento_Juros",1]
  } else if (i>1 & i <= aux) {
  df_DRE["Saldo_Inicial", i] <- df_DRE["Saldo_Devedor", i-1] + df_DRE["Recursos_de_Terceiros", i]
  df_DRE["Pagamento_Juros", i] <- Empreendimento$taxa_real*df_DRE["Saldo_Inicial",i]
  df_DRE["Saldo_Devedor",i] <- df_DRE["Saldo_Inicial", i] + df_DRE["Pagamento_Juros", i]
  } else if (i > aux) {
    df_DRE["Saldo_Inicial", i] <- df_DRE["Saldo_Devedor", i-1]
    df_DRE["Pagamento_de_Principal",c(Empreendimento$anoInicio:30)] <- 
              df_DRE["Saldo_Inicial", Empreendimento$anoInicio]/(30-aux) 
    df_DRE["Pagamento_Juros", i] <- Empreendimento$taxa_real*df_DRE["Saldo_Inicial",i]
    df_DRE["Saldo_Devedor", i] <- df_DRE["Saldo_Inicial", i] - df_DRE["Pagamento_de_Principal",i]
  }
}
df_DRE["Fluxo_de_Caixa_TIR",] <- df_DRE["EBDA",] + df_DRE["Investimentos",]
df_DRE["Fluxo_de_Caixa",c(Empreendimento$anoInicio:30)] <-  df_DRE["Fluxo_de_Caixa_TIR",c(Empreendimento$anoInicio:30)] + 
                df_DRE["Recursos_Proprios", c(Empreendimento$anoInicio:30)] - df_DRE["Recursos_de_Terceiros",c(Empreendimento$anoInicio:30)] -
                df_DRE["Pagamento_Juros",c(Empreendimento$anoInicio:30)] - df_DRE["Pagamento_de_Principal",c(Empreendimento$anoInicio:30)]

