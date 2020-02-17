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
                       mao_de_obra = numeric(), servico_publico = numeric())
Empreendimento$nome <- "Lote_15"
Empreendimento$investimento <- 560497000 #Valor estimado dos investimentos (R$)
Empreendimento$receita_bruta <- 91197289 #Valor estimado em R$
Empreendimento$impostos <- 0.09 #Estimativa da taxa dos impostos totais, exceto IR
Empreendimento$taxa_real <- 0.044 #Estimativa de juros real
Empreendimento$anoInicio <-  5 #Ano de entrada em operação após o leilão
#**** Custos Fixos****
Empreendimento$custos_admin <- 0.0046 #Estimativa percentual dos custos fixos administrativos anuais
Empreendimento$custo_fixo_depreciacao <- 0.056716 #Estimativa percentual dos custos fixos anuais relativo à depreciação
Empreendimento$manutencao <- 0.004 #Estimativa percentual dos custos fixos anuais
Empreendimento$treinamento <- 0.0014 #Estimativa percentual dos custos fixos anuais
#**** Custos Variáveis****
Empreendimento$mao_de_obra <- 0.005 #Estimativa percentual dos custos variáveis anuais
Empreendimento$servico_publico <- 0.005 #Estimativa percentual dos custos variáveis anua

Empreendimento$recursos_proprios <- .3 #Fração dos investimentos com recursos próprios
Empreendimento$rap_Max <- 133273885.72
Empreendimento$rap_Vencedora <- 52510000

####Criação da tabela de dados####
var_DRE <- c("Receita_Bruta", "Impostos", "Receita_Liquida", 
             "Custos_Fixos_Admin","Custos_Fixos_Depreciacao",
             "Custos_Fixos_Manutencao","Custos_Fixos_Treinamento",
             "Custos_Var_Mao_de_obra", "Custos_Var_Servicos_Publicos",
             "Total_Custos","Lucro_Bruto","Imposto_de_Renda", 
             "Lucro_Liquido", "Depreciacao","EBTDA","Investimentos", 
             "Reinvestimento", "Valor_Residual", "Fluxo_de_Caixa_TIR",
             "Recursos_Proprios", "Recursos_de_Terceiros","Pagamento_de_Juro",
             "Pagamento_de_Principal", "Fluxo_de_Caixa","Fluxo_de_Caixa_Acumulado", 
             "Fluxo_de_Caixa_Acumulado_conta_reserva", "ICSD_Caixa_Anual",
             "ICSD_Caixa_Acumulado","TIR_Projeto", "Fluxo_Caixa_TIR_Acionista",
             "TIR_Acionista")
receita <- matrix(nrow = length(var_DRE), ncol = 30)
rownames(receita)<- var_DRE
colnames(receita) <- c(1:30)
df_DRE <- as.data.frame(receita)
rm(receita)


df_DRE["Receita_Bruta", c(Empreendimento$anoInicio:30)] <- Empreendimento$receita_bruta
df_DRE["Impostos",] <- Empreendimento$impostos*df_DRE["Receita_Bruta",]
df_DRE["Receita_Liquida",] <- df_DRE["Receita_Bruta",]-df_DRE["Impostos",]

####Alocação custos fixos #####
df_DRE["Custos_Fixos_Admin",] <- Empreendimento$custos_admin*Empreendimento$investimento
df_DRE["Custos_Fixos_Depreciacao",c(Empreendimento$anoInicio:30)] <- Empreendimento$custo_fixo_depreciacao*Empreendimento$investimento
df_DRE["Custos_Fixos_Manutencao",c(Empreendimento$anoInicio:30)] <- Empreendimento$manutencao*Empreendimento$investimento
df_DRE["Custos_Fixos_Treinamento",c(Empreendimento$anoInicio:30)] <- Empreendimento$treinamento*Empreendimento$investimento
####Alocação custos variáveis #####
df_DRE["Custos_Var_Mao_de_obra",c(Empreendimento$anoInicio:30)] <- Empreendimento$mao_de_obra*Empreendimento$investimento
df_DRE["Custos_Var_Servicos_Publicos",c(Empreendimento$anoInicio:30)] <- Empreendimento$servico_publico*Empreendimento$investimento
#### Custos Totais ####
df_DRE["Total_Custos",] <- colSums(df_DRE[c("Custos_Fixos_Admin", "Custos_Fixos_Depreciacao","Custos_Fixos_Treinamento",
                                            "Custos_Fixos_Manutencao", "Custos_Var_Mao_de_obra", "Custos_Var_Servicos_Publicos"),],na.rm = TRUE)





