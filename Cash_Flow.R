#Scrip para execução das rotinas de cálculo do fluxo de caixa
#Objetivo: Calcular os principais indicadores finaceiros: TIR do Proejto, Indíce de cobertura

library(tidyverse)
library("tidyr")
library(dplyr)
library(lubridate)

Empreendimento <- list(nome = character(),investimento = numeric(), receita_bruta = numeric()
                             , taxa_real= numeric(), recursos_proprios = numeric(),
                             rap_Max = numeric(), rap_Vencedora = numeric(), impostos = numeric())
Empreendimento$nome <- "Lote_15"
Empreendimento$investimento <- 560497000
Empreendimento$receita_bruta <- 91197289
Empreendimento$impostos <- 9
Empreendimento$taxa_real <- 4.4
Empreendimento$recursos_proprios <- 30
Empreendimento$rap_Max <- 133273885.72
Empreendimento$rap_Vencedora <- 52510000
var_DRE <- c("Receita_Bruta", "Impostos", "Receita_Liquida", 
             "Custos_Fixos_Admin","Custos_Fixos_Depreciacao",
             "Custos_Fixos_Manutencao","Custos_Fixos_Treinamento",
             "Custos_Var_Mao_de_obra", "Custos_Var_Servicos_Publicos",
             "Custos_Var_Total_Custos","Lucro_Bruto","Imposto_de_Renda", 
             "Lucro_Liquido", "Depreciacao","EBTDA","Investimentos", 
             "Reinvestimento", "Valor_Residual", "Fluxo_de_Caixa_TIR",
             "Recursos_Proprios", "Recursos_de_Terceiros","Pagamento_de_Juro",
             "Pagamento_de_Principal", "Fluxo_de_Caixa","Fluxo_de_Caixa_Acumulado", 
             "Fluxo_de_Caixa_Acumulado_conta_reserva", "ICSD_Caixa_Anual",
             "ICSD_Caixa_Acumulado","TIR_Projeto", "Fluxo_Caixa_TIR_Acionista",
             "TIR_Acionista")
receita <- matrix(nrow = length(var_DRE), ncol = 30)
rownames(receita) <- var_DRE
colnames(receita) <- c(1:30)
receita[rowname = c("Receita_Bruta"), c(5:30)] <- Empreendimento$receita_bruta
receita[rownames= c("Impostos"),c(5:30)] <- Empreendimento$impostos

Empreendimento$receita <- receita 
                       
                                

