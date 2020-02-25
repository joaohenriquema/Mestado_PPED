source("Cash_Flow_v2.r")
a <- list()
for (i in 1:nrow(t)) {
  print(i)
  a[i] <- list(fCash_Flow(t$contrato_da_receita[i],t$invest_contrato[i],t$taxa_juros[i],
                     t$ano_inicio[i], t$proposta_RAP[i]))
}

# Convet a lista para um data.frame
a <- as.data.frame(matrix(unlist(a), nrow=length(a), byrow=T))
names(a) <- c("Lote", "TIR","TIR_ACIONISTA","Indice_Cobertura" ,"anoInicio", "Juros")

ggplot(a, aes(x = Lote, y = TIR )) + geom_point()

# 
# LoteJ <- fCash_Flow(as.numeric(t$contrato_da_receita)[240], as.numeric(t$invest_contrato[240])
#                     , 0.044, 5, as.numeric(t$proposta_RAP[240]))

# exemplo lapply
# x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
# # compute the list mean for each list element
# lapply(a, mean)
