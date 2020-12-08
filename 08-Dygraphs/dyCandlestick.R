




# Gráficos Interativos de Séries Tempoarais com Dygraphs

# https://rstudio.github.io/dygraphs/



# Pacote
install.packages("dygraphs")
library(dygraphs)
library(xts)




# Dados ------------------------------------------------------------------------
data(sample_matrix)
m <- tail(sample_matrix, n = 32)
head(m)
View(m) # um mês de dados 


# Plot candlestick/OHLC charts (OHLC = open/high/low/close)
dygraph(m) %>% dyCandlestick()





# First four data series  + linha para a média ---------------------------------
m <- cbind(m, apply(m[, 1:3], 1, mean))

# Add col para a média 
colnames(m)[5] <- "Mean"
str(m)
View(m)


# Plot
dygraph(m) %>% dyCandlestick()






# Compactar os dados do gráfico anual/trimestral/mensal/semanal/diário ---------

# library(xts)

# Dados
data(sample_matrix)

# Plot
dygraph(sample_matrix) %>% dyCandlestick(compress = TRUE)




# End











