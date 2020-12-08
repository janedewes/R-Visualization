


# Gráficos Interativos de Séries Tempoarais com Dygraphs

# https://rstudio.github.io/dygraphs/



# Pacote
install.packages("dygraphs")
library(dygraphs)
library(xts)
library(quantmod)


################################################################################
# Dados da Microsoft 
quantmod::getSymbols("MSFT", from = "2020-06-01", auto.assign=TRUE)



# Dataset
View(MSFT)
microsoft <- cbind(MSFT[, 1:4])
View(microsoft)




# Plot
dygraph(MSFT[, 4], main = "Microsoft Share Price") %>% dySeries("MSFT.Close", label = "MSFT") %>%
  dyLimit(as.numeric(MSFT[1, 4]), color = "red")




# Plot Candlestick
dygraph(microsoft) %>% dyCandlestick()





################################################################################
# Dados Microsoft e HP 
getSymbols(c("MSFT", "HPQ"), from = "2020-06-01", auto.assign=TRUE)

View(HPQ)

# Agrupar os dados em um dataset
stocks <- cbind(MSFT[,2:4], HPQ[,2:4]) # todas as linhas, col 32 até a col 4
View(stocks)

dygraph(stocks, main = "Microsoft and HP Share Prices") %>% 
  dySeries(c("MSFT.Low", "MSFT.Close", "MSFT.High"), label = "MSFT") %>%
  dySeries(c("HPQ.Low", "HPQ.Close", "HPQ.High"), label = "HPQ")




################################################################################
# Petrobrás


# Pacotes
library(quantmod)
library(xts)
library(moments)
library(dygraphs)


# Período
startDate = as.Date("2020-06-01")
endDate = as.Date("2020-12-07")


# Dados da Petrobras
getSymbols("PETR4.SA", src = "yahoo", from = startDate, to = endDate, auto.assign = T)

class(PETR4.SA)
head(PETR4.SA)
View(PETR4.SA)
tail(PETR4.SA)



# Analisando os dados de fechamento 
PETR4.SA.Close <- PETR4.SA[, "PETR4.SA.Close"] 
View(PETR4.SA.Close)


# Plot de fechamento
dygraph(PETR4.SA.Close, main = "Petrobrás")


# Plot Candlestick
petra <- cbind(PETR4.SA[, 1:4])
View(petra)
summary(petra)
dygraph(petra) %>% dyCandlestick()






# End