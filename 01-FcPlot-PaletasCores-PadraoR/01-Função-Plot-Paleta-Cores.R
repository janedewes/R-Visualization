


# Func Plot e Paleta de Cores


# Def dir
setwd("")
getwd()




# OBS: uma vez definido o background color dos gráficos com a fç par, ela sempre será usada, 
# é preciso usar a fç novamente para aterar essa cor. 




# Criando um plot básico -------------------------------------------------------
x <- rnorm(1000)
?plot
plot(x)
plot(x, col = "red") #cor
colors() #cores disponíveis



# Carregando dados -------------------------------------------------------------
vendas <- read.csv("vendas.csv", header = TRUE)
View(vendas)
str(vendas)


# Especificando o tipo de plot - l para linha ----------------------------------
plot(vendas$units ~ as.Date(vendas$date, "%d/%m/%y"), type = "l",  col = "blue") # type = "l" linhas
plot(vendas$units ~ as.Date(vendas$date, "%d/%m/%y"), type = "b",  col = "darkblue") # type = "b" ambos
plot(vendas$units ~ as.Date(vendas$date, "%d/%m/%y"), type = "h",  col = "red") # type = "h" linhas verticais
?plot


# Definindo as cores de fundo --------------------------------------------------
?par
par(bg = "gray") # backgroundcolor vale para todas as seçoes seguintes!!
plot(x)



# Gerando contraste entre a área de desenho e a área do gráfico ----------------
x <- rnorm(100)
plot(x, type = "n")
x <- par("usr")
rect(x[1],x[3],x[2],x[4], col = "lightgray")
points(x)




# Definindo as elementos de texto: axis labels, titles, plot titles e legends --
plot(x,  main = "Título do Plot", 
     col.axis = "blue", 
     col.lab = "red", 
     col.main = "darkblue", 
     xlab = "Mês", ylab = "Vendas")



# Formatando -------------------------------------------------------------------
par(col.axis = "black", col.lab = "#444444", col.main = "darkblue")
plot(x)



# Adicionando título -----------------------------------------------------------
par(col.axis = "black", col.lab = "#444444", col.main = "darkblue")
plot(x, col.axis = "blue", col.lab = "red", col.main = "darkblue", xlab = "Mês", ylab = "Vendas")
title("Vendas de 2010", col.main = "blue")



# Selecionando a combinação de cores e paletas ---------------------------------
?palette
palette(c("red", "blue", "green", "orange"))
palette("default")




# Pacote RColorBrewer ------------------------- Instalar -----------------------
install.packages("RColorBrewer")
library(RColorBrewer)



# Visualizando a paleta de cores ------------ Paleta Cores ---------------------
display.brewer.all()
brewer.pal(7, "YlOrRd")
display.brewer.pal(7, "YlOrRd")
palette(brewer.pal(7, "YlOrRd"))
pal1<- brewer.pal(7, "YlOrRd")



# Definindo as fontes e tamanho -------------------- Fontes --------------------
par(family = "serif", font = 2)
names(pdfFonts())



# Definindo o sistema de símbolos do plot e tamanhos ---------------------------
chuvas <- read.csv("chuvas.csv")
View(chuvas)

par(bg = "white")

plot(chuvas$Tokyo, ylim = c(0,250), 
     main = "Chuva Média por Mês", 
     xlab = "Mês", ylab = "Chuvas(mm)", pch = 1)




# Acrescentando outros dados ao gráficos ---------------------------------------
points(chuvas$NewYork, pch = 2)
points(chuvas$London, pch = 3)
points(chuvas$Berlin, pch = 4)



# Legenda 
?legend
legend("top", legend = c("Tokyo", "New York", "London", "Berlin"), 
       ncol = 4, cex = 0.8, bty = "n", pch = 1:4)




# Escolhendo estilos de linha e largura ----------------------------------------
plot(chuvas$Tokyo, 
     ylim = c(0,250), 
     main = "Chuva Média por Mês", 
     xlab = "Mês", 
     ylab = "Chuvas(mm)", 
     type = "l", 
     lty = 1, 
     lwd = 2)



# Acrescentando linhas ---------------------------------------------------------
lines(chuvas$NewYork, lty = 2, lwd = 2) 
lines(chuvas$London, lty = 3, lwd = 2)
lines(chuvas$Berlin, lty = 4, lwd = 2)


# Legenda
legend("top", legend = c("Tokyo", "New York", "London", "Berlin"), 
       ncol = 4, cex = 0.8, bty = "n", lty = 1:4, lwd = 2)




# Definindo estilo da área do gráfico ------------------------------------------
x <- rnorm(100)
par(bty = "l")
plot(x)



# Adicionando contorno ---------------------------------------------------------
par(oma = c(1,1,1,1))
plot(x, bty = "l")
box(which = "figure")




# End 