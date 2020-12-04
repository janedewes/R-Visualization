


# Criando Gráficos com ggplot2


# Def dir
setwd("")
getwd()



# Instalando e carregando o pacote ---------------------------------------------
install.packages("ggplot2")
library(ggplot2)



# Definindo o conjunto de dados com dataset tips -------------------------------
??tips
data(tips, package = 'reshape2') 
View(tips)




# Camada 1 ---------------------------------------------------------------------
?aes # estética (escala)
??aes
camada1 <- geom_point(
  mapping = aes(x = total_bill, y = tip, color = sex),
  data = tips,
  size = 3
)

# Chamar a fç(ggplot) e chamar a camada1
ggplot() + camada1 # montar o plot




# Contruindo um modelo de regressão --------------------------------------------
modelo_base <- lm(tip ~ total_bill, data = tips)

modelo_fit <- data.frame(
  total_bill = tips$total_bill, 
  predict(modelo_base, interval = "confidence")
)

head(modelo_fit)

# Camada 2 ---------------------------------------------------------------------
camada2 <- geom_line(
  mapping = aes(x = total_bill, y = fit),
  data = modelo_fit,
  color = "darkred"
)


ggplot() + camada1 + camada2 #montar o plot



# Camada 3 ---------------------------------------------------------------------
camada3 <- geom_ribbon(
  mapping = aes(x = total_bill, ymin = lwr, ymax = upr),
  data = modelo_fit,
  alpha = 0.3
)


ggplot() + camada1 + camada2 + camada3 # montar o plot



# ------------------------------------------------------------------------------
# Versão final otimizada ---------Com um [unico comando fazemos oq foi feito nas 3 camadas acima-----------
ggplot(tips, aes(x = total_bill, y = tip)) +
  geom_point(aes(color = sex)) +
  geom_smooth(method = 'lm')
# ------------------------------------------------------------------------------



# Gravando o gráfico em um objeto ----------------------------------------------
myplot <- ggplot(tips, aes(x = total_bill, y = tip)) +
  geom_point(aes(color = sex)) +
  geom_smooth(method = 'lm')


class(myplot)
print(myplot)





################################################################################
############################## Usando Base Plotting System #####################

# Colocando gráficos lado a lado na área de desenho (diferente do parplot!!!   )


# Carregando os dados ----------------------------------------------------------
bikes <- read.csv("bikes.csv")
head(bikes)
str(bikes)
# Transf em fator:
bikes$season <- factor(bikes$season, levels = c(1,2,3,4), labels = c("Primavera", "Verão", "Outono", "Inverno"))
attach(bikes) 
head(bikes$season)



# Dividindo a área de desenho em 4 sub-áreas
par(mfrow = c(2,2)) # 2 linhas e 2 cols



# Coletando amostras dos dados ---------------- em subsets----------------------
primavera <- subset(bikes, season == "Primavera")$cnt #cnt = contagem 
verao <- subset(bikes, season == "Verão")$cnt
outono <- subset(bikes, season == "Outono")$cnt
inverno <- subset(bikes, season == "Inverno")$cnt




# Desenhando os gráficos
hist(primavera, prob = TRUE, xlab = "Aluguel Diário na Primavera", main = "")
lines(density(primavera))


hist(verao, prob = TRUE, xlab = "Aluguel Diário no Verão", main = "")
lines(density(verao))


hist(outono, prob = TRUE, xlab = "Aluguel Diário no Outono", main = "")
lines(density(outono))


hist(inverno, prob = TRUE, xlab = "Aluguel Diário no Inverno", main = "")
lines(density(inverno))





################################################################################
############################ Usando ggplot2 ####################################
str(bikes)

# fç do ggplot2: qplot
# Var: season
# facet_wrap: divisão em diferentes areas de desnehos
# geom_histogram: tipo de grafico

# 4 plots lado a lado:
qplot(cnt, data = bikes) + facet_wrap(~ season, nrow = 2) + geom_histogram(fill = "blue") 

# 4 plots sobreppostos:
qplot(cnt, data = bikes, fill = season) 





# Gráficos lado a lado na MESMA área de gráfico---------------------------------
qplot(season, cnt, data = bikes, geom = c("boxplot"), fill = season) # usando a fç qplot
ggplot(bikes, aes(x = season, y = cnt)) + geom_boxplot() # usando a fç ggplot





# ------------------------------------------------------------------------------
# Plots multivariados --------------------insfos de diversas variaveis ---------
bikes <- read.csv("bikes.csv")


# Esse tipo de gráfico requer variáveis categóricas (converter p fator)
bikes$season <- factor(bikes$season, levels = c(1,2,3,4), labels = c("Primavera", "Verão", "Outono", "Inverno"))
bikes$weathersit <- factor(bikes$weathersit, levels = c(1,2,3), labels = c("Sol", "Nublado", "Chuva"))
bikes$windspeed.fac <- cut(bikes$windspeed, breaks = 3, labels = c("Baixo", "Médio", "Alto"))
bikes$weekday <- factor(bikes$weekday, levels = c(0:6), labels = c("Dom", "Seg", "Ter", "Qua", "Qui", "Sex", "Sab"))
attach(bikes)  

str(bikes)



# Criando o objeto plot
plot <- ggplot(bikes, aes(temp, cnt)) # col temp e cnt



# Adicionando camadas ao plot + (add ao plot acima, o windspeed) + (weekday) 
plot + geom_point(size = 3, 
                  aes(color = factor(windspeed.fac))) + 
  geom_smooth(method = "lm", se = FALSE, col = "red") + 
  facet_grid(weekday ~ season) + 
  theme(legend.position = "bottom") 




################################################################################
# Scatter Plot e Scatter Plot 3d



# Scatter Plot

# Dados
data = data.frame(cond = rep(c("Obs 1", "Obs 2"), 
                             each = 10), var1 = 1:100 + rnorm(100, sd = 9), 
                  var2 = 1:100 + rnorm(100,sd = 16))
head(data)


# Plot (com ggplot) ------------------------------------------------------------
ggplot(data, aes(x = var1, y = var2)) +    
  geom_point(shape = 1) +  
  geom_smooth(method = lm , color = "red", se = FALSE)  


# Scatter Plot 3D --------------------------------------------------------------
install.packages("scatterplot3d") # Outro pacote
library(scatterplot3d)



# Definindo o tamanho da área de desenho ----------- # Essa fç muda a AREA DE DESENHO! ---------------------
par(mfrow = c(1,1))

# Plot
scatterplot3d(x = mtcars$wt,
              y = mtcars$disp,
              z = mtcars$mpg)


# Plot
scatterplot3d(mtcars$wt, mtcars$disp, mtcars$mpg, 	
              pch=16, highlight.3d = TRUE, angle = 20,
              xlab = "Peso", ylab = "Deslocamento", zlab = "Consumo de Combustível(mpg)",
              type = "h", 
              main = "Relacionamento Entre Características de Automóveis")





############################# BarPlot, Histograma e Polígono de Frequência #############################

# Bar Plot

# Dados
data = data.frame(grupo = c("A ","B ","C ","D ") , 
                  valor = c(33,62,56,67) , 
                  num_obs = c(100,500,459,342))



# Gerando a massa de dados
data$right = cumsum(data$num_obs) + 30 * c(0:(nrow(data)-1))
data$left = data$right - data$num_obs 



# Plot (fç geom_rect para um gráfico de barras ) -------------------------------
ggplot(data, aes(ymin = 0)) + 
  geom_rect(aes(xmin = left, xmax = right, 
                ymax = valor, colour = grupo, fill = grupo)) +
  xlab("Número de obs") + ylab("Valor")



# Histograma (dataset = diamonds) ----------------------------------------------
ggplot(diamonds, aes(carat)) +
  geom_histogram()

# passar o tamanho do bins (não muito curto  nem muito largo)
ggplot(diamonds, aes(carat)) +
  geom_histogram(binwidth = 0.01)

ggplot(diamonds, aes(carat)) +
  geom_histogram(bins = 200)

ggplot(diamonds, aes(price, fill = cut)) +
  geom_histogram(binwidth = 500)



# Polígono de Frequência -------------------------------------------------------
ggplot(diamonds, aes(price, colour = cut)) +
  geom_freqpoly(binwidth = 500)




# Para facilitar a comparação de distribuições com contagens muito diferentes, colocamos densidade no eixo y 
# em vez da contagem padrão
ggplot(diamonds, aes(price, ..density.., colour = cut)) +
  geom_freqpoly(binwidth = 500)






######################## Personalizando o Gráfico ------------------------------

# Dados
head(mtcars)


# Plot simples
ggplot(data = mtcars, aes(x = disp, y = mpg)) + geom_point()


# Outro aspecto que pode ser mapeado nesse gráfico é a cor dos pontos -------- alterar a cor dos pontos ---------
ggplot(data = mtcars, 
       aes(x = disp, y = mpg, 
           colour = as.factor(am))) + geom_point()



# No entanto, tambem podemos mapear uma variável contínua à cor dos pontos:
ggplot(mtcars, aes(x = disp, y = mpg, colour = cyl)) + geom_point()



# Também podemos mapear o tamanho dos pontos à uma variável de interesse:
# A legenda é inserida no gráfico automaticamente
ggplot(mtcars, aes(x = disp, y = mpg, colour = cyl, size = wt)) + geom_point()



# Os geoms definem qual forma geométrica será utilizada para a visualização dos dados no gráfico. 
ggplot(mtcars, aes(x = as.factor(cyl), y = mpg)) + geom_boxplot()



# Histogramas
ggplot(mtcars, aes(x = mpg), binwidth = 30) + geom_histogram()



# Gráfico de Barras
ggplot(mtcars, aes(x = as.factor(cyl))) + geom_bar()


# Personalizando os gráficos
colors()


ggplot(mtcars, aes(x = as.factor(cyl), y = mpg, 
                   colour = as.factor(cyl))) + geom_boxplot()

# cores
ggplot(mtcars, aes(x = as.factor(cyl), y = mpg, 
                   fill = as.factor(cyl))) + geom_boxplot()

ggplot(mtcars, 
       aes(x = as.factor(cyl), y = mpg)) + 
  geom_boxplot(color = "blue", fill = "seagreen4")




# Podemos alterar os eixos
ggplot(mtcars, aes(x = mpg)) + 
  geom_histogram() + 
  xlab("Milhas por galão") + ylab("Frequência")



# Alterar os limites do gráfico
ggplot(mtcars, aes(x = mpg)) + geom_histogram() + xlab("Milhas por galão") + ylab("Frequência") +
  xlim(c(0, 40)) +
  ylim(c(0,8))


# Legendas
ggplot(mtcars, aes(x = as.factor(cyl), fill = as.factor(cyl))) + 
  geom_bar() +
  labs(fill = "cyl")


# Trocando a posição da legenda (fç theme)
ggplot(mtcars, aes(x = as.factor(cyl), fill = as.factor(cyl))) + 
  geom_bar() +
  labs(fill = "cyl") +
  theme(legend.position="top")

# Sem legenda
ggplot(mtcars, aes(x = as.factor(cyl), fill = as.factor(cyl))) + 
  geom_bar() +
  guides(fill=FALSE)



# Facets ---------------------Dividir a area de desenho ------------------------

# horizontal
ggplot(mtcars, aes(x = mpg, y = disp, colour = as.factor(cyl))) + 
  geom_point() + 
  facet_grid(am~.)

# vertical
ggplot(mtcars, aes(x = mpg, y = disp, colour = as.factor(cyl))) +
  geom_point() + 
  facet_grid(.~am)



############### Plots diferentes juntos (diferente de Facet) ###################
library(gridExtra)
library(ggplot2)


# Dataset diamonds
data(diamonds)


# Histograma como plot1
plot1 <- qplot(price, data = diamonds, binwidth = 1000)


# ScatterPlot como plot2
plot2 <- qplot(carat, price, data = diamonds, colour = cut)


# Combina os 2 plots na mesma área
grid.arrange(plot1, plot2, ncol = 1) # a fç grid.arrange do gridExtra vai juntar os 2 plots!!



############################### Facets com reshape #############################
library(reshape2) #reformular o conj de dados
library(plotly) #Para publicar online


# dataset tips
sp <- ggplot(tips, aes(x=total_bill, y=tip/total_bill)) + geom_point(shape=1)

# facetd_grid horizontal
sp + facet_grid(sex ~ .)

# add o plotly
ggplotly()

#Facet_grid vertical
sp + facet_grid(. ~ sex)

# add o plotly
ggplotly()

# 4 plots
sp + facet_wrap( ~ day, ncol = 2)

# add o plotly
ggplotly()

# Varios grids
ggplot(mpg, aes(displ, hwy)) + geom_point() + facet_wrap(~manufacturer)

# add o plotly
ggplotly()





# End