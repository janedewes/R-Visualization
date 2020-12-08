


# Gráficos Interativos de Séries Tempoarais com Dygraphs

# https://rstudio.github.io/dygraphs/
  


# Pacote
install.packages("dygraphs")
library(dygraphs)
ls("package:dygraphs")
??dygraphs

# Dataset ------------------------------ Data ----------------------------------
lungDeaths <- cbind(mdeaths, fdeaths)
head(lungDeaths)
View(lungDeaths)
str(lungDeaths)
??lungDeaths

# both sexes (ldeaths), 
# males (mdeaths),
# females (fdeaths)



# Gráfico -------------------------------- Plot --------------------------------
dygraph(lungDeaths)


# Gráfico com seletor na parte inferior ------------ detalhar infoos -----------
dygraph(lungDeaths) %>% dyRangeSelector()



# Labels (area)-----------------------------------------------------------------
dygraph(lungDeaths) %>%
  dySeries("mdeaths", label = "Male") %>%
  dySeries("fdeaths", label = "Female") %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)



# StepPlots --------------------------------------------------------------------
dygraph(lungDeaths, main = "Deaths from Lung Disease (UK)") %>%
  dyOptions(stepPlot = TRUE)



# Drawpoints -------------------------------------------------------------------
dygraph(ldeaths, main = "Deaths from Lung Disease (UK)") %>%
  dyOptions(drawPoints = TRUE, pointSize = 2)




# Combinação  de plots ---------------------------------------------------------
lungDeaths1 <- cbind(mdeaths, fdeaths, ldeaths)

dygraph(lungDeaths1, main = "Deaths from Lung Disease (UK)") %>%
  dySeries("fdeaths", stepPlot = TRUE, color = "red") %>% 
  dyGroup(c("mdeaths", "ldeaths"), drawPoints = TRUE, color = c("blue", "green"))








########################### Gerando previsões ##################################
hw <- HoltWinters(ldeaths)

# Usando a fç predict
predicted <- predict(hw, n.ahead = 72, prediction.interval = TRUE) # fç predict 



# Plot das previsões --------------------- fç Predict -----------------------------
dygraph(predicted, main = "Predicted Lung Deaths (UK)") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))




# End
