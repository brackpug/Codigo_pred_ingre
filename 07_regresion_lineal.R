library(data.table)
library(readxl)
library(stringr)
library(dplyr)
library(sandwich)
library(lmtest)
library(ranger)
library(ggplot2)
library(writexl)
library(tidyr)

options(scipen = 999)
dir.p <- getwd()
dir.b <- paste(dir.p, "BDD", sep = "/")
dir.s <- paste(dir.p, "Scripts", sep = "/")
dir.r <- paste(dir.p, "Resultados", sep = "/")
dir.m <- paste(dir.p, "Modelos", sep = "/")

setwd(dir.b)
load("DatosModelamientoFinal_1.RData")
dim(data)

formula_g1 <- "INGRESO_REAL ~ DEUDA_TOTAL_SICOM_OP_24M+
PROM_XVEN_SICOM_OP_36M+
ANTIGUEDAD_OP_SICOM+
DEUDA_TOTAL_SBS_OP_24M+
salPromD36M319+
cuotaEstimadaD24M416+
MaxMontoOpD24M417+
antiguedadOpTcBanCoo390+
PROM_XVEN_SBS_OP_36M+
salProm36M303+
NOPE_APERT_SBS_OP_36M+
maxMontoOp096+
maySalVenBanD24M270+
numOpsVig36MD333+
PorcCuentaPropia.x+
cuotaD053_P25+
PorcCuentaAhorrosCorriente.x+
PorcPersComprasCred.x"

formula_g2 <- "INGRESO_REAL ~ maxMontoOp096+
salProm36M303+
salPromD36M319+
MaxMontoOpD24M417+
DEUDA_TOTAL_SICOM_OP_24M+
ANTIGUEDAD_OP_SICOM+
PROM_XVEN_SICOM_OP_24M+
cuotaEstimadaD24M416+
numMesesInfoCredBanCoopD36M421+
PROM_XVEN_SBS_OP_36M+
PorcPersPrest.x+
ANTIGUEDAD_OP_SBS+
salProm36M303_M+
PorcRecibeRemesas.x+
PorcSoltero.x+
PorcPrimaria.x+
cuotaTotOp059_M+
PorcHogaresNegocios.x"

#Datos de modelamiento
mod_g1 <- data[ModVal == 0 & GRUPO_CUOTA2 == "G1"]
mod_g2 <- data[ModVal == 0 & GRUPO_CUOTA2 == "G2"]

#modelo de regresión
modelo_G1 <- lm(as.formula(formula_g1), data = mod_g1)
modelo_G2 <- lm(as.formula(formula_g2), data = mod_g2)



# predicciones sobre el G1 ------------------------------------------------

data$INGRESO_EST_G1 <- predict(modelo_G1, newdata = data)

# Definir los cortes y etiquetas para los rangos
breaks <- c(450, 520, 670, 980, 1300, 2500)
labels <- c("[450-520]", "(520-670]", "(670-980]", "(980-1300]", "(1300-2500]")

# Categorizar los ingresos reales y estimados
data$RANGO_REAL <- cut(data$INGRESO_REAL, breaks = breaks, labels = labels, include.lowest = TRUE)
data$RANGO_EST <- cut(data$INGRESO_EST_G1, breaks = breaks, labels = labels, include.lowest = TRUE)

# Cuantiles de las predicciones estimadas
quantile(data[data$GRUPO_CUOTA2 == "G1", ]$INGRESO_EST_G1, probs = seq(0, 1, by = 0.02), na.rm=TRUE)

# Matriz de coincidencia para el conjunto de datos de validación
table(data[data$ModVal == 0 & data$GRUPO_CUOTA2 == "G1", ]$RANGO_REAL, data[data$ModVal == 0 & data$GRUPO_CUOTA2 == "G1", ]$RANGO_EST)
table(data[data$ModVal == 1 & data$GRUPO_CUOTA2 == "G1", ]$RANGO_REAL, data[data$ModVal == 1 & data$GRUPO_CUOTA2 == "G1", ]$RANGO_EST)

# Error Cuadrático Medio (MSE)
data$MSE <- (data$INGRESO_REAL - data$INGRESO_EST_G1)^2
data[data$GRUPO_CUOTA2 == "G1", .(MSE = mean(MSE, na.rm=TRUE)), by = ModVal][order(ModVal)]

# Error Absoluto Medio (MAE)
data$MAE <- abs(data$INGRESO_REAL - data$INGRESO_EST_G1)
data[data$GRUPO_CUOTA2 == "G1", .(MAE = mean(MAE, na.rm=TRUE)), by = ModVal][order(ModVal)]

# MSE por rango real para ModVal = 0
data[data$GRUPO_CUOTA2 == "G1" & data$ModVal == 0, .(MSE = mean(MSE, na.rm=TRUE)), by = RANGO_REAL][order(RANGO_REAL)]

# MSE por rango real para ModVal = 1
data[data$GRUPO_CUOTA2 == "G1" & data$ModVal == 1, .(MSE = mean(MSE, na.rm=TRUE)), by = RANGO_REAL][order(RANGO_REAL)]

# Proporción de coincidencias
round(prop.table(table(data[data$ModVal == 0 & data$GRUPO_CUOTA2 == "G1", ]$RANGO_REAL, data[data$ModVal == 0 & data$GRUPO_CUOTA2 == "G1", ]$RANGO_EST), 1),2)

#Gráfico
ggplot() + 
    geom_density(data = data[ModVal == 0 & GRUPO_CUOTA2 == "G1"], aes(x = INGRESO_REAL, fill = "Ingreso Real"), alpha = 0.6, adjust = 3) + 
    geom_density(data = data[ModVal == 0 & GRUPO_CUOTA2 == "G1"], aes(x = INGRESO_EST_G1, fill = "Ingreso Estimado"), alpha = 0.6, adjust = 3) + 
    xlab("Ingreso") + 
    ylab("Densidad") + 
    scale_fill_hue(labels = c("Estimado", "Real")) +
    labs(fill = "Ingreso")



# predicciones sobre G2 ---------------------------------------------------

data$INGRESO_EST_G2 <- predict(modelo_G2, newdata = data)

# Definir los cortes y etiquetas para los rangos
breaks <- c(450, 800, 1200, 2000, 4000, 35000)
labels <- c("[450-800]", "(800-1200]", "(1200-2000]", "(2000-4000]", "(4000-35000]")

# Categorizar los ingresos reales y estimados
data$RANGO_REAL <- cut(data$INGRESO_REAL, breaks = breaks, labels = labels, include.lowest = TRUE)
data$RANGO_EST <- cut(data$INGRESO_EST_G2, breaks = breaks, labels = labels, include.lowest = TRUE)

# Cuantiles de las predicciones estimadas
quantile(data[data$GRUPO_CUOTA2 == "G2", ]$INGRESO_EST_G2, probs = seq(0, 1, by = 0.02), na.rm=TRUE)

# Matriz de coincidencia para el conjunto de datos de validación
table(data[data$ModVal == 0 & data$GRUPO_CUOTA2 == "G2", ]$RANGO_REAL, data[data$ModVal == 0 & data$GRUPO_CUOTA2 == "G2", ]$RANGO_EST)
table(data[data$ModVal == 1 & data$GRUPO_CUOTA2 == "G2", ]$RANGO_REAL, data[data$ModVal == 1 & data$GRUPO_CUOTA2 == "G2", ]$RANGO_EST)

# Error Cuadrático Medio (MSE)
data$MSE <- (data$INGRESO_REAL - data$INGRESO_EST_G2)^2
data[data$GRUPO_CUOTA2 == "G2", .(MSE = mean(MSE, na.rm=TRUE)), by = ModVal][order(ModVal)]

# Error Absoluto Medio (MAE)
data$MAE <- abs(data$INGRESO_REAL - data$INGRESO_EST_G2)
data[data$GRUPO_CUOTA2 == "G2", .(MAE = mean(MAE, na.rm=TRUE)), by = ModVal][order(ModVal)]

# MSE por rango real para ModVal = 0
data[data$GRUPO_CUOTA2 == "G2" & data$ModVal == 0, .(MSE = mean(MSE, na.rm=TRUE)), by = RANGO_REAL][order(RANGO_REAL)]

# MSE por rango real para ModVal = 1
data[data$GRUPO_CUOTA2 == "G2" & data$ModVal == 1, .(MSE = mean(MSE, na.rm=TRUE)), by = RANGO_REAL][order(RANGO_REAL)]

# Proporción de coincidencias
round(prop.table(table(data[data$ModVal == 0 & data$GRUPO_CUOTA2 == "G2", ]$RANGO_REAL, data[data$ModVal == 0 & data$GRUPO_CUOTA2 == "G2", ]$RANGO_EST), 1),2)

ggplot() + 
    geom_density(data = data[ModVal == 0 & GRUPO_CUOTA2 == "G2"], aes(x = INGRESO_REAL, fill = "Ingreso Real"), alpha = 0.6, adjust = 3) + 
    geom_density(data = data[ModVal == 0 & GRUPO_CUOTA2 == "G2"], aes(x = INGRESO_EST_G2, fill = "Ingreso Estimado"), alpha = 0.6, adjust = 3) + 
    xlab("Ingreso") + 
    ylab("Densidad") + 
    scale_fill_hue(labels = c("Estimado", "Real")) +
    labs(fill = "Ingreso")
