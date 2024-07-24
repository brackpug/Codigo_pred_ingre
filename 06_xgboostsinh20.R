library(data.table)
library(readxl)
library(stringr)
library(dplyr)
library(sandwich)
library(lmtest)
library(corrplot)
library(ranger)
library(ggplot2)
library(writexl)
library(tidyr)
library(xgboost)

options(scipen = 999)
dir.p <- getwd()
dir.b <- paste(dir.p, "BDD", sep = "/")
dir.s <- paste(dir.p, "Scripts", sep = "/")
dir.r <- paste(dir.p, "Resultados", sep = "/")
dir.m <- paste(dir.p, "Modelos", sep = "/")

setwd(dir.b)
load("DatosModelamientoFinal_1.RData")
dim(data)

boots_2tail <- function(data, corte1 = 0, corte2 = 0, porc1, porc2){
    nb <- which(data$INGRESO_REAL <= corte1)
    na <- which(data$INGRESO_REAL >= corte2)
    nc <- which(data$INGRESO_REAL > corte1 & data$INGRESO_REAL < corte2)
    perc1 <- length(nb)/nrow(data)
    perc2 <- length(na)/nrow(data)
    cat("La cola izquierda tiene un peso inicial del : ", perc1, "\n")
    cat("La cola derecha tiene un peso inicial del : ", perc2, "\n")
    cat(" --- Aplicación del remuestreo --- \n")
    nperc1 <- floor(porc1*length(nb)/perc1)
    nperc2 <- floor(porc2*length(na)/perc2)
    set.seed(12345)
    nb <- sample(nb, size = nperc1, replace = TRUE)
    na <- sample(na, size = nperc2, replace = TRUE)
    data <- data[c(na, nb, nc), ] # Nueva base
    nb <- which(data$INGRESO_REAL <= corte1)
    na <- which(data$INGRESO_REAL >= corte2)
    perc1 <- length(nb)/nrow(data)
    perc2 <- length(na)/nrow(data)
    cat("La cola izquierda tiene un peso final del : ", perc1, "\n")
    cat("La cola derecha tiene un peso final del : ", perc2, "\n")
    return(data)
}

mod_g1 <- data[ModVal == 0 & GRUPO_CUOTA2 == "G1"]
mod_g2 <- data[ModVal == 0 & GRUPO_CUOTA2 == "G2"]

test_g1 <- data[ModVal == 1 & GRUPO_CUOTA2 == "G1"]
test_g2 <- data[ModVal == 1 & GRUPO_CUOTA2 == "G2"]

#Remuestreo de las colas
rmod_g1 <- boots_2tail(mod_g1, corte1 =  500, corte2 = 1500, porc1 = 0.4, porc2 = 0.45)
rmod_g2 <- boots_2tail(mod_g2, corte1 =  500, corte2 = 5700, porc1 = 0.25, porc2 = 0.6)

#quantiles
quantile(mod_g2$INGRESO_REAL, probs = seq(0,1,by=0.02))

#Variables
y1 <- "INGRESO_REAL"
predictoras1 <- c("DEUDA_TOTAL_SICOM_OP_24M",
                  "PROM_XVEN_SICOM_OP_36M",
                  "ANTIGUEDAD_OP_SICOM",
                  "numMesesInfoCredBanCoopD36M421",
                  "DEUDA_TOTAL_SBS_OP_24M",
                  "salPromD36M319",
                  "cuotaEstimadaD24M416",
                  "MaxMontoOpD24M417",
                  "antiguedadOpTcBanCoo390",
                  "PROM_XVEN_SBS_OP_36M",
                  "ANTIGUEDAD_OP_SBS",
                  "salProm36M303",
                  "maxMontoOp096",
                  "MAX_DVEN_SBS_OP_36M",
                  "numOpsVig36MD333",
                  "salProm36M303_P25",
                  "ANTIGUEDAD_OP_SC",
                  "PorcCuentaPropia.x",
                  "cuotaD053_P25",
                  "PorcPersComprasCred.x",
                  "PorcCasado.x",
                  "consumoTC087_M",
                  "PorcSocios.x",
                  "PorcSoltero.x",
                  "gastoPersonal093")

predictoras2 <- c("maxMontoOp096",
"salProm36M303",
"salPromD36M319",
"MaxMontoOpD24M417",
"DEUDA_TOTAL_SICOM_OP_24M",
"ANTIGUEDAD_OP_SICOM",
"PROM_XVEN_SC_OP_36M",
"cuotaEstimadaD24M416",
"ANTIGUEDAD_OP_SC",
"MESES_INFO",
"score001Actual",
"PROM_XVEN_SBS_OP_36M",
"PorcPersPrest.x",
"ANTIGUEDAD_OP_SBS",
"salProm36M303_M",
"PorcCuentaCaja.x",
"DEUDA_TOTAL_SBS_OP_24M",
"PorcPrimaria.x",
"PorcHogaresNegocios.x",
"salOpDiaCom005_M")

train_matrix <- xgb.DMatrix(data = as.matrix(rmod_g1[, ..predictoras1]), label = rmod_g1[[y1]])
test_matrix <- xgb.DMatrix(data = as.matrix(test_g2[, ..predictoras2]), label = test_g2[[y1]])

train_matrix_r <- xgb.DMatrix(data = as.matrix(rmod_g2[, ..predictoras2]), label = rmod_g2[[y1]])

params <- list(
    objective = "reg:squarederror",  # Función de pérdida para regresión
    eta = 0.03,                       # Tasa de aprendizaje
    max_depth = 7,# Profundidad máxima del árbol
    min_child_weight = 1000,
    subsample = 0.8,                 # Proporción de muestras utilizadas para cada árbol
    colsample_bytree = 0.8           # Proporción de características utilizadas para cada árbol
)

xgboost_model1 <- xgb.train(params = params,
                           data = train_matrix_r,
                           nrounds = 500,                 # Número de iteraciones (árboles)
                           watchlist = list(train = train_matrix_r, test = test_matrix),
                           early_stopping_rounds = 10,   # Detenerse temprano si no mejora
                           verbose = 1)



preds <- predict(xgboost_model1, test_matrix)
actuals <- test_g2[[y1]]
# Calcular RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((preds - actuals)^2))
print(paste("RMSE: ", rmse))
predictions <- predict(xgboost_model1, test_matrix)
head(predictions)



# Grupo1 ------------------------------------------------------------------


# Crear una DMatrix para todo el conjunto de datos
all_data_matrix <- xgb.DMatrix(data = as.matrix(data[, ..predictoras1]))

# Realizar la predicción
data[, INGRESO_EST_XGB := predict(xgboost_model1, all_data_matrix)]

data[, RANGO_EST := cut(INGRESO_EST_XGB, breaks = c(450, 520, 650, 800, 1300, 2500), labels = c("[450-520]", "(520-650]", "(650-800]", "(800-1300]", "(1300-2500]"))]
data[, RANGO_REAL := cut(INGRESO_REAL, breaks = c(450, 520, 650, 800, 1300, 2500), labels = c("[450-520]", "(520-650]", "(650-800]", "(800-1300]", "(1300-2500]"))]

# Matriz de coincidencia para el grupo G1
coincidence_matrix_modval_0 <- data[ModVal == 0 & GRUPO_CUOTA2 == "G1"][, table(RANGO_REAL, RANGO_EST)]
coincidence_matrix_modval_1 <- data[ModVal == 1 & GRUPO_CUOTA2 == "G1"][, table(RANGO_REAL, RANGO_EST)]

# Imprimir las matrices de coincidencia
print(coincidence_matrix_modval_0)
print(coincidence_matrix_modval_1)

# Calcular MSE
data[, MSE := (INGRESO_REAL - INGRESO_EST_XGB)^2]
mse_by_modval <- data[GRUPO_CUOTA2 == "G1"][, .(MSE = mean(MSE)), by = ModVal][order(ModVal)]
print(mse_by_modval)

# Calcular MAE
data[, MAE := abs(INGRESO_REAL - INGRESO_EST_XGB)]
mae_by_modval <- data[GRUPO_CUOTA2 == "G1"][, .(MAE = mean(MAE)), by = ModVal][order(ModVal)]
print(mae_by_modval)

# Calcular MSE por rango real
mse_by_rango_real_modval_0 <- data[GRUPO_CUOTA2 == "G1" & ModVal == 0][, .(MSE = mean(MSE)), by = RANGO_REAL][order(RANGO_REAL)]
mse_by_rango_real_modval_1 <- data[GRUPO_CUOTA2 == "G1" & ModVal == 1][, .(MSE = mean(MSE)), by = RANGO_REAL][order(RANGO_REAL)]

print(mse_by_rango_real_modval_0)
print(mse_by_rango_real_modval_1)

# Proporciones en la matriz de coincidencia
proportions_modval_0 <- round(prop.table(coincidence_matrix_modval_0, 1), 2)
proportions_modval_1 <- round(prop.table(coincidence_matrix_modval_1, 1), 2)
print(proportions_modval_0)
print(proportions_modval_1)
# Graficar densidades
plot(density(data[ModVal == 0 & GRUPO_CUOTA2 == "G2"]$INGRESO_REAL), main = "Densidad de INGRESO_REAL")
plot(density(data[ModVal == 0 & GRUPO_CUOTA2 == "G2"]$INGRESO_EST_XGB), main = "Densidad de INGRESO_EST_XGB")


# Obtener la importancia de las características
importance_matrix <- xgb.importance(feature_names = predictoras2, model = xgboost_model1)

# Mostrar la importancia de las características
print(importance_matrix)



# Grupo2 ------------------------------------------------------------------


# Crear una DMatrix para todo el conjunto de datos
all_data_matrix <- xgb.DMatrix(data = as.matrix(data[, ..predictoras2]))

# Realizar la predicción
data[, INGRESO_EST_XGB := predict(xgboost_model1, all_data_matrix)]

data[, RANGO_EST := cut(INGRESO_EST_XGB, breaks = c(450, 800, 1200, 2000, 4000, 35000), labels = c("[450-800]", "(800-1200]", "(1200-2000]", "(2000-4000]", "(4000-35000]"))]
data[, RANGO_REAL := cut(INGRESO_REAL, breaks = c(450, 800, 1200, 2000, 4000, 35000), labels = c("[450-800]", "(800-1200]", "(1200-2000]", "(2000-4000]", "(4000-35000]"))]

# Matriz de coincidencia para el grupo G2
coincidence_matrix_modval_0 <- data[ModVal == 0 & GRUPO_CUOTA2 == "G2"][, table(RANGO_REAL, RANGO_EST)]
coincidence_matrix_modval_1 <- data[ModVal == 1 & GRUPO_CUOTA2 == "G2"][, table(RANGO_REAL, RANGO_EST)]

# Imprimir las matrices de coincidencia
print(coincidence_matrix_modval_0)
print(coincidence_matrix_modval_1)

# Calcular MSE
data[, MSE := (INGRESO_REAL - INGRESO_EST_XGB)^2]
mse_by_modval <- data[GRUPO_CUOTA2 == "G2"][, .(MSE = mean(MSE)), by = ModVal][order(ModVal)]
print(mse_by_modval)

# Calcular MAE
data[, MAE := abs(INGRESO_REAL - INGRESO_EST_XGB)]
mae_by_modval <- data[GRUPO_CUOTA2 == "G2"][, .(MAE = mean(MAE)), by = ModVal][order(ModVal)]
print(mae_by_modval)

# Calcular MSE por rango real
mse_by_rango_real_modval_0 <- data[GRUPO_CUOTA2 == "G2" & ModVal == 0][, .(MSE = mean(MSE)), by = RANGO_REAL][order(RANGO_REAL)]
mse_by_rango_real_modval_1 <- data[GRUPO_CUOTA2 == "G2" & ModVal == 1][, .(MSE = mean(MSE)), by = RANGO_REAL][order(RANGO_REAL)]

print(mse_by_rango_real_modval_0)
print(mse_by_rango_real_modval_1)

# Proporciones en la matriz de coincidencia
proportions_modval_0 <- round(prop.table(coincidence_matrix_modval_0, 1), 2)
proportions_modval_1 <- round(prop.table(coincidence_matrix_modval_1, 1), 2)
print(proportions_modval_0)
print(proportions_modval_1)
# Graficar densidades
plot(density(data[ModVal == 0 & GRUPO_CUOTA2 == "G2"]$INGRESO_REAL), main = "Densidad de INGRESO_REAL")
plot(density(data[ModVal == 0 & GRUPO_CUOTA2 == "G2"]$INGRESO_EST_XGB), main = "Densidad de INGRESO_EST_XGB")


# Obtener la importancia de las características
importance_matrix <- xgb.importance(feature_names = predictoras2, model = xgboost_model1)

# Mostrar la importancia de las características
print(importance_matrix)



#Grafico del remuestreo
ggplot() + 
    geom_density(data = mod_g2, aes(x = INGRESO_REAL, fill = "Ingreso Real"), alpha = 0.6, adjust = 3) + 
    geom_density(data = rmod_g2, aes(x = INGRESO_REAL, fill = "Ingreso Estimado"), alpha = 0.6, adjust = 3) + 
    xlab("Ingreso") + 
    ylab("Densidad") + 
    scale_fill_hue(labels = c("Con remuestreo", "Sin remuestreo")) +
    labs(fill = "Ingreso")

#grafico 
ggplot() + 
    geom_density(data = data[ModVal == 0 & GRUPO_CUOTA2 == "G2"], aes(x = INGRESO_REAL, fill = "Ingreso Real"), alpha = 0.6, adjust = 3) + 
    geom_density(data = data[ModVal == 0 & GRUPO_CUOTA2 == "G2"], aes(x = INGRESO_EST_XGB, fill = "Ingreso Estimado"), alpha = 0.6, adjust = 3) + 
    xlab("Ingreso") + 
    ylab("Densidad") + 
    scale_fill_hue(labels = c("Estimado", "Real")) +
    labs(fill = "Ingreso")
ggplot() + 
    geom_density(data = data[ModVal == 1 & GRUPO_CUOTA2 == "G1"], aes(x = INGRESO_REAL, fill = "Ingreso Real"), alpha = 0.6, adjust = 3) + 
    geom_density(data = data[ModVal == 1 & GRUPO_CUOTA2 == "G1"], aes(x = INGRESO_EST_XGB, fill = "Ingreso Estimado"), alpha = 0.6, adjust = 3) + 
    xlab("Ingreso") + 
    ylab("Densidad") +
    scale_fill_hue(labels = c("Estimado", "Real")) +
    labs(fill = "Ingreso")
