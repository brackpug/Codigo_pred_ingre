############################################################################
##########                 Modelamiento Predictor                 ##########
############################################################################

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
library(gbm)


options(scipen = 999)
dir.p <- getwd()
dir.b <- paste(dir.p, "BDD", sep = "/")
dir.s <- paste(dir.p, "Scripts", sep = "/")
dir.r <- paste(dir.p, "Resultados", sep = "/")
dir.m <- paste(dir.p, "Modelos", sep = "/")


# Función de reemplazo NA's
reemplazo_col = function(dt, vars, valor){ 
    na.replace = function(v, value=valor) { v[is.na(v)] = value; v }
    for (i in vars)
        eval(parse(text=paste("dt[,",i,":=na.replace(",i,")]")))
}
# Funcion de rebalanceo :)
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

setwd(dir.b)
load("DatosModelamientoFinal_1.RData")
dim(data)
reemplazo_col(data, c("antiguedadSri349"), 0)

# Distribución Modelamiento/Validación
data[GRUPO_CUOTA2 == "G1"][,.N,by=ModVal][order(ModVal)]
quantile(data[GRUPO_CUOTA2 == "G1"]$INGRESO_REAL, probs = seq(0,1,by=0.05))

mod_g1 <- data[ModVal == 0 & GRUPO_CUOTA2 == "G1"]
mod_g2 <- data[ModVal == 0 & GRUPO_CUOTA2 == "G2"]

rmod_g1 <- boots_2tail(mod_g1, corte1 =  500, corte2 = 1500, porc1 = 0.4, porc2 = 0.45)

rmod_g2 <- boots_2tail(mod_g2, corte1 =  490, corte2 = 4800, porc1 = 0.10, porc2 = 0.5)

# Distribuciones muestras (modelamiento)
quantile(mod_g2$INGRESO_REAL, probs = seq(0,1,by=0.02))
quantile(rmod_g1$INGRESO_REAL, probs = seq(0,1,by=0.02))

quantile(rmod_g2$INGRESO_REAL, probs = seq(0,1,by=0.01))

plot(density(mod_g1$INGRESO_REAL))
plot(density(rmod_g1$INGRESO_REAL))



# GRUPO 1
formula_g1 <- "INGRESO_REAL ~ DEUDA_TOTAL_SICOM_OP_24M+
PROM_XVEN_SICOM_OP_36M+
ANTIGUEDAD_OP_SICOM+
numMesesInfoCredBanCoopD36M421+
DEUDA_TOTAL_SBS_OP_24M+
salPromD36M319+
cuotaEstimadaD24M416+
MaxMontoOpD24M417+
antiguedadOpTcBanCoo390+
PROM_XVEN_SBS_OP_36M+
salProm36M303+
NOPE_APERT_SBS_OP_36M+
maxMontoOp096+
MAX_DVEN_SBS_OP_24M+
maySalVenBanD24M270+
numOpsVig36MD333+
PorcCuentaPropia.x+
cuotaD053_P25+
PorcCuentaAhorrosCorriente.x+
PorcPersComprasCred.x+
PorcSoltero.x"

# GRUPO 2
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


# Gradient Boosting Machine
gbm_g1 <- gbm(formula = as.formula(formula_g1), data = mod_g1, n.trees = 500, n.minobsinnode = 550, shrinkage = 0.03, distribution = "laplace")
rgbm_g1 <- gbm(formula = as.formula(formula_g1), data = rmod_g1, n.trees = 500, n.minobsinnode = 850, shrinkage = 0.03, distribution = "laplace")
#rrgbm_g1 <- gbm(formula = as.formula(formula_g1), data = rrmod_g1, n.trees = 400, n.minobsinnode = 1600, shrinkage = 0.03, distribution = "laplace")

gbm_g2 <- gbm(formula = as.formula(formula_g2), data = mod_g2, n.trees = 500, n.minobsinnode = 590, shrinkage = 0.03, distribution = "laplace")
rgbm_g2 <- gbm(formula = as.formula(formula_g2), data = rmod_g2, n.trees = 500, n.minobsinnode = 880, shrinkage = 0.03, distribution = "laplace")


summary(gbm_g1)
summary(rgbm_g1)


summary(gbm_g2)
summary(rgbm_g2)


# Predicción Grupo 1 sobre toda la base ----
data[, INGRESO_EST_G1 := predict(rgbm_g1, n.trees = rgbm_g1$n.trees, data)]
data[, RANGO_REAL := cut(INGRESO_REAL, breaks = c(450, 520, 670, 980, 1300, 2500), labels = c("[450-520]", "(520-670]", "(670-980]", "(980-1300]", "(1300-2500]"))]
data[, RANGO_EST := cut(INGRESO_EST_G1, breaks = c(450, 520, 670, 980, 1300, 2500), labels = c("[450-520]", "(520-670]", "(670-980]", "(980-1300]", "(1300-2500]"))]
# Matriz de coincidencia
quantile(data[GRUPO_CUOTA2 == "G1"]$INGRESO_EST_G1, probs = seq(0,1,by=0.02))
data[ModVal == 0 & GRUPO_CUOTA2 == "G1"][,table(RANGO_REAL, RANGO_EST)]
data[ModVal == 1 & GRUPO_CUOTA2 == "G1"][,table(RANGO_REAL, RANGO_EST)]
# Métricas
data[, MSE := (INGRESO_REAL - INGRESO_EST_G1)^2][GRUPO_CUOTA2 == "G1"][,list(MSE = mean(MSE)), by = ModVal][order(ModVal)] # Error cuadrático medio (MSE)
data[, MAE := abs(INGRESO_REAL - INGRESO_EST_G1)][GRUPO_CUOTA2 == "G1"][,list(MAE = mean(MAE)), by = ModVal][order(ModVal)] # Error absoluto medio (MAE)
data[GRUPO_CUOTA2 == "G1" & ModVal == 0][,list(MSE = mean(MSE)), by = RANGO_REAL][order(RANGO_REAL)]
data[GRUPO_CUOTA2 == "G1" & ModVal == 1][,list(MSE = mean(MSE)), by = RANGO_REAL][order(RANGO_REAL)]

round(prop.table(data[ModVal == 0 & GRUPO_CUOTA2 == "G1"][,table(RANGO_REAL, RANGO_EST)],1),2)

plot(density(data[ModVal == 0 & GRUPO_CUOTA2 == "G1"]$INGRESO_REAL))
plot(density(data[ModVal == 0 & GRUPO_CUOTA2 == "G1"]$INGRESO_EST_G1))

setwd(dir.m)
save(list = c("rgbm_g1"), file = "Modelo_Grupo1.RData", envir = .GlobalEnv)
rm(list = c("mod_g1", "rmod_g1", "gbm_g1", "rgbm_g1", "formula_g1"))


# Predicción Grupo 2 sobre toda la base ----
data[, INGRESO_EST_G2 := predict(rgbm_g2, n.trees = rgbm_g2$n.trees, data)]
data[, RANGO_REAL := cut(INGRESO_REAL, breaks = c(450, 800, 1200, 2000, 4000, 35000), labels = c("[450-800]", "(800-1200]", "(1200-2000]", "(2000-4000]", "(4000-35000]"))]
data[, RANGO_EST := cut(INGRESO_EST_G2, breaks = c(450, 800, 1200, 2000, 4000, 35000), labels = c("[450-800]", "(800-1200]", "(1200-2000]", "(2000-4000]", "(4000-35000]"))]
# Matriz de coincidencia
quantile(data[GRUPO_CUOTA2 == "G2"]$INGRESO_EST_G2, probs = seq(0,1,by=0.02))
data[ModVal == 0 & GRUPO_CUOTA2 == "G2"][,table(RANGO_REAL, RANGO_EST)]
data[ModVal == 1 & GRUPO_CUOTA2 == "G2"][,table(RANGO_REAL, RANGO_EST)]
# Métricas
data[, MSE := (INGRESO_REAL - INGRESO_EST_G2)^2][GRUPO_CUOTA2 == "G2"][,list(MSE = mean(MSE)), by = ModVal][order(ModVal)] # Error cuadrático medio (MSE)
data[, MAE := abs(INGRESO_REAL - INGRESO_EST_G2)][GRUPO_CUOTA2 == "G2"][,list(MAE = mean(MAE)), by = ModVal][order(ModVal)] # Error absoluto medio (MAE)
data[GRUPO_CUOTA2 == "G2" & ModVal == 0][,list(MSE = mean(MSE)), by = RANGO_REAL][order(RANGO_REAL)]
data[GRUPO_CUOTA2 == "G2" & ModVal == 1][,list(MSE = mean(MSE)), by = RANGO_REAL][order(RANGO_REAL)]


round(prop.table(data[ModVal == 0 & GRUPO_CUOTA2 == "G2"][,table(RANGO_REAL, RANGO_EST)],1),2)

plot(density(data[ModVal == 0 & GRUPO_CUOTA2 == "G2"]$INGRESO_REAL))
plot(density(data[ModVal == 0 & GRUPO_CUOTA2 == "G2"]$INGRESO_EST_G2))



setwd(dir.m)
save(list = c("rgbm_g2"), file = "Modelo_Grupo2.RData", envir = .GlobalEnv)
rm(list = c("mod_g2", "rmod_g2", "gbm_g2", "rgbm_g2", "formula_g2"))



rm(list = c("boots_higher_tail", "boots_2tail"))



#Gráfico

ggplot() + 
    geom_density(data = data[ModVal == 0 & GRUPO_CUOTA2 == "G2"], aes(x = INGRESO_REAL, fill = "Ingreso Real"), alpha = 0.6, adjust = 3) + 
    geom_density(data = data[ModVal == 0 & GRUPO_CUOTA2 == "G2"], aes(x = INGRESO_EST_G2, fill = "Ingreso Estimado"), alpha = 0.6, adjust = 3) + 
    xlab("Ingreso") + 
    ylab("Densidad") + 
    scale_fill_hue(labels = c("Estimado", "Real")) +
    labs(fill = "Ingreso")
ggplot() + 
    geom_density(data = data[ModVal == 1 & GRUPO_CUOTA2 == "G2"], aes(x = INGRESO_REAL, fill = "Ingreso Real"), alpha = 0.6, adjust = 3) + 
    geom_density(data = data[ModVal == 1 & GRUPO_CUOTA2 == "G2"], aes(x = INGRESO_EST_G2, fill = "Ingreso Estimado"), alpha = 0.6, adjust = 3) + 
    xlab("Ingreso") + 
    ylab("Densidad") +
    scale_fill_hue(labels = c("Estimado", "Real")) +
    labs(fill = "Ingreso")


#Grafico del remuestreo

ggplot() + 
    geom_density(data = mod_g2, aes(x = INGRESO_REAL, fill = "Ingreso Real"), alpha = 0.6, adjust = 3) + 
    geom_density(data = rmod_g2, aes(x = INGRESO_REAL, fill = "Ingreso Estimado"), alpha = 0.6, adjust = 3) + 
    xlab("Ingreso") + 
    ylab("Densidad") + 
    scale_fill_hue(labels = c("Con remuestreo", "Sin remuestreo")) +
    labs(fill = "Ingreso")




# Grid parametros ---------------------------------------------------------

results <- data.frame(n_trees=integer(),
                         min_node=integer(),
                         RMSE=numeric())

param_grid <- expand_grid(
    'n_trees' = c(300, 400, 500),
    'min_node' = c(590, 600, 650, 700)
)


for (i in 1:nrow(param_grid)) {
    set.seed(123)
    
    # Ajustar el modelo con los hiperparámetros actuales
    modelo <- gbm(
        formula = as.formula(formula_g2),
        data = mod_g2, 
        n.trees = param_grid$n_trees[i],
        n.minobsinnode = param_grid$min_node[i],
        shrinkage = 0.03,
        distribution = "laplace",
        cv.folds = 5,  # Usar validación cruzada con 5 folds
        keep.data = FALSE,
        verbose = FALSE
    )
    
    # Obtener el RMSE (Root Mean Squared Error) de la validación cruzada
    cv_error <- min(modelo$cv.error)
    
    # Almacenar los resultados en el data frame
    results <- rbind(results, data.frame(
        n_trees = param_grid$n_trees[i],
        min_node = param_grid$min_node[i],
        RMSE = cv_error
    ))
}

# Ver los resultados
print(results)

library(openxlsx)
write.xlsx(results, file = "grid_gbmd_mod2.xlsx", sheetName = "Sheet1", rowNames = FALSE)
