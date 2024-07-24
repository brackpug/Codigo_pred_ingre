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
library(h2o)


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

#Data entrenamiento
mod_g1 <- data[ModVal == 0 & GRUPO_CUOTA2 == "G1"]
mod_g2 <- data[ModVal == 0 & GRUPO_CUOTA2 == "G2"]

test_g1 <- data[ModVal == 1 & GRUPO_CUOTA2 == "G1"]
test_g2 <- data[ModVal == 1 & GRUPO_CUOTA2 == "G2"]

#Remuestreo
rmod_g1 <- boots_2tail(mod_g1, corte1 =  500, corte2 = 1500, porc1 = 0.4, porc2 = 0.45)
rmod_g2 <- boots_2tail(mod_g2, corte1 =  490, corte2 = 4800, porc1 = 0.10, porc2 = 0.5)


h2o.init()

entrenar1_h20 <- as.h2o(mod_g1)
entrenar2_h20 <- as.h2o(mod_g2)

test1_h20 <- as.h2o(test_g1)
test2_h20 <- as.h2o(test_g2)

#Variables
y1_h20 <- "INGRESO_REAL"
predictoras1 <- c("DEUDA_TOTAL_SICOM_OP_24M",
"PROM_XVEN_SICOM_OP_24M",
"PROM_XVEN_SICOM_OP_36M",
"ANTIGUEDAD_OP_SICOM",
"numMesesInfoCredBanCoopD36M421",
"DEUDA_TOTAL_SBS_OP_24M",
"salPromD36M319",
"cuotaEstimadaD24M416",
"MaxMontoOpD24M417",
"DEUDA_TOTAL_SICOM_OP_12M",
"NOPE_APERT_SICOM_OP_36M",
"MVALVEN_SICOM_OP_24M",
"PROM_VEN_SICOM_OP_24M",
"PROM_MAX_DVEN_SICOM_OP_24M",
"MAX_DVEN_SICOM_OP_24M",
"maySalVenComD24M272",
"MVALVEN_SICOM_OP_36M",
"PROM_VEN_SICOM_OP_36M",
"PROM_MAX_DVEN_SICOM_OP_36M",
"MAX_DVEN_SICOM_OP_36M",
"maySalVenComD36M296",
"SalVenHis36MDDCom411",
"antiguedadOpTcBanCoo390",
"NENT_VEN_SICOM_OP_24M",
"PROM_XVEN_SBS_OP_36M",
"NENT_VEN_SICOM_OP_36M",
"antiguedadOpBanCoo388",
"PROM_MAX_DVEN_N_OP_24M",
"PROM_XVEN_SBS_OP_24M",
"PROM_XVEN_SICOM_OP_12M",
"ANTIGUEDAD_OP_SBS",
"PROM_MAX_DVEN_N_OP_36M",
"salProm36M303",
"DEUDA_TOTAL_SBS_OP_12M",
"saldoOpVenSfrTCS36M379",
"maxMontoOp096",
"MAX_DVEN_SBS_OP_36M",
"MAX_DVEN_SBS_OP_24M",
"SalCalHisYTR36MDDBan407",
"maySalVenBanD24M270",
"maySalVenBanD36M294",
"MVALVEN_SBS_OP_36M",
"MVALVEN_SBS_OP_24M",
"maySalVen24M269",
"numOpsVig36MD333",
"PROM_VEN_SBS_OP_36M",
"NENT_VEN_SBS_OP_36M",
"PROM_VEN_SBS_OP_24M",
"NENT_VEN_SBS_OP_24M",
"numOpsVencidas24M104",
"salProm36M303_P25",
"maySalVen36M293",
"numMesesSinVenDesdeUltVen100",
"numOpsVencidas36M105",
"antiguedadOpTcCoo389",
"ANTIGUEDAD_OP_SC",
"DEUDA_TOTAL_SBS_OP_6M",
"MVALVEN_SICOM_OP_12M",
"PROM_VEN_SICOM_OP_12M",
"PROM_MAX_DVEN_SICOM_OP_12M",
"MAX_DVEN_SICOM_OP_12M",
"maySalVenCom12M242",
"maySalVenComD12M248",
"MAX_DVEN_SBS_OP_12M",
"PorcCuentaPropia.x",
"cuotaD053_P25",
"PorcCuentaAhorrosCorriente.x",
"PorcPersComprasCred.x",
"CUOTA_EST_OP",
"PROM_XVEN_SC_OP_36M",
"maySalVenBanD12M246",
"NENT_VEN_SICOM_OP_12M",
"NOPE_VENC_1A30_OP_24M",
"MVALVEN_SBS_OP_12M",
"PROM_XVEN_SBS_OP_12M",
"PROM_VEN_SBS_OP_12M",
"NENT_VEN_SBS_OP_12M",
"DEUDA_TOTAL_SC_OP_24M",
"PromViviendas.x",
"PROM_MAX_DVEN_N_OP_12M",
"PorcCasado.x",
"consumoTC087_M",
"PorcSocios.x",
"NOPE_NDI_OP_36M",
"numAcreedores348", 
"DEUDA_TOTAL_SBS_OP_3M",
"saldoOpVenSfrCodeudor36M381",
"numAcreedoresOpBanCooComD414",
"peorNivelRiesgoValorOpBanCooComD415",
"PROM_XVEN_SC_OP_24M",
"DEUDA_TOTAL_SICOM_OP_6M",
"PorcSoltero.x",
"gastoPersonal093")


xgboost_modelo1 <- h2o.xgboost(x = predictoras1,
                             y = y1_h20,
                             training_frame = entrenar1_h20,
                             validation_frame = test1_h20,
                             ntrees = 300,               # Número de árboles
                             max_depth = 7,             # Profundidad máxima de los árboles
                             learn_rate = 0.03, # Tasa de aprendizaje 
                             min_rows = 500,
                             keep_cross_validation_predictions = TRUE,
                             stopping_rounds = 500,
                             stopping_metric = "RMSE",
                             seed = 1234)

rendimiento1 <- h2o.performance(xgboost_modelo1,newdata = test1_h20)
rendimiento1

