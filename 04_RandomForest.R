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
library(ggplot2)
options(scipen = 999)
dir.p <- getwd()
dir.b <- paste(dir.p, "BDD", sep = "/")
dir.s <- paste(dir.p, "Scripts", sep = "/")
dir.r <- paste(dir.p, "Resultados", sep = "/")

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
boots_higher_tail <- function(data, corte1 = 0, corte2 = 0, porc1, porc2){
    nb <- which(data$INGRESO_REAL >= corte1 & data$INGRESO_REAL < corte2)
    na <- which(data$INGRESO_REAL >= corte2)
    nc <- which(data$INGRESO_REAL < corte1)
    perc1 <- length(nb)/nrow(data)
    perc2 <- length(na)/nrow(data)
    cat("El peso del primer tramo inicial es : ", perc1, "\n")
    cat("El peso del segundo tramo inicial es : ", perc2, "\n")
    cat(" --- Aplicación del remuestreo --- \n")
    nperc1 <- floor(porc1*length(nb)/perc1)
    nperc2 <- floor(porc2*length(na)/perc2)
    set.seed(12345)
    nb <- sample(nb, size = nperc1, replace = TRUE)
    na <- sample(na, size = nperc2, replace = TRUE)
    data <- data[c(na, nb, nc), ] # Nueva base
    nb <- which(data$INGRESO_REAL >= corte1 & data$INGRESO_REAL < corte2)
    na <- which(data$INGRESO_REAL >= corte2)
    perc1 <- length(nb)/nrow(data)
    perc2 <- length(na)/nrow(data)
    cat("El peso del primer tramo final es : ", perc1, "\n")
    cat("El peso del segundo tramo final es : ", perc2, "\n")
    return(data)
}

# Carga de la data de modelamiento ----------------------------------------

setwd(dir.b)
load("DatosModelamientoFinal_1.RData")
dim(data)
reemplazo_col(data, c("antiguedadSri349"), 0) #no correr hasta ver la tabla de valores perdidos



# Ver las correlaciones ---------------------------------------------------
# Variables con valores constantes
constante <- function(x){
    if(class(x)=="numeric"){
        cte <- min(x, na.rm = TRUE)==max(x, na.rm = TRUE)
    } else {
        tc <- prop.table(table(x))>=0.99
        cte <- any(tc)
    }
    return(cte) 
}

# Porcentaje de NA's
porcNA <- function(x){
    porc <- mean(is.na(x))
    return(porc) 
}

mod <- data[ModVal == 0 & GRUPO_CUOTA == "G1"]
# Identificación de variables con alto porcentaje de NA's
porc <- sort(sapply(mod, porcNA), decreasing = TRUE)
PorcentajeNA <- data.frame(names(porc), as.numeric(porc))
colnames(PorcentajeNA) <- c("Var", "Porc")
dvars <- setdiff(colnames(mod), names(porc)[porc > 0.3]) # Almacena variables validas

# Identificación de variables constantes
dvars <- dvars[!unname(unlist(sapply(mod[,dvars,with=FALSE], constante)))]
rm(list = c("PorcentajeNA", "porc", "porcNA", "constante"))
mod <- mod[,dvars,with=FALSE]
vnum <- colnames(mod)[unname(sapply(mod, class)) %in% c("numeric", "integer")]
vcat <- colnames(mod)[unname(sapply(mod, class)) %in% c("character", "logical")]
dnum <- mod[, vnum, with=FALSE]
dcat <- mod[, vcat, with=FALSE]


#matriz de correlaciones
flattenCorrMatrix <- function(cormat,pmat){
    ut<-upper.tri(cormat)
    data.frame(
        Variable_fila=rownames(cormat)[row(cormat)[ut]],
        Variable_columna=rownames(cormat)[col(cormat)[ut]],
        correlacion=(cormat)[ut],
        p_valor=pmat[ut]
    )
}

library(dplyr)
library(corrplot)
library(readxl)
library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)
library(GGally)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(Hmisc)
library(caret)
library(performanceEstimation)
library(writexl)
mat_corr<-cor(dnum, use = "complete.obs")
res2 <- rcorr(as.matrix(mat_corr))
matriz_correlaciones <- flattenCorrMatrix(res2$r,res2$P)

matriz_ordenada <- arrange(matriz_correlaciones, desc(abs(correlacion)))
write_xlsx(matriz_ordenada, path = "C:/Users/Brad/Desktop/TIC/Diseño de TIC/matriz.xlsx")
rm(list = c("mat_corr", "matriz_ordenada", "matriz_correlaciones", "res2","mod","dcat","dnum"))
rm(flattenCorrMatrix)
# Código en si ------------------------------------------------------------


###Ver procentaje de valores nulos

##prueba
tipos <- sapply(data, function(x) class(x))
print(tipos)



nulos_num <- sort(colSums(is.na(data)),decreasing=FALSE)
nulos_porc <- sort(colMeans(is.na(data)),decreasing = FALSE)
perdidios_num <- sort(colSums(data==""),decreasing = FALSE)
perdidos_porc <- sort(colMeans(data==""), decreasing = FALSE)

#categoricas y numericas
vnum <- colnames(data)[unname(sapply(data, class)) %in% c("numeric", "integer")]
vcat <- colnames(data)[unname(sapply(data, class)) %in% c("character", "logical")]
dnum <- data[, vnum, with=FALSE]
dcat <- data[, vcat, with=FALSE]

mediaten <- colMeans(dnum,na.rm = TRUE)
medianaten <- sapply(dnum, median,na.rm=TRUE)
maxten <- sapply(dnum, max,na.rm=TRUE)
minten <- sapply(dnum, min,na.rm=TRUE)
p1 <- sapply(dnum, function(x) quantile(x,probs=c(0.01),na.rm=TRUE))
p2 <- sapply(dnum, function(x) quantile(x,probs=c(0.02),na.rm=TRUE))
p5 <- sapply(dnum, function(x) quantile(x,probs=c(0.05),na.rm=TRUE))
p10 <- sapply(dnum, function(x) quantile(x,probs=c(0.1),na.rm=TRUE))
p25 <- sapply(dnum, function(x) quantile(x,probs=c(0.25),na.rm=TRUE))
p75 <- sapply(dnum, function(x) quantile(x,probs=c(0.75),na.rm=TRUE))
p90 <- sapply(dnum, function(x) quantile(x,probs=c(0.9),na.rm=TRUE))
p95 <- sapply(dnum, function(x) quantile(x,probs=c(0.95),na.rm=TRUE))
p98 <- sapply(dnum, function(x) quantile(x,probs=c(0.98),na.rm=TRUE))
p99 <- sapply(dnum, function(x) quantile(x,probs=c(0.99),na.rm=TRUE))

medidas <- data.frame(Variable=names(mediaten),
                      media=mediaten,
                      mediana=medianaten,
                      max=maxten,
                      min=minten,
                      pe1=p1,
                      pe2=p2,
                      pe5=p5,
                      pe10=p10,
                      pe25=p25,
                      pe75=p75,
                      pe90=p90,
                      pe95=p95,
                      pe98=p95,
                      pe99=p99)

#medidas_ten <- sapply(dnum, function(x) c(media = mean(x, na.rm=TRUE),
 #                                         mediana = median(x, na.rm=TRUE),
  #                                        maximo = max(x, na.rm=TRUE),
   #                                       minimo = min(x, na.rm=TRUE),
    #                                      p=quantile(x,probs=c(0.1,0.2,0.25,0.75,0.90,0.99),na.rm=TRUE))) 

#medidas_tendencia <- as.data.frame(t(medidas_ten))


tabla_analisis <- data.frame(Variable=names(nulos_porc),
                             Num_nulos=nulos_num,
                             Porc_nulos=nulos_porc)

tabla_analisis2 <- data.frame(Variable=names(perdidios_num),
                              Num_perd=perdidios_num,
                              Porc_perd=perdidos_porc)

tablafin <- left_join(tabla_analisis,medidas, by="Variable")
tablafin2 <- left_join(tablafin,tabla_analisis2, by="Variable")

write_xlsx(tablafin2,"Vista_variables.xlsx")


#imputacion en variables
data[is.na(salPromD36M319), salPromD36M319 := 0] #minimo era cer0
data[is.na(MESES_INFO), MESES_INFO := 1]
data[is.na(cuotaD053_P25), cuotaD053_P25 := 157.6687392] #media
data[is.na(salProm36M303_P25), salProm36M303_P25 := 0]
data[is.na(PorcCuentaPropia.x), PorcCuentaPropia.x :=0.033]
data[is.na(PorcCuentaAhorrosCorriente.x), PorcCuentaAhorrosCorriente.x :=0.1458]
data[is.na(PorcPersComprasCred.x), PorcPersComprasCred.x :=0.0544]
data[is.na(PromViviendas.x), PromViviendas.x :=0.5239]
data[is.na(PorcCasado.x), PorcCasado.x :=0.2893]
data[is.na(PorcSocios.x), PorcSocios.x :=0]
data[is.na(PorcSoltero.x), PorcSoltero.x :=0.1610]


data[is.na(PromVehiculosxVivienda.x), PromVehiculosxVivienda.x := 0.19] #mediana
data[is.na(estadoCivil), estadoCivil:= 1] #soltero
data[is.na(PorcPersPrest.x), PorcPersPrest.x:= 0.1111] #minimo
data[is.na(PorcHipotecas.x), PorcHipotecas.x:= 0] #minimo
data[is.na(salOpDiaCoo004_M), salOpDiaCoo004_M:= 2475.82] #media
data[is.na(salProm36M303_M), salProm36M303_M := 1486.742] #p1
data[is.na(PorcCuentaCaja.x), PorcCuentaCaja.x:= 0.001]#minimo
data[is.na(PorcCuentaCoop.x), PorcCuentaCoop.x:= 0.010904804]#minimo
data[is.na(salOpDia008_M), salOpDia008_M:= 0] #minimo
data[is.na(PorcTvxCableProv.x), PorcTvxCableProv.x :=0.10374891]#min
data[is.na(PorcHogaresTerrenos.x), PorcHogaresTerrenos.x:=0.069148936]
data[is.na(salOpVenCom011_M), salOpVenCom011_M:=0]
data[is.na(PromDepartamentos.x), PromDepartamentos.x:=0.016]
data[is.na(PromVehiculosUsoComercial.x), PromVehiculosUsoComercial.x:=0.08]
data[is.na(PromLocalesCom.x), PromLocalesCom.x:=0.02]
data[is.na(PorcPrimaria.x), PorcPrimaria.x:=0.5]
data[is.na(PromVehiculosUsoHogar.x), PromVehiculosUsoHogar.x:=0.13]
data[is.na(cuotaTotOp059_M), cuotaTotOp059_M:=355.66]
data[is.na(salOpDiaCom005_M), salOpDiaCom005_M:=1.05]
data[is.na(PromNumTerrenos.x), PromNumTerrenos.x:= 1.2]
data[is.na(PorcVivPropiaProv.x), PorcVivPropiaProv.x:=0.25]
data[is.na(PorcRecibeRemesas.x), PorcRecibeRemesas.x:= 0]#minimo
data[is.na(PorcCuentaAhorrosCorriente.x), PorcCuentaAhorrosCorriente.x:=0.145888594] #minimo
data[is.na(salOpVenCom011_M), salOpVenCom011_M:= 0] #minimo
data[is.na(cuotaCoo055_M),cuotaCoo055_M:= 164.2918088] #media
data[is.na(cuotaCom056_M), cuotaCom056_M:= 8.721821558] #media
data[is.na(PorcHogaresNegocios.x), PorcHogaresNegocios.x:= 0.2]


# Distribución Modelamiento/Validación
data[GRUPO_CUOTA2 == "G1"][,.N,by=ModVal][order(ModVal)]
quantile(data[GRUPO_CUOTA2 == "G1"]$INGRESO_REAL, probs = seq(0,1,by=0.02))

#Poner los datos para modelar
mod_g1 <- data[ModVal == 0 & GRUPO_CUOTA2 == "G1"]
mod_g2 <- data[ModVal == 0 & GRUPO_CUOTA2 == "G2"]


#Remuestreo de las dos colas
rmod_g1 <- boots_2tail(mod_g1, corte1 =  480, corte2 = 1800, porc1 = 0.2, porc2 = 0.20)
rmod_g2 <- boots_2tail(mod_g2, corte1 =  550, corte2 = 3000, porc1 = 0.3, porc2 = 0.35)


# Distribuciones muestras (modelamiento)
quantile(mod_g1$INGRESO_REAL, probs = seq(0,1,by=0.02))
quantile(mod_g2$INGRESO_REAL, probs = seq(0,1,by=0.01))

# Distribuciones muestras remuestreadas (modelamiento)
quantile(rmod_g1$INGRESO_REAL, probs = seq(0,1,by=0.02))
quantile(rmod_g2$INGRESO_REAL, probs = seq(0,1,by=0.01))


plot(density(mod_g1$INGRESO_REAL))
plot(density(rmod_g1$INGRESO_REAL))

quantile(mod_g1$PROM_DEUDA_TOTAL_SICOM_OP_24M, probs = seq(0,1,by=0.01))
summary(mod_g1$NOPE_APERT_SBS_OP_36M)

# GRUPO 1
formula_g1 <- "INGRESO_REAL ~ DEUDA_TOTAL_SICOM_OP_24M+
PROM_XVEN_SICOM_OP_36M+
ANTIGUEDAD_OP_SICOM+
numMesesInfoCredBanCoopD36M421+
DEUDA_TOTAL_SBS_OP_24M+
salPromD36M319+
cuotaEstimadaD24M416+
MaxMontoOpD24M417+
PROM_VEN_SICOM_OP_24M+
antiguedadOpTcBanCoo390+
PROM_XVEN_SBS_OP_36M+
ANTIGUEDAD_OP_SBS+
salProm36M303+
NOPE_APERT_SBS_OP_36M+
saldoOpVenSfrTCS36M379+
maxMontoOp096+
MAX_DVEN_SBS_OP_24M+
SalCalHisYTR36MDDBan407+
maySalVenBanD24M270+
numOpsVig36MD333+
PorcCuentaPropia.x+
cuotaD053_P25+
PorcCuentaAhorrosCorriente.x+
PorcPersComprasCred.x+
PROM_XVEN_SC_OP_36M+
PorcSoltero.x+
gastoPersonal093"

# GRUPO 2
formula_g2 <- "INGRESO_REAL ~ maxMontoOp096+
salProm36M303+
MaxMontoOpD24M417+
DEUDA_TOTAL_SICOM_OP_24M+
ANTIGUEDAD_OP_SICOM+
PROM_XVEN_SC_OP_36M+
PROM_XVEN_SICOM_OP_36M+
cuotaEstimadaD24M416+
ANTIGUEDAD_OP_SC+
DEUDA_TOTAL_SC_OP_24M+
antiguedadOpTcBanCoo390+
score001Actual+
numMesesInfoCrediticiaTCS377+
PROM_XVEN_SBS_OP_36M+
ANTIGUEDAD_OP_SBS+
salOpDiaCoo004_M+
PorcCuentaCaja.x+
DEUDA_TOTAL_SBS_OP_24M+
cuotaCoo055_M+
PorcSoltero.x+
PorcPrimaria.x+
SalTotOpD383+
PorcHogaresNegocios.x"


# Random Forest
rf_g1 <- ranger(formula = as.formula(formula_g1), data = mod_g1, num.trees = 500, mtry =9, min.node.size = 550, importance = 'impurity', write.forest = TRUE, seed = 1234)
rrf_g1 <- ranger(formula = as.formula(formula_g1), data = rmod_g1, num.trees = 500, mtry = 9, min.node.size = 720, importance = 'impurity', write.forest = TRUE, seed = 1234)


rf_g2 <- ranger(formula = as.formula(formula_g2), data = mod_g2, num.trees = 500, mtry = 8, min.node.size = 590, importance = 'impurity', write.forest = TRUE, seed = 1234)
rrf_g2 <- ranger(formula = as.formula(formula_g2), data = rmod_g2, num.trees = 500, mtry = 8, min.node.size = 750, importance = 'impurity', write.forest = TRUE, seed = 1234)

print(rf_g1)
print(rf_g2)

print(rrf_g1)
print(rrf_g2)


imp_g1 <- data.table(Variable=names(ranger::importance(rf_g1)), Valor=unname(ranger::importance(rf_g1)))[, Porcentaje := round(Valor/max(Valor), 2)][order(desc(Valor))]
imp_g2 <- data.table(Variable=names(ranger::importance(rf_g2)), Valor=unname(ranger::importance(rf_g2)))[, Porcentaje := round(Valor/max(Valor), 2)][order(desc(Valor))]
rimp_g1 <- data.table(Variable=names(ranger::importance(rrf_g1)), Valor=unname(ranger::importance(rrf_g1)))[, Porcentaje := round(Valor/max(Valor), 2)][order(desc(Valor))]
rimp_g2 <- data.table(Variable=names(ranger::importance(rrf_g2)), Valor=unname(ranger::importance(rrf_g2)))[, Porcentaje := round(Valor/max(Valor), 2)][order(desc(Valor))]

imp_g1
imp_g2

rimp_g1
rimp_g2


# Predicción Grupo 1 sobre toda la base ----
data[, INGRESO_EST_G1 := predict(object=rrf_g1, data=data)$predictions] #Aquí cambiar el object 
data[, RANGO_REAL := cut(INGRESO_REAL, breaks = c(450, 520, 670, 980, 1300, 2500), labels = c("[450-520]", "(520-670]", "(670-980]", "(980-1300]", "(1300-2500]"))]
data[, RANGO_EST := cut(INGRESO_EST_G1, breaks = c(450, 520, 670, 980, 1300, 2500), labels = c("[450-520]", "(520-670]", "(670-980]", "(980-1300]", "(1300-2500]"))]
# Matriz de coincidencia
quantile(data[GRUPO_CUOTA2 == "G1"]$INGRESO_EST_G1)
data[ModVal == 0 & GRUPO_CUOTA2 == "G1"][,table(RANGO_REAL, RANGO_EST)]
data[ModVal == 1 & GRUPO_CUOTA2 == "G1"][,table(RANGO_REAL, RANGO_EST)]
# Métricas
data[, MSE := (INGRESO_REAL - INGRESO_EST_G1)^2][GRUPO_CUOTA2 == "G1"][,list(MSE = mean(MSE)), by = ModVal][order(ModVal)] # Error cuadrático medio (MSE)
data[, MAE := abs(INGRESO_REAL - INGRESO_EST_G1)][GRUPO_CUOTA2 == "G1"][,list(MAE = mean(MAE)), by = ModVal][order(ModVal)] # Error absoluto medio (MAE)
data[GRUPO_CUOTA2 == "G1" & ModVal == 0][,list(MSE = mean(MSE)), by = RANGO_REAL][order(RANGO_REAL)]
data[GRUPO_CUOTA2 == "G1" & ModVal == 1][,list(MSE = mean(MSE)), by = RANGO_REAL][order(RANGO_REAL)]



# Predicción Grupo 2 sobre toda la base ----
data[, INGRESO_EST_G2 := predict(object=rrf_g2, data=data)$predictions]
data[, RANGO_REAL := cut(INGRESO_REAL, breaks = c(450, 800, 1200, 2000, 4000, 35000), labels = c("[450-800]", "(800-1200]", "(1200-2000]", "(2000-4000]", "(4000-35000]"))]
data[, RANGO_EST := cut(INGRESO_EST_G2, breaks = c(450, 800, 1200, 2000, 4000, 35000), labels = c("[450-800]", "(800-1200]", "(1200-2000]", "(2000-4000]", "(4000-35000]"))]
# Matriz de coincidencia
quantile(data[GRUPO_CUOTA2 == "G2"]$INGRESO_EST_G2)
data[ModVal == 0 & GRUPO_CUOTA2 == "G2"][,table(RANGO_REAL, RANGO_EST)]
data[ModVal == 1 & GRUPO_CUOTA2 == "G2"][,table(RANGO_REAL, RANGO_EST)]
# Métricas
data[, MSE := (INGRESO_REAL - INGRESO_EST_G2)^2][GRUPO_CUOTA2 == "G2"][,list(MSE = mean(MSE)), by = ModVal][order(ModVal)] # Error cuadrático medio (MSE)
data[, MAE := abs(INGRESO_REAL - INGRESO_EST_G2)][GRUPO_CUOTA2 == "G2"][,list(MAE = mean(MAE)), by = ModVal][order(ModVal)] # Error absoluto medio (MAE)
data[GRUPO_CUOTA2 == "G2" & ModVal == 0][,list(MSE = mean(MSE)), by = RANGO_REAL][order(RANGO_REAL)]
data[GRUPO_CUOTA2 == "G2" & ModVal == 1][,list(MSE = mean(MSE)), by = RANGO_REAL][order(RANGO_REAL)]



data[, MARCA_PRED := ifelse(INGRESO_EST_G1 <= INGRESO_REAL + 200 & INGRESO_EST_G1 >= INGRESO_REAL - 200, 1, 0)]
data[ModVal == 0 & GRUPO_CUOTA2 == "G1"][,.N,by=MARCA_PRED]
round(prop.table(data[ModVal == 0 & GRUPO_CUOTA2 == "G2"][,table(RANGO_REAL, RANGO_EST)],1),2)

ggplot(data[ModVal == 0 & GRUPO_CUOTA2 == "G1"], aes(x=INGRESO_REAL, y=INGRESO_EST_G1, colour=MARCA_PRED)) + geom_point()


#Gráfico

ggplot() + 
    geom_density(data = data[ModVal == 0 & GRUPO_CUOTA2 == "G2"], aes(x = INGRESO_REAL, fill = "Ingreso Real"), alpha = 0.6, adjust = 3) + 
    geom_density(data = data[ModVal == 0 & GRUPO_CUOTA2 == "G2"], aes(x = INGRESO_EST_G2, fill = "Ingreso Estimado"), alpha = 0.6, adjust = 3) + 
    xlab("Ingreso") + 
    ylab("Densidad") + 
    scale_fill_hue(labels = c("Estimado", "Real")) +
    labs(fill = "Ingreso")
ggplot() + 
    geom_density(data = data[ModVal == 1 & GRUPO_CUOTA2 == "G1"], aes(x = INGRESO_REAL, fill = "Ingreso Real"), alpha = 0.6, adjust = 3) + 
    geom_density(data = data[ModVal == 1 & GRUPO_CUOTA2 == "G1"], aes(x = INGRESO_EST_G1, fill = "Ingreso Estimado"), alpha = 0.6, adjust = 3) + 
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



plot(density(data[ModVal == 0 & GRUPO_CUOTA2 == "G2"]$INGRESO_REAL))
plot(density(data[ModVal == 0 & GRUPO_CUOTA2 == "G2"]$INGRESO_EST_G2))






plot(res$Ing_Real, res$Ing_Est)








# Grid de hiperparámetros evaluados
# ==============================================================================
param_grid <- expand_grid(
    'num_trees' = c(300, 400, 500),
    'mtry'      = c(6, 7, 8),
    'min_node' = c(590, 600, 700, 800)
)

oob_error <- rep(NA, nrow(param_grid))

for(i in 1:nrow(param_grid)){
    modelo <- ranger(
        formula   = as.formula(formula_g2),
        data      = mod_g2, 
        num.trees = param_grid$num_trees[i],
        mtry      = param_grid$mtry[i],
        min.node.size = param_grid$min_node[i],
        seed      = 1234
    )
    oob_error[i] <- sqrt(modelo$prediction.error)
}





resultados <- data.table(param_grid)
resultados[, error := oob_error]
resultados <- resultados[order(error)]

####Nueva funca 

library(openxlsx)
write.xlsx(resultados, file = "grid_randforest_mod2.xlsx", sheetName = "Grid", rowNames = FALSE)


