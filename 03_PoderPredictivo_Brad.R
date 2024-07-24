############################################################################
##########                 Modelamiento Predictor                 ##########
############################################################################
library(data.table)
library(readxl)
library(stringr)
library(dplyr)
options(scipen = 999)
dir.p <- getwd()
dir.b <- paste(dir.p, "BDD", sep = "/")
dir.s <- paste(dir.p, "Scripts", sep = "/")
dir.r <- paste(dir.p, "Resultados", sep = "/")

setwd(dir.b)
load("DatosModelamiento.RData")
#rm(list = c("data"))
dim(data)


# adjuntar nuevas bases ---------------------------------------------------

cod2dig <- function(vector){
    vector <- ifelse(str_length(vector)==1,paste0('0',vector),vector)
    return(vector)
}

# Datos descriptivos Buró
burop <- fread("DatosDescriptivosBuro_PorParroquia.txt", dec=",")[,c(2:144)]
colnames(burop)[1:3] <- c("PROVINCIA", "CANTON", "PARROQUIA")
burop$PROVINCIA <- cod2dig(burop$PROVINCIA)
burop$CANTON <- cod2dig(burop$CANTON)
burop$PARROQUIA <- cod2dig(burop$PARROQUIA)
burop[, CodCruce := paste0(PROVINCIA, CANTON, PARROQUIA)]
burop[,c("PROVINCIA", "CANTON", "PARROQUIA") := NULL]

buroc <- fread("DatosDescriptivosBuro_PorProvinciaYCanton.txt", dec=",")
colnames(buroc)[1:3] <- c("CodCruce", "PROVINCIA", "CANTON")
buroc$PROVINCIA <- cod2dig(buroc$PROVINCIA)
buroc$CANTON <- cod2dig(buroc$CANTON)
buroc[, CodCruce := paste0(PROVINCIA, CANTON, "00")]
buroc[,c("PROVINCIA", "CANTON") := NULL]

setdiff(colnames(buroc), colnames(burop))
setdiff(colnames(burop), colnames(buroc))
vars <- colnames(buroc)
buro <- rbindlist(list(buroc[, vars, with = FALSE], burop[, vars, with = FALSE]))
rm(list = c("buroc", "burop", "vars", "sri"))

#HAcer lo mismo con la base original
data$provincia <- cod2dig(data$provincia)
data$canton <- cod2dig(data$canton)
data$parroquia <- cod2dig(data$parroquia)
data[, parroquia := ifelse(is.na(parroquia), "00", parroquia)]
data[,canton := ifelse(is.na(canton),"00", canton)]
data[,provincia := ifelse(is.na(provincia),"00", provincia)]
# Creamos llave de cruce
data[, CodCruce := paste0(provincia, canton, parroquia)]
data[, CodCruce2 :=paste0(provincia,canton)]
#Cruzar buro con data 
data<- left_join(data,buro,by="CodCruce")

#Cruzar uno por uno

#base del INEC
data_inec <- read_excel("Indicadores_Censo_V3.0.xlsx")
data_inec_def <- data.table(data_inec)
# Creamos llave de cruce
data_inec_def[, CodCruce := paste0(CodParroquia)]
data_inec_def[, CodCruce2 :=paste0(CodCanton)]
#data_cruzada <- buro[data_inec_def, on = "CodCruce"]
#Aqui esta la data cruzada de las tres bases nuevas
#rm(list=c("data_inec","data_inec_def"))


#curzar con la del INEc
data<- left_join(data,data_inec_def,by="CodCruce")


moneop <- data_inec_def[,c("CodCruce","CodParroquia","CodProvincia","Provincia","Canton","Parroquia"):=NULL]

moneop <- moneop[,lapply(.SD, mean, na.rm = TRUE), by=CodCanton]
moneop[,CodCruce2 :=paste0(CodCanton)]

defing <- left_join(data,moneop,by="CodCruce2")

columnas_x <- grep("\\.x$", names(defing), value = TRUE)

# Recorrer cada columna .x y realizar el reemplazo
for (col_x in columnas_x) {
    col_y <- sub("\\.x$", ".y", col_x)  # Obtener el nombre de la columna .y correspondiente
    defing[[col_x]][is.na(defing[[col_x]])] <- defing[[col_y]][is.na(defing[[col_x]])]  # Realizar el reemplazo
}

columnas_y <- grep("\\.y$", names(defing), value = TRUE)

# Eliminar las columnas encontradas
defing <- defing[, (columnas_y):=NULL]

data <- defing

# lo mismo de antes -------------------------------------------------------

# Sujetos con cuota estimada y sin cupo de tarjeta de crédito
#info <- info[TIENE_CUPO == "NO" & TIENE_CUOTA == "SI"]
#dim(info)
data[,.N,by=GRUPO_CUOTA2]
# Muestra de modelamiento y validación
set.seed(4321)
marca <- sample(1:nrow(data), size=floor(0.5*nrow(data)), replace = FALSE)
data[, ModVal := 1:nrow(data)]
data[, ModVal := ifelse(ModVal %in% marca, 1, 0)]
data[, .N, by=ModVal]
data[,table(GRUPO_CUOTA2, ModVal, useNA = "always")]

# Ejecución de variables acumuladas y ratios
setwd(dir.s)
source("./Scripts/fConstrRatios.R")
# datos: data consolidada a nivel de sujeto
system.time(
    info <- fun_acum(data = data)
)
#system.time(
 #   info <- fun_ratios(data = data)
#)
source("./Scripts/fConstrRatiosCombinados.R")
#system.time(
#    info <- fun_comb(data = data)
#)
rm(list = c("fun_acum", "fun_ratios", "fun_comb"))


# Rangos del ingreso estimado
quantile(data[ModVal == 0 & GRUPO_CUOTA2 == "G1"]$INGRESO_REAL, probs = c(0.3,0.7)) # Cuartiles Ingreso Grupo G1
quantile(data[ModVal == 0 & GRUPO_CUOTA2 == "G2"]$INGRESO_REAL, probs = c(0.3,0.7)) # Cuartiles Ingreso Grupo G2
#quantile(data[ModVal == 0 & GRUPO_CUOTA2 == "G3"]$INGRESO_REAL, probs = c(0.3,0.7)) # Cuartiles Ingreso Grupo G3(ya no hay)

data[, RANGO_INGRESO_G1 := ifelse(INGRESO_REAL <= 540, 1, ifelse(INGRESO_REAL <= 840, 2, 3))]
data[, RANGO_INGRESO_G2 := ifelse(INGRESO_REAL <= 640, 1, ifelse(INGRESO_REAL <= 1260, 2, 3))]#multiplos de 5
#data[, RANGO_INGRESO_G3 := ifelse(INGRESO_REAL <= 640, 1, ifelse(INGRESO_REAL <= 1450, 2, 3))]

data[ModVal == 0 & GRUPO_CUOTA2 == "G1"][,.N,by=RANGO_INGRESO_G1][order(RANGO_INGRESO_G1)]
data[ModVal == 0 & GRUPO_CUOTA2 == "G2"][,.N,by=RANGO_INGRESO_G2][order(RANGO_INGRESO_G2)]
#data[ModVal == 0 & GRUPO_CUOTA2 == "G3"][,.N,by=RANGO_INGRESO_G3][order(RANGO_INGRESO_G3)]

setwd(dir.b)
save(list = c("data"), file = "DatosModelamientoFinal_1.RData", envir = .GlobalEnv)


# Cálculo del KS y VI
setwd(dir.p)
source("./Scripts/KS_Gen.R")

mod <- data[ModVal == 0 & GRUPO_CUOTA2 == "G2"]
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
VI <- sort(sapply(dcat, TestVI, y=mod$RANGO_INGRESO_G2), decreasing = T) # Revisar variable dependiente
dVI <- data.frame(names(VI), VI)
colnames(dVI) <- c("Variable", "VI"); rownames(dVI) <- NULL

KS <- sapply(seq_along(dnum), function(i){test_ks_ksample(dnum[[i]], mod$RANGO_INGRESO_G2)}) # Revisar variable dependiente
dKS <- data.frame(colnames(dnum), KS); dKS <- dKS[order(dKS$KS, decreasing = TRUE),]
colnames(dKS) <- c("Variable", "KS"); rownames(dKS) <- NULL
dKS
rm(list = c("KS", "TestKS", "TestVI", "dnum", "dcat", "vnum", "vcat", "VI", "dvars", "mod"))


library(openxlsx)
setwd(dir.r)
write.xlsx(list("Var_Cuantitativas" = dKS, "Var_Cualitativas" = dVI), file = "Poder_Discriminante_G2.xlsx")


#No correr lo de abajo, solo era de pruebas ####

# nuevo codigo ------------------------------------------------------------

setwd(dir.b)

cod2dig <- function(vector){
    vector <- ifelse(str_length(vector)==1,paste0('0',vector),vector)
    return(vector)
}

# Datos descriptivos Buró
burop <- fread("DatosDescriptivosBuro_PorParroquia.txt", dec=",")[,c(2:144)]
colnames(burop)[1:3] <- c("PROVINCIA", "CANTON", "PARROQUIA")
burop$PROVINCIA <- cod2dig(burop$PROVINCIA)
burop$CANTON <- cod2dig(burop$CANTON)
burop$PARROQUIA <- cod2dig(burop$PARROQUIA)
burop[, CodCruce := paste0(PROVINCIA, CANTON, PARROQUIA)]
burop[,c("PROVINCIA", "CANTON", "PARROQUIA") := NULL]

buroc <- fread("DatosDescriptivosBuro_PorProvinciaYCanton.txt", dec=",")
colnames(buroc)[1:3] <- c("CodCruce", "PROVINCIA", "CANTON")
buroc$PROVINCIA <- cod2dig(buroc$PROVINCIA)
buroc$CANTON <- cod2dig(buroc$CANTON)
buroc[, CodCruce := paste0(PROVINCIA, CANTON, "00")]
buroc[,c("PROVINCIA", "CANTON") := NULL]

setdiff(colnames(buroc), colnames(burop))
setdiff(colnames(burop), colnames(buroc))
vars <- colnames(buroc)
buro <- rbindlist(list(buroc[, vars, with = FALSE], burop[, vars, with = FALSE]))
rm(list = c("buroc", "burop", "vars", "sri"))

#base del INEC
data_inec <- read_excel("Indicadores_Censo_V3.0.xlsx")
data_inec_def <- data.table(data_inec)
# Enceramos la variable Provincia, Canton, Parroquia #esto no necesitaria
data_inec_def[, CodParroquia := ifelse(is.na(CodParroquia), "00", CodParroquia)]
# Creamos llave de cruce
data_inec_def[, CodCruce := paste0(CodParroquia)]
data_cruzada <- buro[data_inec_def, on = "CodCruce"]
#Aqui esta la data cruzada de las tres bases nuevas

#HAcer lo mismo con la base original
data$provincia <- cod2dig(data$provincia)
data$canton <- cod2dig(data$canton)
data$parroquia <- cod2dig(data$parroquia)
data[, parroquia := ifelse(is.na(parroquia), "00", parroquia)]
# Creamos llave de cruce
data[, CodCruce := paste0(provincia, canton, parroquia)]

data_prueba <- left_join(data,data_cruzada,by="CodCruce")

