##########################################################################
##########                 Análisis Univariados                 ##########
##########################################################################
library(data.table)
library(ggplot2)
library(tidyverse)
options(scipen = 999)
dir.p <- getwd()
dir.b <- paste(dir.p, "BDD", sep = "/")
dir.s <- paste(dir.p, "Scripts", sep = "/")
dir.r <- paste(dir.p, "Resultados", sep = "/")

# Diferencia fechas
num_anios <- function(end_date, start_date){
    ed <- as.POSIXlt(end_date)
    sd <- as.POSIXlt(start_date)
    anio <- floor((12 * (ed$year - sd$year) + (ed$mon - sd$mon))/12)
    return(anio)
}
num_meses <- function(end_date, start_date){
    ed <- as.POSIXlt(end_date)
    sd <- as.POSIXlt(start_date)
    mes <- floor(12 * (ed$year - sd$year) + (ed$mon - sd$mon))
    return(mes)
}

# Función de reemplazo NA's
reemplazo_col = function(dt, vars, valor){ 
    na.replace = function(v, value=valor) { v[is.na(v)] = value; v }
    for (i in vars)
        eval(parse(text=paste("dt[,",i,":=na.replace(",i,")]")))
}
# Función para la estructuración
myspread <- function(df, key, value){
    # quote key
    keyq <- rlang::enquo(key)
    # break value vector into quotes
    valueq <- rlang::enquo(value)
    s <- rlang::quos(!!valueq)
    df %>% gather(variable, value, !!!s) %>%
        unite(temp, !!keyq, variable) %>%
        spread(temp, value, fill=0)
}

setwd(dir.b)
list.files()
load("DatosConsolidadosInicial.RData")


# Marca Bancarizado
dim(info)
reemplazo_col(info, c("salProm36M303", "cuota052", "maxCupoTC089", "ingreso136"), 0)
info[, BANCARIZADO_36M := ifelse(salProm36M303 > 0, "BANCARIZADO", "NO BANCARIZADO")]
info[, BANCARIZADO_AVAL := ifelse(ingreso136 == 0, "NO BANCARIZADO", "BANCARIZADO")]
info[, table(BANCARIZADO_36M, BANCARIZADO_AVAL)]

info[,.N,by=BANCARIZADO_AVAL]

quantile(info[BANCARIZADO_AVAL == "BANCARIZADO"]$INGRESO_REAL, probs = seq(0,1,by=0.1))
quantile(info[BANCARIZADO_AVAL == "BANCARIZADO"]$ingreso136, probs = seq(0,1,by=0.1))

 # Histograma Ingresos
info <- info[BANCARIZADO_AVAL == "BANCARIZADO"]

d1 <- data.table(VALOR = c(info$INGRESO_REAL, info$ingreso136))
d1[, INGRESO := c(rep("REAL", 927647), rep("ESTIMADO", 927647))]

d1 <- d1[VALOR <= 7500]

ggplot(d1, aes(x=VALOR, fill=INGRESO)) + geom_histogram(position="identity", alpha=0.6, binwidth=250) + xlab("Ingreso") + ylab("Casos") + ggtitle("Ingreso Real vs Estimado")
ggplot(d1, aes(x=VALOR, fill=INGRESO)) + geom_density(alpha=.6, adjust=3) + xlab("Ingreso") + ylab("Densidad") + ggtitle("Ingreso Real vs Estimado")


# Reglas que filtran casos atípicos
# 1) La cuota estimada actual no puede superar al ingreso
info[, M_CUOTA_INGRESO := ifelse(cuota052 > INGRESO_REAL, "SI", "NO")]
info[,.N,by=M_CUOTA_INGRESO]
quantile(info[M_CUOTA_INGRESO == "NO"]$cuota052, probs = seq(0,1,by=0.1))

# 2) El cupo de la TC no puede superar notablemente al ingreso
info[, M_CUPO_INGRESO := ifelse(maxCupoTC089 > 5*INGRESO_REAL & INGRESO_REAL <= 1000, "SI", "NO")]
info[M_CUOTA_INGRESO == "NO"][,.N,by=M_CUPO_INGRESO]
quantile(info[M_CUOTA_INGRESO == "NO" & M_CUPO_INGRESO == "NO"]$maxCupoTC089, probs = seq(0,1,by=0.1))

# 3) Morosidad mayor a 60 días
info[, Morosidad := maxMorosidadBan132 + maxMorosidadCoo133 + maxMorosidadCom134]
info[, M_MOROSIDAD := ifelse(Morosidad > 60, "SI", "NO")]
info[M_CUOTA_INGRESO == "NO" & M_CUPO_INGRESO == "NO"][,.N,by=M_MOROSIDAD]
quantile(info[M_CUOTA_INGRESO == "NO" & M_CUPO_INGRESO == "NO" & M_MOROSIDAD == "NO"]$Morosidad, probs = seq(0,1,by=0.1))

info <- info[M_CUOTA_INGRESO == "NO" & M_CUPO_INGRESO == "NO" & M_MOROSIDAD == "NO"]
# Tiene cupo de TC
info[, TIENE_CUPO := ifelse(maxCupoTC089 > 0, "SI", "NO")]
info[,.N,by=TIENE_CUPO]

# Tiene Cuota Estimada Actual
info[, TIENE_CUOTA := ifelse(cuota052 > 0, "SI", "NO")]
info[,.N,by=TIENE_CUOTA]
info[, table(TIENE_CUPO, TIENE_CUOTA)]


# Quintiles para comparación
r1 <- info[TIENE_CUPO == "SI" & TIENE_CUOTA == "SI"]
quantile(r1$INGRESO_REAL, probs = c(0.2,0.4,0.6,0.8))
quantile(r1$ingreso136, probs = c(0.2,0.4,0.6,0.8))
r1[, R_INGRESO := cut(INGRESO_REAL, breaks = c(0, 670.0, 962.5, 1450.0, 2566.0, 1000000), labels = c("<=670.0", "670.0-962.5", "962.5-1450.0", "1450.0-2566.0", ">2566.0"))]
r1[,.N,by=R_INGRESO]
r1[, R_PREDICTOR := cut(ingreso136, breaks = c(0, 670.0, 962.5, 1450.0, 2566.0, 1000000), labels = c("<=670.0", "670.0-962.5", "962.5-1450.0", "1450.0-2566.0", ">2566.0"))]
r1[,.N,by=R_PREDICTOR]
r1[,table(R_INGRESO, R_PREDICTOR)]

r2 <- info[TIENE_CUPO == "NO" & TIENE_CUOTA == "SI"]
quantile(r2$INGRESO_REAL, probs = c(0.2,0.4,0.6,0.8))
quantile(r2$ingreso136, probs = c(0.2,0.4,0.6,0.8))
r2[, R_INGRESO := cut(INGRESO_REAL, breaks = c(0, 520, 700, 951, 1680, 1000000), labels = c("<=520", "520-700", "700-951", "951-1680", ">1680"))]
r2[,.N,by=R_INGRESO]
r2[, R_PREDICTOR := cut(ingreso136, breaks = c(0, 520, 700, 951, 1680, 1000000), labels = c("<=520", "520-700", "700-951", "951-1680", ">1680"))]
r2[,.N,by=R_PREDICTOR]
r2[,table(R_INGRESO, R_PREDICTOR)]



d2 <- data.table(VALOR = c(info[TIENE_CUPO == "SI" & TIENE_CUOTA == "SI"]$INGRESO_REAL, info[TIENE_CUPO == "SI" & TIENE_CUOTA == "SI"]$ingreso136))
d2[, INGRESO := c(rep("REAL", 231186), rep("ESTIMADO", 231186))]

d2 <- d2[VALOR <= 7500]

ggplot(d2, aes(x=VALOR, fill=INGRESO)) + geom_histogram(position="identity", alpha=0.6, binwidth=250) + xlab("Ingreso") + ylab("Casos") + ggtitle("Ingreso Real vs Estimado")
ggplot(d2, aes(x=VALOR, fill=INGRESO)) + geom_density(alpha=.6, adjust=3) + xlab("Ingreso") + ylab("Densidad") + ggtitle("Ingreso Real vs Estimado")


d3 <- data.table(VALOR = c(info[TIENE_CUPO == "NO" & TIENE_CUOTA == "SI"]$INGRESO_REAL, info[TIENE_CUPO == "NO" & TIENE_CUOTA == "SI"]$ingreso136))
d3[, INGRESO := c(rep("REAL", 298449), rep("ESTIMADO", 298449))]

d3 <- d3[VALOR <= 7500]

ggplot(d3, aes(x=VALOR, fill=INGRESO)) + geom_histogram(position="identity", alpha=0.6, binwidth=250) + xlab("Ingreso") + ylab("Casos") + ggtitle("Ingreso Real vs Estimado")
ggplot(d3, aes(x=VALOR, fill=INGRESO)) + geom_density(alpha=.6, adjust=3) + xlab("Ingreso") + ylab("Densidad") + ggtitle("Ingreso Real vs Estimado")



# Sujetos sin info actual 
data <- info[TIENE_CUPO == "NO" & TIENE_CUOTA == "NO"]
dim(data)
################ Esto me separa la base que necesito############## 

data_provincia <- data[,list(ingreso_med=mean(INGRESO_REAL),n_sujetos= .N,porcentaje=(.N/77819)*100), by=provincia]
setorder(data_provincia,ingreso_med)
quantile(data_provincia$ingreso_med,probs = seq(0.1,by=0.05))

data <- left_join(data,data_provincia,by="provincia")

#Se elimina la provincia con valores NA
data_provincia <- na.omit(data_provincia)

data[,INGRESO_PROV := ifelse(provincia==1,1546.9363,
                             ifelse(provincia==2,946.4261,
                                    ifelse(provincia==3,1572.7032,
                                           ifelse(provincia==4,1348.0178,
                                                  ifelse(provincia==5,1100.2576,
                                                         ifelse(provincia==6,1170.3155,
                                                                ifelse(provincia==7,1250.7834,
                                                                       ifelse(provincia==8,1004.8202,
                                                                              ifelse(provincia==9,947.6545,
                                                                                     ifelse(provincia==10,954.9656,
                                                                                            ifelse(provincia==11,1933.7366,
                                                                                                   ifelse(provincia==12,821.6898,
                                                                                                          ifelse(provincia==13,848.7534,
                                                                                                                 ifelse(provincia==14,1932.6922,
                                                                                                                        ifelse(provincia==15,771.0191,
                                                                                                                               ifelse(provincia==16,843.8703,
                                                                                                                                      ifelse(provincia==17,1121.0951,
                                                                                                                                             ifelse(provincia==18,1089.3125,
                                                                                                                                                    ifelse(provincia==19,2346.4249,
                                                                                                                                                           ifelse(provincia==20,1395.2350,
                                                                                                                                                                  ifelse(provincia==21,1107.4758,
                                                                                                                                                                         ifelse(provincia==22,1113.8164,
                                                                                                                                                                                ifelse(provincia==23,1035.3601,
                                                                                                                                                                                       ifelse(provincia==24,785.7918,975.5568))))))))))))))))))))))))]

#rangos
data[,R_INGRESOS := ifelse(INGRESO_REAL<=450, 450,
                           ifelse(INGRESO_REAL<=1000,1000,
                                  ifelse(INGRESO_REAL<=2500,2500,
                                         ifelse(INGRESO_REAL<=5000,5000,10000))))]

data[,R_INGRESOS2 := ifelse(INGRESO_REAL<=450,450,
                            ifelse(INGRESO_REAL>450 & INGRESO_REAL<=1000,1000,
                                   ifelse(INGRESO_REAL>1000 & INGRESO_REAL<=2500,2500,
                                          ifelse(INGRESO_REAL>2500 & INGRESO_REAL<=5000,5000,10000))))]

data[,R_provincia:= ifelse(ingreso_med<=1100, 1100,10000)]
                           

data[,table(R_INGRESOS2, R_provincia)]

round(prop.table(data[,table(R_INGRESOS2, R_provincia)]),2)

#grupos
data[,GRUPO_CUOTA := ifelse(INGRESO_REAL<=2500 & INGRESO_PROV<=955,"G1",
                            ifelse(INGRESO_REAL>450 & INGRESO_REAL<=5000 & INGRESO_PROV>955 &INGRESO_PROV<=1220,"G2",
                                   ifelse(INGRESO_REAL>450 & INGRESO_PROV>1220,"G3", "EXCLUIR")))]

data[,GRUPO_CUOTA2 := ifelse(ingreso_med<=1100 & INGRESO_REAL>450 & INGRESO_REAL<=2500,"G1",
                             ifelse(ingreso_med>1100 & INGRESO_REAL>450,"G2","EXCLUIR"))]

data[,list(Sujetos=.N, Media=mean(INGRESO_REAL),Mediana=median(INGRESO_REAL),Media=mean(ingreso136),Mediana=median(ingreso136)),by=GRUPO_CUOTA2][order(GRUPO_CUOTA2)]


#Esto para el grafico
d1 <- data[TIENE_CUPO == "NO" & GRUPO_CUOTA2 %in% c("G1", "G2")][,.(INGRESO_REAL, ingreso136, GRUPO_CUOTA2)]
d1 <- d1[INGRESO_REAL <= 7500]
ggplot(d1, aes(x=INGRESO_REAL, fill=GRUPO_CUOTA2)) + geom_density(alpha=.6, adjust=3) + xlab("Ingreso") + ylab("Densidad") + ggtitle("Ingreso Real vs Grupo")
ggplot(d1, aes(x=ingreso136, fill=GRUPO_CUOTA2)) + geom_density(alpha=.6, adjust=3) + xlab("Ingreso") + ylab("Densidad") + ggtitle("Ingreso Estimado vs Grupo")




r2 <- data[TIENE_CUPO == "NO" & TIENE_CUOTA == "NO"]
quantile(r2$INGRESO_REAL, probs = c(0.2,0.4,0.6,0.8,1))
quantile(r2$ingreso136, probs = c(0.2,0.4,0.6,0.8,1))
r2[, R_INGRESO := cut(INGRESO_REAL, breaks = c(0, 500, 630, 840, 1330, 1000000), labels = c("<=500", "500-630", "630-840", "840-1330", ">1330"))]
r2[,.N,by=R_INGRESO]
r2[, R_PREDICTOR := cut(ingreso136, breaks = c(0, 500, 630, 840, 1330, 1000000), labels = c("<=500", "500-630", "630-840", "840-1330", ">1330"))]
r2[,.N,by=R_PREDICTOR]
r2[,table(R_INGRESO, R_PREDICTOR)]


d3 <- data.table(VALOR = c(data[TIENE_CUPO == "NO" & TIENE_CUOTA == "NO"]$INGRESO_REAL, data[TIENE_CUPO == "NO" & TIENE_CUOTA == "NO"]$ingreso136))
d3[, INGRESO := c(rep("REAL", 77819), rep("ESTIMADO", 77819))]

d3 <- d3[VALOR <= 7500]

ggplot(d3, aes(x=VALOR, fill=INGRESO)) + geom_histogram(position="identity", alpha=0.6, binwidth=250) + xlab("Ingreso") + ylab("Casos") + ggtitle("Ingreso Real vs Estimado")
ggplot(d3, aes(x=VALOR, fill=INGRESO)) + geom_density(alpha=.6, adjust=3) + xlab("Ingreso") + ylab("Densidad") + ggtitle("Ingreso Real vs Estimado")




#Guardar datos
setwd(dir.b)
save(list = c("data"), file = "DatosModelamiento.RData", envir = .GlobalEnv)



#########################################################################

setwd(dir.b)
load("CuotaHis.RData")
length(unique(aux$numeroIdentificacion))# Sujetos sin info actual 

quitar_coma <- function(vector){
    res <- as.numeric(sub(pattern = "\\,", replacement = "\\.", x = vector))
    return(res)
}

aux$cuota052 <- quitar_coma(aux$cuota052)
aux$cupoTC086 <- quitar_coma(aux$cupoTC086)
aux[, ID := bit64::as.integer64(numeroIdentificacion)]
aux[, fechaCorte := as.Date(fechaCorte, format = "%Y-%m-%d")]
aux <- aux[,.(ID, cuota052, cupoTC086, fechaCorte)]
aux[, MES := num_meses("2021-12-31", fechaCorte)]
aux[,.N,by=MES][order(MES)]

r1 <- aux %>% dplyr::filter(MES > 0 & MES <= 6) %>% group_by(ID) %>% dplyr::summarise(cuota052 = max(cuota052), cupoTC086 = max(cupoTC086)) %>% setDT(.)
colnames(r1) <- c("ID", "MaxCuota_6M", "MaxCupoTC_6M")
r2 <- aux %>% dplyr::filter(MES > 0 & MES <= 12) %>% group_by(ID) %>% dplyr::summarise(cuota052 = max(cuota052), cupoTC086 = max(cupoTC086)) %>% setDT(.)
colnames(r2) <- c("ID", "MaxCuota_12M", "MaxCupoTC_12M")
r3 <- aux %>% dplyr::filter(MES > 0 & MES <= 24) %>% group_by(ID) %>% dplyr::summarise(cuota052 = max(cuota052), cupoTC086 = max(cupoTC086)) %>% setDT(.)
colnames(r3) <- c("ID", "MaxCuota_24M", "MaxCupoTC_24M")
rm(list = c("aux"))

data <- r1[data, on = "ID"]
data <- r2[data, on = "ID"]
data <- r3[data, on = "ID"]
reemplazo_col(data, c("MaxCuota_24M", "MaxCupoTC_24M", "MaxCuota_12M", "MaxCupoTC_12M", "MaxCuota_6M", "MaxCupoTC_6M"), 0)

# 1) La cuota estimada actual no puede superar al ingreso
data[, M_CUOTA_INGRESO_HIS := ifelse(MaxCuota_12M > INGRESO_REAL, "SI", "NO")]
data[,.N,by=M_CUOTA_INGRESO_HIS]
quantile(data[M_CUOTA_INGRESO_HIS == "NO"]$MaxCuota_12M, probs = seq(0,1,by=0.1))

# 2) El cupo de la TC no puede superar notablemente al ingreso
data[, M_CUPO_INGRESO_HIS := ifelse(MaxCupoTC_12M > 5*INGRESO_REAL & INGRESO_REAL <= 1000, "SI", "NO")]
data[M_CUOTA_INGRESO_HIS == "NO"][,.N,by=M_CUPO_INGRESO_HIS]
quantile(data[M_CUOTA_INGRESO_HIS == "NO" & M_CUPO_INGRESO_HIS == "NO"]$MaxCupoTC_12M, probs = seq(0,1,by=0.1))

data <- data[M_CUOTA_INGRESO_HIS == "NO" & M_CUPO_INGRESO_HIS == "NO"]

data[, TIENE_CUPO_HIS := ifelse(MaxCupoTC_12M > 0, "SI", "NO")]
data[, TIENE_CUOTA_HIS := ifelse(MaxCuota_12M > 0, "SI", "NO")]
data[, table(TIENE_CUPO_HIS, TIENE_CUOTA_HIS)]


data <- data[TIENE_CUOTA_HIS == "SI"]
dim(data)
# quantile(data$MaxCuota_12M, probs = c(0.5))
data[, R_INGRESOS := ifelse(INGRESO_REAL <= 425, 425,
                            ifelse(INGRESO_REAL <= 1000, 1000,
                                   ifelse(INGRESO_REAL <= 2500, 2500,
                                          ifelse(INGRESO_REAL <= 5000, 5000, 10000))))]
data[, R_CUOTA := ifelse(MaxCuota_12M <= 107, 107,
                         ifelse(MaxCuota_12M <= 435, 435, 10000))]
data[,table(R_INGRESOS, R_CUOTA)]

data[, GRUPO_CUOTA_HIS := ifelse(MaxCuota_12M <= 107 & INGRESO_REAL > 425 & INGRESO_REAL <= 2500, "G1",
                             ifelse(MaxCuota_12M > 107 & MaxCuota_12M <= 435 & INGRESO_REAL > 425 & INGRESO_REAL <= 5000, "G2",
                                    ifelse(MaxCuota_12M > 435 & INGRESO_REAL > 425, "G3", "EXCLUIR")))]
data[TIENE_CUOTA_HIS == "SI"][,list(Sujetos=.N, Media = mean(INGRESO_REAL), Mediana = median(INGRESO_REAL), Media = mean(ingreso136), Mediana = median(ingreso136)), by=GRUPO_CUOTA_HIS][order(GRUPO_CUOTA_HIS)]
d1 <- data[TIENE_CUOTA_HIS == "SI" & GRUPO_CUOTA_HIS %in% c("G1", "G2", "G3")][,.(INGRESO_REAL, ingreso136, GRUPO_CUOTA_HIS)]
d1 <- d1[INGRESO_REAL <= 7500]
ggplot(d1, aes(x=INGRESO_REAL, fill=GRUPO_CUOTA_HIS)) + geom_density(alpha=.6, adjust=3) + xlab("Ingreso") + ylab("Densidad") + ggtitle("Ingreso Real vs Grupo")
ggplot(d1, aes(x=ingreso136, fill=GRUPO_CUOTA_HIS)) + geom_density(alpha=.6, adjust=3) + xlab("Ingreso") + ylab("Densidad") + ggtitle("Ingreso Estimado vs Grupo")


# Distribución por entidad
info <- info[TIENE_CUOTA == "SI"]
info[M_CUOTA_INGRESO == "NO" & M_CUPO_INGRESO == "NO"][, 
      list(Casos = .N, Minimo = min(INGRESO_REAL), Q1 = quantile(INGRESO_REAL, probs = 0.25), Q2 = quantile(INGRESO_REAL, probs = 0.5), Q3 = quantile(INGRESO_REAL, probs = 0.75), Maximo = max(INGRESO_REAL),
           MinimoE = min(ingreso136), Q1E = quantile(ingreso136, probs = 0.25), Q2E = quantile(ingreso136, probs = 0.5), Q3E = quantile(ingreso136, probs = 0.75), MaximoE = max(ingreso136)), by = ENTIDAD]

quantile(info[TIENE_CUPO == "SI"]$maxCupoTC089, probs = c(0.25, 0.75))
quantile(info[TIENE_CUPO == "NO"]$cuota052, probs = c(0.25, 0.75))

# Rangos
info[, R_INGRESOS := ifelse(INGRESO_REAL <= 425, 425,
                             ifelse(INGRESO_REAL <= 1000, 1000,
                                    ifelse(INGRESO_REAL <= 2500, 2500,
                                           ifelse(INGRESO_REAL <= 5000, 5000, 10000))))]
info[, R_CUOTA := ifelse(cuota052 <= 107, 107,
                          ifelse(cuota052 <= 435, 435, 10000))]
info[, R_CUPO  := ifelse(maxCupoTC089 <= 1000, 1000,
                          ifelse(maxCupoTC089 <= 4000, 4000, 10000))]

info[TIENE_CUPO == "NO"][,table(R_INGRESOS, R_CUOTA)]
info[TIENE_CUPO == "SI"][,table(R_INGRESOS, R_CUPO)]


# Sujetos que no tienen TC
info[, GRUPO_CUOTA := ifelse(cuota052 <= 107 & INGRESO_REAL > 425 & INGRESO_REAL <= 2500, "G1",
                             ifelse(cuota052 > 107 & cuota052 <= 435 & INGRESO_REAL > 425 & INGRESO_REAL <= 5000, "G2",
                                    ifelse(cuota052 > 435 & INGRESO_REAL > 425, "G3", "EXCLUIR")))]
info[TIENE_CUPO == "NO"][,list(Sujetos=.N, Media = mean(INGRESO_REAL), Mediana = median(INGRESO_REAL), Media = mean(ingreso136), Mediana = median(ingreso136)), by=GRUPO_CUOTA][order(GRUPO_CUOTA)]

d1 <- info[TIENE_CUPO == "NO" & GRUPO_CUOTA %in% c("G1", "G2", "G3")][,.(INGRESO_REAL, ingreso136, GRUPO_CUOTA)]
d1 <- d1[INGRESO_REAL <= 7500]
ggplot(d1, aes(x=INGRESO_REAL, fill=GRUPO_CUOTA)) + geom_density(alpha=.6, adjust=3) + xlab("Ingreso") + ylab("Densidad") + ggtitle("Ingreso Real vs Grupo")
ggplot(d1, aes(x=ingreso136, fill=GRUPO_CUOTA)) + geom_density(alpha=.6, adjust=3) + xlab("Ingreso") + ylab("Densidad") + ggtitle("Ingreso Estimado vs Grupo")

# Sujetos que tienen TC
info[, GRUPO_CUPO := ifelse(maxCupoTC089 <= 1000 & INGRESO_REAL > 425 & INGRESO_REAL <= 2500, "G1",
                             ifelse(maxCupoTC089 > 1000 & maxCupoTC089 <= 4000 & INGRESO_REAL > 425 & INGRESO_REAL <= 5000, "G2",
                                    ifelse(maxCupoTC089 > 4000 & INGRESO_REAL > 425, "G3", "EXCLUIR")))]
info[TIENE_CUPO == "SI"][,list(Sujetos=.N, Media = mean(INGRESO_REAL), Mediana = median(INGRESO_REAL), Media = mean(ingreso136), Mediana = median(ingreso136)), by=GRUPO_CUPO][order(GRUPO_CUPO)]
d1 <- info[TIENE_CUPO == "SI" & GRUPO_CUPO %in% c("G1", "G2", "G3")][,.(INGRESO_REAL, ingreso136, GRUPO_CUPO)]
d1 <- d1[INGRESO_REAL <= 7500]
ggplot(d1, aes(x=INGRESO_REAL, fill=GRUPO_CUPO)) + geom_density(alpha=.6, adjust=3) + xlab("Ingreso") + ylab("Densidad") + ggtitle("Ingreso Real vs Grupo")
ggplot(d1, aes(x=ingreso136, fill=GRUPO_CUPO)) + geom_density(alpha=.6, adjust=3) + xlab("Ingreso") + ylab("Densidad") + ggtitle("Ingreso Estimado vs Grupo")



setwd(dir.b)
save(list = c("info", "data"), file = "DatosModelamiento.RData", envir = .GlobalEnv)


 