

# LIBRERÍAS:

library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(xlsx)
library(knitr)
library(tidyr)
library(NbClust)
library(factoextra)
library(cluster)


# Importamos el dataset
cajamar_data_set = read.csv("./Cajamar.csv",header = TRUE,sep=";")

# Vemos la estructura de los datos
str(cajamar_data_set)

# Cambiamos el formato de Dia a Date
cajamar_data_set$DIA = as.Date(cajamar_data_set$DIA)

# Convertimos en un datable
cajamar_mod <- as.data.table(cajamar_data_set)


## DISTRIBUCION RENTAS

#Creamos la tabla renta alta como el numero de transacciones que tienen una probabilidad de renta alta mayor o igual que un 70%

TABLA_RENTA_ALTA <- cajamar_mod %>% 
  count(PROP_R_ALTA >= 0.70) %>% 
  mutate(frecuencia_relativa = round(n/sum(n), 2)) %>% 
  mutate(TIPO = "RENTA ALTA")

colnames(TABLA_RENTA_ALTA) <- c("COND", "NUM_OP", "PORCENTAJE_TOTAL", "TIPO")

#Creamos la tabla renta media como el numero de transacciones que tienen una probabilidad de renta media mayor o igual que un 70%

TABLA_RENTA_MEDIA <- cajamar_mod %>% 
  count(PROP_R_MEDIA >= 0.70) %>% 
  mutate(frecuencia_relativa = round(n/sum(n), 2)) %>% 
  mutate(TIPO = "RENTA MEDIA")

colnames(TABLA_RENTA_MEDIA) <- c("COND", "NUM_OP", "PORCENTAJE_TOTAL", "TIPO")

#Creamos la tabla renta baja como el numero de transacciones que tienen una probabilidad de renta baja mayor o igual que un 70%

TABLA_RENTA_BAJA <- cajamar_mod %>% 
  count(PROP_R_BAJA >= 0.70) %>% 
  mutate(frecuencia_relativa = round(n/sum(n), 2)) %>% 
  mutate(TIPO = "RENTA BAJA")

colnames(TABLA_RENTA_BAJA) <- c("COND", "NUM_OP", "PORCENTAJE_TOTAL", "TIPO")

#Creamos la tabla renta media joven como el numero de transacciones que tienen una probabilidad de renta media y baja mayor o igual a un 40% o de mas de un 30 y 50% en alguna de las dos categorías.

TABLA_RENTA_MEDIA_BAJA <- cajamar_mod %>% 
  count((PROP_R_MEDIA >= 0.40 & PROP_R_BAJA >= 0.40)|(PROP_R_MEDIA >= 0.30 & PROP_R_BAJA >= 0.50) | (PROP_R_MEDIA >= 0.50 & PROP_R_BAJA >= 0.30)) %>% 
  mutate(frecuencia_relativa = round(n/sum(n), 2)) %>% 
  mutate(TIPO ="RENTA MEDIA-BAJA")

colnames(TABLA_RENTA_MEDIA_BAJA) <- c("COND", "NUM_OP", "PORCENTAJE_TOTAL", "TIPO")

#Creamos la tabla rentas como la unión de las 4 tablas anteriores

TABLA_RENTAS <- rbind(TABLA_RENTA_ALTA, TABLA_RENTA_MEDIA,
                      TABLA_RENTA_BAJA, TABLA_RENTA_MEDIA_BAJA)

#Representamos los porcentajes de operaciones de las distintas categorías de renta

kable(TABLA_RENTAS %>% filter(COND==TRUE) %>% 
        select(TIPO, PORCENTAJE_TOTAL) %>% 
        rbind(c("TOTAL_CATEGORIAS", sum(.$PORCENTAJE_TOTAL))))

#Creamos el campo Categoria Renta con los criterios mencionados anteriormente (ver PDF):
cajamar_mod <-
  cajamar_mod %>% 
  mutate(CATEGORIA_RENTA = ifelse(PROP_R_ALTA > 0.70, "Renta_Alta", 
                                  ifelse(PROP_R_MEDIA > 0.70, "Renta_media",
                                         ifelse(PROP_R_BAJA > 0.70, "Renta_baja",
                                                ifelse(((PROP_R_MEDIA > 0.40 & PROP_R_BAJA > 0.40)|
                                                          (PROP_R_MEDIA >= 0.30 & PROP_R_BAJA >= 0.50)|
                                                          (PROP_R_MEDIA >= 0.50 & PROP_R_BAJA >= 0.30)),
                                                       "Renta_media_baja",
                                                       "Resto")))))

cajamar_mod %>%  
  group_by(CATEGORIA_RENTA, SECTOR) %>% 
  summarise(IMPORTES = sum(IMPORTE)) %>% 
  ggplot(aes(x=CATEGORIA_RENTA, y=IMPORTES)) + 
  geom_bar(stat="identity") + facet_wrap(~SECTOR) +
  ggtitle("DISTRIBUCION CONSUMO POR SECTOR Y CATEGORIA RENTA")



## DISTRIBUCION EDAD

#Creamos la tabla joven como el numero de transacciones que tienen una probabilidad de joven superior a un 70%

TABLA_JOVEN <- cajamar_mod %>% 
  count(PROP_JOVEN > 0.70) %>% 
  mutate(frecuencia_relativa = round(n/sum(n), 2)) %>% 
  mutate(TIPO = "JOVEN")

colnames(TABLA_JOVEN) <- c("COND", "NUM_OP", "PORCENTAJE_TOTAL", "TIPO")

#Creamos la tabla adulto como el numero de transacciones que tienen una probabilidad de adulto superior a un 70%
TABLA_ADULTO <- cajamar_mod %>% 
  count(PROP_ADULTO>0.70) %>% 
  mutate(frecuencia_relativa = round(n/sum(n), 2)) %>% 
  mutate(TIPO = "ADULTO")

colnames(TABLA_ADULTO) <- c("COND","NUM_OP","PORCENTAJE_TOTAL","TIPO")

#Creamos la tabla pensionista como el numero de transacciones que tienen una probabilidad de pensionista superior a un 70%
TABLA_PENSIONISTA <- cajamar_mod %>% 
  count(PROP_PENSIONISTA>0.70) %>% 
  mutate(frecuencia_relativa = round(n/sum(n),2)) %>% 
  mutate(TIPO = "PENSIONISTA")

colnames(TABLA_PENSIONISTA) <- c("COND", "NUM_OP", "PORCENTAJE_TOTAL", "TIPO")

#Creamos la tabla edad como la unión de las 3 tablas anteriores
TABLA_EDAD <- rbind(TABLA_JOVEN,TABLA_ADULTO,TABLA_PENSIONISTA)

#Representamos los porcentajes de operaciones de las distintas categorías de edad
kable(TABLA_EDAD %>% filter(COND==TRUE) %>% 
        select(TIPO,PORCENTAJE_TOTAL) %>% 
        rbind(c("TOTAL_CATEGORIAS",sum(.$PORCENTAJE_TOTAL))))

#Creamos el campo Categoria edad con los criterios mencionado anteriormente (ver PDF).
cajamar_mod <-
  cajamar_mod %>% 
  mutate(CATEGORIA_EDAD = ifelse(PROP_JOVEN >= 0.70,"Joven",
                                 ifelse(PROP_ADULTO > 0.70, "Adulto",
                                        ifelse(PROP_PENSIONISTA >0.70,"Pensionista","Resto"))))

cajamar_mod %>%  
  group_by(CATEGORIA_EDAD,SECTOR) %>% 
  summarise(IMPORTES = sum(IMPORTE)) %>% 
  ggplot(aes(x=CATEGORIA_EDAD,y=IMPORTES)) + 
  geom_bar(stat="identity") + 
  facet_wrap(~SECTOR) +
  ggtitle("DISTRIBUCION CONSUMO POR SECTOR Y CATEGORIA EDAD")


##DISTRIBUCION SEXO

#Creamos la tabla mujer como el numero de transacciones que tienen una probabilidad de sexo de entre un 0 y 30 por ciento.

TABLA_MUJER <- cajamar_mod %>% 
  count(cajamar_mod$PROP_SEX >= 0 & cajamar_mod$PROP_SEX < 0.30) %>% 
  mutate(frecuencia_relativa = round(n/sum(n), 2)) %>% 
  mutate(TIPO = "MUJER")

colnames(TABLA_MUJER) <- c("COND", "NUM_OP", "PORCENTAJE_TOTAL", "TIPO")

#Creamos la tabla Hombre como el numero de transacciones que tienen una probabilidad de sexo superior a un 70 por ciento.

TABLA_HOMBRE <- cajamar_mod %>% 
  count(cajamar_mod$PROP_SEX >= 0.70) %>% 
  mutate(frecuencia_relativa = round(n/sum(n),2)) %>% 
  mutate(TIPO = "HOMBRE")

colnames(TABLA_HOMBRE) <- c("COND", "NUM_OP", "PORCENTAJE_TOTAL", "TIPO")

#Creamos la tabla Mixto como el numero de transacciones que tienen una probabilidad de sexo de entre un 30 y 70 por ciento.

TABLA_MIXTO <- cajamar_mod %>% 
  count(cajamar_mod$PROP_SEX >= 0.3 & cajamar_mod$PROP_SEX < 0.70) %>% 
  mutate(frecuencia_relativa = round(n/sum(n), 2)) %>% 
  mutate(TIPO = "MIXTO")
colnames(TABLA_MIXTO) <- c("COND", "NUM_OP", "PORCENTAJE_TOTAL", "TIPO")

# Creamos la tabla Sexo como la unión de las 3 tablas anteriores

TABLA_SEXO <- rbind(TABLA_MUJER,TABLA_MIXTO,TABLA_HOMBRE)


kable(TABLA_SEXO %>% filter(COND==TRUE) %>% 
        select(TIPO,PORCENTAJE_TOTAL) %>% 
        rbind(c("TOTAL_CATEGORIAS",sum(.$PORCENTAJE_TOTAL))))

# Creamos la variable Categoria Sexo

cajamar_mod <- cajamar_mod %>% 
  mutate(CATEGORIA_SEXO= ifelse(cajamar_mod$PROP_SEX >= 0 & cajamar_mod$PROP_SEX < 0.30,"Mujeres",ifelse(cajamar_mod$PROP_SEX >= 0.3 & cajamar_mod$PROP_SEX < 0.70,"Mixto","Hombres")))

cajamar_mod %>%  
  group_by(CATEGORIA_SEXO,SECTOR) %>% 
  summarise(IMPORTES = sum(IMPORTE)) %>% 
  ggplot(aes(x=CATEGORIA_SEXO,y=IMPORTES)) + geom_bar(stat="identity") + facet_wrap(~SECTOR) +
  ggtitle("DISTRIBUCION CONSUMO POR SECTOR Y CATEGORIA SEXO")



## ANALISIS CLIMA

# Importamos el Dataset con los datos de clima diarios

Clima <- read.xlsx("./Clima diario.xlsx", header = TRUE, sheetIndex=1)

# Convertimos la fecha a formato fecha
Clima$fecha <- as.Date(Clima$fecha)

# Cambiamos el nombre de las variables
colnames(Clima) <- c("DIA","PREC","SOL","TMAX","TMED","TMIN")

# Creamos una tabla con el consumo por Dia
RESUMEN_TIEMPO <- cajamar_mod %>% 
  group_by(DIA) %>% 
  summarise(IMPORTE = sum(IMPORTE))

# Creamos una tabla con el consumo por Dia y Sector
RESUMEN_TIEMPO_SECTOR <- cajamar_mod %>% 
  group_by(DIA, SECTOR) %>% 
  summarise(IMPORTE = sum(IMPORTE))

# Hacemos un inner join de las tablas Resumen tiempo con la tabla Clima por la variable DIA
RESUMEN_TIEMPO <- inner_join(RESUMEN_TIEMPO, Clima, by = "DIA")
RESUMEN_TIEMPO_SECTOR <- inner_join(RESUMEN_TIEMPO_SECTOR, Clima, by = "DIA")

#Importamos los dias festivos
FESTIVOS <- read.xlsx("./Datasets/Calendario_Festivos.xlsx",header = TRUE,sheetIndex=1)

#Cambiamos el nombre de la columna a DIA para posteriormente hacer el inner join con una tabla en el que la variable tiene el mismo nombre
colnames(FESTIVOS) <- c("DIA")

#Quitamos los NA
FESTIVOS <- na.omit(FESTIVOS)

#Convertimos el dia a formato fecha
FESTIVOS$DIA <- as.Date(FESTIVOS$DIA,format="%Y-%m-%d")

#Convertios la tabla en un tibble
FESTIVOS <- as_tibble(FESTIVOS)

#Creamos la columan Festivo con Valor si en todos los casos
FESTIVOS["FESTIVO"] <- "SI"

#Convertimos el tipo de esta variable a factor
FESTIVOS$FESTIVO <- as.factor(FESTIVOS$FESTIVO)

#Hacemos un inner join de las tablas Resumen tiempo con la tabla FESTIVOS
RESUMEN_TIEMPO <- RESUMEN_TIEMPO %>% full_join(FESTIVOS,by="DIA")
RESUMEN_TIEMPO_SECTOR <- RESUMEN_TIEMPO_SECTOR %>%  full_join(FESTIVOS,by="DIA")

#Creamos la variable lluvia cuando los valores de lluvia son superiores a 0.20 mm

RESUMEN_TIEMPO <- RESUMEN_TIEMPO %>% 
  mutate(LLUVIA = ifelse(PREC > 0.2, "Si", "No"))
RESUMEN_TIEMPO <- RESUMEN_TIEMPO %>% 
  filter(IMPORTE != "NA")
RESUMEN_TIEMPO_SECTOR <- RESUMEN_TIEMPO_SECTOR %>% 
  mutate(LLUVIA = ifelse(PREC > 0.2, "Si", "No"))

ggplot(RESUMEN_TIEMPO) + 
  geom_point(aes(x=TMED,y=IMPORTE)) + 
  geom_smooth(method='lm', formula = y ~ x, aes(x=TMED,y=IMPORTE)) + 
  ggtitle("CORRELACION CONSUMO DIARIO Y TEMPERATURA MEDIA")
paste("La correlación de Temperatura media y el consumo diario es de un ", 
      round(cor(RESUMEN_TIEMPO$IMPORTE, RESUMEN_TIEMPO$TMED), 2))

ggplot(RESUMEN_TIEMPO) + 
  geom_point(aes(x = TMAX, y = IMPORTE)) + 
  geom_smooth(method = 'lm', formula = y ~ x, aes(x = TMAX, y = IMPORTE)) + 
  ggtitle("CORRELACION CONSUMO DIARIO Y TEMPERATURA MAXIMA")
paste("La correlación de Temperatura máxima y el consumo diario es de un ",
      round(cor(RESUMEN_TIEMPO$IMPORTE, RESUMEN_TIEMPO$TMAX), 2))

ggplot(RESUMEN_TIEMPO) + 
  geom_point(aes(x = SOL, y = IMPORTE)) + 
  geom_smooth(method='lm', formula = y ~ x, aes(x = SOL, y = IMPORTE)) +
  ggtitle("CORRELACION CONSUMO DIARIO Y EXPOSICION SOLAR")
paste("La correlación de la exposición solar y el consumo diario es de un ",
      round(cor(RESUMEN_TIEMPO$IMPORTE,RESUMEN_TIEMPO$SOL),2))


#Creamos la variable mes y dia de la semana en las tablas Resumen Tiempo y Resumen tiempo Sector.

RESUMEN_TIEMPO <- RESUMEN_TIEMPO %>% 
  mutate(MES= months(DIA),DIA_SEMANA = weekdays(DIA))
RESUMEN_TIEMPO_SECTOR <- RESUMEN_TIEMPO_SECTOR %>% 
  mutate(MES= months(DIA),DIA_SEMANA = weekdays(DIA))

ggplot(RESUMEN_TIEMPO) + 
  geom_boxplot(aes(x = LLUVIA , y = IMPORTE)) +
  facet_wrap(~ MES) + 
  ggtitle("EFECTO LLUVIA EN EL CONSUMO POR MES")

# No apreciamos ninguna relación significativa.


ggplot(RESUMEN_TIEMPO_SECTOR) + 
  geom_boxplot(aes(x=LLUVIA,y=IMPORTE)) + 
  facet_wrap(~SECTOR) + 
  ggtitle("EFECTO LLUVIA EN EL CONSUMO POR SECTOR")

# No apreciamos ninguna relación significativa


ggplot(RESUMEN_TIEMPO_SECTOR) + 
  geom_point(aes(x = TMAX , y = IMPORTE)) + 
  geom_smooth(method = 'lm', formula = y ~ x, aes(x = TMAX, y = IMPORTE)) + 
  facet_wrap(~ SECTOR) + 
  ggtitle("CORRELACION ENTRE TEMPERATURA MÁXIMA Y SECTOR")

# Como se puede apreciar los sectores que se ven ligeramente afectados por la exposición la temperatura máxima 
# son la alimentación, Hipermercados y moda y complementos. Con una relación inversa, a mayor temperatura máxima 
# menos consumo en dichos sectores.


ggplot(RESUMEN_TIEMPO_SECTOR) + 
  geom_point(aes(x = SOL, y = IMPORTE)) + 
  geom_smooth(method = 'lm', formula = y ~ x, aes(x = SOL, y = IMPORTE)) + 
  facet_wrap(~ SECTOR) + 
  ggtitle("CORRELACION ENTRE EXPOSICION SOLAR Y SECTOR")

# Como se puede apreciar los sectores que se ven ligeramente afectados por la exposición solar son la alimentación, 
# Hipermercados y moda y complementos. Con una relación inversa, a mayor temperatura máxima menos consumo en dichos sectores.



## ANALISIS CLUSTER

### TRATAMIENTO PREVIO

# Creamos las variables year y mes
cajamar_mod <- cajamar_mod %>% 
  mutate(MES = months(DIA), YEAR = format(DIA, "%Y"), DIA_LAB = weekdays(DIA))

# Convertimos a factor las variables Categoria Sexo, Categoria Renta, Categoria Edad, Año, Mes, Código Postal Cliente y Código Postal del Comercio
cajamar_mod$CATEGORIA_SEXO <- as.factor(cajamar_mod$CATEGORIA_SEXO)
cajamar_mod$CATEGORIA_EDAD <- as.factor(cajamar_mod$CATEGORIA_EDAD)
cajamar_mod$CATEGORIA_RENTA <- as.factor(cajamar_mod$CATEGORIA_RENTA)
cajamar_mod$YEAR <- as.factor(cajamar_mod$YEAR)
cajamar_mod$MES <- as.factor(cajamar_mod$MES)
cajamar_mod$CP_CLIENTE <- as.factor(cajamar_mod$CP_CLIENTE)
cajamar_mod$CP_COMERCIO <- as.factor(cajamar_mod$CP_COMERCIO)
cajamar_mod$DIA_LAB <- as.factor(cajamar_mod$DIA_LAB)

# Creamos un Dataframe con los variables que potencialmente utilizaremos para el análisis clúster.

df_cluster <- cajamar_mod %>% 
  select(CP_COMERCIO, CP_CLIENTE, SECTOR, IMPORTE, NUM_OP, CATEGORIA_EDAD, CATEGORIA_RENTA,
         CATEGORIA_SEXO, YEAR, MES, DIA_LAB, FRANJA_HORARIA, DIA)

# Creamos la variable Festivo. Los domingos tomará valor 1 y el resto de días cero.
df_cluster <- mutate(df_cluster, FESTIVO = ifelse(df_cluster$DIA_LAB == "Sunday", 1, 0))

# Si el dia se encuentra dentro de la base de datos de festivos ponemos 1 y sino cero
df_cluster$FESTIVO <- ifelse(df_cluster$FESTIVO == 1, 1, ifelse(df_cluster$DIA %in% FESTIVOS$DIA, 1, 0))

# Sustituimos los ceros y uno por los factores no y si respectivamente
df_cluster$FESTIVO <- factor(df_cluster$FESTIVO,levels = c(0,1),labels= c("No","Si"))

# Representación grafica del consumo sectorial, distinguiendo entre festivos y no festivos. 
# Como se puede apreciar en los meses de enero, marzo y abril hay valores extremos o outliers en los días festivos que coinciden con la navidad y semana santa.


TIPO_DIA <- df_cluster %>%   distinct(DIA,FESTIVO)

df_cluster %>% 
  group_by(DIA,MES) %>% 
  summarise(IMPORTE = sum(IMPORTE)) %>% 
  inner_join(TIPO_DIA,by= c("DIA")) %>% 
  ggplot(aes(x=FESTIVO,y=IMPORTE)) + geom_boxplot() + facet_wrap(~MES) +
  ggtitle("DISTRIBUCION CONSUMO POR MES Y DIA (FESTIVO/NO FESTIVO)")

# Creamos la variable Origen. Si el código postal del cliente se encuentra dentro de los códigos postales de cliente ponemos ciudad y sino provincia.
df_cluster <- df_cluster %>% 
  mutate(ORIGEN = ifelse(CP_CLIENTE %in% CP_COMERCIO, "Ciudad", "Provincia"))

# Almacenamos los Códigos postales de los clientes, Categorías de Edad, Renta y Sexo
CP_CLIENTES <- unique(df_cluster$CP_CLIENTE)
CATEGORIAS_EDAD <- unique(df_cluster$CATEGORIA_EDAD)
CATEGORIAS_RENTA <- unique(df_cluster$CATEGORIA_RENTA)
CATEGORIAS_SEXO <- unique(df_cluster$CATEGORIA_SEXO)


# Hacemos agrupación por campos (ver PDF):
  
# Creamos un Dataframe con los filtros y la agrupación mencionada anteriormente.
# Creamos un campo llamado variable que es la concentación de los nombres de los campos Categoria edad, renta, sexo y sector.

CP_CLIENTE_VARIABLES <- df_cluster %>% 
  group_by(CP_CLIENTE, CATEGORIA_EDAD, CATEGORIA_RENTA, CATEGORIA_SEXO, SECTOR) %>%
  summarise(Importe = sum(IMPORTE)) %>% 
  ungroup() %>% 
  mutate(VARIABLE = paste(CATEGORIA_EDAD, CATEGORIA_RENTA,
                          CATEGORIA_SEXO, SECTOR,sep = "_")) %>% 
  dplyr::select(CP_CLIENTE, VARIABLE, Importe)

# Guardamos las distintas variables de los códigos postales.
VARIABLES_CP_CLIENTE <- unique(CP_CLIENTE_VARIABLES$VARIABLE)

# Para vectorizar las variables por código postal, hacemos un spread de esta manera creamos tantas columnas como valores de la columna VARIABLE hay.
df_cluster_cp_cliente <- CP_CLIENTE_VARIABLES %>% spread(VARIABLE,Importe)

# Cambiamos el nombre de las filas poniendo el nombre de los códigos postales de los clientes
rownames(df_cluster_cp_cliente) <- df_cluster_cp_cliente$CP_CLIENTE

# Quitamos la variable CP CLiente
df_cluster_cp_cliente <- df_cluster_cp_cliente %>% 
  dplyr::select(-CP_CLIENTE)

# Sustituimos los NAs por ceros
df_cluster_cp_cliente[is.na(df_cluster_cp_cliente)] <- 0

# Exploramos los valores de los estadísticos de las variables

q_stats = data.frame(
  Min = apply(df_cluster_cp_cliente, 2, min), #mínimo
  Med = apply(df_cluster_cp_cliente, 2, median), #mediana
  Mean = apply(df_cluster_cp_cliente, 2, mean), #media
  SD = apply(df_cluster_cp_cliente, 2, sd), #Desviación típica
  Max = apply(df_cluster_cp_cliente, 2, max))  #Maximo
q_stats <- round(q_stats,1)
head(q_stats)

# Como se puede apreciar hay bastante dispersión en los datos, 
# una alternativa para reducir la dispersión es realizar una tranformación logarítmica de los datos.

df_cluster_cp_cliente_t <- log1p(df_cluster_cp_cliente)

q_stats_2 = data.frame(
  Min = apply(df_cluster_cp_cliente_t, 2, min), #mínimo
  Med = apply(df_cluster_cp_cliente_t, 2, median), #mediana
  Mean = apply(df_cluster_cp_cliente_t, 2, mean), #media
  SD = apply(df_cluster_cp_cliente_t, 2, sd), #Desviación típica
  Max = apply(df_cluster_cp_cliente_t, 2, max))  #Maximo
q_stats_2 <- round(q_stats_2,1)
head(q_stats_2)

# Con la tranformación logarítmica se ha reducido considerablemente la dispersión en los datos.


###CLUSTER

#### CLUSTER JERÁRQUICO: DENDOGRAMA

#Pasamos a calcular las distancias entre las observaciones, utilizaremos la distancía euclídea.

q.dist = get_dist(df_cluster_cp_cliente_t, stand = TRUE, method = "euclidean")

q.hc = hclust(q.dist, method = "ward.D2")

plot(hclust(q.dist, method = "ward.D2"), cex=0.7, main="Dendrograma", ylab="Anchura",
     xlab = "AC. Método de Ward. Distancia euclídea")
rect.hclust(q.hc, k=2, border = 2:3)

# De acuerdo con lo oservado en el dendograma dividiremos las observaciones en 2 grupos.

grp = cutree(q.hc, k = 3)


#### CLUSTER NO JERÁRQUICO: PAM

pam.q = pam(df_cluster_cp_cliente_t, 2)

fviz_cluster(pam.q, data=df_cluster_cp_cliente_t, labelsize=8)

D <- daisy(df_cluster_cp_cliente_t)

plot(silhouette(pam.q$clustering, D), col=1:2, border=NA)

# El promedio de la silueta presenta un valor razonable. 

resultados_cluster <- data.frame(cbind(row.names(df_cluster_cp_cliente),pam.q$clustering))

colnames(resultados_cluster) <- c("CP_CLIENTE","CLUSTER")

df_cluster <- merge(df_cluster,resultados_cluster,by= "CP_CLIENTE")

write.csv2(df_cluster,"df_cluster.csv",sep="|",row.names = FALSE)
