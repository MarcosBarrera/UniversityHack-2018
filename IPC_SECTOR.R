#Cargamos las librerias

library(xlsx)
library(dplyr)
library(zoo)
library(corrplot)
library(ggplot2)
library(tidyr)

#Cargamos los datos oficiales de IPC junto con la base de datos que ha sido proporcionada

IPC = read.xlsx("./Data/IPC.xlsx", sheetIndex = 1)

df = read.csv("./Data/Cajamar.csv",sep =";")

df$DIA <- as.Date(df$DIA)
#Creamos las variables Year y Mes.
df <- df %>% mutate(YEAR= format(DIA,'%Y'),MES = months(DIA))

#Agrupamos por mes, año y sector
TABLA_MES_YEAR_SECTOR = df %>% group_by(MES,YEAR,SECTOR) %>% 
  summarise(IMPORTE=sum(IMPORTE)) %>% 
  mutate(MES_YEAR = paste(YEAR,MES,sep="-")) %>% 
  ungroup() %>% 
  select(MES_YEAR,SECTOR,IMPORTE)

#Añadimos las columnas año y mes a la tabla del IPC
IPC = IPC %>% mutate(MES= months(Mes),YEAR= format(Mes,"%Y")) %>% 
  mutate(MES_YEAR = paste(YEAR,MES,sep="-")) %>% 
  na.omit() %>% 
  select(MES_YEAR,VariaciÃ³n)

#Modificamos el nombre de las columnas
colnames(IPC) = c("MES_YEAR","VARIACION")

#Hacemos una unión de las tablas por la columna mes_year
TABLA_MES_YEAR_SECTOR_IPC = inner_join(TABLA_MES_YEAR_SECTOR,IPC,by="MES_YEAR")
TABLA_MES_YEAR_SECTOR_IPC = TABLA_MES_YEAR_SECTOR_IPC %>%  ungroup()

ggplot(TABLA_MES_YEAR_SECTOR_IPC, aes(x=IMPORTE,y=VARIACION)) + 
  geom_point() + 
  stat_smooth() +  
  facet_wrap(~SECTOR) + 
  ggtitle("CORRELACION ENTRE IPC Y CONSUMO SECTOR")


#Hacemos un spread por sector, de esta manera cada sector tiene una columna
TABLA_SECTOR_IPC = TABLA_MES_YEAR_SECTOR_IPC %>% spread(SECTOR,IMPORTE)

#Creamos la matriz de correlacion
cor.mat = cor(TABLA_SECTOR_IPC[,-1])

#Representamos las correlaciones en un correlograma
corrplot(cor.mat,hclust.method="median",order="hclust",type="upper",addrect = 4,outline = T)

#Descargamos los datos en un nuevo archivo .csv
write.csv2(TABLA_SECTOR_IPC,"TABLA_SECTOR_IPC_MENSUAL.csv",row.names = FALSE)

















