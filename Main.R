#Cargamos las librerías a usar durante el proyecto (algunas quizas no las usamos pero por ahora ante la duda las cargamos, en todo caso luego las sacamos)
library(tidyverse)
library(readxl)
library(writexl)
library(haven) # importar datos de paquetes estadísticos (Stata, SPSS, SAS)


#COMIENZA
#Este es el dataframe original, contiene datos del CPI de Japón desde 1970 hasta 2022
View(X2022_Japan_CPI_GoodsAndServiceClassificationIndex)
CPI_Japon <- read_csv("2022_Japan_CPI_GoodsAndServiceClassificationIndex.csv") 

#Primero vamos a ver el resumen, porque tiene 53 filas (observaciones) con 79 datos cada una
str(CPI_Japon)#Luego de ejecutar esto, lo primero que podemos observar es que todos los datos del datasett son de tipo double
summary(CPI_Japon)