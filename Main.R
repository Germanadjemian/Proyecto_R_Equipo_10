#Cargamos las librerías a usar durante el proyecto (algunas quizas no las usamos pero por ahora ante la duda las cargamos, en todo caso luego las sacamos)
library(tidyverse)
library(readxl)
library(writexl)
library(haven) # importar datos de paquetes estadísticos (Stata, SPSS, SAS)
library(janitor) #Para limpiar


#COMIENZA
#Este es el dataframe original, contiene datos del CPI de Japón desde 1970 hasta 2022
CPI_Japon <- read_csv("2022_Japan_CPI_GoodsAndServiceClassificationIndex.csv") 

#Primero vamos a ver el resumen, porque tiene 53 filas (observaciones) con 79 datos cada una
str(CPI_Japon)#Luego de ejecutar esto, lo primero que podemos observar es que todos los datos del datasett son de tipo double
summary(CPI_Japon)


#Agarro todos los demás elementos por separado y limpio los nombres de las columnas
all_individual_items <- CPI_Japon|>
  select(-"All items",-"All items, less fresh food", 
         -"All items, less imputed rent", -"All items, less imputed rent & fresh food", 
         -"All items, less fresh food and energy", 
         -"All items, less food (less alcoholic beverages) and energy") |> 
  clean_names() 


#Saco la columna que tiene NA's
all_individual_items <- all_individual_items |> 
                      select(-"water_sewerage_charges")

#Filtro los valores para el año de inicio
year_1970_individual_items <- all_individual_items |> 
                              filter(year == 1970)

#Filtro los valores para el año de fin
year_2020_individual_items <- all_individual_items |> 
                              filter(year == 2020)

#Calculo la variación de los productos en esos años
variation_50_years = year_2020_individual_items |> select(-"year") - year_1970_individual_items |> select(-"year")

#Obtengo los valores y 
values <- as.numeric(variation_50_years[1, ])
names <- names(variation_50_years)

# Ordenar de mayor a menor y obtener los nombres de las 5 columnas con mayor valor
top5_names <- names[order(values, decreasing = TRUE)[1:5]]

# Extraer esas columnas
variation_top5 <- variation_50_years[, top5_names, drop = FALSE]

#Agarro el CPI de todos los productos juntos según el datasett
all_items <- CPI_Japon|>
                      select("year","all_items")
all_items


all_individual_items

all_individual_items_clean <- na.omit(all_individual_items)
                                         
#Chequeamos que haya concordancia entre el campo all_items y el promedio de all_individual_items
all_individual_items_prom <-rowSums(all_individual_items)/length(all_individual_items)
all_individual_items_prom
all_individual_items_prom_cleaned <- rowSums(all_individual_items_clean)/length(all_individual_items_clean)
all_individual_items_prom_cleaned


                                         