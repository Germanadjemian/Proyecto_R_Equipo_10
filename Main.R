# Cargamos librerías
library(tidyverse)
library(readxl)
library(writexl)
library(haven)    # importar datos de paquetes estadísticos (Stata, SPSS, SAS)
library(janitor)  # limpiar nombres

# COMIENZA
CPI_Japon <- read_csv("2022_Japan_CPI_GoodsAndServiceClassificationIndex.csv") |> 
  clean_names()  # limpiamos de entrada

# Exploración inicial
str(CPI_Japon)
summary(CPI_Japon)

# Filtramos solo los ítems individuales (quitamos los agregados)
all_individual_items <- CPI_Japon |>
  select(-all_items,
         -all_items_less_fresh_food,
         -all_items_less_imputed_rent,
         -all_items_less_imputed_rent_fresh_food,
         -all_items_less_fresh_food_and_energy,
         -all_items_less_food_less_alcoholic_beverages_and_energy)

# Tomamos los años de princpio y fin de la muestra para ver la variación
year_1970_individual_items <- all_individual_items |> filter(year == 1970)
year_2020_individual_items <- all_individual_items |> filter(year == 2020)

# Calculamos variación en 50 años
variation_50_years <- year_2020_individual_items |> select(-year) - 
  year_1970_individual_items |> select(-year)

# Top 5 variaciones
values <- as.numeric(variation_50_years[1, ])
names <- names(variation_50_years)
top5_names <- names[order(values, decreasing = TRUE)[1:5]]
variation_top5 <- variation_50_years[, top5_names]

# CPI general (all items)
all_items <- CPI_Japon |> select(year, all_items)

# Eliminamos filas con NA's
all_individual_items_clean <- na.omit(all_individual_items)

# Comprobación de concordancia con promedio de ítems individuales
all_individual_items_prom <- rowMeans(all_individual_items |> select(-year), na.rm = TRUE) #Ahora esto hace que descuente los NA y divide entre n columnas menos, equivalente al num de NA que haya en la observación
all_individual_items_prom_only_with_complete_rows <- rowMeans(all_individual_items_clean |> select(-year))

#Agrego el promedio casero a estos dataframes
all_individual_items <- all_individual_items |> mutate (prom = rowMeans(select(all_individual_items, -year), na.rm = TRUE)) #El punto es para referirse al datraframe actual, osea all_individual_items

# Resultados
view(all_items)
view(all_individual_items)
all_individual_items_prom
all_individual_items_prom_only_with_complete_rows


################################### FORMALIDADES ################################### 

###### Parte 2 Análisis del dataset (tomamos el datasett original) #####
print("PARTE 2 \n")
#Cantidad de filas y columnas
print(paste("número de filas:", nrow(CPI_Japon), "|","número de columnas:", ncol(CPI_Japon)))

#Explicar que representa cada fila esta en el informe

#Tipos de las varaibles
str(CPI_Japon) #Podemos ver que absolutamente todas son col_double()

#Analizar si hay  en términos de todas las variables
#Para saber esto comparamos el número de filas del datasett original, con el número de filas de un datasett con los duplicados limpiados
nrow(CPI_Japon) == nrow(distinct(CPI_Japon)) #Devuelve TRUE, por lógica no hay duplicados
distinct(CPI_Japon)

#Analizar si hay missings NA (spoiler si los hay, al menos hasta la columna 36)
is.na(CPI_Japon) #Acá nos podemos fijar uno a uno en cada fila (si hay algún true indica un NA)
sum(is.na(CPI_Japon))#Acá sumamos todos los NA, si da al menos uno es que al menos uno hay, pero esto nos indica la cantidad total de NA en el datasett.
which(is.na(CPI_Japon), arr.ind = TRUE) #Con esto podemos saber las posiciones exactas de los NA (el arr.ind nos devuelve la posición del NA como si el df fuera una matriz)
CPI_Japon[1,35] #Ejemplo de como comprobar que las posiciones que nos dio la función anterior son NA efectivamente
# Podemos observar que aparecen muchos NA en la columna 35 con las primeras observaciones
#También podemos observar que aparecen muchos NA en las ultimas columnas, en especial en la 79, hasta la 35a observación
#Todo esto pueden ser indicadores o revelar sesgos o pistas de posibles acontecimientos, por ejemplo que hace tiempo, no se teníam en cuenta estos daatos, quizás porque no se podían medir o no se consideraban importantes.


##### Parte 3 Análisis exploratorio de datos (EDA) #####