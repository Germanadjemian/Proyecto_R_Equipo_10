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

# Quitamos columna con NA's (si existe)
if ("water_sewerage_charges" %in% names(all_individual_items)) {
  all_individual_items <- all_individual_items |> select(-water_sewerage_charges)
}

# Filtramos años extremos
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
all_individual_items_prom <- rowMeans(all_individual_items |> select(-year))
all_individual_items_prom_cleaned <- rowMeans(all_individual_items_clean |> select(-year))

# Resultados
all_items
head(all_individual_items_prom)
head(all_individual_items_prom_cleaned)