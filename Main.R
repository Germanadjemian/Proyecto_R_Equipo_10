# Cargamos librerías
library(tidyverse)
library(readxl)
library(writexl)
library(haven)    # importar datos de paquetes estadísticos (Stata, SPSS, SAS)
library(janitor)  # limpiar nombres
library(dplyr)
library(ggplot2)
library(pheatmap)

### Pregunta a GPT (no es el código final):
        # Dado el siguiente contexto:
        # clean_names(CPI_Japon)
        # all_items <- CPI_Japon|>
        #      select("All items")
        # all_items
        # 
        # #Agarro todos los demás elementos por separado
        # all_individual_items <- CPI_Japon|>
        #   clean_name|>
        #   select(-"Year", -"All items","All items, less fresh food", -"All items, less imputed rent", "All items, less imputed rent & fresh food", "All items, less fresh food and energy", "All items, less food (less alcoholic beverages) and energy")
        # ¿Estoy usando mal el cleannames?
        #¿Debería escribir correctamente el nombre de las columnas en función a como quedaron luego de la limpieza?
        #¿si las instancio de esta manera debería ser antes de hacer la limpieza y borrando el clean names en la asignacióndel all_individual_items?

# COMIENZA
CPI_Japon <- read_csv("2022_Japan_CPI_GoodsAndServiceClassificationIndex.csv") |> 
  clean_names()  # limpiamos de entrada


# Parte 2 Análisis del dataset -----------------------------------------------------------------
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



# Parte 3  Análisis exploratorio de datos (EDA)  -----------------------------------------------------------------


# ANTES DE PROSEGUIR CON ESTA PARTE INSTANCIAMOS LAS VARIABLES A USAR Y HACEMOS LIMPIEZA

# CPI general (all items)
all_items <- CPI_Japon |> select(year, all_items)

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

#Deducimos la variación total del CPI en los últimos 50 años
variation_50_years <- year_2020_individual_items |> select(-year) - 
  year_1970_individual_items |> select(-year)


#Agrego el promedio casero a estos dataframes
all_individual_items <- all_individual_items |> mutate (prom = rowMeans(select(all_individual_items, -year), na.rm = TRUE)) 

#vemos que el "promedio casero" no concuerda con el valor de all_items del dataframe original, lo que confirma que este fue hecho con ponderaciones
view(all_items)
view(all_individual_items)


#Hacemos la variacion del CPI de todos los items año a año
all_items_variation <- all_items |> 
  mutate(variation_all_items = all_items - lag(all_items))

all_items_variation <- all_items_variation |> 
  filter(year != 1970)

### Pregunta a GPT: ¿Como usar funcion cut para intervalos en R?

all_items_variation$variation_group <- cut(all_items_variation$variation_all_items,
                                           breaks = c(-Inf, 0, 2, 4, Inf)) #Estas son las franjas que elejimos 
                                            #Las elejimos para posteriormente usarlas a modo de variables categoricas

#Instanciamos el subdataframe que usaremos para la correlación
all_items_and_food <- CPI_Japon |>
  select(
    year,
    all_items,
    food
  )

all_items_and_food <- all_items_and_food |> 
  mutate(variation_all_items = all_items - lag(all_items)) |> 
  mutate(variation_food = food - lag(food))

all_items_and_food <- all_items_and_food |> 
  filter(year != 1970)



## 3.i ---------------------------------------------------------------------

### Calculo la frecuencia absoluta y relativa

all_items_variation |> 
  count(variation_group, name = "frec_abs") |> #Con count contamos las veces que aparece cada variable (categórica)
  mutate(freq_rel=frec_abs/sum(frec_abs)*100) |> #usamos un mutate sobre la respuesta de ese count para calcular la frec rel
  adorn_totals() #Agrega una fila final con los totales 


## 3.ii --------------------------------------------------------------------
#Estadísticos descriptivos de por lo menos una variable: media, mediana, desviación estándar.

summary(CPI_Japon) #Acá están los cálculos con respecto al datasett original
# CPI general (all items)

### Calculo la media y la mediana de la variacion interanual


all_items_variation |> 
  summarise(
    media= mean(variation_all_items),
    mediana= median(variation_all_items),
    desv_est= sd(variation_all_items),
    coef_var= desv_est*100/mean(variation_all_items)
  )


## 3.iii -------------------------------------------------------------------

###Pregunta a GPT: ¿Como poner colores diferentes personalizados a 
###                gráficos de barras en R con ggplot2?

### Gráfico de Barras

colores <- c("lightblue", "mediumaquamarine", "wheat2", "plum") #Vector de colores para emprolijar la gráfica


all_items_variation |> 
  ggplot(aes(x=variation_group))+
  geom_bar(fill = colores)


### Gráfico de dispersión
all_items_and_food |> 
  ggplot(aes(x=variation_food,y=variation_all_items))+
  geom_point(color = "rosybrown")


### Coeficiente de correlación
all_items_and_food |> 
  summarise(cor_all_items_food = cor(variation_food,variation_all_items))

# están muy corelacionadas positivamente


###Gráfico de dispersión vs año
all_items_variation |> 
  ggplot(aes(x=year,y=variation_all_items))+
  geom_point(color = "mediumaquamarine")


## Boxplot

### Sin desagregar
all_items_variation |> 
  ggplot(aes(variation_all_items))+
  stat_boxplot(geom = "errorbar",  
               width = 0.2) +
  geom_boxplot(fill = "mediumaquamarine") 


### Desagregado por intervalos
all_items_variation |> 
  ggplot(aes(variation_all_items, variation_group))+
  stat_boxplot(geom = "errorbar",  
               width = 0.2) +
  geom_boxplot(fill = "mediumaquamarine") 


## Gráficos de líneas

### Pregunta a GPT: ¿Como agregar mas de una linea a un gráfico de lineas con ggplot?

#Gráfica de la evolución de los valores absolutos del CPI de: all items, food, all items less fresh food
CPI_Japon |>
  select(year, all_items, food, all_items_less_fresh_food) |> 
  ggplot(aes(x = year)) +
  geom_line(aes(y = all_items, color = "All Items"), size = 1) +
  geom_line(aes(y = food, color = "Food"), size = 0.5) +
  geom_line(aes(y = all_items_less_fresh_food, color = "All Items Less Fresh Food"), size = 0.5) +
  labs(
    title = "Evolución del CPI: General vs Comida vs All Items sin incluir fresh food",
    x = "Año",
    y = "Índice CPI",
    color = "Categoría"
  ) +
  theme_minimal()

#Gráfica de la evolución de la variación del CPI de: all items, food, all items less fresh food

# Primero agregamos la variación de all_items_less_fresh_food al df que ya existe (y le sacamos el año 1970)
all_items_and_food_and_all_items_less_fresh_food <- CPI_Japon |>
  select(year, all_items, food, all_items_less_fresh_food) |>
  mutate(
    variation_all_items = all_items - lag(all_items),
    variation_food = food - lag(food),
    variation_all_items_less_fresh_food = all_items_less_fresh_food - lag(all_items_less_fresh_food)
  ) |>
  filter(year != 1970)

  
#Y ahora sí, gráficamos
all_items_and_food_and_all_items_less_fresh_food |>
    ggplot(aes(x = year)) +
    geom_line(aes(y = variation_all_items, color = "Var. All Items"), size = 1) +
    geom_line(aes(y = variation_food, color = "Var. Food"), size = 0.5) +
    geom_line(aes(y = variation_all_items_less_fresh_food, color = "Var. All Items Less Fresh Food"), size = 0.5) +
    labs(
      title = "Variación interanual del CPI: General vs Comida vs All Items sin incluir Fresh Food",
      x = "Año",
      y = "Variación interanual (%)",
      color = "Categoría"
    ) +
    theme_minimal()
  

#Para responder a nuestra pregunta

  # Top 5 variaciones
  values <- as.numeric(variation_50_years[1, ])
  names <- names(variation_50_years)
  top5_names <- names[order(values, decreasing = TRUE)[1:5]]
  variation_top5 <- variation_50_years[, top5_names]
  variation_top5


  # Realizamos una matriz de correlaciones para visualizar el impacto de estas variables entre sí y con el general (all_items)
  
  significant_items <- CPI_Japon |> 
    select(year, all_items, other_miscellaneous, school_fees, personal_care_services, education, tobacco)
  
  # Hacemos la variación interanual del CPI de los items significantes (top 5) año a año
  significant_items_variation <- significant_items |> 
    mutate(
      var_all_items = all_items - lag(all_items),
      var_other_miscellaneous = other_miscellaneous - lag(other_miscellaneous),
      var_school_fees = school_fees - lag(school_fees),
      var_personal_care_services = personal_care_services - lag(personal_care_services),
      var_education = education - lag(education),
      var_tobacco = tobacco - lag(tobacco)
    ) |> 
    filter(!is.na(var_all_items))  # quitamos la fila 1970 que tiene NA
  
  
  significant_items_variation |>
    ggplot(aes(x = year)) +
    geom_line(aes(y = var_all_items, color = "Var. All Items"), size = 1) +
    geom_line(aes(y = var_other_miscellaneous, color = "Var. Other Miscellaneous"), size = 0.5) +
    geom_line(aes(y = var_school_fees, color = "Var. School Fees"), size = 0.5) +
    geom_line(aes(y = var_personal_care_services, color = "Var. Personal Care Services"), size = 0.5) +
    geom_line(aes(y = var_education, color = "Var. Education"), size = 0.5) +
    geom_line(aes(y = var_tobacco, color = "Var. Tobacco"), size = 0.5) +
    labs(
      title = "Variación interanual del CPI: 5 categorías con mayor variación y General",
      x = "Año",
      y = "Variación interanual (%)",
      color = "Categoría"
    ) +
    theme_minimal()
  
  # Nos quedamos solo con las columnas de variaciones, es casi como usar un IN pero solo aplica al principio
  variations_only <- significant_items_variation |> 
    select(starts_with("var_"))
  
  # Creamos la matriz de correlación
  matriz <- variations_only |> 
    select(where(is.numeric)) |>  
    cor()
  
  
  # Visualizamos con un heatmap
  
  pheatmap(
    #matriz de correlación
    matriz,
    #pongo para que muestre el coeficiente de correlación en el gráfico
    display_numbers = TRUE,        # muestra los coeficientes en el gráfico
    #saco clustering
    cluster_rows = FALSE, 
    cluster_cols = FALSE,
    main = "Correlación entre variaciones interanuales (Top 5 + All Items)"
  )
  
  
  




