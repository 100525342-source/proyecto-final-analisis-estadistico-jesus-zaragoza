# ============================================================
# 0. Preparación
# ============================================================

# Instalamos y cargamos las librerías a emplear (solo si es necesario)
paquetes <- c("dplyr", "tidyr", "ggplot2", "naniar", "corrplot")
instalar <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(instalar) > 0) install.packages(instalar)

library(dplyr)     
library(tidyr)     
library(ggplot2)   
library(naniar)    
library(corrplot)  


# Importamos el fichero (debe estar en el directorio de trabajo)
imdb_top_1000 <- read.csv("imdb_top_1000.csv",
                          stringsAsFactors = FALSE,
                          encoding = "UTF-8")

# Creamos una copia del dataset original para trabajar
fichero_trabajo <- imdb_top_1000


# ============================================================
# 1. Dimensiones del dataset
# ============================================================

# Número de observaciones y variables
nrow(fichero_trabajo) #Tamaño de la muestra
ncol(fichero_trabajo) #Nº de variables

# Dimensiones generales del dataset
dim(fichero_trabajo)

# ============================================================
# 2. Identificación de variables y tipos de datos
# ============================================================
str(fichero_trabajo)

# ============================================================
# 3. Limpieza de datos
# ============================================================

# ------------------------------------------------------------
# 3.1. Identificación y tratamiento de inconsistencias
# ------------------------------------------------------------

# Comprobamos los valores de la variable Released_Year
anios_numericos <- suppressWarnings(
  as.integer(fichero_trabajo$Released_Year)
)

# Identificamos los valores que no se pueden convertir a año
valores_no_validos <- unique(
  fichero_trabajo$Released_Year[
    !is.na(fichero_trabajo$Released_Year) &
      is.na(anios_numericos)
  ]
)
valores_no_validos

# Buscamos los registros cuyo año no es numérico
fichero_trabajo[
  !is.na(fichero_trabajo$Released_Year) &
    is.na(anios_numericos),
  c("Series_Title", "Released_Year", "Director")
]

# Corregimos el año de estreno de Apollo 13
fichero_trabajo$Released_Year[
  fichero_trabajo$Series_Title == "Apollo 13"
] <- "1995"

# Una vez corregido, convertimos el año a formato numérico
fichero_trabajo$Released_Year <- as.integer(fichero_trabajo$Released_Year)

# Creamos una variable que muestre la década de estreno 
#A partir de esta variable, desarrollaremos algunos gráficos
fichero_trabajo$Decada <- floor(fichero_trabajo$Released_Year / 10) * 10
fichero_trabajo$Decada[fichero_trabajo$Decada == 2020] <- 2010
fichero_trabajo$Decada <- factor(
  fichero_trabajo$Decada,
  labels = c(paste0(seq(1920, 2000, 10), "s"), "2010-20")
)

# Transformamos la variable duración a formato numérico y eliminamos <<min>> (texto)
fichero_trabajo$Runtime <- as.integer(
  gsub("[^0-9]", "", fichero_trabajo$Runtime)
)

# Transformamos la variable recaudación a formato numérico, eliminando las comas
fichero_trabajo$Gross <- as.numeric(
  gsub(",", "", fichero_trabajo$Gross)
)

# Comprobamos los tipos de datos después de las conversiones
str(fichero_trabajo)

# Revisamos ejemplos de las variables transformadas
head(fichero_trabajo$Released_Year)
head(fichero_trabajo$Runtime)
head(fichero_trabajo$Gross)

# Mostramos los valores ausentes tras la limpieza
colSums(is.na(fichero_trabajo))

# ------------------------------------------------------------
# 3.2. Identificación de observaciones incompletas
# ------------------------------------------------------------

# Número de filas incompletas (con al menos un NA)
sum(!complete.cases(fichero_trabajo))

# Número de filas completas (sin ningún NA)
sum(complete.cases(fichero_trabajo))

# Porcentaje de filas incompletas
mean(!complete.cases(fichero_trabajo)) * 100

# Guardamos las filas incompletas para su revisión
filas_incompletas <- fichero_trabajo[
  !complete.cases(fichero_trabajo),
]

# ------------------------------------------------------------
# 3.3. Identificación y tratamiento de valores ausentes
# ------------------------------------------------------------

# Detectamos posibles cadenas de texto vacías en todas las variables
fichero_trabajo %>%
  summarise(across(
    everything(), 
    ~ sum(trimws(as.character(.x)) == "", na.rm = TRUE)
  ))

# Sustituimos las cadenas vacías por NA en las variables afectadas
fichero_trabajo$Certificate[
  trimws(fichero_trabajo$Certificate) == ""] <- NA

fichero_trabajo$Gross[
  trimws(fichero_trabajo$Gross) == ""] <- NA

# Calculamos el número de valores ausentes por variable
faltantes <- colSums(is.na(fichero_trabajo))

# Obtenemos el porcentaje de valores ausentes por variable
porcentaje <- round(
  colMeans(is.na(fichero_trabajo)) * 100, 2
)

# Creamos una tabla resumen
tabla_na <- data.frame(
  Variable = names(faltantes),
  Valores_ausentes = as.integer(faltantes),
  Porcentaje = as.numeric(porcentaje)
)

# La ordenamos de mayor a menor en función de los valores ausentes
tabla_na <- tabla_na[
  order(-tabla_na$Valores_ausentes),
]

# Mostramos la tabla
tabla_na

#Gráfico sobre porcentaje de los valores NA por década
gg_miss_fct(
  fichero_trabajo %>% select(Decada, Certificate, Meta_score, Gross),
  fct = Decada
) +
  labs(
    title = "Porcentaje de valores ausentes por década de estreno",
    x = "Década de estreno",
    y = NULL
  ) +
  scale_fill_viridis_c(name = "% de NA") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Tabla sobre el porcentaje de valores ausentes por década de estreno
fichero_trabajo %>%
  group_by(Decada) %>%
  summarise(
    peliculas = n(),
    gross = round(mean(is.na(Gross)) * 100, 1),
    metascore = round(mean(is.na(Meta_score)) * 100, 1),
    certificate = round(mean(is.na(Certificate)) * 100, 1)
  )

# ------------------------------------------------------------
# 3.4. Detección de registros duplicados
# ------------------------------------------------------------

# Contamos las filas completamente duplicadas
sum(duplicated(fichero_trabajo))

#Al no haber filas duplicadas, no hace falta mostrarlas.

# Identificamos títulos que aparecen más de una vez
titulos_repetidos <- fichero_trabajo[
  duplicated(fichero_trabajo$Series_Title) |
    duplicated(
      fichero_trabajo$Series_Title,
      fromLast = TRUE
    ),
]

# Mostramos título, año y director
titulos_repetidos[
  order(titulos_repetidos$Series_Title),
  c("Series_Title", "Released_Year", "Director")
]

# ------------------------------------------------------------
# 3.5. Identificación de posibles valores atípicos
# ------------------------------------------------------------

# Obtenemos un resumen estadístico de las variables cuantitativas 
summary(fichero_trabajo[
  c("IMDB_Rating", "Runtime", "Meta_score",
    "No_of_Votes", "Gross")
])

# Representación conjunta de los cinco boxplots
par(mfrow = c(2, 3))

boxplot(fichero_trabajo$IMDB_Rating,
        main = "Puntuación IMDb",
        ylab = "Puntuación", col = "lightblue")

boxplot(fichero_trabajo$Runtime,
        main = "Duración",
        ylab = "Minutos", col = "lightgreen")

boxplot(fichero_trabajo$Meta_score,
        main = "Metascore",
        ylab = "Puntuación", col = "lightgrey")

boxplot(fichero_trabajo$No_of_Votes,
        main = "Número de votos",
        ylab = "Votos", col = "lightyellow")

boxplot(fichero_trabajo$Gross,
        main = "Recaudación",
        ylab = "Recaudación",
        col = "lightpink",
        yaxt = "n")

axis(
  side = 2,
  at = seq(0, 1000000000, by = 200000000),
  labels = c("0", "200 M", "400 M", "600 M", "800 M", "1.000 M")
)

par(mfrow = c(1, 1))


# Creamos un objeto con las cinco variables que vamos a revisar
variables_atipicas <- fichero_trabajo[
  c("IMDB_Rating", "Runtime", "Meta_score",
    "No_of_Votes", "Gross")
]

# Contamos los posibles valores atípicos de cada variable
sapply(variables_atipicas, function(x) {
  length(boxplot.stats(na.omit(x))$out)
})

# Consultamos los valores atípicos concretos de cada variable
lapply(variables_atipicas, function(x) {
  boxplot.stats(na.omit(x))$out
})

# Ordenamos las duraciones de menor a mayor
sort(na.omit(fichero_trabajo$Runtime))

# Identificamos las películas con duraciones atípicas
duraciones_atipicas <- boxplot.stats(
  na.omit(fichero_trabajo$Runtime)
)$out

fichero_trabajo[
  fichero_trabajo$Runtime %in% duraciones_atipicas,
  c("Series_Title", "Released_Year", "Runtime")
]

# ============================================================
# 4. Análisis univariante
# ============================================================

# ------------------------------------------------------------
# 4.1. Análisis de las variables cuantitativas
# ------------------------------------------------------------

# Seleccionamos las variables cuantitativas
variables_cuantitativas <- fichero_trabajo[
  c("IMDB_Rating", "Runtime", "Meta_score",
    "No_of_Votes", "Gross")
]

# Obtenemos las principales medidas descriptivas
summary(variables_cuantitativas)

# Calculamos la desviación típica de cada variable
sapply(variables_cuantitativas, sd, na.rm = TRUE)

# Histogramas de las variables cuantitativas
par(mfrow = c(2, 3))

hist(fichero_trabajo$IMDB_Rating,
     main = "Puntuación IMDb",
     xlab = "Puntuación", col = "lightblue")

hist(fichero_trabajo$Runtime,
     main = "Duración de las películas",
     xlab = "Minutos", col = "lightgreen")

hist(fichero_trabajo$Meta_score,
     main = "Metascore",
     xlab = "Puntuación", col = "lightgrey")

hist(fichero_trabajo$No_of_Votes,
     main = "Número de votos",
     xlab = "Votos", col = "lightyellow")

hist(fichero_trabajo$Gross,
     main = "Recaudación",
     xlab = "Recaudación",
     col = "lightpink",
     xlim = c(0, 1000000000),
     xaxt = "n")

axis(
  side = 1,
  at = seq(0, 1000000000, by = 200000000),
  labels = c("0", "200 M", "400 M", "600 M", "800 M", "1.000 M")
)

par(mfrow = c(1, 1))

# ------------------------------------------------------------
# 4.2. Análisis de las variables cualitativas
# ------------------------------------------------------------

# Frecuencia de cada combinación de géneros
frecuencias_genero <- sort(
  table(fichero_trabajo$Genre),
  decreasing = TRUE
)

# Mostramos los 10 géneros o combinaciones de géneros más frecuentes
head(frecuencias_genero, 10)

# Gráfico de los 10 valores más frecuentes de Genre
par(mar = c(10, 5, 4, 2)) #aumentamos el margen inferior

barplot(
  head(frecuencias_genero, 10),
  main = "Géneros más frecuentes",
  xlab = "",
  ylab = "Frecuencia",
  las = 2,
  cex.names = 0.7,
  ylim = c(0, 100),
  col = "lightblue"
)

# Añadimos el título del eje x, debajo de las etiquetas
mtext("Género o combinación de géneros",
      side = 1, line = 8) 

# Obtenemos la frecuencia de cada género por separado
generos_individuales <- fichero_trabajo %>%
  separate_rows(Genre, sep = ",\\s*") %>%
  count(Genre, name = "Peliculas", sort = TRUE) %>%
  mutate(Porcentaje = round(Peliculas / nrow(fichero_trabajo) * 100, 1))

generos_individuales

#Creamos un gráfico de barras hztal para representarlas
ggplot(generos_individuales,
       aes(x = reorder(Genre, Peliculas), y = Peliculas)) +
  geom_col(fill = "#2a78b5", width = 0.7) +
  geom_text(aes(label = Peliculas), hjust = -0.2, size = 3.3) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Número de películas que incluyen cada género",
    x = NULL,
    y = "Número de películas"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())

# Unificamos las dos denominaciones de la clasificación UA
fichero_trabajo$Certificate[
  fichero_trabajo$Certificate == "U/A"
] <- "UA"

# Obtenemos las frecuencias de las clasificaciones por edades
frecuencias_certificate <- sort(
  table(fichero_trabajo$Certificate, useNA = "no"),
  decreasing = TRUE
)

# Creamos la tabla sin columnas adicionales
tabla_certificate <- data.frame(
  Clasificacion = names(frecuencias_certificate),
  Frecuencia = as.vector(frecuencias_certificate),
  Porcentaje = as.vector(
    round(prop.table(frecuencias_certificate) * 100, 2)
  )
)

# Mostramos la tabla
tabla_certificate

# Gráfico horizontal de las clasificaciones por edades
par(mar = c(5, 8, 4, 2))

barplot(
  frecuencias_certificate,
  main = "Distribución de las clasificaciones por edades",
  xlab = "Frecuencia",
  ylab = "",
  horiz = TRUE,
  las = 1,
  col = "lightgreen",
  xlim = c(0, 250),
  cex.names = 0.8
)

# Distribución de las películas por década
fichero_trabajo %>%
  count(Decada) %>%
  ggplot(aes(x = Decada, y = n)) +
  geom_col(fill = "#2a78b5", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 3.3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Número de películas por década de estreno",
    x = "Década de estreno",
    y = "Número de películas"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank())

# ============================================================
# 5. Análisis bivariante
# ============================================================

# ------------------------------------------------------------
# 5.1. Puntuación IMDb y número de votos
# ------------------------------------------------------------

# Omitimos los valores ausentes que afecten al análisis
datos_1 <- na.omit(
  fichero_trabajo[, c("IMDB_Rating", "No_of_Votes")]
)

# Consultamos el número de observaciones disponibles tras eliminar los ausentes
nrow(datos_1)

# Diagrama de dispersión
plot(
  datos_1$No_of_Votes,
  datos_1$IMDB_Rating,
  main = "Relación entre puntuación IMDb y número de votos",
  xlab = "Número de votos",
  ylab = "Puntuación IMDb",
  pch = 19,
  cex = 0.7,
  col = "steelblue"
)

# Añadimos la línea de tendencia
abline(
  lm(IMDB_Rating ~ No_of_Votes, data = datos_1),
  col = "orange",
  lwd = 2
)
# Coeficiente de correlación
cor(datos_1$IMDB_Rating, datos_1$No_of_Votes)

# ------------------------------------------------------------
# 5.2. Recaudación y número de votos
# ------------------------------------------------------------

# Omitimos los valores ausentes que afecten al análisis
datos_2 <- na.omit(
  fichero_trabajo[, c("Gross", "No_of_Votes")]
)

# Consultamos el número de observaciones disponibles tras eliminar los ausentes
nrow(datos_2)

# Diagrama de dispersión
# Ajustamos los márgenes del gráfico
par(mar = c(5, 5, 4, 2))

plot(
  datos_2$No_of_Votes,
  datos_2$Gross,
  main = "Relación entre recaudación y número de votos",
  xlab = "Número de votos",
  ylab = "",
  pch = 19,
  cex = 0.7,
  col = "lightgreen",
  yaxt = "n",
  ylim = c(0, 1000000000),
  mgp = c(3, 1, 0)
)

# Título del eje Y desplazado hacia la izquierda
mtext("Recaudación (dólares)", side = 2, line = 4)

# Personalizamos el eje Y para hacerlo más comprensible
axis(
  2,
  at = seq(0, 1000000000, by = 200000000),
  labels = c("0", "200 M", "400 M", "600 M", "800 M", "1.000 M"),
  las = 1, 
  cex.axis = 0.8
)

# Añadimos la línea de tendencia
abline(
  lm(Gross ~ No_of_Votes, data = datos_2),
  col = "yellow",
  lwd = 2
)

# Coeficiente de correlación de Pearson
cor(datos_2$Gross, datos_2$No_of_Votes)

# ------------------------------------------------------------
# 5.3. Puntuación IMDb y Metascore
# ------------------------------------------------------------

# Omitimos los valores ausentes que afecten al análisis
datos_3 <- na.omit(
  fichero_trabajo[, c("IMDB_Rating", "Meta_score")]
)

# Consultamos el número de observaciones disponibles tras eliminar los ausentes
nrow(datos_3)

# Diagrama de dispersión
plot(
  datos_3$Meta_score,
  datos_3$IMDB_Rating,
  main = "Relación entre puntuación IMDb y Metascore",
  xlab = "Metascore",
  ylab = "Puntuación IMDb",
  pch = 19,
  cex = 0.7,
  col = "darkgreen"
)

# Añadimos la línea de tendencia
abline(
  lm(IMDB_Rating ~ Meta_score, data = datos_3),
  col = "red",
  lwd = 2
)

# Coeficiente de correlación de Pearson
cor(datos_3$IMDB_Rating, datos_3$Meta_score)


# Matriz de correlación de las variables cuantitativas
# Agrupamos las 5 variables
variables_cor <- fichero_trabajo[
  c("IMDB_Rating", "Runtime", "Meta_score",
    "No_of_Votes", "Gross")
]

# Definimos la matriz
matriz_cor <- cor(
  variables_cor,
  use = "pairwise.complete.obs"
)

# Creamos el gráfico (la matriz de correlación)
corrplot(
  matriz_cor,
  method = "circle",
  type = "upper",
  diag = FALSE,
  addCoef.col = "black",
  number.cex = 0.85,
  tl.col = "black",
  tl.cex = 0.9,
  tl.srt = 45,
  cl.cex = 0.8,
  cl.offset = 1,
  cl.align.text = "c",
  mar = c(0, 0, 2, 3),
  title = "Matriz de correlaciones entre variables cuantitativas"
)

