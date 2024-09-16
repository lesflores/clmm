#--------------------
# clmm en R
# Los modelos clmm permiten analizar variables de respuesta ordinales (como escalas de Likert)
# mientras permiten el uso de efectos aleatorios.
# Ejemplo hipotético: Supongamos que mi variable dependiente es la "Satisfacción con la democracia"
# en una escala de Likert con 5 categorías ordinales: "Muy insatisfecho/a", "Insatisfecho/a",
# "Neutral", "Satisfecho/a", "Muy satisfecho/a".
#--------------------

# Variable dependiente
# "Satisfacción con la democracia" (ordinal)
# 1 = Muy insatisfecho/a
# 2 = Insatisfecho/a
# 3 = Neutral
# 4 = Satisfecho/a
# 5 = Muy satisfecho/a

# Variables independientes
# 1. Confianza en las instituciones (ordinal)
# Medida en una escala de Likert:
# 1 = Ninguna confianza
# 2 = Poca
# 3 = Algo 
# 4 = Mucha

# 2. Participación política (binaria)
# 0 = No ha participado en act políticas
# 1 = Ha participado en al menos una act política (voto, manifestaciones, pertenencia a partidos, etc.)

# 3. Percepción de la corrupción (ordinal)
# 1 = Mucha corrupción
# 2 = Algo
# 3 = Poca
# 4 = Ninguna

# 4. Ideología política (categórica)
# Escala del 1 al 10, donde 1 es extrema izquierda y 10 es extrema derecha.

# 5. Percepción de exclusión o discriminación (binaria)
# 0 = No ha sufrido exclusión o discriminación
# 1 = Ha sufrido exclusión o discriminación en alguna ocasión

# 6. Frecuencia de consumo de noticias políticas (ordinal)
# 1 = Nunca
# 2 = Rara vez
# 3 = A veces
# 4 = Frecuentemente
# 5 = Todos los días

#--------------------
# Modelo clmm:

library(readxl)
library(tidyverse)
library(ordinal)

datos <- read_excel("Satisfaccion_democracia.xlsx")
glimpse(datos)

# Convertir "satisfaccion_democracia" a un factor ordenado
datos$Satisfaccion_democracia <- factor(datos$Satisfaccion_democracia, 
                                        ordered = TRUE)

# Ajuste del modelo
modelo <- clmm(Satisfaccion_democracia ~ Confianza_instituciones + 
                 Participacion_politica + Percepcion_corrupcion + 
                 Ideologia_politica + Percepcion_exclusion + 
                 Frecuencia_noticias + (1|Municipio), # efecto aleatorio por municipio.
               data = datos)

summary(modelo)
