#--------------------
# clmm en R
# Los modelos clmm permiten analizar variables de respuesta ordinales (como escalas de Likert)
# mientras permiten el uso de efectos aleatorios.
# Ejemplo hipotético: Supongamos que mi variable dependiente es la "Satisfacción con la democracia"
# en una escala de Likert con 5 categorías ordinales: "Muy insatisfecho/a", "Insatisfecho/a",
# "Neutral", "Satisfecho/a", "Muy satisfecho/a".
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

# Modelo
modelo <- clmm(Satisfaccion_democracia ~ Confianza_instituciones + 
                 Participacion_politica + Percepcion_corrupcion + 
                 Ideologia_politica + Percepcion_exclusion + 
                 Frecuencia_noticias + (1|Municipio), # efecto aleatorio por municipio.
               data = datos)

summary(modelo)
