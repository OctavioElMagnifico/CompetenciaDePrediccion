# Producir predicciones a partir de un archivo de test.
# $ Rscript predecir.R <test_data_file> <preds_file>
# Sin argumentos, lee de 'test.txt' y escribe a 'preds.txt'
library(randomForest)
library(tidyverse)

wd <- "~/maestria/CompetenciaDePrediccion"
fname <- "modelo.RData"
# - `fname` contiene la funcion `predecir` que a partir de un dataframe,
#   produce un vector de predicciones.
load(file.path(wd, fname))

args <- commandArgs(trailingOnly = TRUE)

if (is.na(args[2])) {
  args[2] = "preds.txt"
}
if (is.na(args[1])) {
  # default output file
  args[1] = "test.txt"
}

datos <- read_table(
  args[1],
  col_names = c('ciclo', 'minmem', 'maxmem', 'cache', 'mincan', 'maxcan', 'perfo')
) %>% mutate(
  freq = 1/ciclo)

preds <- predecir(newdata=datos)
write.table(preds, args[2])