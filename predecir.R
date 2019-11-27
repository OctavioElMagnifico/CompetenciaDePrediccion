# Producir predicciones a partir de un archivo de test.
# $ Rscript predecir.R <test_data_file> <preds_file>
# Sin argumentos, lee de 'test.txt' y escribe a 'preds.txt'
library(randomForest)

wd <- getwd()
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

datos <- read.table(
  args[1],
  col.names = c('ciclo', 'minmem', 'maxmem', 'cache', 'mincan', 'maxcan', 'perfo')
)
datos$freq = 1/datos$ciclo

preds <- predecir(newdata=datos)
write.table(preds, args[2])