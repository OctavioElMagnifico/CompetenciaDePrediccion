# CompetenciaDePrediccion
Competencia de predicción sobre datos de rendimiento relativo de computadoras. 

# Cómo Predecir con el Modelo sobre los Datos de *Test* reservados

  Al ejecutar la función sin parámetros, por ejemplo desde la terminal `Rscript predecir.R`, esta espera encontrar en el directorio donde se halle un archivo llamado `tests.txt` con el mismo formato del archivo `Concurso_Estima.txt` que recibimos.
  En caso de hallarlo levanta la sesión llamada `modelo.RData` que contiene el modelo seleccionado entrenado y genera las predicciones. Estas quedan almacenadas en un nuevo archivo llamado `preds.txt`.
  También acepta nombres para el archivo de entrada y el de salida, `Rscript predecir.R <test_data_file> <preds_file>`.
  
  Alternativamente, se puede directamente cargar la sesión `modelo.RData` y predecir usando la clásica interface de R ,en este caso con `predict(modelo,DatosNuevos)`.
  
  Nuestro desarrollo está resumido en el guión `entrenar.R`.
