library(broom)
library(tidyverse)
library(caret)
# Si tiene problemas instalando glmnet en R < 3.6, necesita una version 2.x de glmnet
# glmnetURL <- "https://cran.r-project.org/src/contrib/Archive/glmnet/glmnet_2.0-18.tar.gz"
# install.packages(glmnetURL, repos=NULL, type="source")
library(glmnet)
library(pls)
#modelos esotéricos
library(randomForest)
library(quantregForest)
library(KRLS)
library(kernlab)
library(nnls)

datos <- read_table(
  "Concurso_Estima.txt",
  col_names = c('ciclo', 'minmem', 'maxmem', 'cache', 'mincan', 'maxcan', 'perfo')
) %>% mutate(
  freq = 1/ciclo,
  promcan = (mincan + maxcan)/2,
  prommem = (minmem + maxmem)/2
)

comparar <- function(modelos, datos, control) {
  resumen <- tibble(
    modelo = modelos,
    train = map(modelo, train, method = "lm", data = datos, trControl = control),
    resultados = map(train, "results")) %>%
    unnest(resultados) %>%
    select(-train) %>%
    arrange(RMSE)

  return(resumen)
}

postResampleAdHoc <- function(alfa = 1, inversa = identity) {
  post_resample <- function(pred, obs){
    isNA <- is.na(pred)
    pred <- inversa(pred[!isNA])
    obs <- inversa(obs[!isNA])
    if (!is.factor(obs) && is.numeric(obs)) {
      if (length(obs) + length(pred) == 0) {
        out <- rep(NA, 3)
      }
      else {
        if (length(unique(pred)) < 2 || length(unique(obs)) < 
            2) {
          resamplCor <- NA
        }
        else {
          resamplCor <- try(cor(pred, obs, use = "pairwise.complete.obs"), 
                            silent = TRUE)
          if (class(resamplCor) == "try-error") 
            resamplCor <- NA
        }
        SqErr <- (pred - obs)^2
        poda <- SqErr <= quantile(SqErr, alfa)
        msePodado <- mean(SqErr[poda])
        rSqPodado <- cor(pred[poda], obs[poda], use = "pairwise.complete.obs")
        mse <- mean(SqErr)
        mae <- mean(abs(pred - obs))
        out <- c(sqrt(msePodado), rSqPodado^2, sqrt(mse), resamplCor^2, mae)
      }
      names(out) <- c("aRMSE", "aRsquared", "RMSE", "Rsquared", "MAE")
    }
    else {
      if (length(obs) + length(pred) == 0) {
        out <- rep(NA, 2)
      }
      else {
        pred <- factor(pred, levels = levels(obs))
        requireNamespaceQuietStop("e1071")
        out <- unlist(e1071::classAgreement(table(obs, pred)))[c("diag", 
                                                                 "kappa")]
      }
      names(out) <- c("Accuracy", "Kappa")
    }
    if (any(is.nan(out))) 
      out[is.nan(out)] <- NA
    out
  }
  return(post_resample)
}

adHocSummary <- function(alfa = 1, inversa = identity){
  post_resample <- postResampleAdHoc(alfa, inversa)
  function(data, ...) {
    post_resample(data[, "pred"], data[, "obs"])
  }
}

modelosIdenticos <- list(
  perfo ~ freq + prommem + cache + promcan,
  perfo ~ (freq + prommem + cache + promcan)^2,
  perfo ~ (freq + prommem + cache + promcan)^3,
  perfo ~ polym(freq, minmem, maxmem, cache, mincan, maxcan, degree = 2),
  perfo ~ polym(freq, prommem, cache, promcan, degree = 2),
  perfo ~ polym(freq, prommem, cache, promcan, degree = 3),
  perfo ~ polym(freq, prommem, cache, promcan, degree = 4)
)

modelosRadicales <- list(
  sqrt(perfo) ~ freq + prommem + cache + promcan,
  sqrt(perfo) ~ (freq + prommem + cache + promcan)^2,
  sqrt(perfo) ~ (freq + prommem + cache + promcan)^3,
  sqrt(perfo) ~ polym(freq, minmem, maxmem, cache, mincan, maxcan, degree = 2),
  sqrt(perfo) ~ polym(freq, prommem, cache, promcan, degree = 2),
  sqrt(perfo) ~ polym(freq, prommem, cache, promcan, degree = 3),
  sqrt(perfo) ~ polym(freq, prommem, cache, promcan, degree = 4)
)

modelosLogaritmicos <- list(
  log(perfo) ~ freq + prommem + cache + promcan,
  log(perfo) ~ (freq + prommem + cache + promcan)^2,
  log(perfo) ~ (freq + prommem + cache + promcan)^3,
  log(perfo) ~ polym(freq, minmem, maxmem, cache, mincan, maxcan, degree = 2),
  log(perfo) ~ polym(freq, prommem, cache, promcan, degree = 2),
  log(perfo) ~ polym(freq, prommem, cache, promcan, degree = 3),
  log(perfo) ~ polym(freq, prommem, cache, promcan, degree = 4)
)

alfa <- 0.8
summaryId <- adHocSummary(alfa, identity) # Id(entidad)
summaryRad <- adHocSummary(alfa, function(x) x^2) # Rad(ical)
summaryLog <- adHocSummary(alfa, exp)

trContId <- trainControl(
  summaryFunction = summaryId,
  method = "LGOCV", number = 100, p = 0.75
)
trContRad <- trainControl(
  summaryFunction = summaryRad,
  method = "LGOCV", number = 100, p = 0.75
)
trContLog <- trainControl(
  summaryFunction = summaryLog,
  method = "LGOCV", number = 100, p = 0.75
)
trContDef <- trainControl(method = "LGOCV", number = 100, p = 0.75)

idres <- comparar(modelosIdenticos, datos, trContId)
radres <- comparar(modelosRadicales, datos, trContRad)
logres <- comparar(modelosLogaritmicos, datos, trContLog)
lmres <- bind_rows(idres, radres, logres)

glmnetGrid <- expand.grid(
  alpha = c(1),
  lambda = 10^seq(-1.5, 1, 0.05)
)

lassores <- train(modelosRadicales[[5]],
      datos,
      trControl = trContRad, metric = "aRMSE", maximize = FALSE,
      method = "glmnet", tuneGrid = glmnetGrid
)

plsres <- train(modelosLogaritmicos[[1]],
      datos,
      trControl = trContLog,
      method = "pls", tuneGrid = expand.grid(ncomp = 1:5)
)

poires <- train(modelosIdenticos[[5]],
                datos,
                trControl = trContId,
                method = "glm", family = "poisson"
                )

res <- bind_rows(
  lmres,
  as_tibble(lassores$results),
  as_tibble(plsres$results)
)

## Para Bosque Aleatorio
## Quantile Random Forest	qrf	Regression	quantregForest	mtry

qrfres <- train(modelosIdenticos[[1]],
                datos,
                trcontrol = trContId,
                method = "qrf",
                tuneGrid = expand.grid( mtry = 5:10 )
)

## Random Forest	rf	Classification, Regression	randomForest	mtry


rfres <- train(modelosRadicales[[2]],
                datos,
                trcontrol = trContRad,
                method = "rf",
                tuneGrid = expand.grid( mtry = 3:7 )
                )

## Pâra GAM
## Regresión de Nucleos

## Polynomial Kernel Regularized Least Squares	krlsPoly	Regression	KRLS	lambda, degree

# Este método se niega a ceptar 'trControl'. diría que queda descartado.

krlspolyres <- train(modelosIdenticos[[2]],
               datos,
               method = "krlsPoly",
               tuneGrid = expand.grid(
                                      lambda = 10^( seq(-1,0.5,by=0.1) ),
                                      degree = 1
                                      )
               )



## Relevance Vector Machines with Linear Kernel	rvmLinear	Regression	kernlab	None
## Relevance Vector Machines with Polynomial Kernel	rvmPoly	Regression	kernlab	scale, degree
## Relevance Vector Machines with Radial Basis Function Kernel	rvmRadial	Regression	kernlab	sigma
## Non-Negative Least Squares	nnls	Regression	nnls	None
#robusto
## Robust Linear Model	rlm	Regression	MASS	intercept, psi


# Espio los "mejores modelos" rapidamente
arrange(res, aRMSE) %>% head(4)

# Ignorar, esto servira para predecir mas adelante
modelo <- lm(disp ~ cyl, mtcars)
predecir <- function(newdata){
  predict(modelo, newdata)
}
save("modelo", "predecir", file = "modelo.RData")
