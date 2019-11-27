library(broom)
library(tidyverse)
library(caret)
# Si tiene problemas instalando glmnet en R < 3.6, necesita una version 2.x de glmnet
# glmnetURL <- "https://cran.r-project.org/src/contrib/Archive/glmnet/glmnet_2.0-18.tar.gz"
# install.packages(glmnetURL, repos=NULL, type="source")
library(glmnet)
library(randomForest)

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

modelos <- c(
  "freq + prommem + cache + promcan",
  "(freq + prommem + cache + promcan)^2",
  "(freq + prommem + cache + promcan)^3",
  "freq + mincan + maxcan + minmem + maxmem + cache",
  "(freq + mincan + maxcan + minmem + maxmem + cache)^2",
  "(freq + mincan + maxcan + minmem + maxmem + cache)^3",
  "polym(freq, minmem, maxmem, cache, mincan, maxcan, degree = 2)",
  "polym(freq, prommem, cache, promcan, degree = 2)"
)
modelos <- modelos[1]

alfa <- 0.8
inversas <- list(
  "identity" = identity,
  "sqrt" = function(x){ x^2 },
  "log" = exp
)

crearControl <- function(link, alfa) {
  trainControl(
    summaryFunction = adHocSummary(alfa, inversas[[link]]),
    method = "LGOCV", number = 100, p = 0.75
  )
}
crearFormula <- function(link, rhs) {
  as.formula(paste(link, "(perfo) ~ ", rhs))
}
ayudanteTrain <- function(formula, metodo, control, datos) {
  train(formula, datos, method = metodo, trControl = control)
}

atipicos <- c(123, 27)
mejorFormula <- log(perfo) ~ (freq + mincan + maxcan + minmem + maxmem + cache)^2

rftrain <- train(mejorFormula,
      datos[-atipicos,],
      method = "rf",
      tuneGrid = expand.grid(mtry = c(1:4)),
      trControl = crearControl("log", 0.8),
      metric = "aRMSE",
      maximize = FALSE,
      ntree = 2000
)
rftrain2 <- train(mejorFormula,
                 datos[-atipicos,],
                 method = "rf",
                 tuneGrid = expand.grid(mtry = c(1)),
                 trControl = crearControl("log", 0.8),
                 metric = "aRMSE",
                 maximize = FALSE,
                 ntree = 10000
)
modelo <- randomForest(mejorFormula, datos[-atipicos,], ntree = 2000, mtry = 1)
predecir <- function(newdata){
  predict(modelo, newdata)
}
save("modelo", "predecir", file = "modelo.RData")
