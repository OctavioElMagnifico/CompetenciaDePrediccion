library(broom)
library(tidyverse)
library(caret)
library(RobStatTM)
datos <- read_table(
  "Concurso_Estima.txt",
  col_names = c('ciclo', 'minmem', 'maxmem', 'cache', 'mincan', 'maxcan', 'perfo')
)

datos <- mutate(
  datos,
  freq = 1/ciclo,
  promcan = (mincan + maxcan)/2,
  prommem = (minmem + maxmem)/2,
  cach = cache/10,
  chavg = ceiling((mincan + maxcan)/2),
  speed = 1/ciclo,
  chcap = chavg * speed * 10,
  mavg = (minmem + maxmem)/2 * 10 - 3
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

postResamplePodado <- function (pred, obs, alfa) {
  isNA <- is.na(pred)
  pred <- pred[!isNA]
  obs <- obs[!isNA]
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
      SE <- (pred - obs)^2
      SE_ <- SE[SE <= quantile(SE, alfa)]
      mse <- mean(SE_)
      mae <- mean(abs(pred - obs))
      out <- c(sqrt(mse), resamplCor^2, mae)
    }
    names(out) <- c("RMSE", "Rsquared", "MAE")
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

defaultSummaryPodado <- function (data, lev = NULL, model = NULL) {
  if (is.character(data$obs)) 
    data$obs <- factor(data$obs, levels = lev)
  postResamplePodado(data[, "pred"], data[, "obs"], 0.85)
}

modelos <- list(
  perfo ~ freq + minmem + maxmem + cache + mincan + maxcan,
  
  perfo ~ (freq + minmem + maxmem + cache + mincan + maxcan)^2,
  perfo ~ (freq + minmem + maxmem + cache + mincan + maxcan)^3,
  
  perfo ~ cach + chcap + mavg,
  perfo ~ cach + chcap + log(mavg),
  perfo ~ (cach + chcap + log(mavg))^2,
  perfo ~ cach + chcap + mavg,
  
  perfo ~ (cach + chcap + mavg)^2,
  perfo ~ (cach + chcap + mavg)^2 + cach*chcap*mavg,
  perfo ~ (cach + chcap + mavg)^3,
  perfo ~ polym(cach, chcap, mavg, degree = 2),
  
  perfo ~ polym(freq, minmem, maxmem, cache, mincan, maxcan, degree = 2),
  perfo ~ polym(freq, prommem, cache, promcan, degree = 2)
  # sqrt(perfo) ~ ciclo + minmem + maxmem + cache + mincan + maxcan,
  # sqrt(perfo) ~ (ciclo + minmem + maxmem + cache + mincan + maxcan)^2,
  # sqrt(perfo) ~ cach + chavg + speed + chcap + mavg,
  # sqrt(perfo) ~ (cach + chavg + speed + chcap + mavg)^2
)

trCont <- trainControl(
  summaryFunction = defaultSummaryPodado,
  method = "LGOCV", number = 100, p = 0.75
)

comparar(modelos, datos, trCont)

slm(perfo ~ polym(freq, prommem, cache, promcan, degree = 2),
   datos) %>% tidy()
lm(perfo ~ polym(freq, minmem, maxmem, cache, mincan, maxcan, degree = 2),
   datos) %>% summary()

glmnetGrid <- expand.grid(
  alpha = seq(0, 1, 0.25),
  lambda = 10^seq(-1, 3, 0.1)
)
train(perfo ~ polym(freq, prommem, cache, promcan, degree = 2), datos,
      trControl = trCont, method = "glmnet", tuneGrid = glmnetGrid)

train(perfo ~ polym(freq, prommem, cache, promcan, degree = 2), datos,
      trControl = trCont, method = "pls", tuneGrid = expand.grid(ncomp = 1:10))
