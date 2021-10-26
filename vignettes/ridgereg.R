## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(githubinstall)
library(smlm)
library(caret)
library(MASS)
library(leaps)
set.seed(1234)

## ----step2, message=FALSE-----------------------------------------------------
df <- Boston
index <- createDataPartition(df$crim, times = 1, p = 0.8, list = FALSE)
train <- df[index,]
test <- df[-index,]

## ----step3a, message=FALSE----------------------------------------------------
lm_mod <- train(medv ~ ., data = train, method = "lm")

## ----step3b, message=FALSE----------------------------------------------------
forward_mod <- train(medv ~ ., data = train, method = "leapForward")

## ----step3c, message=FALSE----------------------------------------------------
rdg_reg <- list(type = "Regression",
                library = "smlm",
                loop = NULL)

rdg_reg$parameters <- data.frame(parameter = c("lambda"),
                      class = c("numeric"),
                      label = c("Lambda"))

rdg_reg$grid <- function(x, y, len = NULL, search = "grid") {
  out <- expand.grid(lambda = seq(from = 0, to = 1, by = 0.1))
  out
}

rdg_reg$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) { 
  df <- as.data.frame(cbind(x, y))
  mod_formula <- as.formula(paste0("y~", paste(colnames(df)[1:(ncol(df)-1)], collapse = "+")))
  mod <- ridgereg(data = df, formula = mod_formula, lambda = param$lambda)
}

rdg_reg$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
  modelFit$predict(modelFit, newdata)
}

rdg_reg$prob <- list(NULL)

ridge_mod <- train(medv ~ ., data = train, 
                   method = rdg_reg,
                   trControl = trainControl(method = "repeatedcv", repeats = 3))

## ----step4--------------------------------------------------------------------
lm_pred <- predict(lm_mod, test)
forward_pred <- predict(forward_mod, test)
ridge_pred <- predict(ridge_mod, test)

RMSE(lm_pred, test$medv)
RMSE(forward_pred, test$medv)
RMSE(ridge_pred, test$medv)

