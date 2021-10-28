## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(smrid)
library(caret)
library(MASS)
library(leaps)
set.seed(1234)

## ----step2, message=FALSE-----------------------------------------------------
df <- Boston
index <- createDataPartition(df$crim, times = 1, p = 0.8, list = FALSE)
train <- df[index,]
test <- df[-index,]

