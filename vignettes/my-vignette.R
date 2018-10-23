## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(RcourseLAB4)
library(ggplot2)

## ------------------------------------------------------------------------
linreg(formula = Petal.Length ~ Species, data = iris)

## ------------------------------------------------------------------------
summary(linreg(formula = Petal.Length ~ Species, data = iris))

## ------------------------------------------------------------------------
RcourseLAB4::theme_liu1()

RcourseLAB4::theme_liu2()

