## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(ggplot2)

## ------------------------------------------------------------------------
lm <- RcourseLAB4::linreg(formula = Petal.Length ~ Species, data = iris)

