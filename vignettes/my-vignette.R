## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
linreg <- function(formula, data){
  if(class(formula)=="formula")
  {
    X <- model.matrix(formula, data)
    y <- data[,all.vars(formula)[1]] #pick up independent variable
    
    #calculate regression coefficients
    regressioncoeffs <- solve(t(X) %*% X) %*% t(X) %*% y
    
    #calculate fitted values
    fittedvals <- X %*% regressioncoeffs
    
    #residuals
    residualvals <- y-fittedvals
    
    #the degree of freedom
    degreeoffree <- nrow(X)-ncol(X)
    
    #the residual variance
    residualvariance <- (t(residualvals) %*% residualvals) / degreeoffree
    
    #variance of regression coefficients
    varianceofcoeff <- diag(as.vector(residualvariance) * solve(t(X) %*% X))
    
    #t-values of each coeffcient 
    tvalues <- regressioncoeffs / sqrt(varianceofcoeff)
    
    #p-values
    pvalues <- 2*pt(abs(tvalues),degreeoffree)
    
    calculatedstatistics <- list("regressioncoeffs"=regressioncoeffs, "fittedvals"=fittedvals, "residualvals"=residualvals, "degreeoffree"=degreeoffree, "residualvariance"=residualvariance, "varianceofcoeff"=varianceofcoeff, "tvalues"=tvalues, "pvalues"=pvalues)
    class(calculatedstatistics) <- "linreg"
    return(calculatedstatistics)
  }
  else
  {
    stop("The first argument should be of type formula")
  }
}


## ------------------------------------------------------------------------
# print method for linreg
print.linreg <- function(x){
  
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  cat("Coefficients:\n")
  print(format(t(x[["regressioncoeffs"]])[1,]), print.gap = 2L, 
        quote = FALSE)
}

# plot methd for linreg
plot.linreg <- function(x){
  
}

# resd method for linreg
resid.linreg <- function(x) {
  as.vector(x[["residualvals"]])
}

pred.linreg <- function(x) {
  x[["fittedvals"]]
}

coef.linreg <- function(x) {
  c(t(x[["regressioncoeffs"]]))
}


## ------------------------------------------------------------------------

theme_liu1 <- function(){
  ggplot2::theme_bw()+
  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#FFFFFF", colour = "#666666"),
        panel.grid.major = ggplot2::element_line(colour = "#54d8e0", size=0.3),
        panel.grid.minor = ggplot2::element_line(colour = "#54d8e0", size=0.3),
        axis.line = ggplot2::element_line(size =0.3, colour = "#54d8e0"),
        axis.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
        axis.ticks = ggplot2::element_line(colour = "#54d8e0"),
        legend.key = ggplot2::element_rect(colour = "#54d8e0"),
        panel.border = ggplot2::element_rect(fill = NA, colour = "#54d8e0"),
        strip.background = ggplot2::element_rect(fill = "grey80", colour = "#54d8e0"))
}

ggplot2::ggplot(faithful, ggplot2::aes(x=eruptions, y=waiting))+
  ggplot2::geom_point()+
  theme_liu1()


theme_liu2 <- function(){
  ggplot2::theme_bw()+
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#54d8e0", colour = "#666666"),
          panel.grid.major = ggplot2::element_line(colour = "#FFFFFF", size=0.3),
          panel.grid.minor = ggplot2::element_line(colour = "#FFFFFF", size=0.3),
          axis.line = ggplot2::element_line(size =0.3, colour = "#54d8e0"),
          axis.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
          axis.ticks = ggplot2::element_line(colour = "#54d8e0"),
          legend.key = ggplot2::element_rect(colour = "#54d8e0"),
          panel.border = ggplot2::element_rect(fill = NA, colour = "#54d8e0"),
          strip.background = ggplot2::element_rect(fill = "grey80", colour = "#54d8e0"))
     #annotation_custom(picc, xmin=5, xmax=7, ymin=30, ymax=45)
}

ggplot2::ggplot(iris, ggplot2::aes(x=Sepal.Length, y=Petal.Length))+
  ggplot2::geom_point()+
  theme_liu2()


# imgg <- png::readPNG("/Users/aashana/RcourseLAB4/images/liuLogo.png")
# picc <- as.raster(imgg, 10, 10, 10, 10, angle = 0, interpolate = TRUE)


