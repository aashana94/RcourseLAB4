linreg <- function(formula, data){
  if(class(formula)=="formula")
  {
    X <- model.matrix(formula, data)
    y <- iris[,all.vars(formula)[1]] #pick up independent variable
    
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



