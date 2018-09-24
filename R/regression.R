#' Function to implement multiple linear regression without using lm function of R
#' @name linreg
#' @param formula as formula object
#' @param data as data.frame
#' @return an object of class linreg as an S3 class
#' @export linreg
#' @examples linreg(formula = Petal.Length ~ Species, data = iris)
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
    pvalues <- 2*pt(abs(tvalues),degreeoffree,lower.tail = FALSE)
    
    #building return object
    calculatedstatistics <- list("regressioncoeffs"=regressioncoeffs, 
                                 "fittedvals"=fittedvals, 
                                 "residualvals"=residualvals, 
                                 "degreeoffree"=degreeoffree, 
                                 "residualvariance"=residualvariance, 
                                 "varianceofcoeff"=varianceofcoeff, 
                                 "tvalues"=tvalues, 
                                 "pvalues"=pvalues,
                                 "call"=match.call())
    #update class of the object
    class(calculatedstatistics) <- "linreg"
    return(calculatedstatistics)
  }
  else
  {
    stop("The first argument should be of type formula")
  }
}


#' Function to implement print method for linreg class
#' @name print
#' @param object of class linreg
#' @return prints text to replicate the print.lm behaviour
#' @examples print(linreg(formula = Petal.Length ~ Species, data = iris))
print <- function(x) {
  UseMethod("print",x)
}

print.linreg <- function(x){
  
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  cat("Coefficients:\n")
  print(format(t(x[["regressioncoeffs"]])[1,]))
  cat("\n")
  
}


#' Function to implement plot methd for linreg class using ggplot
#' @name plot
#' @param object of class linreg
#' @return prints the plots to replicate the plot.lm behaviour
plot <- function(x) {
  UseMethod("plot",x)
}

plot.linreg <- function(x){
  op <- par(ask=TRUE)
  for (i in 1:2)
  {
    inputdataplot <- data.frame("Residuals"=x[["residualvals"]],"FittedValues"=x[["fittedvals"]])
    if(i==1){
      print(ggplot(inputdataplot,aes(x=FittedValues, y=Residuals)) + 
              geom_point(shape=1) +
              stat_summary(fun.y=median, color="red", geom="line", size=1) +
              scale_x_continuous(name = "Fitted Values") +
              scale_y_continuous(name = "Residuals", limits=c(-1.5,1.5)) +
              ggtitle("Residuals vs Fitted"))
    }
    
    if(i==2){
      print(ggplot(inputdataplot,aes(x=FittedValues, y=sqrt(abs((x[["residualvals"]] - mean(x[["residualvals"]])) / as.vector(sqrt(x[["residualvariance"]])))))) + 
              geom_point(shape=1) +
              stat_summary(fun.y=mean, color="red", geom="line", size=1) +
              scale_x_continuous(name = "Fitted Values") +
              scale_y_continuous(name = expression(sqrt("Standardized Residuals")), limits=c(0,2)) +
              ggtitle("Scale-Location"))
    }
    on.exit(par(op))
    i<-i+1
  }
}

#' Function to implement resd method for linreg class
#' @name resid
#' @param object of class linreg
#' @return replicate the resid.lm behaviour
resid <- function(x) {
  UseMethod("resid",x)
}
resid.linreg <- function(x) {
  as.vector(x[["residualvals"]])
}

#' Function to implement pred method for linreg class
#' @name pred
#' @param object of class linreg
#' @return replicate the pred.lm behaviour
pred <- function(x) {
  UseMethod("pred",x)
}

pred.linreg <- function(x) {
  x[["fittedvals"]]
}

#' Function to implement coef method for linreg class
#' @name coef
#' @param object of class linreg
#' @return replicate the coef.lm behaviour
coef <- function(x) {
  UseMethod("coef",x)
}

coef.linreg <- function(x) {
  c(t(x[["regressioncoeffs"]]))
}

# summary method for linreg classs
summary <- function(x) {
  UseMethod("summary",x)
}

summary.linreg <- function(x) {
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), 
      "\n\n", sep = "")
  cat("Coefficients:\n")
  m<-matrix(,3,5)
  colnames(m)<-c("Estimate","Std. Error","t value","p value","")
  rownames(m)<-row.names(x[["regressioncoeffs"]])
  
  pvals<-x[["pvalues"]]
  symbolforpvalues<-symnum(pvals, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1,1), symbols = c('***', '**', '*', '.', ' '))
  
  m[,1]<-round(x[["regressioncoeffs"]][,1],5)
  m[,2]<-round(sqrt(x[["varianceofcoeff"]]),5)
  m[,3]<-round(x[["tvalues"]][,1],2)
  m[,4]<- pvals
  m[,5]<- as.vector(symbolforpvalues[,1])
  m<-noquote(m)
  print(m)
  cat("\n---\nSignif. codes:",attributes(symbolforpvalues)$"legend")
  cat("\n\nResidual standard error:",round(sqrt(x[["residualvariance"]][1,1]),2),"on", x[["degreeoffree"]] , "degrees of freedom")
}


#' Function to implement QR Decomposition calculations
#' @name qrlinreg
#' @param formula as formula object
#' @param data as data.frame
#' @return an object of class qrlinreg as an S3 class
#' @export qrdecompositioncalculation
#' @examples linreg(formula = Petal.Length ~ Species, data = iris)
qrdecompositioncalculation <- function(formula, data){
  if(class(formula)=="formula")
  {
    X <- model.matrix(formula, data)
    y <- data[,all.vars(formula)[1]] #pick up independent variable
    
    QR <- qr(X)
    beta <- solve(QR,y)
    y_hat <- X %*% beta
    res <- as.vector(y-y_hat)
    se_sq<-sum(res^2)/(nrow(X)-QR$rank)
    beta_var<-diag(chol2inv(QR$qr)*se_sq)
    
    qrcalculatedstatistics <- list("qrregressioncoeffs"=beta, 
                                   "qrvarianceofcoeff"=beta_var)
    class(qrcalculatedstatistics) <- "qrlinreg"
    return(qrcalculatedstatistics)
  }
  else
  {
    stop("The first argument should be of type formula")
  }
}