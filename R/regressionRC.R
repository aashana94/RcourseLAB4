#' Making a linear regression function using RC(Reference class)
#'
#' @param formula as object
#' @param data as numeric
#'
#' @return linreg
#' @export
#' @examples linreg(formula = Petal.Length ~ Species, data = iris)
linregg <- setRefClass("linregg",
                         fields = list(
                           formula="formula",
                           data="character",
                           regressioncoeffs="matrix",
                           fittedValues="matrix",
                           residuals="matrix",
                           df="integer",
                           residualVar="numeric",
                           coefficientsVar = "numeric",
                           t_values = "matrix"
                           ),
                         methods= list(

                           #initialize all fields
                           initialize = function(formula,data){
                             # making a model matrix for the given data
                             X <- model.matrix(formula, data)

                             # separarting out independent variables
                             y <- data[,all.vars(formula)[1]]

                             # regressions coef ß^=inverse(transpose(X)*X) * transpose(X)*y
                             regressioncoeffs <<- (solve(t(X) %*% X)) %*% (t(X) %*% y)

                             # fitted values y^=Xß^
                             fittedValues <<- X %*% regressioncoeffs

                             # Residuals eˆ= y − yˆ = y − X βˆ (X βˆ is the fitted values we calculated above)
                             residuals <<- y - fittedValues

                             # degrees of freedom df = n − p (n - k -1)
                             # n:number of observations, p: number of parameters in the model, k:number of independent variables
                             df <<- nrow(data)-ncol(X)

                             # The residual variance: σˆ2 = eTe / df
                             residualVar <<- as.numeric((t(residuals)%*%residuals)/df)

                             # variance of the regression coefficients: Var(β)=σˆ2(t(X).X)^-1
                             coefficientsVar <<-residualVar * diag(solve(t(X)%*%X))

                             #t-values for each coefficient: tβ =  βˆ/ squareroot(􏰂V ar(β)ˆ)
                             t_values <<- regressioncoeffs / sqrt(coefficientsVar)

                             formula <<- formula

                             data <<- deparse(substitute(data))

                           },
                           print <- function(){
                             "Show formula and coeffients"
                           }
                         )
  )
linregg(formula = Petal.Length ~ Species, data = iris)