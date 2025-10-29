# Automatically generated from the noweb directory
predict.coxph <- function(object, newdata, 
                          type=c("lp", "risk", "expected", "terms", "survival"),
                          se.fit=FALSE, na.action=na.pass,
                          terms=names(object$assign), collapse, 
                          reference=c("strata", "sample", "zero"), ...) {
  
  Call <- match.call()
  type <-match.arg(type)

  else survival <- FALSE
  
  n <- object$n
  Terms <-  object$terms
  
  if (type != 'expected') Terms2 <- delete.response(Terms)
  
  na.action.used <- object$na.action
  n <- length(object$residuals)
  
  reference <- match.arg(reference)
  
  

    oldstrat <- rep(0L, n)
    offset <- 0
    use.x <- FALSE


n2 <- n




else if (reference!= "zero") 
        newx <- newx - rep(object$means, each=nrow(newx))
    }
    
    if (type=='lp' || type=='risk') {
      if (use.x) pred <- drop(newx %*% coef)
      if (se.fit) se <- sqrt(rowSums((newx %*% object$var) *newx))
      
      if (type=='risk') {
        pred <- exp(pred)
        if (se.fit) se <- se * sqrt(pred)  # standard Taylor series approx
      }
    }