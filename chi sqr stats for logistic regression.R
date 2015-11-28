logistic.chi<- function (new.model){
  model.chi<- new.model$null.deviance - new.model$deviance
  chidf<- new.model$df.null - new.model$df.residual
  chisq.prob<- 1- pchisq(model.chi, chidf)
  cat("Model Chi Sqr   :", model.chi, "\n")
  cat("Model df        :", chidf, "\n")
  cat("Probability     :", chisq.prob, "\n")
}