logisticPseudoR2s<- function(log.model) {
  dev<- log.model$deviance
  null.dev<- log.model$null.deviance
  model.n<- length(log.model$fitted.values)
  R.l<- 1- dev/null.dev
  R.cs<- 1- exp(-(null.dev - dev)/model.n)
  R.n<- R.cs / (1- (exp (-(null.dev/ model.n))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3), "\n")
}