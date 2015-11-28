diagnostic.logistric.reg<- function(new.model, data.set, number.predictors){
  #Standardized residuals: no more than 5% should be greater than 2
  #and no more than 1% greater than 2.5. Cases geater than 3 should be inspected
  data.set$standardized.residuals<- rstandard(new.model)
  data.set$large.residuals<- data.set$standardized.residuals>2 | data.set$standardized.residuals < -2
  large.residual.percent<- sum(data.set$large.residuals)/nrow(data.set)*100
  cat("Residual >2           : ", large.residual.percent, "%\n")
  
  #Leverage: number of predictors + 1 divided by sample size
  #Values should not exceed 2x or 3x (depending on who you ask)
  expected.leverage<- (number.predictors+1)/nrow(data.set)
  data.set$leverage<- hatvalues(new.model)
  data.set$large.leverage<- data.set$leverage> 3*expected.leverage
  cat("Expected leverage     : ", expected.leverage, "\n")
  cat("Cases with 3x leverage: ", sum(data.set$large.leverage), "\n")
  
  #DFBeta values should be less than 1
  data.set$dfbeta<- dfbeta(new.model)
  data.set$large.dfbeta<- data.set$dfbeta>1
  cat("Cases with DFbeta > 1 : ", sum(data.set$large.dfbeta), "\n")
  
}