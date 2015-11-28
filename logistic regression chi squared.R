#Function that calculates the chi squared statistic and it's significance
#second model is optional. If only one model is input then it would calculate
#relative to the null (frequentist)

log.regression.chisq<- function(model1, model2 = lm(rnorm(20) ~ 0)){
        if (is.empty.model(model2)) {
                modelChi<- model1$null.deviance - model1$deviance
                chidf<- model1$df.null - model1$df.residual
                chisq.prob<- 1-pchisq(modelChi, chidf)
                cat("Chi Squared with df", chidf, "= ", modelChi,
                    "\np=", round(chisq.prob, 3))
        }
        else {
                modelChi<- model1$deviance - model2$deviance
                chidf<- model1$df.residual - model2$df.residual
                chisq.prob<- 1-pchisq(modelChi, chidf)
                cat("Chi Squared with df", chidf, "= ", modelChi,
                    "\np=", round(chisq.prob, 3))
        }
}