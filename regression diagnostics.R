regression.diagnostics<- function(model, data.set, number.of.predictors){
        data.set$residuals<-resid(model)
        data.set$standardized.residuals<- rstandard(model)
        data.set$studentized.residuals<-rstudent(model)
        data.set$cooks.distance<-cooks.distance(model)
        data.set$dfbeta<-dfbeta(model)
        data.set$dffit<-dffits(model)
        data.set$leverage<-hatvalues(model)
        data.set$covariance.ratios<-covratio(model)
        data.set
        
        ## Large residuals
        data.set$large.residuals<- data.set$standardized.residuals>2 | data.set$standardized.residuals< -2
        large.residual.percent<- sum(data.set$large.residuals)/nrow(data.set)*100
        cat("Residual >2           : ", large.residual.percent, "%. Should be <5%\n")

        ## Extra large residuals
        data.set$extra.large.residuals<- data.set$standardized.residuals>3 | data.set$standardized.residuals< -3
        extra.large.residual.percent<- sum(data.set$extra.large.residuals)/nrow(data.set)*100
        cat("Residual >3           : ", extra.large.residual.percent, "%. Should be <1%\n")
        
        ## Show the actual cases which don't meet criteria for residuals
        #cat("Cases that don't meet the >2 cutoff:", data.set[data.set$large.residual,1])
        cat("\nCases that don't meet the >3 cutoff:", data.set[data.set$extra.large.residual, 1], "\n\n")
        
        ## For cases that have large residuals, examine the cooks distance, leverage and covariance ratios
        #to see if there is undue influence
        
        case.wise.diagnostics<-data.set[data.set$large.residuals, c("cooks.distance", "leverage", "covariance.ratios")]
        leverage.cutoff<- 0.06 * (number.of.predictors + 1/nrow(data.set))
        cooks.cutoff<- 1
        cov.min<- 1-(3*(number.of.predictors+1)/nrow(data.set))
        cov.max<- 1+(3*(number.of.predictors+1)/nrow(data.set))
        
        cat("Leverage cutoff (3x average leverage): ", leverage.cutoff, "\n")
        cat("Covariance Range: ", "<", cov.min, "OR",">", cov.max, "\n")
        cat("Cooks Cutoff: ", ">", cooks.cutoff, "\n\n")

        subset(case.wise.diagnostics, cooks.distance>cooks.cutoff | 
                       leverage>0.01 | 
                       covariance.ratios< cov.min | covariance.ratios > cov.max)
         
}