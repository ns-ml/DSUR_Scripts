ttestEffectSize <- function(ttest){
        t<- ttest$statistic[[1]]
        df<- ttest$parameter[[1]]
        r<- sqrt(t^2/(t^2+df))
        cat("r =", round(r, 3))
}