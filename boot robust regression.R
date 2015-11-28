##Page 850 DSUR
bootReg <- function (formula, data, i)
{
        d <- data [i,]
        fit <- lm(formula, data = d)
        return(coef(fit))
}