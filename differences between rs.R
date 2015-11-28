#-------Differences between independent rs-----

zdifference<-function(r1, r2, n1, n2)
{zd<-(atanh(r1)-atanh(r2))/sqrt(1/(n1-3)+1/(n2-3))
 p <-1 - pnorm(abs(zd))
 print(paste("Z Difference: ", zd))
 print(paste("One-Tailed P-Value: ", p))
}

#-------Differences between dependent rs-----

tdifference<-function(rxy, rxz, rzy, n) 
{  df<-n-3
   td<-(rxy-rzy)*sqrt((df*(1 + rxz))/(2*(1-rxy^2-rxz^2-rzy^2+(2*rxy*rxz*rzy))))
   p <-pt(td, df)
   print(paste("t Difference: ", td))
   print(paste("One-Tailed P-Value: ", p))
}