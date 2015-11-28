#group.col is the column containing the grouping variable
#group.1 and group.2 are the two difference groups, listed in ""
#the name of the column for grouping MUST be "Group"
ttestfromMeans<- function (dat, group.1, group.2, outcome){
        #Calculte the mean, SD and N for each group
        x1<- mean(dat[dat$Group==group.1, outcome])
        x2<- mean(dat[dat$Group==group.2, outcome])
        sd1<- sd(dat[dat$Group==group.1, outcome])
        sd2<- sd(dat[dat$Group==group.2, outcome])
        n1<- length(dat[dat$Group==group.1, outcome])
        n2<- length(dat[dat$Group==group.2, outcome])
        
        df<- n1 + n2 - 2
        poolvar<-(((n1-1)*sd1^2)+((n2-1)*sd2^2))/df
        t<-(x1-x2)/sqrt(poolvar*((1/n1)+(1/n2)))
        sig<-2*(1-(pt(abs(t),df)))
        paste("t(df = ", df, ") = ", t, ", p = ", sig, sep = "")
}

        


        