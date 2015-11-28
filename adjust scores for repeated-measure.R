##Function to adjust pairs of variables obtained from a repeated measures desgin
#coln1 is the number of the first column, coln2 is the second columns
rmMeanAdjust<- function(dataframe, coln1 =1, coln2=2){
  var.names<- names(dataframe)
  p.mean<- (dataframe[,coln1]+dataframe[,coln2])/2
  grandmean<- mean(c(dataframe[,coln1], dataframe[,coln2]))
  adj<- grandmean-p.mean
  varA_adj<- dataframe[,coln1]+adj
  varB_adj<- dataframe[,coln2]+adj
  output<- data.frame(varA_adj, varB_adj)
  names(output)<- c(paste(var.names[1], "Adj", sep = "_"), paste(var.names[2], "Adj", sep = "_"))
  return(output)
}