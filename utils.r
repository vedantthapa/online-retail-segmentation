# train kmeans for different values of k 
multiKmeans <- function(data,lo,hi, iter)
{
   err=array((hi-lo+1)*2,dim=c((hi-lo+1),2))
   for(i in lo:hi)
   {
       rowNum=i-lo+1
       err[rowNum,1]=i
       set.seed(42)
       err[rowNum,2]=kmeans(data,i,iter)$tot.withinss
   }
   err[,2]
}

# create detect outlier function
detect_outlier <- function(x) {
  Quantile1 <- quantile(x, probs=.25)
  Quantile3 <- quantile(x, probs=.75)
  IQR = Quantile3-Quantile1
  
  x > Quantile3 + (IQR*1.5) | x < Quantile1 - (IQR*1.5)
}


# create remove outlier function
remove_outlier <- function(dataframe,
                           columns=names(dataframe)) {
  for (col in columns) {
    dataframe <- dataframe[!detect_outlier(dataframe[[col]]), ]
  }
  dataframe
}

