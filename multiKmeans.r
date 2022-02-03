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

