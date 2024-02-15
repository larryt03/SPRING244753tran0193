#' Title Generate Barplots of Random Samples
#'
#' @param n The number of samples to draw from the set
#' @param iter The number of iterations to repeat the sampling process. Default is 10.
#' @param time The time in seconds to pause between generating each barplot. Default is 0.5.
#'
#' @return Generates barplots
#' @export
#'
#' @examples mysample(n = 20, iter = 10, time = 1)
mysample=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )

    #release the table
    Sys.sleep(time)
  }
}
