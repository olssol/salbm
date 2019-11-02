# dosamp: read a sample from samples based on the sampno column, tilt by alphas
dosamp <- function( sampno, samples,  rf.ntree = 1000, rfseeds, rf.nodesize = 1, alphas = c(0) )  {
    print(sampno)
   ncolsamp               <-   ncol(samples) - 1
   sample                 <-   samples[ samples[,1] == sampno, 2:(ncolsamp+1) ]

   sample                 <-   as.data.frame(sample)
   colnms                 <-   paste("y",1:ncolsamp,sep="")
   names(sample)          <-   colnms
   sample[]               <-   lapply(sample,function(x) factor(x,levels=c("0","1","2")))

   # random forest joint prob distribution
   jp                     <-   rfjp( data = sample, ntree = rf.ntree, seed = rfseeds[sampno], nodesize = rf.nodesize )
   tiltResults            <-   lapply( alphas, function(x) tilt(x,jp) )
   tiltResults            <-   do.call(rbind,tiltResults)
   tiltResults            <-   as.data.frame(tiltResults)
   tiltResults[,"sample"] <-   sampno
   names(tiltResults)     <-   c("alpha", paste( "E", 1:ncolsamp, sep=""), paste( "Esum", 1:ncolsamp, sep=""), "sample")
   return(tiltResults)
}
