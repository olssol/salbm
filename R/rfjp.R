# random forest joint prob distribution
rfjp <- function( data, ntree = 1000, seed = -1, nodesize = 1 )
  {
     colnms  <-  names(data)
     nrows   <-  nrow(data)
     ncols   <-  ncol(data)
       
     jp <- c( mean(data[,1]==0), mean(data[,1]==1), mean(data[,1]==2) )
     for ( j in 2:ncols ) {
       inames       <- colnms[1:(j-1)]
       modelF       <- paste( colnms[j], "~", paste( inames, sep = " ", collapse="+" ) ) 
       TData        <- data[, colnms[1:j] ]
       rfout        <- rfsrc( as.formula(modelF), data = TData, ntree= ntree, seed = seed - j, ensemble = "all", bootstrap = "by.root", sampsize = nrows, samptype = "swr", nodesize = nodesize )
 
       ndata        <- data.frame(mkPatr3( j - 1 ))
       names(ndata) <- inames
       ndata[]      <- lapply( ndata, function(x) factor(x, levels=c("0","1","2")))
  
       pre          <- predict.rfsrc(rfout,newdata=ndata)$predicted
       jp           <- as.vector(jp) * pre
     }  
     jp  <- as.vector(jp)
     return( jp ) 
  }
