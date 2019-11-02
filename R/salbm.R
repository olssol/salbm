salbm <- function( data, trtname = "trt", trtlev = c(1,2),
                  rf.ntree = 1000, primeseeds = c(-8,-9), rf.nodesize = 1,
                  nbootstraps = 1000, alphas ) 
{
  data        <-   as.data.frame(data)
  dnames      <-   names(data)
  onames      <-   dnames[ dnames != trtname ]
  data1       <-   data[ data[,trtname] == trtlev[1], onames]
  data2       <-   data[ data[,trtname] == trtlev[2], onames]

  data1[ is.na(data1) ] <- 2
  data2[ is.na(data2) ] <- 2

  data1[] <- lapply( data1, function(x) factor(x,levels=c("0","1","2"))) 
  data2[] <- lapply( data2, function(x) factor(x,levels=c("0","1","2")))

  ## Fix the seeds if necessary
  if ( is.null(primeseeds)     ) primeseeds <- c(-8,-9)
  if ( length(primeseeds) == 1 ) primeseeds <- rep(primeseeds,2)
  primeseeds[ primeseeds  > 0 ] <- -1 * primeseeds[ primeseeds > 0 ] 
  primeseeds[ primeseeds == 0 ] <- -6

  ## Treatment 1
  colnms1      <-   names(data1)
  nrows1       <-   nrow(data1)
  ncols1       <-   ncol(data1)

  # random forest joint prob distribution
  jp1          <-   rfjp( data = data1, ntree = rf.ntree, seed = primeseeds[1], nodesize = rf.nodesize )

  tiltResults1 <-   lapply( alphas, function(x) tilt(x, jp1) )
  tiltResults1 <-   do.call(rbind,tiltResults1)
  tiltResults1 <-   cbind(1,tiltResults1)
  tiltResults1 <-   as.data.frame(tiltResults1)
  names(tiltResults1) <- c("trt","alpha", paste( "E", 1:ncols1, sep=""), paste( "Esum", 1:ncols1, sep=""))

  ## Treatment 2
  colnms2      <-   names(data2)
  nrows2       <-   nrow(data2)
  ncols2       <-   ncol(data2)

  # random forest joint prob distribution
  jp2          <-   rfjp( data = data2, ntree = rf.ntree, seed = primeseeds[2], nodesize = rf.nodesize )

  tiltResults2 <-   lapply( alphas, function(x) tilt(x, jp2) )
  tiltResults2 <-   do.call(rbind,tiltResults2)
  tiltResults2 <-   cbind(2,tiltResults2)
  tiltResults2 <-   as.data.frame(tiltResults2)
  names(tiltResults2) <- c("trt","alpha", paste( "E", 1:ncols2, sep=""), paste( "Esum", 1:ncols2, sep=""))

  rnames       <-   c(paste( "E", 1:ncols1, sep=""), paste( "Esum", 1:ncols1, sep=""))
  tiltResultsD <-   tiltResults2
  tiltResultsD[,rnames] <- tiltResults2[,rnames] - tiltResults1[,rnames]
  tiltResultsD[,"trt"]  <- -1

  if ( nbootstraps > 0 ) {
    repseeds1   <-   -1 * sample(11:9999, nbootstraps, replace = TRUE )  
    repseeds2   <-   -1 * sample(11:9999, nbootstraps, replace = TRUE )
      
    pat         <-   mkPatr3(ncols1)
    jp1         <-   cbind( pat, jp1)
    indx        <-   sample( 1:nrow(jp1), nbootstraps * nrows1, prob = jp1[,ncol(jp1)], replace=TRUE)
    ix2         <-   lapply( 1:nbootstraps, function(x) rep(x,nrows1) )
    ix2         <-   unlist(ix2)
    samples1    <-   jp1[ indx, 1:ncols1 ]
    samples1    <-   cbind(ix2,samples1)

    lout1 <- lapply( 1:nbootstraps, dosamp, samples=samples1, rf.ntree = rf.ntree, rfseeds = repseeds1, rf.nodesize = rf.nodesize, alphas = alphas )
    lout1 <- do.call( rbind, lout1 )
    onm   <- names(lout1) 
    lout1 <- cbind(1,lout1)
    names(lout1) <- c("trt",onm)

    CI1 <- salbmCI( alphas, main = tiltResults1, bs = lout1 )
    CI1 <- as.data.frame(CI1)
    CI1[,"CType"] <- rnames[ CI1[,2] - 2 ]
    names(CI1) <- c("alpha", "type", "Estimate", "CILow1", "CIHigh1", "CILow2", "CIHigh2", "CType")

    for ( jj in 1:ncols1 ) {
       CI1[ CI1[,"CILow2" ] < 0 & CI1[ , "CType"] == paste("E",jj,sep=""), "CILow2" ] <- 0
       CI1[ CI1[,"CILow2" ] > 1 & CI1[ , "CType"] == paste("E",jj,sep=""), "CILow2" ] <- 1
       CI1[ CI1[,"CIHigh2"] < 0 & CI1[ , "CType"] == paste("E",jj,sep=""), "CIHigh2"] <- 0
       CI1[ CI1[,"CIHigh2"] > 1 & CI1[ , "CType"] == paste("E",jj,sep=""), "CIHigh2"] <- 1
    }
    
    jp2         <-   cbind( pat, jp2)
    indx        <-   sample( 1:nrow(jp2), nbootstraps * nrows2, prob = jp2[,ncol(jp2)], replace=TRUE)
    ix2         <-   lapply( 1:nbootstraps, function(x) rep(x,nrows2) )
    ix2         <-   unlist(ix2)
    samples2    <-   jp2[ indx, 1:ncols2 ]
    samples2    <-   cbind(ix2,samples2)

    lout2 <- lapply( 1:nbootstraps, dosamp, samples=samples2, rf.ntree = rf.ntree, rfseeds = repseeds2, rf.nodesize = rf.nodesize, alphas = alphas )
    lout2 <- do.call( rbind, lout2 )
    onm   <- names(lout2) 
    lout2 <- cbind(2,lout2)
    names(lout2) <- c("trt",onm)

    CI2 <- salbmCI( alphas, main = tiltResults2, bs = lout2 )
    CI2 <- as.data.frame(CI2)
    CI2[,"CType"] <- rnames[ CI2[,2] - 2 ]
    names(CI2) <- c("alpha", "type", "Estimate", "CILow1", "CIHigh1", "CILow2", "CIHigh2", "CType")

    for ( jj in 1:ncols1 ) {
       CI2[ CI2[,"CILow2" ] < 0 & CI2[ , "CType"] == paste("E",jj,sep=""), "CILow2" ] <- 0
       CI2[ CI2[,"CILow2" ] > 1 & CI2[ , "CType"] == paste("E",jj,sep=""), "CILow2" ] <- 1
       CI2[ CI2[,"CIHigh2"] < 0 & CI2[ , "CType"] == paste("E",jj,sep=""), "CIHigh2"] <- 0
       CI2[ CI2[,"CIHigh2"] > 1 & CI2[ , "CType"] == paste("E",jj,sep=""), "CIHigh2"] <- 1
    }
    
    rnames           <-  c(paste( "E", 1:ncols1, sep=""), paste( "Esum", 1:ncols1, sep=""))
    loutD            <-  lout2
    loutD[,rnames]   <-  loutD[,rnames] - lout1[,rnames]
    loutD[,"trt"]    <-  -1

    CID <- salbmCI( alphas, main = tiltResultsD, bs = loutD )
    CID <- as.data.frame(CID)
    CID[,"CType"] <- rnames[ CID[,2] - 2 ]
    names(CID) <- c("alpha", "type", "Estimate", "CILow1", "CIHigh1", "CILow2", "CIHigh2", "CType")
    
    return(list(Results1=tiltResults1,Results2=tiltResults2,ResultsD=tiltResultsD,bootstraps1=lout1,bootstraps2=lout2,bootstrapsD=loutD,CI1=CI1,CI2=CI2,CID=CID))
  }
  return(list(Results1=tiltResults1,Results2=tiltResults2,ResultsD=tiltResultsD))
}
