mkPatr <- function( n ) {
    
  nc      <- n
  nr      <- 2 * 3 ^ (n-1)
  vals    <- c(0,1,2)

  mat     <- matrix(NA,ncol=nc,nrow=nr)
  mat[,1] <- sapply(c(0,1),rep,nr/2)

  if ( n > 1 ) {  
    for ( k in 1:(n-1) ) {
      mat[,k+1] <- sapply(vals,rep,3^(k-1))
    }
  }
  return(mat)
}

mkPatr3 <- function( n ) {
    
  nc      <- n
  nr      <- 3 ^ n
  vals    <- c(0,1,2)

  mat     <- matrix(NA,ncol=nc,nrow=nr)
  mat[,1] <- vals

  if ( n > 1 ) {  
    for ( k in 1:(n-1) ) {
      mat[,k+1] <- sapply(vals,rep,3^(k))
    }
  }
  return(mat)
}
