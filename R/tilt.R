# tilt the values in results by alpha 
tilt <- function( alpha, results ) {
  Res    <- results
  len    <- length(results)
  nn     <- round(log(len,base=3))
  Ealpha <- exp(alpha)
  for ( j in 1:nn ) {
    len  <- length(Res)
    s0   <- seq( 1, len, by = 3 )
    s1   <- s0 + 1
    s2   <- s0 + 2
    A    <- Res[ s0 ]  
    B    <- Res[ s1 ]  
    C    <- Res[ s2 ]
    T    <- A + Ealpha * B
    X <- (( T == 0 ) & ( C > 0 ))
    if ( any(X) > 0 ) {
      A[X] <- A[X] + 1 * C[X] / ( 1 + Ealpha ) 
      B[X] <- B[X] + 1 * Ealpha * C[X] / ( 1 + Ealpha )
    }
    X <- (( T > 0 ) & ( C > 0 ))
    if ( any(X) > 0 ) { 
      A[X] <- A[X] + A[X] * C[X] / T[X]
      B[X] <- B[X] + B[X] * Ealpha * C[X] / T[X]
    }
    Res <- c(A,B)
  }
  w          <-  Res
  len        <-  length(Res)
  sm         <-  rep(0,len)
  Results    <-  rep(NA,2*nn+1)
  Results[1] <-  alpha
  for ( j in 1:nn ) {
    yj                 <-  rep(c(rep( 0, 2^(j-1) ), rep( 1, 2^(j-1))), len / (2^j)) 
    Results[j+1]       <-  weighted.mean( yj, w = w )
    sm                 <-  sm + yj
    Results[nn+1+j]    <-  weighted.mean( sm, w = w )
  }
  return(Results)
}
