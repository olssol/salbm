salbmCI <- function( alphas, main, bs ) {
  for ( a in alphas ) {
    lout <- lapply( 3:(ncol(bs)-1), salbmq, alpha = a, main = main, bs = bs )
    lout <- do.call( rbind, lout )

    if ( a == alphas[1] ) outmat <- lout
    else outmat <- rbind( outmat, lout )
  }
  return(outmat)
}
