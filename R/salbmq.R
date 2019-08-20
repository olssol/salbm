salbmq <- function( j, alpha, main, bs ) {
   e1   <- main[ main[,"alpha"] == alpha, j] 
   vals <- bs[ bs[,"alpha"] == alpha, j ]
   q1   <- quantile(vals,0.025)
   q2   <- quantile(vals,0.975)
   q3   <- quantile(abs(vals-e1), 0.95)
   outv <- c( alpha, j, e1, q1, q2, e1-q3, e1+q3 )
   return(outv)
}
