\name{salbmCI}
\alias{salbmCI}
\title{Sensitivity Analysis for longitudinal binary missing data}
\description{Produces confidence intervals for a salbm object.}
\usage{salbmCI( alphas, main, bs)}
\arguments{
  \item{alphas}{ vector or sensitivity parameters.}
  \item{main}{ set of estimates from the salbm function.}
  \item{bs}{ set of bootstrap estimates from the salbm function.}
}

\details{
 Creates confidence limits from estimates and bootstrap estimates
}
\value{
 salbmCI: returns a table which includes the following:

 { for each alpha and timepoint an estimate and lower and
 upper 95 percent confidence limits and lower and upper symmetric
 confindence limits.}
}
\seealso{
  The salbm_userDoc.pdf file in the Examples subdirectory.
}  
\examples{
data("salbmData")

Results <- salbm(
  data           =  salbmData,
  trtname        =  "trt",       # treatment indicator
  trtlev         =  c(1,2),      # treatment values 
    
  rf.ntree       =    25,        # num trees in the forest
  rf.seed        =  -172,        
  rf.sampsize    =    90,         
  rf.nodesize    =     3,

  nbootstraps    =    10,
  alpha          =  -1:1 )

CID <- salbmCI( -1:1, Results$ResultsD, Results$bootstrapsD ) 
}
