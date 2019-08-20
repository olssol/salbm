\name{salbm}
\alias{salbm}
\title{Sensitivity Analysis for longitudinal binary missing data}
\description{Given data from one arm of a repeated measures clinical
  trial with binary outcomes, produces estimates of the expected value
  of the outcome at the final time-point for a range of  sensitivity
  parameters.}
\usage{salbm(data, trtname = "trt", trtlev = c(1,2), rf.ntree = 1000,
rf.seed = -1, rf.sampsize = floor(nrow(data)/4), rf.nodesize = 1,
nbootstraps = 1000, alphas )
}
\arguments{
  \item{data}{ a dataframe in wide fromat with a column indicating
    treatment group and columns of outcome measures in time order.}
  \item{trtname}{ The name of the treatment column in data.}
  \item{trtlev}{ The levels that the treatment name takes.}
  \item{rf.ntree}{ Number of trees to pass to the rfsrc function.}
  \item{rf.seed}{ Seed to pass to rfsrc function.}
  \item{rf.sampsize}{ Size of sample passed to rfsrc.}
  \item{rf.nodesize}{ Minimum nodesize passed to rfsrc.}
  \item{nbootstraps}{ Number of bootstraps to perform.}
  \item{alphas}{ A vector of sensitivity parameters.}
}

\details{
 The dataframe data is split into the two treatments arms using the
 column trtname. The remaining columns are coded as 0 or 1 for the
 binary outcome and should be in time order.  Unobserved values are
 coded as NA.

 Using the randomForestSRC package is used to estimate the joint
 distribution of the outcome values (including the non-observed outcome
 values).  The sensitivity parameters are then used to tilt the outcome
 values to produce the underlying joint distribution of the underlying
 observed outcomes. 
}
\value{
 salbm returns a list which includes the following:
  \item{Results1}{ the results from treatment 1.}
 
  \item{Results2}{ the results from treatment 2.}

  \item{ResultsD}{ the results from treatment 2 minus treatment 1.}

  \item{bootstraps1}{ Bootstrap results from treatment 1} 

  \item{bootstraps2}{ Bootstrap results from treatment 2} 

  \item{bootstrapsD}{ Bootstrap results from treatment 2 minus treatment
  1} 
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

  nbootstraps    =     5,
  alphas         =  -1:1 )
}
