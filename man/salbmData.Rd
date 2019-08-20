\name{salbmData.}
\docType{data}
\alias{salbmData}
\title{The salbmData randomized trial.}
\description{This dataframe stores data from a repeated measures
  clinical trial.  There are two arms indicated by the trt variable.}
\details{ Each row represents the outcome for one subject at each of 4
  timepoints The outcome data is binary taking the values 0 or 1 when an
  outcome is observed and NA when an outcome in not observed. 
  \itemize{
  \item trt treatment indicator 1 is the control arm and 2 is the active
  arm.
  \item y1 score at baseline takes values 0, 1, or NA  
  \item y2 score at time = 2 takes values 0, 1, or NA 
  \item y3 score at time = 3 takes values 0, 1, or NA 
  \item y4 score at time = 4 takes values 0, 1, or NA 
}
The binary outcome is takes the values 0 and 1 with 1 representing the
outcome of interest. This data is simulated.  There are 100 observations
in each arm.
}
\usage{
data(salbmData)
}
\source{Data are simulated for the samon package}

