#' Get treatment effect
#'
#'
#'
#'
#'
#' @export
#'
salbmCross <- function(rsts, tj = NULL) {
    alphas  <- rsts$Results1$alpha
    M1      <- rsts$Results1
    BS1     <- rsts$bootstraps1
    M2      <- rsts$Results2
    BS2     <- rsts$bootstraps2
    NT      <- (ncol(rsts$Results1)-2)/2

    if (is.null(tj)) {
        tj = NT
    } else {
        stopifnot(tj <= NT & tj > 0)
    }

    nn      <-  length(alphas)^2
    CMat    <-  matrix(NA, nrow=nn, ncol = 12 )
    pos     <-  0
    for ( i in 1:length(alphas) )  {
        a1      <-  alphas[i]
        M1a     <-  M1  [ M1[,"alpha"] == a1,       ]
        BS1a    <-  BS1 [ BS1[,"alpha"] == a1,      ]
        EEst1a  <-  M1a [ , paste("E",tj,sep="")    ]
        EBEst1a <-  BS1a[ , paste("E",tj,sep="")    ]
        SEst1a  <-  M1a [ , paste("Esum",tj,sep="") ]
        SBEst1a <-  BS1a[ , paste("Esum",tj,sep="") ]
        for ( j in 1:length(alphas) ) {
            a2      <-  alphas[j]
            M2a     <-  M2  [ M2[,"alpha"] == a2,       ]
            BS2a    <-  BS2 [ BS2[,"alpha"] == a2,      ]
            EEst2a  <-  M2a [ , paste("E",tj,sep="")    ]
            EBEst2a <-  BS2a[ , paste("E",tj,sep="")    ]
            SEst2a  <-  M2a [ , paste("Esum",tj,sep="") ]
            SBEst2a <-  BS2a[ , paste("Esum",tj,sep="") ]

            e        <-  EEst2a  - EEst1a
            evals    <-  EBEst2a - EBEst1a
            eq1      <-  quantile(evals,0.025)
            eq2      <-  quantile(evals,0.975)
            eq3      <-  quantile(abs(evals-e), 0.95)

            s        <-  SEst2a  - SEst1a
            svals    <-  SBEst2a - SBEst1a
            sq1      <-  quantile(svals,0.025)
            sq2      <-  quantile(svals,0.975)
            sq3      <-  quantile(abs(svals-s), 0.95)

            pos          <-  pos + 1
            CMat[ pos, ] <- c( a1, a2, e, eq1, eq2, e - eq3, e + eq3, s, sq1, sq2, s - sq3, s + sq3 )
        }
    }
    colnames(CMat) <- c( "alpha1", "alpha2",
                        "E",  "Elb1", "Eub1",  "Elb2",  "Eub2",
                        "ES", "ESlb1", "ESub1", "ESlb2", "ESub2")
    return(CMat)
}


#' Get treatment effect
#'
#'
#'
#'
#'
#' @export
#'
salbmCombine <- function(rsts, arms = c("Arm 0", "Arm 1")) {
    M1            <-   rsts$Results1
    M2            <-   rsts$Results2
    BS1           <-   rsts$bootstraps1
    BS2           <-   rsts$bootstraps2

    alphas        <-   unique(M1[,"alpha"])
    ncols         <-   (ncol(M1) - 2)/2
    rnames        <-   c(paste( "E", 1:ncols, sep=""), paste( "Esum", 1:ncols, sep=""))
    onames        <-   c("alpha", "type", "Estimate",
                         "CILow1", "CIHigh1", "CILow2", "CIHigh2", "CType", "J", "Group")

    mb12          <- list(list(M = M1, BS = BS1),
                          list(M = M2, BS = BS2))

    rst <- NULL
    for (i in 1:2) {
        CI1           <-   salbmCI(alphas=alphas, main = mb12[[i]]$M, bs = mb12[[i]]$BS)
        CI1           <-   as.data.frame(CI1)
        CI1[,"CType"] <-   rnames[CI1[,2] - 2]
        CI1[,"J"]     <-   0:(ncols - 1)
        CI1[,"Group"] <-   arms[i]
        names(CI1)    <-   onames

        for ( jj in 1:ncols ) {
            CI1[ CI1[,"CILow2" ] < 0 & CI1[ , "CType"] == paste("E",jj,sep=""), "CILow2" ] <- 0
            CI1[ CI1[,"CILow2" ] > 1 & CI1[ , "CType"] == paste("E",jj,sep=""), "CILow2" ] <- 1
            CI1[ CI1[,"CIHigh2"] < 0 & CI1[ , "CType"] == paste("E",jj,sep=""), "CIHigh2"] <- 0
            CI1[ CI1[,"CIHigh2"] > 1 & CI1[ , "CType"] == paste("E",jj,sep=""), "CIHigh2"] <- 1
        }

        rst <- rbind(rst, CI1)
    }

    ## differences
    MD             <-  M2
    MD[,rnames]    <-  M2[,rnames] - M1[,rnames]
    BSD            <-  BS2
    BSD[,rnames]   <-  BS2[,rnames] - BS1[,rnames]

    CID <- salbmCI( alphas = alphas, main = MD, bs = BSD )
    CID <- as.data.frame(CID)
    CID[,"CType"] <- rnames[ CID[,2] - 2 ]
    CID[,"J"]     <- 0:(ncols - 1)
    CID[,"Group"] <- "Difference"
    names(CID)    <- onames
    rst           <- rbind(rst, CID)

    rst$Sum       <- rep(c("E", "Esum"), each = ncols)

    out   <- list(Results1 = M1, Results2 = M2, ResultsD = MD,
                  bootstraps1 = BS1, bootstraps2 = BS2, bootstrapsD = BSD,
                  CI = rst)
    return(out)
}
