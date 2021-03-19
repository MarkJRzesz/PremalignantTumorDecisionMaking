# Basic AUC with log scaling function that takes in a vector x. 
# Max value is based on the maximum amount (e.g., money, percent, subjective value), used to normalize the height. 
# Should be converted to either subjective value or percent, as this was not tested using indifference points based
# on specific monetary values, only percent-based questions
# GL is to accommodate if data need to be inverted based on types of questions (e.g., gains vs losses)
# The following functions are based on the AUClog calculation proposed by Borges et al. (2016)
# Default values are arbitrary for IV and logOffset

# AUClog caluclator ----
AUClog <- function(x, IV = c(1, 10, 100, 1000, 10000), maxValue = 100, logOffset = 1, GL = "gain"){
# convenience to give a warning if there is a missing response or IV
    if(length(x) != length(IV)){
        warning("response and independent variable need to be equal length")
    }
# convenience to give a warning that any 0 IV values have been changed to accommodate log scaling as log(0) is undefined
    if(any(IV == 0)){
        print(paste0("smallest value has been converted to ", logOffset))
        IV[1] <- logOffset
    }
# does the math to invert values if needed, not changing this would otherwise result in area above the curve
    if(GL == "gain"){
        b <- as.numeric(x)/maxValue
    } else if(GL == "loss"){
        b <- (maxValue - as.numeric(x))/maxValue
    }
# The following will find the number of intervals, find the natural log differences of intervals, and then calculate AUClog
    holdVec <- vector()
    interNum <- length(IV)-1
    interLength <- diff(log(IV))/sum(diff(log(IV)))
    for(i in 1 : (interNum)){
        holdVec[i] <- (interLength[i]) * (b[i] + b[i+1])/2
    }
# The final AUC value is returned
    return(sum(holdVec))
}

# VUSlog Calculator ----
# Most of the same as above, but now it has been generalized to a 3D discounting problem (two IVs)
# Note that x needs to be a matrix
VUSlog <- function(x, IVcol = c(1, 10, 100, 1000, 10000), IVrow = c(1, 10, 100, 1000, 10000), maxValue = 100, logOffset = 1,
                   GL = "gain"){
# Same convenience warnings as the AUClog calculator
    if(any(IVrow == 0)){
        print(paste0("smallest row value has been converted to ", logOffset))
        IVrow[1] <- logOffset
    }
    if(any(IVcol == 0)){
        print(paste0("smallest column value has been converted to ", logOffset))
        IVcol[1] <- logOffset
    }
    if(ncol(x) != length(IVcol)){
        warning("number response columns and IVcol length need to be equal")
    }
    if(nrow(x) != length(IVrow)){
        warning("number response rows and IVrow length need to be equal")
    }
# Does the same inversion of values if needed
    if(GL == "gain"){
        b <- x/maxValue
    } else if(GL == "loss"){
        b <- (maxValue - x)/maxValue
    }
# Identifies the number of intervals and creates an appropriately sized matrix to calculate VUSlog
    colLog <- ncol(b)-1
    intCol <- diff(log(IVcol))/sum(diff(log(IVcol)))
    rowLog <- nrow(b)-1
    intRow <- diff(log(IVrow))/sum(diff(log(IVrow)))
    holdMat <- matrix(nrow = rowLog, ncol = colLog)
# Convenience prints just to check everything is working not necessary for functioning
    print(intCol)
    print(intRow)
    print(holdMat)
# Performs the VUSlog calculation, iterates through every column for every row.
    for(i in 1:rowLog){
        for(j in 1:colLog)
            holdMat[i, j] <- (intRow[i] * intCol[j]) * ((b[i, j] + b[i + 1, j] + b[i, j + 1] + b[i + 1, j + 1])/4)
    }
# Returns VUSlog
    return(sum(holdMat))
}
