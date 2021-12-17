# Copyright 2021. TIBCO Software Inc.
# This file is subject to the license terms contained
# in the license file that is distributed with this file.

all.equal.qr <- function(target, current, ...)
{
    if (identical(attr(target, "useLAPACK"), attr(current, "useLAPACK"))) {
        return(all.equal(unclass(target), unclass(current), ...))
    }
    diffs <- character()
    onlyMinorDiff <- TRUE
    if (!identical(as.numeric(target$rank), as.numeric(current$rank))) {
        diffs <- c(diffs, "ranks of qr objects differ")
        onlyMinorDiff <- FALSE
    }
    if (!identical(as.numeric(target$pivot), as.numeric(current$pivot))) {
        diffs <- c(diffs, "pivots of qr objects differ")
        onlyMinorDiff <- FALSE
    }
    if (!identical(dim(target$qr), dim(current$qr))) {
        diffs <- c(diffs, "dimensions of qr components of qr objects differ")
        onlyMinorDiff <- FALSE
    } else if (!identical(dimnames(target$qr), dimnames(current$qr))) {
        diffs <- c(diffs, "dimnames of qr components of qr objects differ")
    }
    if (onlyMinorDiff) {
        if (identical(version$language, "R")) {
            diffs <- c(diffs, "In R we cannot compare TERR and R qr objects numerically, use TERR for that")
        } else {
            Y <- seq_len(nrow(target$qr))
            Y <- cbind(Y, sin(Y), cos(Y))
            tmp <- all.equal(qr.coef(target, Y), qr.coef(current, Y), ...)
            if (!isTRUE(tmp)) {
                diffs <- c(diffs, paste("qr.coef(obj$qr,testY) differ:", tmp))
            }
        }
    }
    if (length(diffs)) diffs else TRUE
}
