# Copyright 2021. TIBCO Software Inc.
# This file is subject to the license terms contained
# in the license file that is distributed with this file.

sideBySide <- function(x, PRINTFUN = NULL)
{
    if (!is.list(x)) {
        x <- list(x)
    }
    structure(x, class="sideBySide", .PRINTFUN = PRINTFUN)
}

print.sideBySide <- function(x, ..., PRINTFUN = attr(x, ".PRINTFUN"), sep = " | ", width = getOption("width", 80))
{
    stopifnot(is.list(x), as.integer(width) > 0)
    origWidth <- getOption("width", 80)
    on.exit(options(width=origWidth))
    width <- as.integer(width)
    nx <- length(x)
    if(is.null(PRINTFUN)) {
        PRINTFUN <- print
    } else {
        PRINTFUN <- match.fun(PRINTFUN)
    }
    fx <- tryCatch(lapply(x, function(xi)
        {
            on.exit(options(width = origWidth))
            options(width = max(15L, as.integer(width/nx - 4)))
            capture.output(PRINTFUN(xi, ...))
        }),
        error = function(e) "error")
    if(is.list(fx)) {
        nlines <- max(vapply(fx, length, 0))
        maxWidths <- vapply(fx, FUN.VALUE=0, function(text)max(5L, nchar(text)))
        if (!is.null(names(fx))) {
            maxWidths <- pmax(maxWidths, nchar(names(fx)))
        }
        for(i in seq_along(fx)) {
            fx[[i]] <- c(fx[[i]], rep("", nlines - length(fx[[i]])))
            fx[[i]] <- sprintf(paste0("%-", maxWidths[i], ".", maxWidths[i], "s"), fx[[i]])
        }
        if (!is.null(names(fx))) {
            for(i in seq_along(fx)) {
                fx[[i]] <- c(sprintf(paste0("%-", maxWidths[i], ".", maxWidths[i], "s"), names(fx)[i]), fx[[i]])
            }
            names(fx) <- NULL
        }
        g <- breakIntoLines(maxWidths, width, nchar(sep))
        sg <- split(seq_along(g), g)
        for (i in seq_along(sg)) {
            if (i > 1) {
                cat("\n")
            }
            fxi <- fx[ sg[[i]] ]
            rowsToPrint <- isNotTrailingFalse(!Reduce(`&`, lapply(fxi, grepl, pattern="^[[:space:]]*$")))
            fxi <- lapply(fxi, function(text) text[rowsToPrint])
            writeLines(do.call(paste, c(fxi, list(sep = sep))))
        }
        attrNames <- setdiff(names(attributes(x)), c("class", "names", ".PRINTFUN"))
        if(length(attrNames)) {
            cat("\nAttributes:\n")
            cat(sep = "\n", paste0("  ", capture.output(print(attributes(x)[attrNames]))))
        }
    } else {
        NextMethod("print")
    }
    invisible(x)
}

breakIntoLines <- function (widths, maxSumWidths, sepWidth) 
{
    group <- numeric(length(widths))
    if (length(widths) > 0) {
        curGroupNum <- 1
        group[1] <- curGroupNum
        w <- widths[1]
        for (i in seq_along(widths)[-1]) {
            w <- w + sepWidth + widths[i]
            if (w > maxSumWidths) {
                curGroupNum <- curGroupNum + 1
                w <- widths[i]
            }
            group[i] <- curGroupNum
        }
    }
    group
}

isNotTrailingFalse <- function(x)
{
   stopifnot(is.logical(x))
   rev(cumsum(rev(x))) > 0
}
