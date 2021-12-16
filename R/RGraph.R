RGraph <- function(expr, substitute = TRUE,
    width = deviceArgs[["width"]], height = deviceArgs[["height"]],
    data = NULL, envirData = parent.frame(),
    graphicsDevice = "png", deviceArgs = list(),
    packages = getOption("RGraphPackages"),
    verbose = FALSE, display = FALSE,
    file = NULL,
    returnFilename = FALSE,
    viewer = if (graphicsDevice == "win.metafile") browseURL else getOption("viewer", default=browseURL),
    REvaluator. = RinR::REvaluator)
{
    devs <- list(
        png = list(fileext = ".png", Rfunction = "png", width = 680, height = 680),
        pdf = list(fileext = ".pdf", Rfunction = "pdf", width = 7, height = 7),
        win.metafile = list(fileext = ".wmf", Rfunction = "win.metafile", width = 7, height = 7),
        jpeg = list(fileext = ".jpg", Rfunction = "jpeg", width = 680, height = 480))
    dev <- devs[[graphicsDevice]]
    if (is.null(dev)) {
        stop("'graphicsDevice' must be one of ", deparse(names(devs)))
    }
    stopifnot(is.list(deviceArgs))
    Rfunction <- as.name(dev[["Rfunction"]])
    if (is.null(width)) {
        width <- dev[["width"]]
    }
    if (is.null(height)) {
        height <- dev[["height"]]
    }
    deviceArgs[["width"]] <- width
    deviceArgs[["height"]] <- height
    if (length(file)==0) {
        file <- tempfile("plot", fileext=dev[["fileext"]])
        if (!returnFilename) {
            on.exit(unlink(file))
        }
    }
    graphicsDeviceCall <- as.call(c(list(Rfunction, file), deviceArgs))
    if (substitute) {
        expr <- substitute(expr)
    }
    if (missing(data) || is.null(data)) {
        data <- tryCatch(getRequiredDataNames(expr),
                         error = function(e) {
                             warning("Cannot find data name in expression: ", conditionMessage(e))
                             character()
                         })
    }
    REvaluate(bquote({
                         .(graphicsDeviceCall)
                         .(expr)
                          dev.off()
                     }),
              substitute=FALSE, data=data,
              envirData = envirData, verbose = verbose, packages = packages,
              REvaluator. = REvaluator.)
    if (display){
        on.exit() # cancel removal of file.
        if (is.null(viewer)) {
            viewer <- browseURL
        } else if (!is.function(viewer)) {
            stop("'viewer' is not a function: ", substring(deparse(viewer)[1], 1, 30))
        }
        viewer(file)
    }
    if (returnFilename) {
        file
    } else {
        invisible(readBin(file, what="raw", n=file.info(file)$size))
    }
}

getRequiredDataNames <- function(expr)
{
    scrubExpr <- function(expr, ignore = character()) {
        callee <- if (is.call(expr)) expr[[1]] else NULL
        if (!is.name(callee)) {
            callee <- NULL
        }
        if (is.name(expr) && is.element(as.character(expr), ignore)) {
            # replace any name on the ignore list (like the 'i' from for(i in 1:10)) with a non-name.
            7
        } else if (identical(callee, quote(`$`)) || identical(callee, quote(`@`))) {
            # convert var$component or var@slot to just var.
            scrubExpr(expr[[2]])
        } else if (identical(callee, quote(`::`)) || identical(callee, quote(`:::`))) {
            # :: (or :::) will be evaluated by callee so data must be there already
            5
        } else if (identical(callee, quote(`function`)) || is.function(expr)) {
            # don't look into function definitions
            8
        } else if (identical(callee, quote(`for`))) {
            # ignore the 'i' in for(i in 1:10)fun(i): see first clause of this if statement
            scrubExpr(expr[[4]], ignore = c(ignore, as.character(expr[[2]])))
        } else if (is.recursive(expr)) {
            if (is.call(expr) && (d <- match("data", names(expr), nomatch=0))!=0) {
                # for calls of form FUN(..., data=something) ignore any formulae in the call
                for (i in setdiff(seq_along(expr), d)) {
                    if (is.call(expr[[i]]) && identical(expr[[i]][[1]], quote(`~`))) {
                        expr[[i]] <- 9
                    }
                }
            }
            for (i in seq_along(expr)) {
                expr[[i]] <- scrubExpr(expr[[i]], ignore)
            }
            expr
        }
        else {
            expr
        }
    }
    expr <- scrubExpr(expr)
    all.vars(expr, unique = TRUE)
}
