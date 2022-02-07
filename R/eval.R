# Copyright 2021. TIBCO Software Inc.
# This file is subject to the license terms contained
# in the license file that is distributed with this file.

RinR.env <- new.env()
with(RinR.env,
    {
        serializationVersionToRemote <- 3L
        serializationVersionFromRemote <- 3L
    }
)

with(RinR.env,
    {
        R <- new.env()
        TERR <- new.env()
        interpreterType <- "unknownSInterpreter"
        startExprString <- ""
        cmdFlags <- c("--quiet", "--slave", "--no-init-file", "--no-restore", "--no-save")
        RProgName <- NULL
        RBinDir <- NULL
        sendExprAsRds <- TRUE # TRUE=>send expr via rds file, FALSE=>send deparsed expr in text file
    }
)
with(RinR.env$R,
    {
        interpreterType <- "R"
        RProgName <- getOption("RCommand", default="R")
    }
)
with(RinR.env$TERR,
    {
        interpreterType <- "TERR"
        RProgName <- getOption("TERRCommand", default="TERR")
    }
)

fixBS <- function(filename) {
    if (.Platform$OS.type == "windows") {
        gsub("/", "\\\\", filename)
    } else {
        gsub("\\\\", "/", filename)
    }
}

isDir <- function(filename)
{
    (function(x) !is.na(x) & x)(file.info(filename)[["isdir"]])
}

pushPATH <- function(dir)
{
    # add directory(ies) to front of PATH
    oldPATH <- Sys.getenv("PATH")
    newPATH <- paste(sep=.Platform$path.sep, paste(collapse=.Platform$path.sep, fixBS(dir)), oldPATH)
    Sys.setenv(PATH=newPATH)
    invisible(oldPATH)
}
popPATH <- function()
{
    # remove first directory from PATH
    oldPATH <- Sys.getenv("PATH")
    dirs <- strsplit(oldPATH, .Platform$path.sep, fixed = TRUE)[[1]]
    newPATH <- paste(collapse=.Platform$path.sep, dirs[-1])
    Sys.setenv(PATH=newPATH)
    dirs[1]
}

inDir <- function(dir, expr)
{
    if (dir != "") {
        curdir <- getwd()
        setwd(curdir) # just to provoke possible error
        on.exit(setwd(curdir))
        setwd(dir)
    }
    expr # lazily evaluate now while in working directory 'dir'
}

print.REvaluator <- function(x, complete = FALSE, ...)
{
    cat(sep="", "An REvaluator function\n")
    envir <- environment(x)
    cat(sep="", "   RProgName = ", deparse(envir$RProgName), "\n")
    RBinDir <- envir$RBinDir
    if (is.null(RBinDir)) {
        cat(sep="", "   No RBinDir, will look for RProgName in Sys.getenv(\"PATH\")\n")
    } else {
        cat(sep="", "   RBinDir = ", deparse(RBinDir), "\n")
    }
    if (!is.null(envir$startFlags)) {
        cat(sep="", "   startFlags = ", deparse(envir$startFlags), "\n")
    }
    if (!is.null(envir$envVars)) {
        cat(sep="", "   envVars = ", paste(paste(sep="", names(envir$envVars), "=\"", envir$envVars, "\""), collapse=", "), "\n")
    }
    if (!identical(envir$arch, "")) {
        cat(sep="", "   arch = ", deparse(envir$arch), "\n")
    }
    if (complete) {
        cat(paste(sep="", "   ", c("Code = ", paste("   ", deparse(unclass(x))))), sep="\n")
    } else {
        argString <- sub(" *NULL$", "", paste(gsub("^ +| +$", "", deparse(args(x))), collapse=" "))
        cat(paste(sep="", "   ", "Arguments = ", argString), sep="\n")
    }
    invisible(x)
}

withEnvVars <- function(expr, envVars)
{
    if (length(envVars) == 0) {
        expr
    } else {
        stopifnot(is.character(envVars), !is.null(names(envVars)), all(names(envVars) != ""))
        setOrUnsetEnv <- function(envVars) {
            toUnset <- is.na(envVars)
            if (any(toUnset)) {
                Sys.unsetenv(names(envVars)[toUnset])
            }
            if (any(!toUnset)) {
                do.call("Sys.setenv", as.list(envVars[!toUnset]))
            }
        }
        # work around problem in TERR 1.5:
        buggyGetenv <- !is.na(Sys.getenv("unlikelyToBeSETenvironmentVaRiaBle", unset=NA_character_))
        oldEnvVars <- if (buggyGetenv) {
            unlikelyValue <- "unliKelY vaLue for envri var"
            tmp <- Sys.getenv(names(envVars), unset=unlikelyValue, names=TRUE)
            tmp[tmp == unlikelyValue] <- NA
            tmp
        } else {
            Sys.getenv(names(envVars), unset=NA_character_, names=TRUE)
        }
        on.exit(setOrUnsetEnv(oldEnvVars))
        setOrUnsetEnv(envVars)
        expr
    }
}

withoutRHOMEinAnyPATH <- function(expr, pathNames = c("PATH", "LD_LIBRARY_PATH", "DYLD_LIBRARY_PATH"))
{
    dropRHOMEfromPATH <- function(pathName)
    {
        pathValue <- Sys.getenv(pathName, unset=NA_character_)
        if (!is.na(pathValue)) {
            R_HOME <- normalizePath(R.home())
            pathDirs <- strsplit(pathValue, .Platform$path.sep)[[1]]
            pathDirs <- pathDirs[!startsWith(normalizePath(pathDirs, mustWork = FALSE), R_HOME)]
            pathValue <- paste(pathDirs, collapse = .Platform$path.sep)
        }
        pathValue
    }
    names(pathNames) <- pathNames
    paths <- vapply(pathNames, dropRHOMEfromPATH, FUN.VALUE="")
    withEnvVars(expr, paths)
}

chooseArch <- function(arch, interpreterType)
{
    # only relevant when making an R evaluator on Windows
    # On input, arch=NULL means to choose default value.
    # On output, arch="" means to not use "-arch XX" argument.
    if (identical(interpreterType, "R") && identical(.Platform$OS.type, "windows")) {
        if (is.null(arch)) {
            if (identical(base::version$arch, "i386")) {
                arch <- "32"
            } else { # must be "x86_64"
                arch <- "64"
            }
        } else if (identical(arch, "")) {
            # do nothing to arch
        } else {
            arch <- format(as.character(arch)[1])
            arch <- switch(arch,
                "i386" = , "32" = "32",
                "x64" = , "64" = "64",
                stop("'arch' should be NULL, '32', or '64'"))
        }
    } else {
        arch <- ""
    }
    arch
}

findNonUNCDir <- function()
{
    if (.Platform$OS.type == "windows") {
        isNonUNCDir <- function(f)
        {
            !is.na(f) && ( f != "") && isDir(f) && grepl(ignore.case=TRUE, "^[A-Z]:", f)
        }
        for(dir in c(tempdir(), Sys.getenv("TMP"), Sys.getenv("TEMP"), Sys.getenv("HOME"), Sys.getenv("windir"))) {
            if (isNonUNCDir(dir)) {
                return(dir)
            }
        }
        warning("Cannot find a non-UNC directory in which to run cmd.exe")
    }
    ""
}

makeREvaluator <- function(RProgName,
    RHome = NULL,
    RBinDir = NULL,
    startFlags = NULL,
    envVars = NULL,
    interpreterType = sub("[^[:alpha:]].*$", "", toupper(basename(RProgName))),
    arch = NULL)
{
    knownInterpreterTypes <- objects(RinR.env)
    for (i in knownInterpreterTypes) {
        if (substring(interpreterType, 1, length(i)) == i) {
            interpreterType <- i
        }
    }
    if (!is.element(interpreterType, knownInterpreterTypes)) {
        stop("'interpreterType' ", deparse(interpreterType), " should be or start with one of ", deparse(knownInterpreterTypes))
    }
    envir <- new.env(parent=RinR.env[[interpreterType]])
    assign(envir = envir, "RProgName", RProgName)
    
    if (!is.null(RHome)) {
        if (!isDir(RHome)) {
            stop("'RHome' does not name a directory: ", RHome)
        }
        if (is.null(RBinDir)) {
            RBinDir <- file.path(RHome, "bin")
        }
    }
    if (!is.null(RBinDir)) {
        if (!isDir(RBinDir)) {
            stop("'RBinDir' does not name a directory: ", RBinDir)
        }
        assign(envir = envir, "RBinDir", fixBS(RBinDir))
    }
    if (is.null(RBinDir)) {
        assign(envir = envir, "RFullPath", RProgName) # will find RProgName in $PATH
    } else {
        assign(envir = envir, "RFullPath", fixBS(file.path(RBinDir, RProgName)))
    }
    if (!is.null(startFlags)) {
        assign(envir = envir, "startFlags", startFlags)
        assign(envir = envir, "cmdFlags", c(eval(envir=parent.env(envir), quote(cmdFlags)), startFlags))
    }
    if (!is.null(envVars)) {
        if (!is.character(envVars) || is.null(names(envVars)) || any(names(envVars) == "")) {
            stop("'envVars' must be a character vector with a name for each element")
        }
    }
    assign(envir = envir, "arch", chooseArch(arch, interpreterType))
    assign(envir = envir, "envVars", envVars)
    assign(envir = envir, "nonUNCDir", "") # will be reset in .onLoad's call to configureREvaluator
    
    structure(
        class = "REvaluator",
        with(envir,
            function(expr, substitute=TRUE, data=NULL, envirData=parent.frame(), verbose=FALSE, showTextOutput=FALSE)
            {
                if (substitute) {
                    expr <- substitute(expr)
                }
                stopifnot(is.language(expr))
                saveDataFile <- tempfile()
                rdsFile <- tempfile()
                scriptFile <- tempfile()
                textOutFile <- if (showTextOutput) NULL else tempfile()
                exprRdsFile <- tempfile()
                oldRFormat <- options(saveRFormat = TRUE)
                on.exit({unlink(c(rdsFile, scriptFile, textOutFile, saveDataFile, exprRdsFile)); options(oldRFormat)})
                if (sendExprAsRds) { # sendExprAsRds is stored in environment of this function
                    saveRDS(expr, file = exprRdsFile, version = serializationVersionToRemote)
                }
                SExprString <- paste("{",
                                        paste(deparse(bquote(setwd(.(curdir)), list(curdir=if (.Platform$OS.type == "windows") shortPathName(getwd()) else getwd()))), collapse="\n   "),
                                        if (sendExprAsRds) {
                                            paste0("base::eval(base::readRDS(", deparse(exprRdsFile), "), envir=base::.GlobalEnv)")
                                        } else {
                                            paste(deparse(expr), collapse="\n   ")
                                        },
                                      "}",
                                      sep="\n   ")
                SExprString <- sprintf("`_value` <- structure(names=base::sub(\"TIBCO Enterprise Runtime for R\", \"TERR\", base::version$version.string), base::list(base::try(\n    %s\n)))\n  base::saveRDS(`_value`, file=%s, version=%d)\n  base::q(\"no\")\n", SExprString, deparse(rdsFile), serializationVersionFromRemote)
                if (!is.null(data)) {
                    SExprString <- sprintf("base::load(%s)\n%s", deparse(saveDataFile), SExprString)
                }
                cat(SExprString, file=scriptFile)
                if (verbose) {
                    cat(paste0(RFullPath, ": ", readLines(scriptFile)), sep="\n")
                }
                if (!is.null(data)) {
                    if (is.character(data)) {
                        save(list=data, envir=envirData, file=saveDataFile, version = serializationVersionToRemote)
                    } else if (!is.list(data) || (length(data) > 0 && is.null(names(data)))) {
                        stop("'data' must be a named list or a character vector")
                    } else {
                        save(list=names(data), envir=list2env(data), file=saveDataFile, version = serializationVersionToRemote)
                    }
                }
                # look at 'help cmd' on Windows for details on following two lines
                windowsCmd0 <- if (identical(.Platform$OS.type, "windows")) "cmd /S /C \"" else ""
                windowsCmd1 <- if (identical(.Platform$OS.type, "windows")) "\"" else ""
                if (!is.null(arch) && !identical(arch, "")) {
                    cmdFlags <- c("--arch", arch, cmdFlags)
                }
                cmdFlags <- paste(cmdFlags, collapse = " ")
                SCmd <- sprintf("%s \"%s\" %s < \"%s\" %s %s 2>&1 %s", windowsCmd0, RFullPath, cmdFlags, scriptFile, if (!is.null(textOutFile)) ">" else "", if (!is.null(textOutFile)) shQuote(textOutFile) else "", windowsCmd1)
                if (verbose) {
                    cat(paste0(RFullPath, ": ", SCmd), sep="\n")
                }
                status <- withEnvVars(withoutRHOMEinAnyPATH(withEnvVars(inDir(nonUNCDir, system(SCmd)), c(R_HOME=NA_character_, R_ARCH=NA_character_))), envVars)
                # TODO: have ExprString set exit status on error
                if (status != 0) {
                    warning("Probably could not run command (check PATH or RProgName): ", SCmd)
                }
                if (verbose) {
                    cat(paste0(RProgName, ": ", "exit status =", status), sep="\n")
                }
                if (!file.exists(rdsFile)) {
                    textOut <- if (!is.null(textOutFile) && file.exists(textOutFile)) tryCatch(readLines(textOutFile), error=function(e)"Unreadable text output file") else "<no text output produced>"
                    stop("The ", interpreterType, " interpreter, ", sQuote(RProgName),
                         ", did not make an output file, the program probably could not be found (set options(RinR_R_FULL_PATH) or options(RinR_TERR_FULL_PATH) and call configureREvaluator)",
                         if (!is.null(textOut)) "\nText output from call to interpreter is\n    ", paste(textOut, collapse="\n    ") )
                }
                value <- tryCatch(readRDS(rdsFile), error=function(e)structure(names=RProgName, list(paste("Error:", e))))
                value
            }))
}

configureREvaluator <- function(evaluator, FullPath = if (interpreterType=="TERR") findPathToTERR() else findPathToR(), startFlags = NULL, envVars = NULL, arch = NULL, nonUNCDir = findNonUNCDir())
{
    # RFullPath is used to spawn the R/TERR process
    # RProgName is used in error/log messages
    # RBinDir is not used by the evaluator, but is used here to create RFullPath and is used by print.REvaluator
    envir <- environment(evaluator)
    interpreterType <- get("interpreterType", envir=envir, inherits=TRUE)
    
    if (!grepl("/|\\\\", FullPath)) {
        assign("RFullPath", FullPath, envir=envir)
        assign("RBinDir", NULL, envir=envir)
        assign("RProgName", FullPath, envir=envir)
    } else {
        assign("RFullPath", FullPath, envir=envir)
        assign("RBinDir", dirname(FullPath), envir=envir)
        assign("RProgName", basename(FullPath), envir=envir)
    } 
    if(!is.null(startFlags)) {
        assign(envir = envir, "startFlags", startFlags)
        assign(envir = envir, "cmdFlags", c(eval(envir=parent.env(envir), quote(cmdFlags)), startFlags))
    }
    if (!is.null(envVars)) {
        if (!is.character(envVars) || is.null(names(envVars)) || any(names(envVars) == "")) {
            stop("'envVars' must be a character vector with a name for each element")
        }
    }
    assign(envir = envir, "envVars", envVars)
    assign(envir = envir, "arch", chooseArch(arch, interpreterType))
    assign(envir = envir, "nonUNCDir", nonUNCDir)
    # nothing to return - the environment of the evaluator was altered in place
}

REvaluator <- makeREvaluator("R")
TERREvaluator <- makeREvaluator("TERR")
LocalEvaluator <- local({
    RProgName = "current interpreter instance"
    RBinDir = ""
    structure(
        class = "REvaluator",
        function(expr, substitute=TRUE, data=NULL, envirData = parent.frame() ,verbose=FALSE, showTextOutput)
        {
            # showTextOutput is ignored, treated as TRUE
            if (substitute) {
                expr <- substitute(expr)
            }
            stopifnot(is.language(expr))
            envirEval <- .GlobalEnv
            if (!is.null(data)) {
                if (is.character(data)) {
                    for(nm in data) {
                        assign(nm, get(nm, envir=envirData), envir=envirEval)
                    }
                } else if (!is.list(data) || (length(data) > 0 && is.null(names(data)))) {
                    stop("'data' must be a named list or a character vector")
                } else {
                    for(nm in names(data)) {
                        assign(nm, data[[nm]], envir=envirEval)
                    }
                }
            }
            if (verbose) {
                cat(paste0("Local: ", deparse(expr)), sep="\n")
            }
            structure(names="Local", list(tryCatch(eval(expr, envir=envirEval), error=function(e)paste("Error:", e))))
        })})

DefaultREvaluators <- function()
{
    defaultDefault <- list(RinR::REvaluator, RinR::TERREvaluator)
    ev <- getOption("REvaluators")
    if (is.null(ev)) {
        ev <- defaultDefault
    }
    if (!is.list(ev) || !all(vapply(ev, function(e)class(e)[1], "") == "REvaluator")) {
        warning("getOption(\"REvaluators\") is not a list of REvaluator objects, using the system-default REvaluators");
        ev <- defaultDefault
    }
    ev
}

REvaluate <- function(expr, REvaluator. = RinR::REvaluator, substitute = TRUE, data = NULL, envirData = parent.frame(), verbose = FALSE, packages = character(), ...)
{
    if(substitute) {
        expr <- substitute(expr)
    }
    if (length(packages) > 0) {
        # TERR's parse() does not yet have keep.source argument, so use kludge
        expr <- (function(packages) {
            ks <- options(keep.source = FALSE)
            on.exit(options(ks))
            libraryCalls <- parse(text = paste("{", paste(sprintf("library(\"%s\")\n", as.character(packages)), collapse=""), "}"))[[1]]
            bquote({ .(libraryCalls) ; .(expr) })
        })(packages)
    }
    expr <- bquote(try(.(expr)))
    if (verbose) {
        cat("RinR::REvaluate expr=", deparse(expr), "\n")
    }
    stopifnot(inherits(REvaluator., "REvaluator"))
    rawValue <- REvaluator.(expr, substitute = FALSE, verbose=verbose, data=data, envirData=envirData, ...)[[1]]
    if (inherits(rawValue, "try-error")) {
        stop(as.character(rawValue))
    } else {
        rawValue
    }
}

multiREvaluator <- function(expr, REvaluators=DefaultREvaluators(), substitute = TRUE, data = NULL, envirData = parent.frame(), verbose = FALSE, sideBySide = TRUE, PRINTFUN="print", randomCheck = FALSE, ...)
{
    if(substitute) {
        expr <- substitute(try(expr))
    }
    stopifnot(is.language(expr))
    if(randomCheck) {
        expr <- bquote((.(func))(.(arg)),
                list(func = function(expr) {
                                origSeed <- .GlobalEnv$.Random.seed
                                value <- force(expr)
                                postSeed <- .GlobalEnv$.Random.seed
                                list(value = value, usedRandomSeed = !identical(origSeed, postSeed))},
                     arg = expr))
        values <- unlist(recursive=FALSE, valuesRaw <- lapply(REvaluators, function(REvaluator)REvaluator(expr, substitute=FALSE, verbose=verbose, data=data, envirData=envirData, ...)))
        values <- structure(lapply(values, function(x)x[["value"]]),
                            usedRandomSeed = vapply(values, function(x)x[["usedRandomSeed"]], TRUE))
    } else {
        values <- unlist(recursive=FALSE, lapply(REvaluators, function(REvaluator)REvaluator(expr, substitute=FALSE, verbose=verbose, data=data, envirData=envirData, ...)))
    }
    if (!is.null(names(REvaluators))) {
        # override the default version$version.string names for outputs
        names(values) <- names(REvaluators)
    }
    if (sideBySide) {
        values <- sideBySide(values, PRINTFUN=PRINTFUN)
    }
    values
}

RCompare <- function(expr, REvaluators = DefaultREvaluators(), substitute = TRUE, data = NULL, envirData = parent.frame(), tolerance = 0, returnValues = TRUE, verbose = FALSE, randomCheck = FALSE, ...)
{
    if(substitute) {
        expr <- substitute(try(expr))
    }
    values <- multiREvaluator(expr, REvaluators = REvaluators, substitute = FALSE, data = data, envirData = envirData, verbose = verbose, randomCheck = randomCheck, ...)
    all.equal <- lapply(values[-1], function(v) tryCatch(all.equal(values[[1]], v, tolerance = tolerance), error=function(e)e))
    if (length(all.equal) > 0) {
        names(all.equal) <- paste(names(values)[1], "vs.", names(values)[-1])
    }
    if (returnValues) {
        structure(values, all.equal = all.equal)
    } else {
        all.equal
    }
}
