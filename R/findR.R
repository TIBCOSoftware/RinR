# Copyright 2021. TIBCO Software Inc.
# This file is subject to the license terms contained
# in the license file that is distributed with this file.

findPathToR <- function()
{
    RFullPath <- getOption("RinR_R_FULL_PATH")
    if (is.null(RFullPath)) {
        if (.Platform[["OS.type"]] == "windows") {
            RHOME <- .currentlyRegisteredRHOME()
            if (!is.null(RHOME) && !.looksLikeWindowsRHOMEDirectory(RHOME)) {
                # someone removed RHOME without running uninstall
                RHOME <- NULL
            }
            if (is.null(RHOME)) {
                RHOME <- .findRHOMEDirectory()
            }
            if (is.null(RHOME) && identical(version$langage, "R")) {
                RHOME <- R.home()
            }
            if (is.null(RHOME)) {
                RFullPath <- .findRInPATH() # rely on PATH to find this
            } else {
                RFullPath <- file.path(RHOME, "bin", "R")
            }
        } else {
            RFullPath <- .findRStartupScript()
            if (is.null(RFullPath)) {
                RFullPath <- "Cannot find R executable"
            }
        }
    }
    RFullPath
}

findPathToTERR <- function()
{
    TERRFullPath <- getOption("RinR_TERR_FULL_PATH")
    if (is.null(TERRFullPath) && identical(version$language, "TERR")) {
        TERRFullPath <- file.path(R.home(), "bin", "TERR")
    }
    if (is.null(TERRFullPath)) {
        TERRFullPath <- "TERR"
    }
    TERRFullPath
}

# In Windows registry, look up Software\R-core\R\<Current Version>\InstallPath,
# trying first HKEY_CURRENT_USER, and if not found there HKEY_LOCAL_MACHINE.
# Return NULL if nothing found (or if not on Windows).
.currentlyRegisteredRHOME <- function () 
{
    retval <- NULL
    if (.Platform[["OS.type"]] == "windows") {
        rr <- function(..., hive) {
            key <- paste(sep="\\", ...)
            tryCatch(readRegistry(key, hive=hive), error=function(e)NULL)
        }
        hive <- NULL
        for (testHive in c("HCU", "HLM")) {
            if (!is.null(version <- rr("SOFTWARE", "R-core", "R", hive=testHive)[["Current Version"]])) {
                hive <- testHive
                break
            }
        }
        if (!is.null(hive)) {
            installPath <- rr("SOFTWARE", "R-core", "R", version, hive=hive)[["InstallPath"]]
            if (is.null(installPath)) {
                warning("SOFTWARE\\R-core\\R\\CurrentVersion in ", hive, " is ", version, " but SOFTWARE\\R-core\\R\\", version, "\\InstallPath is not in that registry hive")
            } else {
                version <- gsub(" .*$", "", gsub("^ +", "", version)) # R_system_version does not like the trailing space or " RC" that RSetReg may add to version!
                retval <- structure(installPath, version=tryCatch(R_system_version(version), error=function(e)R_system_version("0.0.0")), hive=hive)
            }
        }
    }
    retval 
}

# On non-Windows only, look in directories listed in Sys.getenv("PATH") for
# the most relevant looking R startup script.  "R" is best.  If
# no "R" anywhere, then look for the highest numbered of "R-<r.version.number>".
.findRStartupScript <- function(PATH = Sys.getenv("PATH"))
{
    if (identical(version[["OS.type"]], "windows")) {
        return(NULL)
    }
    PATH <- .nonTERRPathDirs()
    selectExecutables <- function(fileNames) {
        fileNames[file.access(fileNames, 1) == 0]
    }
    Rscript <- character(0)
    if (length(Rscript) == 0) {
        # First search all directories for "R", returning immediately if found
        for(dir in PATH) {
            Rscript <- selectExecutables(dir(dir, full.names=TRUE, pattern="^R$"))
            if (length(Rscript) > 0) {
                break
            }
        }
    }
    if (length(Rscript) == 0) {
        # Next, find highest version number in R-x.y.z
        maxVersion <- R_system_version("0.0.0")
        for(dir in PATH) {
            Rscripts <- selectExecutables(dir(dir, full.names=TRUE, pattern="^R-[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+$"))
            if (length(Rscripts) > 0) {
                versions <- R_system_version(gsub("^R-", "", basename(Rscripts)))
                if (max(versions) > maxVersion) {
                    i <- which(versions == max(versions))[1]
                    maxVersion <- versions[i]
                    Rscript <- Rscripts[i]
                }
            }
        }
    }
    if (length(Rscript) > 0) {
        Rscript
    } else {
        NULL
    }
}

# On Windows only, search in common places (<disk>:/Program Files/R
# and <disk>:/R) for a directory that looks like an RHOME.  Its name
# should be of the form R-x.y.z and we will chose the one with the
# highest version number.
.looksLikeWindowsRHOMEDirectory <- function(dir)
{
    isDir(file.path(dir, "library", "base")) &
        file.exists(file.path(dir, "bin", "R.exe")) &
        file.exists(file.path(dir, "COPYING"))
}

.findRHOMEDirectory <- function(disks=c("C", "D", "E"),
    dirs=c(file.path("Program Files", "R"), file.path("R")))
{
    RHOME <- NULL
    if (.Platform[["OS.type"]] == "windows") {
        maxVersion <- R_system_version("0.0.0")
        for(diskColon in paste0(disks, ":")) {
            for(dir in dirs) {
                rdirs <- dir(file.path(diskColon, dir), full.names=TRUE, pattern="^R-[[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+$")
                rdirs <- rdirs[.looksLikeWindowsRHOMEDirectory(rdirs)]
                if (length(rdirs) > 0) {
                    versions <- R_system_version(gsub("^R-", "", basename(rdirs)))
                    if (max(versions) > maxVersion) {
                        i <- which(versions == max(versions))[1]
                        maxVersion <- versions[i]
                        RHOME <- rdirs[i]
                    }
                }
            }
        }
    }
    RHOME
}

# If we are usng TERR and looking for "R"
# do not look in any directory under TERR_HOME, as TERR_HOME/bin/<platform>/R
# now fires up TERR.
.nonTERRPathDirs <- function() {
    pathDirs <- strsplit(Sys.getenv("PATH"), .Platform$path.sep)[[1]]
    if (identical(version$language, "TERR")) {
        pathDirs <- Filter(
            local({
                TERR_HOME <- normalizePath(R.home())
                lenTERR_HOME <- nchar(TERR_HOME)
                function(dir) TERR_HOME != substring(normalizePath(dir, mustWork=FALSE), 1, lenTERR_HOME)}),
            pathDirs)
    }
    pathDirs
}

# Last ditch effort to find something called R on PATH (or R.exe on Windows)
.findRInPATH <- function()
{
    pathDirs <- .nonTERRPathDirs()
    Rexe <- if (.Platform$OS.type == "unix") "R" else "R.exe"
    files <- file.path(pathDirs, Rexe)
    info <- file.info(files)
    info <- info[!is.na(info[,1]) & !info[,"isdir"],,drop=FALSE]
    if (nrow(info) > 0) {
        rownames(info)[1]
    } else {
        "Cannot find R executable"
    }
}
