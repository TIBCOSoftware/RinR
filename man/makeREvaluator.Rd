\name{makeREvaluator}
\alias{makeREvaluator}
\alias{configureREvaluator}
\alias{print.REvaluator}
\title{
Make a Function That Evaluates an Expression in an R-language Interpreter
}
\description{
Make a function that evaluates an expression in an R-language intepreter.
}
\usage{
makeREvaluator(RProgName, RHome = NULL, RBinDir = NULL,
    startFlags = NULL, envVars = NULL,
    interpreterType = sub("[^[:alpha:]].*$", "", toupper(basename(RProgName))),
    arch = NULL)
configureREvaluator(evaluator, FullPath, startFlags = NULL, envVars = NULL,
    arch = NULL, nonUNCDir)
\method{print}{REvaluator}(x, complete = FALSE, \dots)
}
\arguments{
  \item{RProgName}{
a character string giving the name of the R interpreter.
}
  \item{RHome}{
a character string giving the home directory of the R interpreter.
}
  \item{startFlags}{
a character string giving command line arguments to the R interpreter.
These are appended to the default command line arguments
\code{" --quiet --vanilla --no-echo"}.
}
  \item{envVars}{
a named vector of character strings specifying the names and values of
environment variables to set before spawning the R interpreter.  A value
of \code{NA_character_} specifies unsetting the variable.
}
  \item{RBinDir}{
a character string giving the bin directory of the R interpreter.
If not given and \code{RHome} is known,
then it is set to be the "bin" subdirectory of \code{RHome}.
}
  \item{interpreterType}{
either \code{"R"} or \code{"TERR"} (case matters),
saying whether the interpreter is a version of open-source R
or of \TERR. If not given, it is determined from the
first few characters in \code{RProgName}.
}
  \item{arch}{
a character string (or \code{NULL}) used only for \R interpreters on Windows to tell
whether you want a 32- or 64-bit intepreter.  \code{"32"} (or \code{"i386"})
means 32-bit and \code{"64"} (or \code{"x64"}) means 64-bit.
\code{NULL}, the default, means to use the same architecture as the
currently running intepreter.  \code{""} means to not ask for any particular
architecture (which currently means to use the 32-bit R interpreter).
}
  \item{x}{
the \code{"REvaluator"} object to print.
}
  \item{complete}{
a logical value.  If \code{TRUE}, print just the arguments to the function. If \code{FALSE}
(the default), print the entire function. 
}
  \item{\dots}{
other arguments to print for \code{REvaluator} objects are silently ignored.
}
  \item{evaluator}{
the \code{REvaluator} object to configure.  It should be an object created by \code{makeREvaluator}.
}
  \item{FullPath}{
a string specifying the full file path to the open-source R or \TERR executable.  The default values 
come from \code{options("RinR_R_FULL_PATH")} or \code{options("RinR_TERR_FULL_PATH")}, if they
are set.  See \strong{Note} for more details.
}
  \item{nonUNCDir}{
a string giving the full name of a directory, in the "diskLetter:/..." format, not the UNC "//machine/..." format,
that will be used on Windows to launch the cmd.exe subprocess from.
No files will be accessed in this directory and the R or TERR subprocess will revert to the directory used
by the calling process before running the requested expressions;
Windows cmd.exe needs to start in a directory whose name starts with a letter then a colon.
If it is the empty string the subprocess will be launched from the current directory.
The default value on Windows is the first of \code{tempdir()}, \code{Sys.getenv("TMP")}, \code{Sys.getenv("TEMP")},
and \code{Sys.getenv("windir")} that starts with a letter and a colon.
On Linux the default value is the empty string.
}
}
\value{
returns a function that starts an R interpreter
in a new process, evaluates the expression there,
and gets the result when the interpreter is done.
See \code{\link{REvaluator}} for a description of the function.
}
\note{
The \code{REvaluator} functions store important values in their associated environments.
(These values include the full file path of the executable to be called, along with the 
arguments to the function. Also, if you used the \code{envVars} argument when you made 
or configured the object to set environment variables, the result of that is stored as well. 
This is useful information if you examine the default evaluators, \code{REvaluator} and 
\code{TERREvaluator}, because it conveys the results of the autoconfiguration.)

These values are not printed by the default \code{print} method.
To display these values, call the appropriate \code{REvaluator} function's \code{print} method.

\code{configureREvaluator} is called from the \code{.onLoad} function for the RinR package,
unless you set \code{options(RinR_DONT_CONFIGURE=TRUE)}.  In that case, or if the full
paths to the open-source R and \TERR executables cannot be found, the default evaluators assume
that executables called "R" and "TERR" are in a directory in \code{Sys.getenv("PATH")}.

\itemize{
\item If \code{configureREvaluator} cannot find the full file path to the open-source R or \TERR 
executables from the default values specified in \code{options("RinR_R_FULL_PATH")} or 
\code{options("RinR_TERR_FULL_PATH")}, \code{configureREvaluator} searches the Windows registry 
and commonly-used places in the file system to find where open-source R and \TERR are installed.  
\item If that search is not successful, \code{configureREvaluator} uses a path based on the value of \code{R.home()}
when configuring the evaluator whose type matches the type of currently running program.  
\item If all else fails, RinR uses "R" for the standard \code{REvaluator} and "TERR" for the standard
\code{TERREvaluator}. Its success depends on whether you reconfigured or used \code{pushPATH} 
to ensure executables by those names are on the system PATH before you use the evaluators.
}
\code{configureREvaluator} modifies the evaluator; it returns nothing useful.
}
\seealso{
\code{\link{REvaluate}},  \code{\link{RCompare}} 
}
\examples{
\dontrun{
R2.15.1 <- makeREvaluator("R", RHome="C:/Program Files/R/R-2.15.1")
REvaluate(version, REvaluator=R2.15.1)
R3.0.1 <- makeREvaluator("R-3.0.1", envVars=c(R_LIBS_SITE="/tmp"))
R3.0.1
REvaluate(.libPaths(), REvaluator=R3.0.1)
configureREvaluator(REvaluator, FullPath="C:/Program Files/R/R-3.0.1/bin/R")
REvaluator
}
}
\keyword{utils}
