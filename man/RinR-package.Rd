\name{RinR}
\alias{RinR-package}
\alias{RinR}
\title{
  The \TERR RinR Package Overview
}
\description{
Provides an overview to the RinR package.
}
\section{Introduction to the RinR Package}{
The RinR package provides functions for running code in open-source R from \TERR, or running code in 
\TERR from open-source R. 
Using RinR, you can: 
\itemize{
\item Compare results of running the same code in different open-source R and \TERR versions (see \code{RCompare}).
\item Call a function from an open-source R package and return the results to \TERR (see \code{REvaluate})).
\item Use open-source R to create a graphic, and then return the graphic to be displayed in TIBCO Spotfire or a browser (see \code{RGraph}).
}
}
\section{Package Functions}{
\describe{
\item{\code{MakeREvaluator}}{
Makes a function of class \code{"REvaluator"} that evaluates an expression in
an R-language intepreter.  The created function starts an R interpreter in a new process,
evaluates the expression in the global environment of that interpreter, and returns the
result in a list when the interpreter is done.  The name in the list is derived from
the value of \code{version$version.string} evaluated in the R-language interpreter.
}
}
\describe{
\item{\code{LocalEvaluator}, \code{REvaluator}, and \code{TERREvaluator}}{
Standard REvaluator objects like those created by \code{MakeREvaluator} and stored in
the \code{RinR} package.  \code{REvaluator} and \code{TERREvaluator} evaluate
the expression in a new \code{R} or \code{TERR} process, but \code{LocalEvaluator}
uses the currently running process.
}
}
\describe{
\item{\code{configureREvaluator}}{
Use to configure an existing \code{REvaluator} object to change
the location of the \code{R} or \code{TERR} executable or to change some
startup options.  If you do not supply the location of the executable, RinR
tries to find it, based on the values of \code{options("RinR_R_FULL_PATH")},
\code{options("RinR_TERR_FULL_PATH")}, values in the Windows registry,
commonly-used locations in the local file system, and directories in \code{Sys.getenv("PATH")}.
\code{configureREvaluator} is called for each of the built-in \code{REvaluator} objects
from the \code{.onLoad} function for the RinR package
(unless you set \code{options(RinR_DONT_CONFIGURE=TRUE)}) so there
is a good chance you do not have to reconfigure the location or the options.
}
}
\describe{
\item{\code{REvaluate}}{
Evaluates the expression in an R-language interpreter via an \code{REvaluator} object and returns its value.
If an error occurs while the interpreter tries to evaluate the expression, \code{REvaluate} calls 
stop with the error message from the interpreter.  That is, calling \code{REvaluate(expr, REvaluator)} acts
like calling just \code{expr}, except that the evaluation is done in another R-language interpreter.
}
}
\describe{
\item{\code{RGraph}}{
Provides for generating graphics files by passing an expression to open-source R and evaluating it there.
Use it to create \code{png}, \code{pdf}, \code{jpeg}, and \code{wmf} files.
}
}
\describe{
\item{\code{MultiREvaluator}}{
Evaluates an R-language expression in one or more R-language interpreters, and returns a list of the results of evaluating the 
expression in each R interpreter. The names of the list are taken from \code{version$version.string} in each interpreter. 
The returned list has the class \code{"sideBySide"}, whose print method attempts to print the outputs beside each other for 
easier visual comparison.
}
}
\describe{
\item{\code{RCompare}}{
Returns the same value as \code{multiREvaluator}, with the addition of the attribute \code{all.equal}, describing the differences between the results.
}
}
\describe{
\item{\code{pushPATH}}{
Adds the specified directory to the front of PATH, so that directory is searched before any others when 
the system looks for an executable to run. \code{pushPATH} returns the value of \code{Sys.getenv("PATH")} before 
it was changed. Altering the path affects nothing outside of \TERR, and is valid only for the current session of 
\TERR.  This is occasionally useful to enable \code{configureREvaluator} to find the \code{R} or \code{TERR} executable.
}
}
\describe{
\item{\code{popPATH}}{
Removes the initial entry from the environment variable PATH. \code{popPATH} returns the name of the directory that was removed from PATH. 
Altering the path affects nothing outside of \TERR, and is valid only for the current session of \TERR.
}
}
\describe{
\item{\code{sideBySide}}{
Makes it easier to compare two or more objects by printing them in side-by-side columns.
}
}
}
\section{Finding the Open-source R and \TERR Engines}{
By default, using \code{configureREvaluator}, RinR configures  
the two standard evaluators (\code{REvaluator} and \code{TERRevaluator}) 
so they know where the \code{R} and \code{TERR} engines are in
the local file system.  On both Windows and Linux, \code{configureREvaluator}
uses \code{getOption("RinR_R_FULL_PATH")} and \code{getOption("RinR_TERR_FULL_PATH")}
to find the open-source R and \TERR executables, respectively. 
If these options are set, they must specify the full path to the open-source R and the 
\TERR executables. For example, for open-source R on Windows, 
this might be "C:/Program Files/R/R-3.0.1/bin/R". For open-source R on Linux, this might be "/usr/local/bin/R-3.0.1". 
If you set these options, no further configuration is necessary. 

To store these configuration options permanently in your \TERR installation,
add a line like the following:
\tabular{lll}{
  \tab  \tab\code{options(RinR_R_FULL_PATH="C:/Program Files/R/R-3.0.1/bin/R")}.
}
to a TERRprofile startup file (for example, TERR_HOME/etc/TERRprofile.site).

The search strategy for when those options are not set is described in the help
file for \code{configureREvaluator}.
}
\seealso{
\code{\link{configureREvaluator}},
\code{\link{REvaluate}},
\code{\link{RCompare}},
\code{\link{RGraph}},
\code{\link{pushPATH}},
\code{\link{multiREvaluator}},
\code{\link{makeREvaluator}}, 
\code{\link{LocalEvaluator}},
\code{\link{sideBySide}}.
}
\examples{
\dontrun{
REvaluate(version$language, REvaluator)
REvaluate(version$language, TERREvaluator)
p <- 60:67
REvaluate(log2(p), data="p", REvaluator)
library(Sdatasets)
polrFit <- REvaluate({library(MASS) ; polr(voice.part ~ height, sng)},
    data = list(sng = Sdatasets::singer))
RGraph(boxplot(with(singer, split(height, voice.part))),
    data= list(singer = Sdatasets::singer), display = TRUE)
}
\dontrun{
options(REvaluators = list(REvaluator, TERREvaluator))
RCompare(lm(hp ~ gear + am, data=mtcars), tol=1e-12)
myData <- subset(iris, Species == "setosa", -Species)
RCompare(prcomp(myData), data = "myData")
}
}
\section{R Version}{
R-3.0.1
}
\keyword{utils}
