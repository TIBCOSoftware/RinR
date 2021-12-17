% Copyright 2021. TIBCO Software Inc.
% This file is subject to the license terms contained
% in the license file that is distributed with this file.

\name{REvaluator}
\alias{REvaluator}
\alias{TERREvaluator}
\alias{LocalEvaluator}
\alias{REvaluate}
\title{
Evaluates an R Expression in a Given R-language Interpreter
}
\description{
Evaluate an R-language expression in open-source R,
in \TERR, 
or in the currently-running R-language interpreter. Returns the result to the
currently-running interpreter.
}
\usage{
LocalEvaluator(expr, substitute = TRUE, data = NULL, envirData = parent.frame(),
    verbose = FALSE, showTextOutput)
TERREvaluator(expr, substitute = TRUE, data = NULL, envirData = parent.frame(),
    verbose = FALSE, showTextOutput = FALSE)
REvaluator(expr, substitute = TRUE, data = NULL, envirData = parent.frame(),
    verbose = FALSE, showTextOutput = FALSE)
REvaluate(expr, REvaluator. = RinR::REvaluator, substitute = TRUE, data = NULL,
    envirData = parent.frame(), verbose = FALSE, packages = character(), \dots)
}
\arguments{
  \item{expr}{
an expression to be evaluated in the given R intepreter.
}
  \item{substitute}{
a logical value.
\itemize{
\item If \code{TRUE} (the default), the unevaluated \code{expr} is sent to
the other interpreter to be evaluated.
\item If \code{FALSE}, the unevaluated \code{expr} is  evaluated before being sent to the
other interpreter.  In this latter case, the evaluated expression
must also be a language object.  \code{FALSE} makes it easier to
call this function from another function.
}
}
  \item{data}{
if \code{NULL} (the default), no data is passed to the target environment.
If not \code{NULL}, \code{data} should be either a character vector
or a list with named components.
\itemize{
\item If \code{data} is a character vector, objects by those
names are copied from \code{envirData} to the interpreter and environment
where \code{expr} is evaluated.
\item If \code{data} is a named list, the components are copied to the
target environment to be stored under their names in the list.

These data objects are available when \code{expr} is evaluated.
}
}
  \item{envirData}{
an environment.  If \code{data} is a character vector, then it is assumed
to name objects in this environment.  Otherwise \code{envirData} is not used.
}
  \item{verbose}{
a logical value.  If \code{TRUE}, these functions print debugging
information as they run.  The default is \code{FALSE}.
}
\item{REvaluator.}{                                                           
an \code{REvaluator} object, as created by the \code{makeREvaluator} function.
The default, \code{REvaluator}, evaluates the expression in open-source R.
}
\item{packages}{
A character vector (or NULL) giving the names of the packages to load
before evaluating \code{expr}.
}
\item{showTextOutput}{
A logical value.  If \code{TRUE} then text output produced when the \code{REvaluator}
evaluates \code{expr} will be displayed as it is produced.  If \code{FALSE},
the default, such test output will not be displayed unless there is an error
finding the R or TERR executable.  \code{RinR::LocalEvaluator} does not use
this argument: it always displays the printed output.
}
\item{\dots}{
\code{REvaluate} will pass any arguments it does not recognize to the
\code{REvaluator} that it invokes.
}
}
\value{
\item{\code{REvaluator}, \code{TERREvaluator}, \code{LocalEvaluator}}{return 
the value of the the evaluated expression,
placed in a list (of length 1) with \code{version$version.string} 
(sometimes shortened) of the interpreter for a name.}

\item{\code{REvaluate}}{returns the value of the the evaluated expression
in the given R interpreter. If an error occurs while that interpreter tries 
to evaluate the expression, \code{REvaluate} calls \code{stop} with the error message
from the interpreter.}
}

\note{
\itemize{
\item The \code{LocalEvaluator} evaluates the expression in the global environment
of the currently-running R interpreter. Assignments made in the course of 
evaluating the expression or copying the data object affects the global environment.
\item \code{multiREvaluator} evaluates the same expression in several
interpreters.
\item \code{RCompare} calls \code{multiREvaluator} and compares the results
by calling \code{all.equal} on them.
\item The open-source R and \TERR evaluators are constructed by \code{makeREvaluator}.
These are default evaluators containing only the name of the interpreter,
not the path to them. Call \code{pushPATH} to add the directory containing
the interpreter's executable to the system \code{PATH} variable.
}
}
\seealso{
\code{\link{RCompare}},
\code{\link{RGraph}},
\code{\link{pushPATH}},
\code{\link{multiREvaluator}},
\code{\link{makeREvaluator}}.
}
\examples{
\dontrun{
REvaluate(version$version.string, LocalEvaluator)
REvaluate(version$language, REvaluator)
REvaluate(version$language, TERREvaluator)
REvaluate(log2(p), TERREvaluator, data=list(p=16:32))
p <- 60:67
REvaluate(log2(p), REvaluator, data="p")
REvaluate({ cat("Hello from subprocess\n"); 1+2+3})
REvaluate({ cat("Hello from subprocess\n"); 1+2+3}, showTextOutput=TRUE)

polrFit <- REvaluate({library(MASS) ; polr(voice.part ~ height, sng)},
    data = list(sng = Sdatasets::singer))
}
}
\keyword{ utils }
