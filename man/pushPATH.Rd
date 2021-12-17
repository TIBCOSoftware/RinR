% Copyright 2021. TIBCO Software Inc.
% This file is subject to the license terms contained
% in the license file that is distributed with this file.

\name{pushPATH}
\alias{pushPATH}
\alias{popPATH}
\title{
Manipulate the System PATH Variable
}
\description{
Add or remove entries from the front of the environment variable PATH:
the list of directories where the operating system looks for executable files.
}
\usage{
pushPATH(dir)
popPATH()
}
\arguments{
  \item{dir}{
a character string containing a directory to add
to the front of \code{Sys.getenv("PATH")}.
}
}
\details{
\itemize{
\item \code{pushPATH(dir)} adds the directory to the front of PATH, so that
the directory is searched before any others when \code{system} looks
for an executable to run.
\item \code{popPATH(dir)} removes the initial entry from PATH.
}
Altering PATH from within \TERR (or open-source R)
affects only where functions like \code{system} look for executables to run.
It has no effect outside of \TERR, and it is valid only for the current session of 
\TERR.
}
\value{
\item{\code{pushPATH}}{returns the value of \code{Sys.getenv("PATH")} before it was changed.}
\item{\code{popPATH}}{returns the name of the directory that was removed from PATH.}
}

\note{
These functions can be useful when using \code{RCompare}, if the default
\code{REvaluator} objects just specify the name of an intepreter, "R" or "TERR",
and not where they are.  The RinR package tries to find the full path to the
interpreters when the package is loaded, but it is not always successful.
}
\seealso{
\code{\link{configureREvaluator}} gives a more direct way to tell the RinR evaluators
where to find the R and TERR executables.
\code{\link{RCompare}}, \code{\link{REvaluator}}
}
\examples{
pushPATH("C:/Program Files/R/R-3.0.1/bin")
REvaluate(version$version.string)
}
%% Add one or more standard keywords, one per line
\keyword{utils}
