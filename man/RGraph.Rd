% Copyright 2021. TIBCO Software Inc.
% This file is subject to the license terms contained
% in the license file that is distributed with this file.

\name{RGraph}
\alias{RGraph}
\title{
Execute Graphical Functions Through R
}
\description{
Provides for generating graphics by passing an expression to open-source R and evaluating it there.
}
\usage{
RGraph(expr, substitute = TRUE,
    width = deviceArgs[["width"]], height = deviceArgs[["height"]],
    data = NULL, envirData = parent.frame(),
    graphicsDevice = "png", deviceArgs = list(),
    packages = getOption("RGraphPackages"),
    verbose = FALSE, display = FALSE,
    file = NULL,
    returnFilename = FALSE,
    viewer = if(graphicsDevice == "win.metafile") browseURL else
        getOption("viewer", default=browseURL),
    REvaluator. = RinR::REvaluator)
}
\arguments{
  \item{expr}{
  an expression that produces the desired graphic.
}
  \item{substitute}{
  a logical value.  If \code{TRUE} (the default), treat the \code{expr} argument as a literal
  expression.  If \code{FALSE}, treat it as something that evaluates to the expression
  to pass to the remote open-source R process.
}
  \item{width}{
  an integer giving the width of the graphic to create.  The units depend
  on the \code{graphicsDevice}.  \code{png} and \code{jpg} use pixels while
  \code{pdf} and \code{win.metafile} use inches.
}
  \item{height}{
  an integer giving the height of the graphic to be created.  The units depend
  on the \code{graphicsDevice}.
}
  \item{data}{
  if not \code{NULL}, then either a character vector or a named list specifying
  the objects to add into the environment where \code{expr} is evaluated.
  If \code{NULL}, then names in the expression, \code{expr}, that are not used
  as function calls are assumed to be data objects that should be sent to
  the plotting evaluator.  Use \code{data=character()} or \code{data=list()} to
  cause it to not look through \code{expr} and to not send any data objects.
}
  \item{envirData}{
  an environment.  If the \code{data} argument is a character vector naming data
  objects, the names are looked up in this environment.
}
  \item{graphicsDevice}{
  a character string specifing the R function that produces the desired graphics
  file format: \code{"png"}, \code{"pdf"}, \code{"jpeg"},
  or, only on Windows, \code{"win.metafile"} (Windows metafile).
}
  \item{deviceArgs}{
  a list with names listing the arguments to pass to the \code{graphicsDevice}.
  The \code{width} and \code{height} arguments are generally passed separately, but
  you can use them here if you use their full names.
}
  \item{packages}{
  a character vector identifying any packages that must be loaded before 
  \code{expr} is evaluated.
}
  \item{verbose}{
  a logical value.  If \code{TRUE} print some debugging information. The default is 
  \code{FALSE}.
}
  \item{display}{
  a logical value. If \code{TRUE}, then the graphic is displayed
  using the function given as the \code{viewer} argument.  In that case, the
  graphics file is not removed when the function is done. The default is \code{FALSE}.
}
  \item{file}{
  a character string naming the graphics file.  If \code{NULL} (the default value),
  a temporary file with a file extension appropriate to the file format is used.
  If a file name is supplied then the file is not removed when the function
  finishes.
}
  \item{returnFilename}{
  a logical value.  If \code{FALSE} (the default), returns, as a \code{raw} vector, the bytes
  in the file.  If \code{TRUE}, returns the name of the file.  In the latter
  case, the file is not removed when the function is done.
}
  \item{viewer}{
  a function that takes a name of a graphics file for an argument and displays the graphics in the file.
  The function \code{browseURL} works well for all supported file formats.
  In RStudio version 0.98.447, \code{getOption("viewer")} is a good choice, except for .wmf files.
}
  \item{REvaluator.}{
  an \code{REvaluator} function for a version of open-source R that can produce graphics.
  \TERR currently cannot produce graphics.
}
}
\details{
  Some functions (like those in \code{"package:lattice"} and \code{"package:ggplot2"})
  must be wrapped inside a \code{print()} statement to display the graphic. 

  This function creates an expression that is passed to \code{REvaluate}.  If you want to
  create some graphics and return non-graphical results in one call to \code{REvaluate}
  you must call \code{REvaluate} directly.

  \code{RGraph} uses the \code{all.vars} function, along with some heuristics,
  to search the expression \code{expr} for names of objects
  that do not look like functions. This behavior can cause some surprises.
  If your \code{expr} involves functions with nonstandard argument evaluation, like \code{~}, \code{with},
  or \code{subset}, \code{RGraph} may try to send objects that do not exist in the caller's environment.
  If your code involves a function as an argument, the function is sent to the remote
  R evaluator and it might not work there (for example, a built-in \TERR function such as 
  \code{na.omit} does not work in open-source R).
}
\value{
If \code{returnFilename} is \code{FALSE},
returns, invisibly, a \code{raw} vector containing the bytes in the graphics file.
Otherwise, returns the name of the graphics file.
}
\seealso{
\code{\link{REvaluate}}
}
\examples{
\dontrun{
library(Sdatasets)
### basic plot
RGraph(hist(fuel.frame$Disp.), data = "fuel.frame", display = TRUE)

### basic plot, sending data under a new name
RGraph(hist(ff$Disp.), data=list(ff = Sdatasets::fuel.frame), display = TRUE)

### image in jpeg format, let Rgraph figure out which dataset needs to be sent
grFile <- RGraph(image(voice.five),
    graphicsDevice="jpeg", deviceArgs=list(pointsize=20),
    display=TRUE, returnFile = TRUE)
unlink(grFile) # clean up when done looking at the file

### use lattice package (so must use print() to make the plot).
### Since xyplot() includes data argument, RGraph does not try
### to send over the vectors named in its formula argument.
RGraph(print(xyplot(Fuel ~ Weight | Type, data=fuel.frame)),
    packages="lattice", display = TRUE)
}
}
\keyword{utils}
