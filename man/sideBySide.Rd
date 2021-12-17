\name{sideBySide}
\alias{sideBySide}
\alias{print.sideBySide}
\title{
Print Objects in Columns Next to Each Other
}
\description{
Makes it easier to compare two or more objects by printing them in
side-by-side columns.
}
\usage{
sideBySide(x, PRINTFUN = NULL)
\method{print}{sideBySide}(x, \dots, PRINTFUN = attr(x, ".PRINTFUN"),
    sep = " | ", width = getOption("width", 80))
}
\arguments{
  \item{x}{
  a list of one or more objects to be printed.
If \code{x} is not a list, it is replaced by \code{list(x)},
a list of length 1 containing the original \code{x}.
}
  \item{PRINTFUN}{
the function (or name of the function) to use to print the elements of \code{x}.
By default, \code{"print"}.
}
  \item{sep}{
a character string used as the separator between the side-by-side printouts of
the elements of \code{x}.
}
  \item{width}{
a positive integer giving the desired width of the printout, in characters.
}
  \item{\dots}{
the arguments passed to \code{PRINTFUN} when the elements of \code{x} are printed.
}
}
\details{
\enumerate{
\item \code{options("width")} is reduced by a factor of \code{length(x)}.
\item The elements of \code{x} are (virtually) printed with that width setting.
\item The (virtually) printed lines are pasted together so the objects
in \code{x} are printed side by side.  
}
If the print methods for the objects do
not keep within the requested reduced width, then the printed outputs will not
be printed side by side.  You might need to expand \code{options("width")}, and
your window width, to get nice results.
}
\value{
returns an object like \code{x} with the class \code{"sideBySide"} attached and
an attribute, \code{".PRINTFUN"},
giving the default printing function to use.
}
\examples{
sideBySide(list(NaiveFactorial=sapply(1:10,function(i)prod(1:i)),
                ViaGamma=gamma((1:10)+1)))
# may need options(width=124)
sideBySide(list(Default=terms(y ~ 1), NoIntercept=terms(y ~ -1)), PRINTFUN="str")
}
\keyword{ utils }
