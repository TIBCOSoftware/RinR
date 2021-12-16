\name{multiREvaluator}
\alias{multiREvaluator}
\alias{DefaultREvaluators}
\alias{RCompare}
\title{
Evaluates an Expression in One or More R-language Interpreters
}
\description{
Evaluates an R-language expression in one or more R-language interpreters
and optionally compares the results.
}
\usage{
RCompare(expr, REvaluators = DefaultREvaluators(), substitute = TRUE,
    data = NULL, envirData = parent.frame(), tolerance = 0,
    returnValues = TRUE, verbose = FALSE, randomCheck = FALSE, \dots)
multiREvaluator(expr, REvaluators = DefaultREvaluators(), substitute = TRUE,
    data = NULL, envirData = parent.frame(), verbose = FALSE,
    sideBySide = TRUE, PRINTFUN = "print", randomCheck = FALSE, \dots)
DefaultREvaluators()
}
\arguments{
  \item{expr}{
an expression to  evaluate.
}
  \item{REvaluators}{
a list of \code{REvaluator} objects
as created by the \code{makeREvaluator} function.
The defaults evaluate the expressions in open-source R
and \TERR.
}
  \item{substitute}{
a logical value. Specifies how to use the \code{expr} argument.
\itemize{
\item If \code{TRUE} (the default), the unevaluated \code{expr} is sent to
the other interpreter to be evaluated.
\item If \code{FALSE}, the unevaluated \code{expr} is evaluated before being sent to the other interpreter. 
The evaluated expression must also be a language object. 

Setting \code{subsitute} to \code{FALSE} makes it easier to call this function from another function.
}
}
  \item{data}{
if not \code{NULL}, then either a character vector or a named list specifying
the objects that must be put into the environment where \code{expr} is
evaluated.  See \code{TERREvaluator} for details.
}
  \item{envirData}{
the environment where items in the character vector \code{data} are found.
}
  \item{verbose}{
a logical value. If \code{TRUE}, debugging information is printed. The default is \code{FALSE}.
}
  \item{tolerance}{
a small positive numeric value that is passed to \code{all.equal} to specify
how far apart two numeric results must be before a difference is reported.
}
  \item{returnValues}{
a logical value.  
\itemize{
\item If \code{TRUE} (the default), then \code{RCompare} returns a list 
containing the results of all the evaluations, along with a list describing how each
result (except the first) differs from the first.
\item If \code{FALSE}, it returns only the description of the differences.
}
}
  \item{sideBySide}{
a logical value.  If \code{TRUE} (the default), then give the output of \code{multiEvaluator}
the class \code{"sideBySide"} so the output components are printed next
to each other, if possible.
}
  \item{PRINTFUN}{
a function or the name of a function. This is passed to \code{sideBySide} to
specify how to print the output components.
}
  \item{randomCheck}{
a logical value.  If \code{TRUE}, an attribute \code{"usedRandomSeed"} is 
added to the output to indicate if random numbers were used during the requested
calculations.  If \code{FALSE}, that information is not returned.

The check determines if \code{.GlobalEnv$.Random.seed} changes during the calculations.
Note that code calling \code{set.seed()} can cause \code{randomCheck} to report a false negative.
}
\item{\dots}{
\code{multiREvaluator} and \code{RCompare} will pass any unrecognized arguments
to the \code{REvaluators} that they invoke.
}
}
\value{
\item{multiREvaluator}{returns a list of the results of evaluating the expression
in each R interpreter. 
The names of the list are taken from \code{version$version.string}
in each interpreter.
The returned list has the class \code{"sideBySide"},
whose print method attempts to print the outputs beside each other
for easier visual comparison.
If the argument \code{randomCheck} is \code{TRUE}, then the output includes
an attribute, \code{usedRandomSeed}, which is a logical vector with one element
per evaluator indicating whether random numbers are used in the calculations
on each evaluator.
}
\item{\code{RCompare}}{usually returns the same value as \code{multiEvaluator},
with the addition of the attribute \code{all.equal},
describing the differences between the results.}
\item{\code{RCompare(..., returnValues)}}{returns just the list of differences.}
\item{\code{DefaultREvaluators}}{returns \code{getOption("REvaluators")}
if it is a list of \code{"REvaluator"} objects.
Otherwise, it returns \code{list(RinR::REvaluator, RinR::TERREvaluator)},
two of the built-in \code{REvaluator}s.}
}

\seealso{
\code{\link{makeREvaluator}}, \code{\link{TERREvaluator}}, \code{\link{sideBySide}} 
}
\examples{
\dontrun{
RCompare(log2(xTest), data = list(xTest=4:16))
RCompare(lm(hp ~ gear + am, data=mtcars), REvaluators=list(REvaluator, LocalEvaluator), tol=1e-12)
myData <- subset(iris, Species == "setosa", -Species)
RCompare(prcomp(myData), data = "myData")
RCompare(kmeans(cbind(sin(1:10), cos(1:10)), 3), randomCheck=TRUE)
}
}
\keyword{utils}
