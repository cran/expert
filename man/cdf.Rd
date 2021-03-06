\name{cdf}
\alias{cdf}
\alias{print.cdf}
\alias{knots.cdf}
\alias{plot.cdf}
\title{Expert Aggregated Cumulative Distribution Function}
\description{
  Compute or plot the cumulative distribution function for objects of
  class \code{"expert"}.
}
\usage{
cdf(x, \dots)

\method{print}{cdf}(x, digits = getOption("digits") - 2, \dots)

\method{knots}{cdf}(Fn, \dots)

\method{plot}{cdf}(x, \dots, ylab = "F(x)", verticals = FALSE,
     col.01line = "gray70")
}
\arguments{
  \item{x}{an object of class \code{"expert"}; for the methods, an
    object of class \code{"cdf"}, typically.}
  \item{digits}{number of significant digits to use, see
    \code{\link{print}}.}
  \item{Fn}{an \R object inheriting from \code{"cdf"}.}
  \item{\dots}{arguments to be passed to subsequent methods, e.g.
    \code{\link{plot.stepfun}} for the \code{plot} method.}
  \item{ylab}{label for the y axis.}
  \item{verticals}{see \code{\link{plot.stepfun}}.}
  \item{col.01line}{numeric or character specifying the color of the
    horizontal lines at y = 0 and 1, see \code{\link{colors}}.}
}
\details{
  The function builds the expert aggregated cumulative distribution
  function corresponding to the results of \code{\link{expert}}.

  The function \code{plot.cdf} which implements the \code{\link{plot}}
  method for \code{cdf} objects, is implemented via a call to
  \code{\link{plot.stepfun}}; see its documentation.
}
\value{
  For \code{cdf}, a function of class \code{"cdf"}, inheriting from the
  \code{"\link{function}"} class.
}
\seealso{
  \code{\link{expert}} to create objects of class \code{"expert"};
  \code{\link{ogive}} for the linear interpolation;
  \code{\link{ecdf}} and \code{\link{stepfun}} for related
  documentation.
}
\examples{
x <- list(E1 <- list(A1 <- c(0.14, 0.22, 0.28),
                     A2 <- c(130000, 150000, 200000),
                     X <- c(350000, 400000, 525000)),
          E2 <- list(A1 <- c(0.2, 0.3, 0.4),
                     A2 <- c(165000, 205000, 250000),
                     X <- c(550000, 600000, 650000)),
          E3 <- list(A1 <- c(0.2, 0.4, 0.52),
                     A2 <- c(200000, 400000, 500000),
                     X <- c(625000, 700000, 800000)))
probs <- c(0.1, 0.5, 0.9)
true.seed <- c(0.27, 210000)
fit <- expert(x, "cooke", probs, true.seed, 0.03)
Fn <- cdf(fit)
Fn
knots(Fn)            # the group boundaries

Fn(knots(Fn))        # true values of the cdf

plot(Fn)             # graphic
}
\keyword{dplot}
\keyword{hplot}