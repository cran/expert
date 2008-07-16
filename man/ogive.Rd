\name{ogive}
\alias{ogive}
\alias{print.ogive}
\alias{knots.ogive}
\alias{plot.ogive}
\title{Ogive of the Aggregate Expert Distribution}
\description{
  Compute a smoothed empirical distribution function for objects of
  class \code{"expert"}.
}
\usage{
ogive(x, \dots)

\method{print}{ogive}(x, digits = getOption("digits") - 2, \dots)

\method{knots}{ogive}(Fn, \dots)

\method{plot}{ogive}(x, main = NULL, xlab = "x", ylab = "F(x)", \dots)
}
\arguments{
  \item{x}{an object of class \code{"expert"}; for the methods, an
    object of class \code{"ogive"}, typically.}
  \item{digits}{number of significant digits to use, see
    \code{\link{print}}.}
  \item{Fn}{an \R object inheriting from \code{"ogive"}.}
  \item{main}{main title.}
  \item{xlab, ylab}{labels of x and y axis.}
  \item{\dots}{arguments to be passed to subsequent methods.}
}
\details{
  The ogive is a linear interpolation of the empirical cumulative
  distribution function.

  The equation of the ogive is
  \deqn{F_n(x) = \frac{(c_j - x) F_n(c_{j-1}) +
      (x - c_{j-1}) F_n(c_j)}{c_j - c_{j - 1}}}{%
    Fn(x) = ((c[j] - x) Fn(c[j-1]) +
    (x - c[j-1]) Fn(c[j]))/(c[j] - c[j-1])}
  for \eqn{c_{j-1} < x \leq c_j}{c[j-1] < x <= c[j]} and where
  \eqn{c_0, \dots, c_r}{c[0], \dots, c[r]} are the \eqn{r + 1} group
  boundaries.
}
\value{
  For \code{ogive}, a function of class \code{"ogive"}, inheriting from the
  \code{"\link{function}"} class.
}
\references{
  Klugman, S. A., Panjer, H. H. and Willmot, G. E. (1998),
  \emph{Loss Models, From Data to Decisions}, Wiley.
}
\seealso{
  \code{\link{expert}} to create objects of class \code{"expert"};
  \code{\link{cdf}} for the true cumulative distribution function;
  \code{\link{approxfun}}, which is used to compute the ogive;
  \code{\link{stepfun}} for related documentation (even though the ogive
  is not a step function).
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
Fn <- ogive(fit)
Fn
knots(Fn)            # the group boundaries

Fn(knots(Fn))        # true values of the empirical cdf
Fn(c(80, 200, 2000)) # linear interpolations

plot(Fn)
}
\keyword{dplot}
\keyword{hplot}