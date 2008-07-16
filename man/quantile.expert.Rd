\name{quantile.expert}
\alias{quantile.expert}
\title{Quantiles of the Aggregate Expert Distribution}
\description{
  Quantile for objects of class \code{"expert"}.
}
\usage{
\method{quantile}{expert}(x, probs = seq(0, 1, 0.25),
         smooth = FALSE, names = TRUE, \dots)
}
\arguments{
  \item{x}{an object of class \code{"expert"}.}
  \item{probs}{numeric vector of probabilities with values
    in \eqn{[0, 1)}.}
  \item{smooth}{logical; when \code{TRUE} and \code{x} is a step
    function, quantiles are linearly interpolated between knots.}
  \item{names}{logical; if true, the result has a \code{names}
    attribute.  Set to \code{FALSE} for speedup with many \code{probs}.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  The quantiles are taken directly from the cumulative distribution
  function defined in \code{x}. Linear interpolation is available for
  step functions.
}
\value{
  A numeric vector, named if \code{names} is \code{TRUE}.
}
\seealso{
  \code{\link{expert}}
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
quantile(fit)                             # default probs
quantile(fit, probs = c(0.9, 0.95, 0.99)) # right tail
}
\keyword{univar}
