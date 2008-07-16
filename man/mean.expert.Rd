\name{mean.expert}
\alias{mean.expert}
\title{Arithmetic Mean of the Aggregate Expert Distribution}
\description{
  Mean of objects of class \code{"expert"}.
}
\usage{
\method{mean}{expert}(x, \dots)
}
\arguments{
  \item{x}{an object of class \code{"expert"}.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  The mean of a distribution with probabilities \eqn{p_1, \dots,
    p_r}{p[1], \dots, p[r]} on intervals defined by the boundaries
  \eqn{c_0, \dots, c_r}{c[1], \dots, c[r]} is
  \deqn{\sum_{j = 1}^r \frac{c_{j - 1} + c_j}{2}\, p_j.}{%
    sum(j; (c[j - 1] + c[j])/2 * p[j]).}
}
\value{
  A numeric value.
}
\seealso{
  \code{\link{expert}} to create objects of class \code{"expert"}
}
\references{
  Klugman, S. A., Panjer, H. H. and Willmot, G. E. (1998),
  \emph{Loss Models, From Data to Decisions}, Wiley.
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
mean(fit)
}
\keyword{univar}