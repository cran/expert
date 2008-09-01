\name{expert}
\alias{expert}
\alias{print.expert}
\alias{summary.expert}
\alias{print.summary.expert}
\title{Modeling of Data Using Expert Opinion}
\description{
  Compute an aggregated distribution from expert opinion using either of
  the Cooke, Mendel-Sheridan or predefined weights models.
}
\usage{
expert(x, method = c("cooke", "ms", "weights"), probs, true.seed,
       alpha = NULL, w = NULL)

\method{print}{expert}(x, \dots)

\method{summary}{expert}(object, \dots)

\method{print}{summary.expert}(x, \dots)
}
\arguments{
  \item{x}{a list giving experts' quantiles for the seed variables and
    the decision variable. See details below for the exact structure of
    the object. For the methods: an object of class \code{"expert"}.}
  \item{method}{method to be used to aggregate distributions.}
  \item{probs}{vector of probabilities corresponding to the quantiles
    given by the experts.}
  \item{true.seed}{vector of true values for the seed variables.}
  \item{alpha}{confidence level in Cooke model. If \code{NULL} or
    missing, the function determines the confidence level that
    maximizes the weight given to the aggregated distribution for the
    seed variables.}
  \item{w}{vector of weights in predefined weights model. If \code{NULL}
    or missing, equal weights are given to each expert.}
  \item{object}{an object of class \code{"expert"}}
  \item{\dots}{further arguments to \code{\link[base]{format}} for the
    \code{print} and \code{print.summary} methods; unused for the
    \code{summary} method.}
}
\details{
  Expert opinion is given by means of quantiles for \eqn{k} seed
  variables and one decision variable. Results for seed variables are
  compared to the true values and used to determine the influence of
  each expert on the aggregated distribution. The three methods supported
  are different ways to aggregate the information provided by the
  experts in one final distribution.

  The aggregated distribution in the \code{"cooke"} method is a convex
  combination of the quantiles, with weights obtained from the
  calibration phase. The \code{"weights"} method is similar, but weights
  are provided in argument to the function.

  In the \code{"ms"} (Mendel-Sheridan) method, the probabilities
  associated with each quantile are adjusted by a bayesian procedure to
  reflect results of the calibration phase.

  Object \code{x} is a list of lists, one for each expert. The latter
  contains \eqn{k + 1} vectors of quantiles, one for each seed variable
  and one for the decision variable (in this order).

  If \code{x} does not contain the 0th and/or the 100th quantile, they
  are determined by removing and adding 10\% of the smallest interval
  containing all quantiles given by the experts to the bounds of this
  interval. Note also that only the Mendel-Sheridan model allows
  non-finite lower and upper bounds.
}
\value{
  Function \code{expert} computes the aggregated distribution using the
  model specified in \code{model}. The value returned is an object of
  class \code{"expert"}.

  An object of class \code{"expert"} is a list containing at least the
  following components:
  \item{breaks}{vector of knots of the aggregated distribution.}
  \item{probs}{vector of probabilities of the aggregated distribution.}
  \item{nexp}{number of experts in the model.}
  \item{nseed}{number of seed variables in the model.}
  \item{quantiles}{vector of probabilities corresponding to the quantiles
    given by the experts.}

  In addition, for \code{method = "cooke"}, a component \code{alpha}
  containing the confidence level: either the value given in argument
  to the function or the optimized value.

  There are methods available to represent (\code{print}), plot
  (\code{plot}), compute quantiles (\code{quantile}), summarize
  (\code{summary}) and compute the mean (\code{mean}) of \code{"expert"}
  objects.
}
\references{
  Cooke, R. (1991), \emph{Expert in Uncertainty}, Oxford University Press.

  Mendel, M. and Sheridan, T. (1989), Filtering information from
    human experts, \emph{IEEE Transactions on Systems, Man and Cybernetics},
    \bold{36}, 6--16.

  Pigeon, M. (2008), \emph{Utilisation d'avis d'experts en actuariat}, M.Sc.
  thesis, \enc{Université}{Universite} Laval.
}
\examples{
## An example with three experts (E1, E2, E3), two seed variables
## (A1, A2) and three quantiles (10th, 50th and 90th).
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

## Cooke model
expert(x, "cooke", probs, true.seed, alpha = 0.03) # fixed alpha
expert(x, "cooke", probs, true.seed)               # optimized alpha

## Mendel-Sheridan model
fit <- expert(x, "ms", probs, true.seed)
fit				# print method
summary(fit)                    # more information

## Predefined weights model
expert(x, "weights", probs, true.seed)             # equal weights
expert(x, "weights", probs, true.seed, w = c(0.25, 0.5, 0.25))
}
\keyword{distribution}
\keyword{models}
\encoding{latin1}
