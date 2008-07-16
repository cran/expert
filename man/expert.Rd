\name{expert}
\alias{expert}
\title{Modeling of Data Using Expert Opinion}
\description{
  Compute an aggregate distribution from expert opinion using either of
  the Cooke, Mendel-Sheridan and predefined weights models.
}
\usage{
expert(x, method = c("cooke", "ms", "weights"), probs, true.seed,
       alpha = NULL, w = NULL)
}
\arguments{
  \item{x}{a list giving experts' quantiles for the seed variables and
    the decision variable. See details below for the exact structure of
    the object.}
  \item{method}{method to be used to aggregate distributions.}
  \item{probs}{vector of probabilities corresponding to the quantiles
    given by the experts.}
  \item{true.seed}{vector of true values for the seed variables.}
  \item{alpha}{confidence level in Cooke model. If \code{NULL} or
    missing, the function determines the confidence level that
    maximizes the weight given to the aggregate distribution for the
    seed variables.}
  \item{w}{vector of weights in predefined weights model. If \code{NULL}
    or missing, equal weights are given to each expert.}
}
\details{
  Expert opinion is given by means of quantiles for \eqn{k} seed
  variables and one decision variable. Results for seed variables are
  compared to the true values and used to determine the influence of
  each expert on the aggregate distribution. The three methods supported
  are different ways to aggregate the information provided by the
  experts in one final distribution.

  The aggregate distribution in the \code{"cooke"} method is a convex
  combination of the quantiles, with weights obtained from the
  calibration phase. The \code{"weights"} method is similar, but weights
  are provided in argument to the function.

  In the \code{"ms"} (Mendel-Sheridan) method, the probabilities
  associated with each quantile are adjusted by a bayesian procedure to
  reflect results of the calibration phase.

  Object \code{x} is a list of lists, one for each expert. The latter
  contains \eqn{k + 1} vectors of quantiles, one for each seed variable
  and one for the decision variable (in this order).
}
\value{
  A function of class \code{"expert"}.

  There are methods available to represent (\code{print}), plot
  (\code{plot}), compute quantiles (\code{quantile}) and compute the
  mean (\code{mean}) of \code{"expert"} objects.
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
expert(x, "ms", probs, true.seed)

## Predefined weights model
expert(x, "weights", probs, true.seed)             # equal weights
expert(x, "weights", probs, true.seed, w = c(0.25, 0.5, 0.25))
}
\keyword{distribution}
\keyword{models}
\encoding{latin1}
