### ===== expert =====
###
### Internal calculations for Cooke model
###
### AUTHORS: Vincent Goulet <vincent.goulet@act.ulaval.ca>,
### Mathieu Pigeon <mathieu.pigeon.3@ulaval.ca>

cooke <- function(nprobs, nexp, nseed, true.seed, qseed, qk, alpha)
{
    ## Compute first set of weights and calibration coefficients.
    cw <- cooke.weights(if (is.null(alpha)) 0 else alpha,
                        nprobs, nexp, nseed, true.seed,
                        qseed[, - (nseed + 1), , drop = FALSE], qk)

    w <- cw$weights

    ## If 'alpha' is NULL, the function determines confidence level
    ## that maximizes the weight given by the model to a virtual
    ## expert who would give aggregate distribution for seed variables.
    if (is.null(alpha))
    {
        ## Determination of optimized alpha: function computes weight
        ## given to the virtual expert for 'alpha' = 0.001, 0.002, ...,
        ## 1.000 and retains maximal value.
        qseedmod <- array(c(qseed,
                          apply(qseed, 2, "%*%", cw$weights)),
                        dim = c(nprobs, nseed + 1, nexp + 1))

        alpha <- cw$calibration - .Machine$double.eps
        qseedmod <- qseedmod[, -(nseed + 1), , drop = FALSE]
        w <- sapply(alpha, FUN <- function(x) cooke.weights(x, nprobs,
                                                            nexp + 1,
                                                            nseed, true.seed,
                                                            qseedmod,
                                                            qk)$weights)

        wmax <- which.max(w[nexp + 1,]) # find the maximum weight
        alpha <- alpha[wmax]            # find the optimized alpha
        w <- w[-(nexp + 1), wmax]/sum(w[-(nexp + 1), wmax])
    }

    ## Compute aggregate distribution
    breaks <- drop(matrix(qseed[, nseed + 1, 1:nexp], ncol = nexp) %*% w)
    list(breaks = breaks, probs = qk)
}

cooke.weights <- function(alpha, nprobs, nexp, nseed, true.seed, qseed, qk)
{
    ## Function to determine in which interquantile space fall the
    ## true values of the seed variable 'i' for each expert. Returns a
    ## (nprobs - 1) x nexp boolean matrix.
    s <- seq_len(nprobs - 1) # interquantile spaces IDs
    fun <- function(i)
        apply(matrix(qseed[, i, ], ncol = nexp), 2,
              function(v) cut(true.seed[i], v, labels = FALSE) == s)
    ## Compute the empirical distribution for each interquantile space
    ## and each expert.
    S <- array(rowSums(sapply(1:nseed, fun))/nseed, c(nprobs - 1, nexp))

    ## Calibration
    calibration <- pchisq(2 * nseed * colSums(S * pmax(log(S/qk), 0)),
                  df = nseed - 1, lower.tail = FALSE)

    ## Entropy
    entropy <- colSums(apply(qseed, c(2, 3), function(x)
                         log(diff(range(x)))) +
                   colSums(qk * log(qk/apply(qseed, c(2, 3),
                                             diff)))) / nseed

    ## Weights
    nnw <- calibration * entropy * (calibration > alpha) # raw weights
    w <- nnw / sum(nnw)                 # normalized weights
    list(weights = w, calibration = calibration)
}
