### ===== expert =====
###
### cdf() returns a function object to compute the cumulative
### distribution function based on the results of expert()
###
### AUTHORS: Vincent Goulet <vincent.goulet@act.ulaval.ca>,
### Mathieu Pigeon <mathieu.pigeon.3@ulaval.ca>

cdf <- function(x, ...)
{
    ## Compute the cumulative distribution function from an
    ## object of class 'expert'.
    if (!inherits(x, "expert"))
        stop("'x' must be an object of class \"expert\"")

    y <- x$probs
    x <- x$breaks

    ## Create an object of class 'cdf'.
    res <- approxfun(x, c(0, cumsum(y)), yleft = 0, yright = 1,
                     method = "constant", ties = "ordered")
    class(res) <- c("cdf", class(res))
    attr(res, "call") <- sys.call()
    res
}

### Essentially identical to stats::print.ecdf().
print.cdf <- function(x, digits = getOption("digits") - 2, ...)
{
    ## Utility function
    numform <- function(x) paste(formatC(x, dig = digits), collapse = ", ")

    ## The rest is adapted from ecdf()
    cat("Aggregate Expert CDF\nCall: ")
    print(attr(x, "call"), ...)
    nc <- length(xxc <- get("x", env = environment(x)))
    nn <- length(xxn <- get("y", env = environment(x)))
    i1 <- 1:min(3, nc)
    i2 <- if (nc >= 4) max(4, nc - 1):nc else integer(0)
    i3 <- 1:min(3, nn)
    i4 <- if (nn >= 4) max(4, nn - 1):nn else integer(0)
    cat("    x = ", numform(xxc[i1]), if (nc > 3) ", ",
        if (nc > 5) " ..., ", numform(xxc[i2]), "\n", sep = "")
    cat(" F(x) = ", numform(xxn[i3]), if (nn > 3) ", ",
        if (nn > 5) " ..., ", numform(xxn[i4]), "\n", sep = "")
    invisible(x)
}

### Identical to stats::knots.stepfun().
knots.cdf <- stats:::knots.stepfun

plot.cdf <- function(x, main = NULL, xlab = "x",
                     ylab = expression(F[n](x)), ...)
{
    if (missing(main))
        main <- {
            cl <- attr(x, "call")
            deparse(if (!is.null(cl)) cl else sys.call())
        }

    kn <- knots(x)
    Fn <- x(kn)
    plot(kn, Fn,  ..., type = "s", pch = 16,
         main = main, xlab = xlab, ylab = ylab)
}
