### ===== experts =====
###
### Quantiles for objects of class "expert"
###
### AUTHORS: Vincent Goulet <vincent.goulet@act.ulaval.ca>,
###          Mathieu Pigeon <mathieu.pigeon.3@ulaval.ca>

quantile.expert <- function(x, probs = seq(0, 1, 0.25),
                            smooth = FALSE, names = TRUE, ...)
{
    ## An empirical and discrete approach is used for
    ## 'expert' objects.
    y <- cumsum(x$probs)
    x <- x$breaks
    ind <- sapply(probs, function(q) match(TRUE, y >= q))

    res <-
        if (smooth)
        {
            h <- (y[ind] - probs) / (y[ind] - y[ind - 1])
            (1 - h) * x[ind - 1] + h * x[ind]
        }
        else
            x[ind]

    if (names)
    {
        dig <- max(2, getOption("digits"))
        names(res) <- formatC(paste(100 * probs, "%", sep = ""),
                              format = "fg", wid = 1, digits = dig)
    }
    res
}
