### ===== experts =====
###
### Print method for object of class "expert"
###
### AUTHORS: Vincent Goulet <vincent.goulet@act.ulaval.ca>,
###          Mathieu Pigeon <mathieu.pigeon.3@ulaval.ca>

print.expert <- function(x, row.names = NULL, check.rows = FALSE,
                         check.names = TRUE, ...)
{
    ## Utility function
    numform <- function(x, w)
        formatC(x, digits = 2, width = w, format = "fg")

    ## The function must be called with an object of 'expert' class.
    breaks <- x$breaks
    probs <- x$probs
    n <- length(breaks)

    ## Return a data frame with formatted class in the
    ## first column.
    w <- max(nchar(formatC(breaks[-1])))            # longest upper boundary
    breaks <- paste( "(", numform(breaks[-n], -1), ", ", numform(breaks[-1], w), "]", sep = "")

    res <- data.frame(breaks, probs, row.names = row.names,
                      check.rows = check.rows, check.names = check.names)
    print(res)
    invisible(x)
}
