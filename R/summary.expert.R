### ===== expert =====
###
### Summary method for object of class "expert"
###
### AUTHORS: Vincent Goulet <vincent.goulet@act.ulaval.ca>,
###          Mathieu Pigeon <mathieu.pigeon.3@ulaval.ca>

summary.expert <- function(object, ...)
{
    class(object) <- c("summary.expert", class(object))
    object
}

print.summary.expert <- function(x, ...)
{
    cat("Call:\n")
    print(attr(x, "call"))
    NextMethod()                       # print.expert()
    if (is.null(x$alpha)) cat("\n")    # alpha not displayed
    cat(" Number of experts: ", x$nexp,
        ",\tNumber of seed variables: ", x$nseed, "\n", sep = "")
    cat(" Quantiles:",
        paste(x$quantiles, collapse = ", "), "\n")
    invisible(x)
}
