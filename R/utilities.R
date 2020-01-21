#' Object Summaries Modified
#'
#' summary is a generic function used to produce result summaries of the results
#' of various model fitting functions. The function invokes particular methods
#' which depend on the class of the first argument.
#'
#' @note this function is a modification of the \code{base::summary} function to add:
#'    - p90
#'    - p10
#'    - st.dev
#'
#' @param object an object for which a summary is desired.
#' @param digits integer, used for number formatting with signif() (for summary.default)
#'    or format() (for summary.data.frame). In summary.default, if not specified
#'    (i.e., missing(.)), signif() will not be called anymore (since R >= 3.4.0,
#'    where the default has been changed to only round in the print and format methods).
#' @param ... additional arguments affecting the summary produced.
#'
#'
#' @return The form of the value returned by summary depends on the class of its argument.
#'    See the documentation of the particular methods for details of what is produced by that method.
#'
#'    The default method returns an object of class c("summaryDefault", "table") which has specialized
#'    format and print methods. The factor method returns an integer vector.
#'
#'    The matrix and data frame methods return a matrix of class "table", obtained by applying summary
#'    to each column and collating the results.
#'
#' @importFrom stats quantile sd
#'
#' @examples
#' \dontrun{
#' summary(data, digits = 2)
#' }
#'
#' @export
summary_mod <- function (object, ..., digits = max(3L, getOption("digits") - 3L)) {
  if (is.factor(object))
    return(summary.factor(object, ...))
  else if (is.matrix(object))
    return(summary.matrix(object, digits = digits, ...))
  value <- if (is.logical(object))
    c(Mode = "logical", {
      tb <- table(object, exclude = NULL)
      if (!is.null(n <- dimnames(tb)[[1L]]) && any(iN <- is.na(n))) dimnames(tb)[[1L]][iN] <- "NA's"
      tb
    })
  else if (is.numeric(object)) {
    nas <- is.na(object)
    object <- object[!nas]
    qq <- stats::quantile(object, probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))
    qq <- signif(c(qq[1L:4L], mean(object), qq[5L:7L], stats::sd(object)), digits)
    names(qq) <- c("Min.", "p90", "1st.Qu.", "Median", "Mean", "3rd.Qu.", "p10", "Max.", "St.Dev")
    if (any(nas))
      c(qq, `NA's` = sum(nas))
    else qq
  }
  else if (is.recursive(object) && !is.language(object) &&
           (n <- length(object))) {
    sumry <- array("", c(n, 3L), list(names(object), c("Length",
                                                       "Class", "Mode")))
    ll <- numeric(n)
    for (i in 1L:n) {
      ii <- object[[i]]
      ll[i] <- length(ii)
      cls <- oldClass(ii)
      sumry[i, 2L] <- if (length(cls))
        cls[1L]
      else "-none-"
      sumry[i, 3L] <- mode(ii)
    }
    sumry[, 1L] <- format(as.integer(ll))
    sumry
  }
  else c(Length = length(object), Class = class(object), Mode = mode(object))
  class(value) <- c("summaryDefault", "table")
  value
}




# # Negation of `%in%`
# # @param x array
# # @param y array
# # @export
# `%!in%` <- function(x, y) {!('%in%'(x, y))}

#' Breaks For log10 Scale
#'
#' @export
log_breaks <- 10^(-10:10)

#' Minor Breaks For log10 Scale
#'
#' @export
log_minor_breaks <- rep(1:9, 21) * (10^rep(-10:10, each = 9))
