#' @title Do a simple peak fit
#'
#' @description Do simple peak fit
#'
#' @param v WIP
#' @param v0start WIP
#' @param fwhhstart WIP
#' @param pstart WIP
#' @param astart WIP
#' @param bstart WIP
#' @param sp WIP
#'
#' @return  WIP
#'
#' @export
#'
#' @importFrom stats nls nls.control
#'
#' @author Aswin Verhoeven
#'
peakfitsimple <- function(v, v0start, fwhhstart, pstart, astart, bstart, sp) {
    dffit <- data.frame(v, sp)
    fit <- stats::nls(
        sp ~ peak(v, v0start, fwhhstart, pstart, a, bstart),
        data = dffit,
        start = list(a = astart),
        algorithm = "port",
        control = stats::nls.control(maxiter = 500)
      )
    afit <- summary(fit)[[10]][, "Estimate"]

    fit <- stats::nls(
        sp ~ peak(v, v0start, fwhhstart, pstart, a, b),
        data = dffit,
        start = list(a = afit, b = bstart),
        algorithm = "port",
        control = stats::nls.control(maxiter = 500)
      )
    afit <- summary(fit)[[10]]["a", "Estimate"]
    bfit <- summary(fit)[[10]]["b", "Estimate"]

    fit <- stats::nls(
        sp ~ peak(v, v0, fwhhstart, pstart, a, b),
        data = dffit,
        start = list(a = afit, b = bfit, v0 = v0start),
        algorithm = "port",
        control = stats::nls.control(maxiter = 500)
      )

    result <- as.list(summary(fit)[[10]][, "Estimate"])
    result$a <- ifelse(result$a < 0, 0, result$a)
    result$v <- v

    return(result)
  }
