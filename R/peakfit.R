#' @title Do a peak fit
#'
#' @description Do peak fit
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
peakfit <- function(v, v0start, fwhhstart, pstart, astart, bstart, sp) {
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
  afit <- summary(fit)[[10]]["a", "Estimate"]
  bfit <- summary(fit)[[10]]["b", "Estimate"]
  v0fit <- summary(fit)[[10]]["v0", "Estimate"]

  fit <- stats::nls(
    sp ~ peak(v, v0, fwhh, pstart, a, b),
    data = dffit,
    start = list(a = afit,
                 b = bfit,
                 v0 = v0fit,
                 fwhh = fwhhstart),
    algorithm = "port",
    control = stats::nls.control(maxiter = 500)
  )
  afit <- summary(fit)[[10]]["a", "Estimate"]
  bfit <- summary(fit)[[10]]["b", "Estimate"]
  v0fit <- summary(fit)[[10]]["v0", "Estimate"]
  fwhhfit  <- summary(fit)[[10]]["fwhh", "Estimate"]

  fit <- stats::nls(
    sp ~ peak(v, v0, fwhh, p, a, b),
    data = dffit,
    start = list(a = afit,
                 b = bfit,
                 v0 = v0fit,
                 fwhh = fwhhfit,
                 p = pstart),
    algorithm = "port",
    control = stats::nls.control(maxiter = 500,
                                 warnOnly = TRUE)
  )

  result <- as.list(summary(fit)[[10]][, "Estimate"])
  result$a <- ifelse(result$a < 0, 0, result$a)
  result$v <- v

  return(result)
}
