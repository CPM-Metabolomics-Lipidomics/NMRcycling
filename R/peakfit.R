#' @title Perform a peak fit
#'
#' @description Function that fits a mixed Gaussian-Lorentzian function to a supplied spectral region. It does this in
#' several steps. These are:
#' 1. Optimize peak area.
#' 2. Optimize the peak area and the baseline level.
#' 3. Same as 2., but also optimize the peak position.
#' 4. Same as 3., but also optimize the peak width.
#' 5. Same as 4., but also optimize the peak shape (Gaussian / Lorentzian).
#'
#' @param v Numeric vector containing the axis values (frequencies, chemical shifts...) for the points in the spectral region.
#' @param v0start Starting value for the peak position optimization.
#' @param fwhhstart Starting value for the peak width (full width at half height) optimization.
#' @param pstart Starting value for the peak shape optimization (Gaussian / Lorentzian).
#' @param astart Starting value for the peak area optimization.
#' @param bstart Starting value for the baseline level optimization.
#' @param sp Numeric vector containing a spectral region, ideally containing a single peak.
#'
#' @return  A list with the optimized peak parameters.
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
