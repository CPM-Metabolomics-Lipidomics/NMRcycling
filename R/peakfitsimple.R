#' @title Perform a peak fit
#'
#' @description Function that fits a mixed Gaussian-Lorentzian function to a supplied spectral region. It does this in
#' several steps. These are:
#' 1. Optimize peak area.
#' 2. Optimize the peak area and the baseline level.
#' 3. Same as 2., but also optimize the peak position.
#' The difference between the function *peakfitsimple* and the function *peakfit* is that *peakfit* also optimizes the peak width and
#' the peak shape, while *peakfitsimple* does not. See also the function *peak*.
#'
#' @param v Numeric vector containing the axis values (frequencies, chemical shifts...) for the points in the spectral region.
#' @param v0start Starting value for the peak position optimization.
#' @param fwhhstart Value for the peak width (full width at half height).
#' @param pstart Value for the peak shape (Gaussian / Lorentzian).
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
