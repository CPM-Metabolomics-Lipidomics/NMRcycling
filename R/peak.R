#' @title Mixed Gaussian-Lorentzian peak
#'
#' @description Function that generates a mixed Gaussian-Lorentzian peak for the given points (frequencies, chemical shifts...)
#'
#' @param v Numeric vector containing the axis values (frequencies, chemical shifts...) for the points in a spectral region.
#' @param v0 Position of the peak on the axis v.
#' @param fwhh Full width at half height of the peak.
#' @param p Peak shape (+Infinity indicates a perfect Gaussian, -Infinity a perfect Lorentzian).
#' @param a Peak area.
#' @param b Baseline level.
#'
#' @return  Numeric vector with the peak intensity at every element of v.
#'
#' @export
#'
#' @author Aswin Verhoeven
#'
peak <- function(v, v0, fwhh, p, a, b) {
  #r <- 1 / (1 + exp(-p)) logistic function reaches limits too fast
  r <- 0.5 + (atan(p) / pi)  # arctan has same basic shape but reaches limits much slower
  ga <- exp(log(2) * (1 - (2 * (v - v0) / fwhh)^2)) * sqrt(log(2) / pi) * (1 / fwhh)
  lo <- (1 / (fwhh^2 + 4 * (v - v0)^2)) * (2 * fwhh / pi)

  return(b + a * (r * ga + (1 - r) * lo))
}
