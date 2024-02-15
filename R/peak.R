#' @title Peak fitting function
#'
#' @description Peak fitting function
#'
#' @param v WIP
#' @param v0 WIP
#' @param fwhh WIP
#' @param p WIP
#' @param a WIP
#' @param b WIP
#'
#' @return  WIP
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
