#' Compute degree-days from daily minimum and maximum temperatures
#'
#' Calculates daily degree-days (thermal units) from Tmin and Tmax using several
#' common methods (average, triangular, sine). Optionally, an upper threshold
#' (Tupper) can be applied in methods supporting it.
#'
#' @param Tmin Daily minimum temperature (°C).
#' @param Tmax Daily maximum temperature (°C).
#' @param Tbase Base (lower developmental threshold) temperature (°C).
#' @param Tupper Optional upper temperature threshold (°C). Required for
#'   \code{"triangle_upper"} and \code{"sine_upper"} methods.
#' @param method Degree-day method. One of \code{"average"}, \code{"average_cut"},
#'   \code{"triangle"}, \code{"triangle_upper"}, \code{"sine"}, \code{"sine_upper"}.
#'
#' @return A numeric value (degree-days) rounded to 3 decimals.
#' @export
degree_days <- function(Tmin, Tmax, Tbase,
                        Tupper = NULL,
                        method = c("average", "average_cut",
                                   "triangle", "triangle_upper",
                                   "sine", "sine_upper")) {
  
  method <- match.arg(method)
  if (Tmax < Tmin) {
    tmp <- Tmin; Tmin <- Tmax; Tmax <- tmp
  }
  
  # ---- Average methods ----
  if (method %in% c("average", "average_cut")) {
    GD <- (Tmax + Tmin) / 2 - Tbase
    return(round(max(0, GD), 3))
  }
  
  # ---- Triangular methods ----
  if (method == "triangle") {
    if (Tmax <= Tbase) return(0)
    if (Tmin >= Tbase) return(round((Tmax + Tmin) / 2 - Tbase, 3))
    return(round((Tmax - Tbase)^2 / (2 * (Tmax - Tmin)), 3))
  }
  
  if (method == "triangle_upper") {
    if (is.null(Tupper)) stop("Tupper required", call. = FALSE)
    if (Tmax <= Tbase || Tmin >= Tupper) return(0)
    if (Tupper >= Tmax) return(degree_days(Tmin, Tmax, Tbase, method = "triangle"))
    Tmax_eff <- min(Tmax, Tupper)
    Tmin_eff <- max(Tmin, Tbase)
    return(round((Tmax_eff - Tmin_eff)^2 / (2 * (Tmax - Tmin)), 3))
  }
  
  # ---- Sine methods ----
  alpha <- (Tmax - Tmin) / 2
  Tmean <- (Tmax + Tmin) / 2
  
  if (method == "sine") {
    if (Tmax <= Tbase) return(0)
    if (Tmin >= Tbase) return(round(Tmean - Tbase, 3))
    x <- (Tbase - Tmean) / alpha
    x <- max(-1, min(1, x))
    theta <- acos(x)
    GD <- (alpha * sin(theta) + (Tmean - Tbase) * (pi - theta)) / pi
    return(round(GD, 3))
  }
  
  # ---- Sine + upper threshold (corrected) ----
  if (method == "sine_upper") {
    if (is.null(Tupper)) stop("Tupper required", call. = FALSE)
    if (Tmax <= Tbase || Tmin >= Tupper) return(0)
    if (Tupper >= Tmax) return(degree_days(Tmin, Tmax, Tbase, method = "sine"))
    
    x1 <- (Tbase - Tmean) / alpha
    x2 <- (Tupper - Tmean) / alpha
    x1 <- max(-1, min(1, x1))
    x2 <- max(-1, min(1, x2))
    
    t1 <- acos(x1)
    t2 <- acos(x2)
    if (t2 > t1) {
      tmp <- t1; t1 <- t2; t2 <- tmp
    }
    
    GD <- (alpha * (sin(t1) - sin(t2)) +
             (Tmean - Tbase) * (t2 - t1)) / pi
    return(round(max(0, GD), 3))
  }
  
  0
}
