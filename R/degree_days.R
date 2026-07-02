#' Compute degree-days from daily minimum and maximum temperatures
#'
#' Calculates daily degree-days (thermal units) from daily minimum and maximum
#' temperatures using average, single triangle, and single sine methods. Methods
#' with an upper threshold count development only within the interval from
#' \code{Tbase} to \code{Tupper}, which is a vertical cutoff approach.
#'
#' @param Tmin Daily minimum temperature (degrees C).
#' @param Tmax Daily maximum temperature (degrees C).
#' @param Tbase Base/lower developmental threshold temperature (degrees C).
#' @param Tupper Optional upper developmental threshold temperature (degrees C).
#' @param method Degree-day method. One of \code{"average"},
#'   \code{"average_cut"}, \code{"triangle"}, \code{"triangle_upper"},
#'   \code{"sine"}, or \code{"sine_upper"}.
#'
#' @return A numeric value (degree-days) rounded to 3 decimals.
#' @export
degree_days <- function(Tmin, Tmax, Tbase,
                        Tupper = NULL,
                        method = c("average", "average_cut",
                                   "triangle", "triangle_upper",
                                   "sine", "sine_upper")) {
  method <- match.arg(method)
  Tmin <- suppressWarnings(as.numeric(Tmin))
  Tmax <- suppressWarnings(as.numeric(Tmax))
  Tbase <- suppressWarnings(as.numeric(Tbase))
  if (!is.null(Tupper)) Tupper <- suppressWarnings(as.numeric(Tupper))

  if (!is.finite(Tmin) || !is.finite(Tmax) || !is.finite(Tbase)) return(NA_real_)
  if (Tmax < Tmin) {
    tmp <- Tmin
    Tmin <- Tmax
    Tmax <- tmp
  }

  upper_methods <- c("average_cut", "triangle_upper", "sine_upper")
  if (method %in% upper_methods) {
    if (is.null(Tupper) || !is.finite(Tupper) || Tupper <= Tbase) {
      stop("A valid Tupper greater than Tbase is required for upper-threshold methods.", call. = FALSE)
    }
  }

  daily_contribution <- function(temp, upper = FALSE) {
    dd <- pmax(temp - Tbase, 0)
    if (upper) dd[temp >= Tupper] <- 0
    dd
  }

  if (method == "average") {
    return(round(max(0, ((Tmin + Tmax) / 2) - Tbase), 3))
  }

  if (method == "average_cut") {
    tmean <- (Tmin + Tmax) / 2
    dd <- if (tmean >= Tupper) 0 else max(0, tmean - Tbase)
    return(round(dd, 3))
  }

  if (Tmax <= Tbase) return(0)
  if (method %in% upper_methods && Tmin >= Tupper) return(0)

  n_steps <- 1440L
  t <- seq(0, 1, length.out = n_steps)

  if (method %in% c("triangle", "triangle_upper")) {
    temp <- ifelse(
      t <= 0.5,
      Tmin + (Tmax - Tmin) * (t / 0.5),
      Tmax - (Tmax - Tmin) * ((t - 0.5) / 0.5)
    )
    dd <- mean(daily_contribution(temp, upper = method == "triangle_upper"))
    return(round(dd, 3))
  }

  if (method %in% c("sine", "sine_upper")) {
    alpha <- (Tmax - Tmin) / 2
    tmean <- (Tmax + Tmin) / 2
    temp <- tmean - alpha * cos(2 * pi * t)
    dd <- mean(daily_contribution(temp, upper = method == "sine_upper"))
    return(round(dd, 3))
  }

  0
}