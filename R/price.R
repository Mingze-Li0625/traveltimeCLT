#' Ride pricing calculator
#'
#' Calculate ride prices based on duration and distance
#'
#' @param duration numeric|list Trip duration in seconds (converted to minutes internally). If list, expects output
#'        from \code{\link{route_time}} list containing `expect_time` elements
#' @param trip_length numeric|list Trip distance in meters (converted to kilometers internally). If list, expects output
#'        from \code{\link{route_length}} containing `total_length` elements
#' @param C0 numeric Base fare (CAD)
#' @param C1 numeric Time rate (CAD/minute)
#' @param C2 numeric Distance rate (CAD/kilometer)
#' @param risk_free numeric Annual risk-free rate (e.g., 0.03 for 3%)
#' @return data.frame with two columns:
#'         - arrive_price: Price charged upon trip completion
#'         - start_price: Price charged when trip begins
#' @examples
#' price(c(10000, 20000), c(100000, 200000), 4, 0.4, 0.8, 0.03)
#' @author Mingze Li <mingzeli7@cmail.carleton.ca>
#' @export
#' @import data.table
price <- function(duration, trip_length, C0 = 3.17, C1 = 0.31, C2 = 0.9, risk_free = 0.0302) {
  if (is.list(duration)) duration <- sapply(duration, function(x) x$expect_time)
  if (is.list(trip_length)) trip_length <- sapply(trip_length, function(x) x$total_length)
  duration <- as.numeric(duration)
  hour_rate <- (1 + risk_free)^(1 / (365 * 24)) - 1
  arrive_price <- C0 + C1 * (duration / 60) + C2 * (trip_length / 1000)
  start_price <- arrive_price / exp(hour_rate * (duration / 3600))
  result <- cbind(arrive_price, start_price)
  result <- data.frame(result)
  result
}
#' Price of the guarantee calculator
#'
#' This function calculates the price of the guarantee at the arrive time.
#'
#'
#' @param t0 arrive time, could be a vector.
#' @param t1 pick up time, could be a vector.
#' @param t2 request time, could be a vector.
#' @param trip_length the length of the trip in meters. This could be a vector.
#' @param K the promised price. This could be a vector. When type is proportion, this input refer
#' as the multiple of the expected trip price.
#' @param C0 the constant pricing coefficient.
#' @param C1 the time pricing coefficient, its unit should be CAD/min.
#' @param C2 the distance pricing coefficient, its unit should be CAD/km.
#' @param risk_free the annual risk free rate, please use real value, like 0.03 for 3% rate.
#' @param zeta the non-negative constant, refer as the percentage threshold.
#' @param type Change the K input to multiple of expected trip price if it is equal to "proportion".
#' @author Mingze Li <mingzeli7@cmail.carleton.ca>
#' @examples
#' arrive_R("2023-01-01 09:00:00", "2023-01-01 08:00:00", "2023-01-01 08:30:00", 40214.33, 40, 3, 0.4, 1, 0.03, 0.2, "")
#' arrive_R("2023-01-01 09:00:00", "2023-01-01 08:00:00", "2023-01-01 08:30:00", 40214.33, 0.8, 3, 0.4, 1, 0.03, 0.2)
#' @export
arrive_R <- function(t0, t2, t1, trip_length, K = 1, C0 = 3.17, C1 = 0.31, C2 = 0.9, risk_free = 0.0302, zeta = 0, type = "proportion") {
  t0 <- as.POSIXct(t0)
  t1 <- as.POSIXct(t1)
  t2 <- as.POSIXct(t2)
  type <- tolower(as.character(type))
  arrive_price <- price(difftime(t0, t1, units = "sec"), trip_length, C0, C1, C2, risk_free)[, 1]
  type <- tolower(as.character(type))
  if (type == "proportion") K <- K * arrive_price
  if (arrive_price >= K * exp(zeta)) {
    arrive_R <- pmax(0, arrive_price - K)
  } else {
    arrive_R <- 0
  }
  arrive_R
}
#' Guarantee pricing calculator (request time)
#'
#' Calculate guarantee price at service request time using travel time predictions
#'
#' @param predict_data data.frame Prediction data from traveltimeCLT containing:
#'        - ETA: predicted time in seconds
#'        - variance: predicted time variance
#' @param t1 POSIXct|character Scheduled pickup time(s)
#' @param t2 POSIXct|character Service request time(s)
#' @param trip_length numeric Trip distance in meters
#' @param K numeric Guaranteed price threshold. When `type = "proportion"`,
#'        represents multiple of expected price
#' @param C0 numeric Base fare (CAD)
#' @param C1 numeric Time rate (CAD/minute)
#' @param C2 numeric Distance rate (CAD/kilometer)
#' @param risk_free numeric Annual risk-free rate (e.g., 0.03 for 3%)
#' @param zeta numeric Non-negative price protection threshold
#' @param type character Pricing type: "proportion" scales K by expected price
#' @return numeric Vector of guarantee prices
#' @author Mingze Li <mingzeli7@cmail.carleton.ca>
#' @examples
#'
#' data <- data.frame(ETA = 1800, variance = 3600)
#' request_R(data, "2023-01-01 08:00:00", "2023-01-01 08:30:00", 40214.33, 40, 3, 0.4, 1, 0.03, 0.2, "")
#' request_R(data, "2023-01-01 08:00:00", "2023-01-01 08:30:00", 40214.33, 0.8, 3, 0.4, 1, 0.03, 0.2)
#' @export
#'
request_R <- function(predict_data, t2, t1, trip_length, K = 1, C0 = 3.17, C1 = 0.31, C2 = 0.9, risk_free = 0.0302, zeta = 0, type = "proportion") {
  t1 <- as.POSIXct(t1)
  t2 <- as.POSIXct(t2)
  C2 <- C2 / 1000 # 0.9 CAD/km -> 0.0009 CAD/m
  C1 <- C1 / 60 # 0.31 CAD/min -> ~0.0051667 CAD/sec

  seconds_per_year <- 365 * 24 * 3600
  r_annual <- log(1 + risk_free)
  r_second <- r_annual / seconds_per_year
  delta_t_seconds <- as.numeric(difftime(t1, t2, units = "sec"))
  if (!all(delta_t_seconds >= 0)) stop("all t2 must be earlier or equal to t1")

  type <- tolower(as.character(type))
  d0 <- C0 + C1 * predict_data$ETA + C2 * trip_length
  if (type == "proportion") K <- K * d0
  d1 <- sqrt(predict_data$variance)
  d4 <- (K * exp(zeta) - d0) / (C1 * d1)
  d5 <- d4 + r_second * d1
  d7 <- predict_data$ETA - r_second * (d1^2) / 2
  discount_factor <- exp(-r_second * delta_t_seconds)
  result <- discount_factor * exp(-r_second * d7) *
    ((d0 - r_second * C1 * (d1^2) - K) * (1 - pnorm(d5)) + C1 * d1 * dnorm(d5))
  result
}
#' Trip price calculator (request time)
#'
#' Calculate discounted trip price at service request time
#'
#' @param predict_data data.frame Prediction data containing ETA (seconds)
#' @param trip_length numeric Trip distance in meters
#' @param C0 numeric Base fare (CAD)
#' @param C1 numeric Time rate (CAD/minute - converted to CAD/second internally)
#' @param C2 numeric Distance rate (CAD/kilometer - converted to CAD/meter internally)
#' @param discount_factor numeric Discount multiplier (default = 1)
#' @param t1 POSIXct|character Scheduled pickup time(s)
#' @param t2 POSIXct|character Service request time(s)
#' @return numeric Vector of discounted prices
#' @author Mingze Li <mingzeli7@cmail.carleton.ca>
#' @examples
#' data <- data.frame(ETA = 1800)
#' request_K(data, "2023-01-01 08:00:00", "2023-01-01 08:30:00", 40214.33)
#' @export
request_K <- function(predict_data, trip_length, C0 = 3.17, C1 = 0.31, C2 = 0.9, discount_factor = 1) {
  C2 <- C2 / 1000 # 0.9 CAD/km -> 0.0009 CAD/m
  C1 <- C1 / 60 # 0.31 CAD/min -> ~0.0051667 CAD/sec

  K <- C0 + C1 * predict_data$ETA + C2 * trip_length
  request_K <- K * discount_factor

  return(request_K)
}
