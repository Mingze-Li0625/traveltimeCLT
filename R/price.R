
#' Ride pricing calculator
#' 
#' This function calculate the ride price.
#' 
#' @param duration the duration of the price in seconds. This could be a vector.
#' @param trip_length the length of the trip in meters. This could be a vector.
#' @param C0 the constant pricing coefficient.
#' @param C1 the time pricing coefficient, its unit should be CAD/min.
#' @param C2 the distance pricing coefficient, its unit should be CAD/km.
#' @param risk_free the annual risk free rate, please use real value, like 0.03 for 3% rate.
#' @return a data.table, the first column is the price when the trip finished, and
#'  the second is the price when the trip begin.
#' @examples
#' price(c(10000,20000),c(100000,200000),4,0.4,0.8,0.03)
#' @author Mingze Li <mingzeli7@cmail.carleton.ca>
#' @export
#' @import data.table
price <- function(duration,trip_length,C0=3.17,C1=0.31,C2=0.9,risk_free=0.0302){
  duration <- as.numeric(duration)
  hour_rate <- (1+risk_free)^(1/(365*24))-1
  arrive_price <- C0+C1*(duration/60)+C2*(trip_length/1000)
  start_price <- arrive_price/exp(hour_rate*(duration/3600))
  result <- cbind(arrive_price,start_price)
  result <- data.table(result)
  result
}
#' 
#' @param t0 arrive time
#' @param t1 pick up time
#' @param t2 request time
arrive_R <- function(t0,t2,t1,trip_length,K="A",C0=3.17,C1=0.31,C2=0.9,risk_free=0.0302,zeta=0){
  t0=as.POSIXct(t0)
  t1=as.POSIXct(t1)
  t2=as.POSIXct(t2)
  textK=tolower(as.character(K))
  arrive_price <- price(difftime(t0,t1,units = "sec"),trip_length,C0,C1,C2,risk_free)[,2]
  if(textK=="a")K=arrive_price
  if(arrive_price>=K*exp(zeta))arrive_R <- pmax(0,arrive_price-K)
  else arrive_R <- 0
  result <- cbind(arrive_R)
  result <- data.table(result)
  result
}
#' Price of the guarantee calculator
#' 
#' This function calculates the price of the guarantee at the request time.
#' 
#' @param predict_data the data produced by the travelCLT predicting function. Every row match
#' the information of t1 and t2. Should have a ETA column in second.
#' @param t1 pick up time, could be a vector.
#' @param t2 request time, could be a vector.
#' @param trip_length the length of the trip in meters. This could be a vector.
#' @param K the promised price. This could be a vector.
#' @param C0 the constant pricing coefficient.
#' @param C1 the time pricing coefficient, its unit should be CAD/min.
#' @param C2 the distance pricing coefficient, its unit should be CAD/km.
#' @param risk_free the annual risk free rate, please use real value, like 0.03 for 3% rate.
#' @param zeta the non-negative constant, refer as the percentage threshold.
#' @author Mingze Li <mingzeli7@cmail.carleton.ca>
#' @examples
#' 
#' data <- data.frame(ETA = 1800, variance = 3600)
#' request_R(data, "2023-01-01 08:00:00", "2023-01-01 08:30:00", 40214.33,20,3,0.4,1,0.03,0.2)  
#' @export
#' 
request_R <- function(predict_data,t2,t1,trip_length,K="A",C0=3.17,C1=0.31,C2=0.9,risk_free=0.0302,zeta=0){
  t1=as.POSIXct(t1)
  t2=as.POSIXct(t2)
  C2 <- C2 / 1000  # 0.9 CAD/km -> 0.0009 CAD/m
  C1 <- C1 / 60    # 0.31 CAD/min -> ~0.0051667 CAD/sec
  
  seconds_per_year <- 365 * 24 * 3600
  r_annual <- log(1 + risk_free)
  r_second <- r_annual / seconds_per_year
  delta_t_seconds <- as.numeric(difftime(t1, t2, units = "sec")) 
  if (!all(delta_t_seconds >= 0)) stop("all t2 must be earlier or equal to t1")

  textK=tolower(as.character(K))
  if(textK=="a")K=C0+C1*predict_data$ETA+C2*trip_length
  
  d0 <- C0+C1*predict_data$ETA+C2*trip_length
  d1 <- sqrt(predict_data$variance)
  d4 <- (K * exp(zeta) - d0) / (C1 * d1)
  d5 <- d4 + r_second * d1
  d7 <- predict_data$ETA - r_second * (d1^2) / 2
  discount_factor <- exp(-r_second * delta_t_seconds)
  result <- discount_factor * exp(-r_second * d7) *
    ((d0 - r_second * C1 * (d1^2) - K) * (1 - pnorm(d5)) - C1 * d1 * dnorm(d5))
  result
}

