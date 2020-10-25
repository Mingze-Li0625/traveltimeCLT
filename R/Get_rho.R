#' Calculate mean of auto-correlation
#' The algorithm only keeps trips ("trips") that have more than 10 ijk links.
#' On these links, we use a function called "get rho" allowing to calculate the mean of
#' the auto-correlation of the speed with a lag up to 5 for each trip.
#' If desired, the function also allows calculation specifically for AM or PM timebins.
#'
#' @param tt Train dataset sampled according to time-bin.
#' @param xx Trips that have more than 10 edges inside the train dataset sampled.
#' @examples
#' get_rho(tt = samp_obj, xx = samp_o)
#' @import data.table
#' @import traveltimeHMM
#' @export

get_rho <- function(tt = NULL, xx = NULL){
  rho = tt[xx$V1][,    drop((acf((1/speed - mean)/(sd+1e-5), plot=FALSE, lag.max=5))[[1]]), by = trip]
  a = rho[, V1[2] ,by = trip][, V1]
  rho = round(mean(a), 2)
  print(paste0("Mean of autocorrelation: ", rho))

  getrho <- list(rho = rho, tt = tt, xx = xx)

  class(getrho) <- append(class(getrho), "rho", after = 0)

  getrho
}
