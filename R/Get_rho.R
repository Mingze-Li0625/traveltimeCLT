#' Travel time estimation modeling using Central Limit Theorem
#'
#' This function allows to calculate travel time estimation confidence intervals on the test set.
#' @param obj.traveltime Object coming from the function traveltimeCLT
#' @param data.test Test set.
#' @param bin Allows to select a specific timebin from the dataset.
#' @param rules Need to represent a list containing, start, end, days and tag for each timebin of the dataset (see example).
#' @examples
#' predict.traveltimeCLT(obj.traveltime = traveltimeCLT, data.test = test, bin = "MR", rules = list(list(start='6:30', end= '9:00', days = 0:6, tag='MR'),list(start='15:00', end= '18:00', days = 0:6, tag='ER')))
#' @import data.table
#' @import traveltimeHMM
#' @export

get_rho <- function(tt = NULL, xx = NULL){
  # A.4 Get rho. The algorithm only keeps trips ("trips") that have more than 10 ijk links.
  # On these links, we will create and use a function called "get rho" allowing to calculate
  # the autocovariance of the speed with a lag up to 5 for each trip.
  # If desired, the function also allows calculation specifically for AM or PM timebins.
  rho = tt[xx$V1][,    drop((acf((1/speed - mean)/(sd+1e-5), plot=FALSE, lag.max=5))[[1]]), by = trip]
  a = rho[, V1[2] ,by = trip][, V1]
  rho = round(mean(a), 2)
  print(paste0("Mean of autocorrelation: ", rho))

  getrho <- list(rho = rho, tt = tt, xx = xx)

  class(getrho) <- append(class(getrho), "rho", after = 0)

  getrho
}
