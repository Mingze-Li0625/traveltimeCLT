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

sample_data <- function(data.train = NULL, bin = NULL, M = NULL){
  # A.3 Sampling train dataset according to model input
  t = data.train[, .N , by=trip][N>10][, trip]
  sample_timebin = data.train[trip %in% t, .(timeBins= if(all(timeBins == timeBins[1])) timeBins[1] else 'remove' ), by=trip]
  sample_timebin = sample_timebin[!timeBins == 'remove']

  if(is.null(bin)){
    samp = sample_timebin[, trip]
    tt = data.train
    xx = tt[, .I[.N>10],by = trip]
  } else {
    samp = sample_timebin[timeBins == bin][, trip]
    samp = samp[sample.int(length(samp),M)]
    tt = data.train[trip %in% samp]
    xx = tt[trip %in% samp, .I[.N>10],by = trip]
  }
  samp_data <- list(samp = samp, tt = tt, xx = xx)

  class(samp_data) <- append(class(samp_data), "samp_data", after = 0)

  samp_data
}
