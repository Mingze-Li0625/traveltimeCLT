#' Sampling train dataset according to input time-bin.
#'
#' @param data.train Transformed train dataset coming from the function Create_graph
#' @param bin Allows to select a specific time-bin from the dataset.
#' @param M The number of samples to use from the train set. It is usefull especially when applying the travel time estimation algorithm on a specific time-bin.

#' @examples
#' sample_data(data.train = graph_obj$data.train, bin = bin, M = M)
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
