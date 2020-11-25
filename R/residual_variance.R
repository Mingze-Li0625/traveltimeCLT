#' Calculate mean of auto-correlation
#' The algorithm only keeps trips ("trips") that have more than 10 ijk links.
#' On these links, we use a function called "get rho" allowing to calculate the mean of
#' the auto-correlation of the speed with a lag up to 5 for each trip.
#' If desired, the function also allows calculation specifically for AM or PM timebins.
#'
#' @param data Train dataset sampled according to time-bin.
#' @param network_parameters Trips that have more than 10 edges inside the train dataset sampled.
#' @param lag maximum lag at which to calculate the acf.  Default is 1.
#' @param nsamples number of random trip to sample for parameter estimation. Default is 500.
#'
#' @examples
#' \dontrun{
#'
#'
#' }
#' @import data.table
#' @export
residual_variance <-function(data, network_parameters, rho = 1L, nsamples=500L, ...){

    if(!is.null(nsamples)){
        samp = sample_trips(data, nsamples, min.links = length(rho))
        dt = data[tripID %in% samp][order(tripID, entry_time)]
    }else dt = data
    
    ## not sure if we need to demain
    V = dt[,predict.traveltimeCLT.trip_specific(entry_time[1], linkID,
                                                distance_meters, rho = rho,
                                                network_parameters, ...), by = tripID]
    
    D = merge(dt[,.(dur = sum(duration_secs)),tripID], V)

    D[, var((dur - ETA)/sqrt(variance))]
}


