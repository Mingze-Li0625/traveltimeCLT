#' Calculate mean of auto-correlation
#' The algorithm only keeps trips ("trips") that have more than 10 ijk links.
#' On these links, we use a function called "get rho" allowing to calculate the mean of
#' the auto-correlation of the speed with a lag up to 5 for each trip.
#' If desired, the function also allows calculation specifically for AM or PM timebins.
#'
#' @param data Train dataset sampled according to time-bin.
#' @param network_parameters Trips that have more than 10 edges inside the train dataset sampled.
#' @param lag
#' @param nsamples
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
        samp = sample_trips(data, nsamples)
        dt = data[tripID %in% samp][order(tripID, time)]
    }else dt = data
    
    ## not sure if we need to demain
    V = dt[,mean_variance_sequences(time[1], linkID, length, rho = rho, network_parameters, ...), by = tripID]

    D = merge(dt[,.(obstt = sum(traveltime)),tripID], V)
    
    return (D[, var(obstt - ETA)/sd])
}


