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
residual_autocorrelation<-function(data, network_parameters, lag=1L, nsamples=500L){
    if(!is.null(nsamples)){
        samp = sample_trips(data, nsamples, min.links = lag + 1)
        dt = from_to_format(data[tripID %in% samp])
    }else dt = data
    
    train = merge(dt, network_parameters,
                  all.x = TRUE,
                  by = c('linkID.from', 'linkID.to', 'timeBin'))[order(tripID,time)]
    
    ## not sure if we need to demain
    rho = train[, .(tripID[1], lag = 0:lag,
                    correlation = drop(acf((1/speed - mean)/(sd+1e-10),
                                           plot=FALSE,
                                           lag.max=lag,
                                           demean=TRUE)$acf)), tripID]
    rho[lag != 0, .(average_correlation = mean(correlation, na.rm = TRUE)), lag]
}


