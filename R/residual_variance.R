#' Estimates the sample variance of the standardized travel residual
#'
#' \code{residual_variance} estimate the sample variance of standardized travel residual for a set of trips.
#'
#' @param data A data frame of trips and their road level travel information, formatted as \code{trips}, see \code{trips} or \code{data(trips); View(trips)}.
#' @param network_parameters An output of \code{link_mean_variance}, see \code{?link_mean_variance}.
#' @param lag Maximum lag at which to calculate the autocorrelations. Default is 1 for the first order-autocorrelations.
#' @param nsamples The number of trips to sample for parameter estimation. Default is 500.
#' @param ... Extra parameters to be passed to \code{predict.traveltimeCLT.trip_specific}.
#'
#' @details The function predicts 'trip-specific' mean and variance of travel time of a sample of trips, given a set of parameter estimates. With such prediction, it estimates the standardized residual and calculates its sample variance. The trip-specific method is a Gaussian-based model, therefore the estimated residual, theoretically, should be 1. Hence, a residual variance of 1.5 resembles \code{sqrt(1.5)-1 = 0.22} of unexplained variability of the model.
#'
#' @return Returns the sample variance of the standardized residual.
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
                                                distance_meters, network_parameters,
                                                rho = rho,
                                                ...), by = tripID]
    
    D = merge(dt[,.(dur = sum(duration_secs)),tripID], V)

    D[, var((dur - ETA)/sqrt(variance))]
}


