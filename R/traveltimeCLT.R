#' Travel time estimation modeling using Central Limit Theorem
#'
#' This function allows to calculate travel time estimation confidence intervals on the test set.
#' @param data A data frame of trips and their road level travel information, formated as \code{trips}, see \code{trips} or \code{View(data(trips))}.
#' @param lag as the highest order of residual autocorrelations to estimate, see \code{?residual_autocorrelation}. (Default \code{lag=1}). 
#' @param nsamples the number of observations sampled to estimate parameters. (Default \code{nsamples=500}).
#' @param min.links as the minimum number of links in trips used to estimate parameters.
#' @param timebin_rules is a list containing, start, end, days and tag for each timebin of the dataset (see example).

#' @examples
#' predict.traveltimeCLT(obj.traveltime = traveltimeCLT, data.test = test, bin = "MR", rules = list(list(start='6:30', end= '9:00', days = 0:6, tag='MR'),list(start='15:00', end= '18:00', days = 0:6, tag='ER')))
#' @import data.table
#' @import traveltimeHMM
#' @export

traveltimeCLT <- function(data, lag = 1L, nsamples=500L, min.links=5L, timebin_rules = NULL){

    ## convert to a data.table format
    if(!'data.table' %in% class(data))  data = data.table(data)

    ## estimate link mean and variance per time bin
    network_params  <- link_mean_variance(data, L = min.links)
    
    ## estimate auto-correlation parameters
    rho <- residual_autocorrelation(data, network_params, lag = lag, nsamples = nsamples)
    
    ## estimate residual variance
    v = residual_variance(data, network_params, rho = rho$average_correlation, nsamples = nsamples)
    
       
    ## returning variables
    traveltimeCLT_obj <- list(network_parameters = network_params,
                              rho = rho,
                              residual_variance = v
                              )
    class(traveltimeCLT_obj) = append(class(traveltimeCLT_obj), "traveltimeCLT", after=0)
    
    invisible(traveltimeCLT_obj)
}
