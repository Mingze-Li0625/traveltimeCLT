#' Travel time estimation modeling using Central Limit Theorem
#'
#' This function allows to calculate travel time estimation confidence intervals on the test set.
#' @param data A data frame of trips and their road level travel information, formated as \code{trips}, see \code{trips} or \code{View(data(trips))}.
#' @param lag as the highest order of residual autocorrelations to estimate, see \code{?residual_autocorrelation}. (Default \code{lag=1}). 
#' @param nsamples the number of observations sampled to estimate parameters. (Default \code{nsamples=500}).
#' @param min.links as the minimum number of links in trips used to estimate parameters.
#' @param timebin_rules is a list containing, start, end, days and tag for each timebin of the dataset (see example).

#' @examples
#' 
#' @import data.table
#' @export
traveltimeCLT <- function(data, lag = 1L, estimate = c('both', 'mean-only'),
                          nsamples=500L,
                          min.links=5L,
                          timebin_rules = NULL){

    estimate <- tryCatch(match.arg(estimate),error=function(cond){
      stop("Parameter 'estimate' should 'both', for estimating the mean and variance, or 'mean-only'")
    })
    
    
    ## convert to a data.table format
    if(!'data.table' %in% class(data))  data = data.table(data)

    numobs = tapply(data$tripID, data$tripID, length)
    if(min(numobs) < lag +1)
        warning(paste(sum(numobs < lag+1),
                      'trips have less than',
                      lag,  'observation, and will not be used to estimate autocorrelations, or residual variance parameters'))
    
    ## estimate link mean and variance per time bin
    network_params  <- link_mean_variance(data, L = min.links)

    if(grepl('both', estimate)){
        ## estimate auto-correlation parameters
        rho <- residual_autocorrelation(data, network_params, lag = lag, nsamples = nsamples)
    
        ## estimate residual variance
        v = residual_variance(data, network_params, rho = rho$average_correlation,
                              nsamples = nsamples, timebin_rules = timebin_rules)
    }else{
        rho <- v <-NULL
    }
    
    ## returning variables
    traveltimeCLT_obj <- list(network_parameters = network_params,
                              rho = rho,
                              residual_variance = v,
                              nsamples = nsamples,
                              min.links = min.links,
                              lag = lag,
                              timebin_rules = timebin_rules,
                              estimate  = estimate
                              )
    class(traveltimeCLT_obj) = append(class(traveltimeCLT_obj), "traveltimeCLT", after=0)
    
    invisible(traveltimeCLT_obj)
}

