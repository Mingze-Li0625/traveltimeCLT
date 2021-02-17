#' Travel time estimation modelling using the Central Limit Theorem
#'
#' This function allows for the calculation of travel time estimation confidence intervals on the test data set.
#' 
#' @param data A data frame of trips and their road level travel information, formated as \code{trips}, see \code{trips} or \code{View(data(trips))}.
#' @param model Specifies whether the \code{trip-specific} or \code{population} models should be used.
#' @param estimate Specifies whether to estimate \code{both} the mean and variance or \code{mean-only}. Only applied with \code{model=trip-specific}.
#' @param lag Maximum lag at which to calculate the autocorrelations. Default is 1 for the first order-autocorrelations.
#' @param nsamples The number of trips to sample for parameter estimation.
#' @param min.links The minimum number of links in each of the sampled trip.
#' @param timebin_rules A list containing, start, end, days and tag for each timebin of the dataset (see example in \code{time_bins}).
#' 
#' @details NULL
#'
#' @return Returns a list of the network parameters (namely the mean and standard deviation of travel time, estimated based on the train data set (see \code{link_mean_variance})) required for estimating travel times for the test data set.
#' 
#' @examples
#' \dontrun{
#' 
#' fit <- traveltimeCLT(train, lag = 1)
#' }
#' 
#' @import data.table
#' @export
traveltimeCLT <- function(data,
                          model = c('trip-specific', 'population'),
                          estimate = c('both', 'mean-only'),
                          lag = 1L,
                          nsamples=500L,
                          min.links=5L,
                          timebin_rules = NULL){

    model <- tryCatch(match.arg(model),error=function(cond){
        stop("Parameter 'model' should 'trip-specific'or 'population'")
    })

    estimate <- tryCatch(match.arg(estimate),error=function(cond){
        stop("Parameter 'estimate' should 'both', for estimating the mean and variance, or 'mean-only'")
    })
    
    ## convert to a data.table format
    if(!'data.table' %in% class(data))  data = data.table(data)

    
    if(grepl('trip-specific', model)){
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
                                  estimate  = estimate,
                                  model = model
                                  )
    }

    if(grepl('population', model)){
        traveltimeCLT_obj <- population_estimates(data, nsamples)
        traveltimeCLT_obj[['model']] <- model
    }

    class(traveltimeCLT_obj) = append(class(traveltimeCLT_obj), "traveltimeCLT", after=0)
    
    invisible(traveltimeCLT_obj)
}

