#' Predict trip-specific travel time
#'
#' \code{predict.traveltimeCLT.trip_specific} returns the predicted mean and variance of travel time for a specific route and start time.
#'
#' @param N Number of links in a route.
#' @param population_parameters An output of \code{population_estimates}, see \code{?population_estimates}.
#' @param level Significance level.
#'
#' @details NULL
#'
#' @return Returns a list of predictions.
#' 
#' @examples
#' \dontrun{
#'
#' }
#' @import data.table
#' @export
predict.traveltimeCLT.population <- function(N, population_parameters, level = 0.95){
    
    mu = population_parameters$mu
    m = population_parameters$nsamples
    sig = population_parameters$sigma.prof
    ETA  = N * mu
    q = abs(qnorm((1 - level)/2))
    
    lwr = ETA - q * sig * sqrt(N * (1 + 1/m))
    upr = ETA + q * sig * sqrt(N * (1 + 1/m))

    list(ETA = N * mu, lwr = lwr, upr =upr)
}

