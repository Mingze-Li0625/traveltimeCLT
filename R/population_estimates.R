#' Estimates the population mean and variance
#'
#' \code{population_estimates} estimates the population mean, variance of travel time
#'
#' @param data A data frame of trips and their road level travel information, formatted as \code{trips}, see \code{trips} or \code{data(trips); View(trips)}.
#' @param nsamples The number of trips to sample for parameter estimation. Default is 500.
#' @param level Significance level. Default is 0.95.
#' 
#' @details
#' 
#' @return 
#' 
#' @examples
#' \dontrun{
#'
#'
#' }
#' @import data.table
#' @export
population_estimates <- function(data, nsamples=500L, level = 0.95){

    if(!'data.table' %in% class(data))  data = data.table(data)
    if(!is.null(nsamples)){
        samp = sample_trips(data, nsamples)
        dt = data[tripID %in% samp][order(tripID, entry_time)]
    }else dt = data

    tr = dt[,.(dur = sum(duration_secs)/.N),tripID]
    
    hatmu = mean(tr$dur)
    hatsigmu = var(tr$dur)
    m = length(unique(dt$tripID))
    alpha = level  + (1-level)/2
    q = qt(alpha, df = m-1)
    lwr = hatmu - q * sqrt(hatsigmu/m)
    upr = hatmu + q * sqrt(hatsigmu/m)
    En = dt[, .N, tripID][, mean(1/N)]

    list(mu = hatmu,
         mu.CI = c(lwr, upr) ,
         nsamples = nsamples,
         sigma2 = hatsigmu,
         En = En,
         sigma.prof = sqrt(hatsigmu/En))
}


