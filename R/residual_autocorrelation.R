#' Auto-correlation of sequence travel residual
#'
#' \code{residual_autocorrelation} Calculates the average autocorrelations of the residual sequence of travel time.
#'
#' @param data A data frame of trips and their road level travel information, formatted as \code{trips}, see \code{trips} or \code{data(trips); View(trips)}.
#' @param network_parameters An output of \code{link_mean_variance}, see \code{?link_mean_variance}.
#' @param lag Maximum lag at which to calculate the autocorrelations. Default is 1 for the first order-autocorrelations.
#' @param nsamples The number of trips to sample for parameter estimation. Default is 500.
#'
#' @details A residual sequence is a sequences of \code{(duration_sec_i - E[duration_sec_i])/sd(duration_sec_i)} for the \code{i} links of a trip. 
#' 
#' @return Returns a data frame of lag order and autocorrelations estimates.
#' @examples
#' \dontrun{
#'
#' }
#' @import data.table
#' @export
residual_autocorrelation<-function(data, network_parameters, lag=1L, nsamples=500L){
    if(lag == 0)
        return(list(lag=0, average_correlation = 1))
    
    if(!is.null(nsamples)){
        samp = sample_trips(data, nsamples, min.links = lag + 1)
        dt = from_to_format(data[tripID %in% samp])
    }else  dt = from_to_format(data)

    train = merge(dt,dt[ ,  network_parameters[[paste0(linkID.from[1],'.', linkID.to[1],'.', timeBin[1])]],
                        by = list(linkID.from, linkID.to, timeBin)]
                 ,all.x = TRUE,
                  by = c('linkID.from', 'linkID.to', 'timeBin'))[order(tripID,entry_time)]
    
    ## not sure if we need to demain
    rho = train[, .(tripID[1], lag = 0:lag,
                    correlation = drop(acf((1/speed - mean)/(sd+1e-10),
                                           plot=FALSE,
                                           lag.max=lag,
                                           demean=TRUE)$acf)), tripID]
    rho[, .(average_correlation = mean(correlation, na.rm = TRUE)), lag]
}

