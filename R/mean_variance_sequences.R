#' \code{mean_varaince_sequences} returns the predicted mean and variance of travel time for a specific route and start time.

#' 
#' @param starttime a \code{POSIXlt} value representing the start time of the trip.
#' @param route a vector of links in the order to be traveled.
#' @param distance a vector of distances to be traveled on each on the links in \code{route}.
#' @param rho a vector of auto-correlation in the order of lag, starting at lag 0.
#' @param network_parameters an output of \code{link_mean_variance}, see \code{?link_mean_variance}.
#' @param final.only a logical indicating whether to return a sequence of means and standard deviation, in the order of \code{route}, or only the final ETA and standard deviation values. Default \code{final.only=TRUE}.
#' @param timebin_rules a \code{list} of time bin rules to be passed to \code{rules2timebins}, see \code{?rules2timebins}. 
#'
#' @details NULL
#'
#' @examples
#' \dontrun{
#'
#' }
#' @import data.table
#' @export
mean_variance_sequences <- function(starttime, route, distance, rho=1,
                                    network_parameters,
                                    finaly.only = TRUE, timebin_rules = NULL){

    shift_multiply <-function(x, k=1, n = length(x)) x[1:(n-k)] * x[(1+k):n]
        
    if(length(route) != length(distance))
        stop('length(route) != length(distance)!')

    if(length(rho) + 1 > length(route))
        stop('autocorrelations cannot be larger than length(route) - 1!')

    if(!is.null(timebin_rules)){
        timebin_rules = list(
            list(start='6:30', end= '9:00', days = 1:5, tag='MorningRush'),
            list(start='15:00', end= '18:00', days = 1:5, tag='EveningRush')
        )
        time_bins_func <- rules2timebins(timebin_rules)
    }else time_bins_func = time_bins
    
    tbin = time_bins(starttime)    
    nlag = length(rho)
    nlink = length(route)

    from  = route[1:(nlink-1)]
    to = route[2:nlink]
    
    g = network_parameters[linkID.from %in% route]
    
    ETA = 0  
    sd.seq  = numeric(nlink)
    mean.seq = numeric(nlink)
    for(i  in 1:(nlink - 1)){
        d = g[linkID.from == from[i] & linkID.to == to[i] & timeBin == tbin]
        ETA = ETA + distance[i] * d$mean
        mean.seq[i] = distance[i] * d$mean
        sd.seq[i] = distance[i] * d$sd
        tbin = time_bins(starttime + ETA)
    }
    ## last linkId.to is NA
    d = g[linkID.from == route[i] & timeBin == tbin & is.na(linkID.to)]
    
    linkETA = ETA + distance[i] * d$mean
    mean.seq[i] = distance[i] * d$mean
    sd.seq[i] = distance[i] * d$sd
    
    sq = sapply(1:length(rho), function(k){
        aux = numeric(nlink)
        a = shift_multiply(sd.seq, k-1, nlink)
        aux[(nlink - length(a) + 1):nlink] <- a
        drop(aux)
    }, USE.NAMES = FALSE)

    rho2 = rho*2
    rho2[1] <- 1
    variance.seq = sq %*% rho2

    if(finaly.only){
        list(ETA = sum(mean.seq), variance = sum(variance.seq))
    }else{
        list(ETA = sum(mean.seq), variance = cumsum(variance.seq))
    }
}

