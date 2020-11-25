#' \code{predict.traveltimeCLT.trip_specific} returns the predicted mean and variance of travel time for a specific route and start time.
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
predict.traveltimeCLT.trip_specific <- function(starttime, route, distance,
                                                rho=1,
                                                network_parameters,
                                                finaly.only = TRUE,
                                                timebin_rules = NULL,
                                                pred.type = c('both', 'mean-only')){

    pred.type <- tryCatch(match.arg(pred.type),error=function(cond){
        stop("Parameter 'pred.type' should be eiter 'both' or 'mean-only'")
    })
        
    shift_multiply <-function(x, k=1, n = length(x)) x[1:(n-k)] * x[(1+k):n]

    key_ <-function(from, to, bin) paste0(from, '.', to, '.', bin)
    extract_ <-function(from, to , bin){
        a = network_parameters[[key_(from, to, bin)]]
        if(is.null(a)){
            a = network_parameters[[key_(from, NA, bin)]]
            if(is.null(a)) a = network_parameters[[key_(NA, NA, bin)]]
        }
        a
    }
    if(length(route) != length(distance))
        stop('length(route) != length(distance)!')
    
    if(length(rho) > length(route) & grepl('both', pred.type))
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
    ETA = 0
    mean.seq = numeric(nlink)
    sd.seq  = numeric(nlink)
    if(grepl('both', pred.type)){
        for(i  in 1:nlink){
            d = extract_(from[i], to[i], tbin)
            ETA = ETA + distance[i] * d$mean
            mean.seq[i] = distance[i] * d$mean
            sd.seq[i] = distance[i] * d$sd
            tbin = time_bins(starttime + ETA)
        }
        sq = sapply(1:length(rho), function(k){
            aux = numeric(nlink)
            a = shift_multiply(sd.seq, k-1, nlink)
            aux[(nlink - length(a) + 1):nlink] <- a
            drop(aux)
        }, USE.NAMES = FALSE)
        
        rho2 = rho*2
        rho2[1] <- 1
        variance.seq = sq %*% rho2
    }else{
        for(i  in 1:nlink){
            d = extract_(from[i], to[i], tbin)
            ETA = ETA + distance[i] * d$mean
            mean.seq[i] = distance[i] * d$mean
            tbin = time_bins(starttime + ETA)
        }
    }

    if(finaly.only){
        if(grepl('both', pred.type))
            list(ETA = sum(mean.seq), variance = sum(variance.seq))
        else list(ETA = sum(mean.seq))
    }else{
        if(grepl('both', pred.type))
            list(ETA = sum(mean.seq), variance = cumsum(variance.seq))
        else
            list(ETA = sum(mean.seq))
    }

}

