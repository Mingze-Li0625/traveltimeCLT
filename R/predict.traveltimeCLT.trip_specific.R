#' Predict trip-specific travel time
#'
#' \code{predict.traveltimeCLT.trip_specific} returns the predicted mean and variance of travel time for a specific route and start time.
#'
#' @param starttime A \code{POSIXlt} value representing the start time of the trip.
#' @param route Vector of links in the order of  travel.
#' @param distance Vector of distances to be traveled on each on the links in \code{route}.
#' @param network_parameters An output of \code{link_mean_variance}, see \code{?link_mean_variance}.
#' @param rho Vector of auto-correlation in the order of lag, starting at lag 0 for no correlation.
#' @param final.only Logical indicating whether to return a sequence of means and standard deviations, in the order of \code{route}, or only the final ETA and standard deviation. Default \code{final.only=TRUE}.
#' @param timebin_rules A \code{list} of time bin rules to be passed to \code{rules2timebins}, see \code{?rules2timebins}.
#' @param pred.type \code{'both'} to predict the mean and variance of travel time and \code{'mean-only'} for the mean. Default \code{'both'}.
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
predict.traveltimeCLT.trip_specific <- function(starttime, route, distance,
                                                network_parameters,
                                                rho=1,
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

