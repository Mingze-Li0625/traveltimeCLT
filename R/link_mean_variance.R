#' Estimates mean and variance of each link
#'
#' \code{link_mean_variance} estimates the mean and variance of travel duration for each link in the data.
#'
#' @param data A data frame of trips and their road level travel information, formatted as \code{trips}, see \code{trips} or \code{data(trips); View(trips)}.
#' @param L The minimum number of observation needed to estimate (and not impute) parameters. Default (\code{L=5}). see 
#' @param bins a vector of predefined naming for time bins. Default \code{unique(data$timeBin)}.
#'
#' @details For links \code{(linkID.from, linkID.to, timeBin)} that have less than \code{L} number of observations, first they are imputed by \code{(linkID.from, timeBin)} estimates, if there at least \code{L} observations in that category, and second by \code{timeBin} estimates.
#' 
#' @return Returns a hashed environment with keys as \code{paste0(linkID.from,'.', linkID.to,'.', timeBin)} and values containing \code{list(mean, sd, imputed_mean, imputed_sd)} as the mean and standard deviation of travel time for each unit, while \code{imputed_mean} and \code{imputed_sd} indicate whether the calculated quantities are imputed or calculated from observed data.
#'
#' @examples
#' \dontrun{
#' 
#' data(trips)
#' link_mean_variance(trips)
#' }
#' @import data.table
#' @export
link_mean_variance <- function(data, L = 5L, bins = unique(data$timeBin)){
    train = from_to_format(data)
    
    ## linkID.from X linkID.to X tbins
    graph = train[, .(timeBin = bins) ,by = list(linkID.from, linkID.to)]
    ## adding NA, NA, timeBin
    graph = rbindlist(list(graph,data.frame(linkID.from = NA, linkID.to = NA, timeBin = bins)))

    ## Adding ID, NA, timeBin
    a = train[, NA, list(linkID.from, timeBin)]
    a[, linkID.to:=V1]; a[, V1 := NULL]
    graph = rbindlist(list(graph, a[, .(linkID.from, linkID.to, timeBin)]))
    
    ## Calculating the mean and  variance per (linkID.from, linkID.to, timeBin)
    from.to.tb = train[, .(mu = mean(1/speed),  sd2 = var(1/speed), N= .N),
                       by = list(linkID.from, linkID.to, timeBin)]


    ## mu.from.tb and sd2.from.tb: We calculate the mean and the variance of the speed
    ## for all the times when the ik link (linkID.from and timeBins pairs
    from.tb = train[, .(mu = mean(1/speed), sd2 = var(1/speed), N= .N),
                    by = list(linkID.from, timeBin)]

    ## mu.tb and sd2.tb: We calculate the mean and the variance of the speed for each timebins.
    tb = train[, .(mu = mean(1/speed), sd2 = var(1/speed), N= .N),
               by = list(timeBin)]
    
    
    ## merging mu and sd2 calculated above to the structure of our graph network
    graph.stat = merge(graph, from.to.tb[N>=L],
                       by= c('linkID.from', 'linkID.to', 'timeBin'), all.x = TRUE)
    graph.stat$N[is.na(graph.stat$N)]<-0

    ## we combine the three in the same data frame
    graph.stat =  merge(graph.stat,
                        from.tb[N>=L, .(mu.from.tb = mu,
                                        sd2.from.tb = sd2,
                                        linkID.from, timeBin)],
                        all.x = TRUE,
                        by.x=c('linkID.from', 'timeBin'),
                        by.y = c('linkID.from', 'timeBin'))

    graph.stat.full =  merge(graph.stat,
                             tb[N>=L, .(mu.tb = mu, sd2.tb = sd2, timeBin)],
                             all.x = TRUE, by.x=c('timeBin'), by.y = c('timeBin'))
    


    ## A.2 For each ijk link, if the value of mu and / or sd2 is missing, then the variance and the mean
    ## of the ijk link are the values of mu.from.tb and sd2.from.tb.
    ## If these values are also missing, then the mean and the variance of the ijk link become
    ## the values of mu.tb and sd2.tb.

    ## imputing average based on time bin average if missing
    graph.stat.full[, muspeed := ifelse(is.na(mu), ifelse(is.na(mu.from.tb), mu.tb, mu.from.tb), mu)]
    graph.stat.full[,  imputed_mu := ifelse(is.na(mu), TRUE, FALSE)]
    ## imputing sd based on time bin average if missing
    graph.stat.full[, sdspeed := ifelse(is.na(sd2), ifelse(is.na(sd2.from.tb), sd2.tb, sd2.from.tb), sd2)]
    graph.stat.full[, imputed_sd2 := ifelse(is.na(sd2) , TRUE, FALSE)]

    ## convert to a hashed environment
    graph.stat.full[, k:=paste0(linkID.from,'.', linkID.to,'.', timeBin)]
    
    net= new.env(hash=TRUE, size = nrow(graph.stat.full))

    graph.stat.full[ , assign(k[1], list(mean = muspeed[1],
                                      sd = sqrt(sdspeed[1]),
                                      imputed_mean = imputed_mu[1],
                                      imputed_sd = imputed_sd2[1]), envir = net),
                    list(linkID.from, linkID.to, timeBin)]

    invisible(net)
    ## shortining the output
    ## invisible( graph.stat.full[,.(linkID.from,
    ##                       linkID.to,
    ##                       timeBin,
    ##                       mean = muspeed,
    ##                       sd = sqrt(sdspeed),
    ##                       imputed_mean,
    ##                       imputed_sd)]
    ##           )

}
