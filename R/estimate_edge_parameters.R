#' This function allows to create the graph of the network and run the algorithm on the train set to get the mean of the autocorrelation and the mean of the residuals.
#' @param data A data frame of trips and their road level travel information, formated as \code{trips}, see \code{trips} or \code{View(data(trips))}.
#' @param L minimum number of observation to estimate (and not impute) parameters. Default (\code{L=5}).

#' @details returns a data frame with columns \code{(linkID.from, linkID.to, timeBin, mean, sd, imputed_mean, imputed_sd)} representing the mean and as used for each unit, while \code{imputed_mean} and \code{imputed_sd} indicate whether the calculated quantity is imputed from time bin estimates, or calculated from observed data.
#' @examples
#'#' @examples
#' \dontrun{
#' data(trips)
#' estimate_edge_parameters(trips)
#' }
#' @import data.table
#' @export
estimate_edge_parameters <- function(data, L = 5L){
    train = from_to_format(data)
    train[, speed :=exp(logspeed)]
    ## Calculating the mean and  variance per (linkID.from, linkID.to, timeBin)
    graph.stat = train[, .(mu = mean(1/speed),  sd2 = var(1/speed), N=N[1]), by = list(linkID.from, linkID.to, timeBin)]
    
    ## We create the structure of the graph network
    bins = unique(train$timeBin)
    full.graph = graph.stat[, .(timeBin = bins) ,by = list(linkID.from, linkID.to)]

    ## merging mu and sd2 calculated above to the structure of our graph network
    graph.stat = merge(full.graph, graph.stat, by= c('linkID.from', 'linkID.to', 'timeBin'), all.x = TRUE)
    graph.stat$N[is.na(graph.stat$N)]<-0

    ## mu.from.tb and sd2.from.tb: We calculate the mean and the variance of the speed
    ## for all the times when the ik link (linkID.from and timeBins pairs
    graph.stat.from.tb = train[, .(mu = mean(1/speed),
                                   sd2 = var(1/speed), N= .N),
                               by = list(linkID.from, timeBin)]

    ## mu.tb and sd2.tb: We calculate the mean and the variance of the speed for each timebins.
    graph.stat.tb = train[, .(mu = mean(1/speed),
                              sd2 = var(1/speed), N= .N),
                          by = list(timeBin)]
    
    ## we combine the three in the same data frame
    graph.stat.full =  merge(graph.stat,
                             graph.stat.from.tb[N>L, .(mu.from.tb = mu,
                                                       sd2.from.tb = sd2,
                                                       linkID.from, timeBin)],
                             all.x = TRUE,
                             by.x=c('linkID.from', 'timeBin'),
                             by.y = c('linkID.from', 'timeBin'))

    graph.stat.full =  merge(graph.stat.full,
                             graph.stat.tb[N>L, .(mu.tb = mu, sd2.tb = sd2, timeBin)],
                             all.x = TRUE, by.x=c('timeBin'), by.y = c('timeBin'))
    


    ## A.2 For each ijk link, if the value of mu and / or sd2 is missing, then the variance and the mean
    ## of the ijk link are the values of mu.from.tb and sd2.from.tb.
    ## If these values are also missing, then the mean and the variance of the ijk link become
    ## the values of mu.tb and sd2.tb.

    ## imputing average based on time bin average if missing
    graph.stat.full[, muspeed := ifelse(is.na(mu), ifelse(is.na(mu.from.tb), mu.tb, mu.from.tb), mu)]
    graph.stat.full[,  imputed_mean := ifelse(muspeed== mu, FALSE, TRUE )]
    ## imputing sd based on time bin average if missing
    graph.stat.full[, sdspeed := ifelse(is.na(sd2), ifelse(is.na(sd2.from.tb), sd2.tb, sd2.from.tb), sd2)]
    graph.stat.full[, imputed_sd := ifelse(sdspeed == sd2  & !is.na(sd2) , FALSE, TRUE)]


    ## shortining the output
    invisible(
        raph.stat.full[,.(linkID.from,
                          linkID.to,
                          timeBin,
                          mean = muspeed,
                          sd = sqrt(sdspeed),
                          imputed_mean,
                          imputed_sd)],
        )
}
