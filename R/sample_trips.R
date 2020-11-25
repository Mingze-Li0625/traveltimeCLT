#' Sampling a random set of trips from a data set
#'
#' \code{sample_trips} samples a random set of trips from \code{data}.
#'
#' @param data A data frame of trips and their road level travel information, formated as \code{trips}, see \code{trips} or \code{View(data(trips))}.
#' @param nsamples as scalar representing the number of trips to sample. Default is 100.
#' @param min.links as minimum number of links in each sampled trip.
#'
#' @details NULL
#'
#' @return \code{sample_trips} return a vector of \code{tripID}'s of the samples trips.
#' 
#' @examples
#' \dontrun{
#' data(trips)
#' # sampling a 100 random trips with minium of 10 links each.
#' index = sample_trips(trips, min.links = 10)
#' trips[trips$tripID %in% index, ]
#' }
#' @export
sample_trips <- function(data, nsamples = 100L, min.links= NULL){
    if(is.null(min.links)){
        aux = unique(data$tripID)
        samp = aux[sample.int(length(aux), nsamples)]
    }else{
        aux = tapply(data$tripID, data$tripID, FUN = length)
        a = which(aux >= min.links)
        samp = as.numeric(names(a[sample.int(length(a),nsamples)]))
    }
    invisible(samp)
}
