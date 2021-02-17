#' Sampling a random set of trips from the data
#'
#' \code{sample_trips} samples a random set of trips from \code{data}.
#'
#' @param data A data frame of trips and their road level travel information, formatted as \code{trips}, see \code{trips} or \code{data(trips); View(trips)}.
#' @param nsamples The number of trips to sample. Default is 100.
#' @param min.links The minimum number of links in each of the sampled trips.
#'
#' @details NULL
#'
#' @return Returns a vector of \code{tripID}'s of the sampled trips. Those trips are to be removed from the data, it is up to the user to do so.
#' 
#' @examples
#' \dontrun{
#' 
#' data(trips)
#' # sampling a 100 random trips with minimum of 10 links each.
#' index = sample_trips(trips, min.links = 10)
#' trips[trips$tripID %in% index, ]    # sub-setting based on sampled trips
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
