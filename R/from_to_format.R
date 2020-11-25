#' Format trip observations
#'
#' \code{from_to_format} adjust the data to from linkID to linkID format.
#' 
#' @param data A data frame of trips and their road level travel information, formated as \code{trips}, see \code{trips} or \code{View(data(trips))}.
#'
#' @details NULL
#'
#' @return \code{from_to_format} returns the data frame with extra columns (linkID.from, linkID.to), and \code{N} representing the number of (i = linkID.from, j = linkID.to, k = timeBin) is present in the dataset.
#' 
#' @examples
#' \dontrun{
#' data(trips)
#' from_to_format(trips)
#' }
#' @import data.table
#' @export
from_to_format <- function(data){
    ## Setting the graph
    dt = data
    dt[, linkID.from := linkID, by = tripID]
    dt[, linkID.to := shift(linkID, type = 'lead'), by = tripID]
    dt[, N:=.N, by =list(linkID.from, linkID.to, timeBin) ]
    dt
}
