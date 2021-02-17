#' Formatting observed trips
#'
#' \code{from_to_format} reformats the data to the "from linkID to linkID" structure.
#' 
#' @param data A data frame of trips and their road level travel information, formatted as \code{trips}, see \code{trips} or \code{data(trips); View(trips)}.
#'
#' @details NULL
#'
#' @return Returns a data frame with extra columns (linkID.from, linkID.to), and \code{N} representing the number of (i = linkID.from, j = linkID.to, k = timeBin) that are present in the dataset.
#' 
#' @examples
#' \dontrun{
#' 
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
    invisible(dt)
}
