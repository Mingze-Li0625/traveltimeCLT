#' This function allows to predict the travel time estimation on the test set.
#'
#' @param object an output of \code{traveltimeCLT}, of class \code{traveltimeCLT}.
#' @param newdata a data frame of new trips.
#' @param pred.intervals Type of prediction itervals, \code{trip-specific}, \code{population}, or \code{none}.
#' @param level Tolerance levels.
#' @examples
#' \dontrun{
#'
#'}
#' @import data.table
#' @export
predict.traveltimeCLT <- function(object, newdata,
                                  pred.interval = c('trip-specific', 'population', 'none'),
                                  level = 0.95){

    interval <- tryCatch(match.arg(pred.interval),error=function(cond){
        stop("Parameter 'pred.interval' should be eiter 'trip-specific', 'population', or 'none'")
    })
    
    if(!'data.table' %in% class(newdata))  newdata = data.table(newdata)[order(tripID, entry_time)]

    pred = newdata[, predict.traveltimeCLT.trip_specific(entry_time[1], linkID, distance_meters,
                                                         object$rho$average_correlation,
                                                         object$network_parameters,
                                                         finaly.only = TRUE,
                                                         pred.type = object$estimate
                                                         ),by = tripID]
    
    q = qnorm(level)
    if(grepl('both', object$estimate))
        if(grepl('trip-specific', interval)){
            v = sqrt(object$residual_variance)
            pred[, lwr := ETA - q * v * sqrt(variance)]
            pred[, upr := ETA + q * v * sqrt(variance)]
        }
    return(data.frame(pred))
}

