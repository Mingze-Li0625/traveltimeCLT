#' Predict travel time
#'
#' \code{predict.traveltimeCLT} predicts expected travel time (ETA) and a prediction interval based on the \code{trip-specific} and \code{population} methods.
#'
#' @param object An output of \code{traveltimeCLT}, of class \code{traveltimeCLT}.
#' @param newdata  A data frame of new trips and their road level travel information, formatted as \code{trips}, see \code{trips} or \code{data(trips); View(trips)}.
#' @param level Significance levels.
#' @param ... passing extra parameter, for example, time bin rules to be passed to \code{rules2timebins}, see \code{?rules2timebins} in trip-specific method.
#'
#' @details Both the \code{trip-specific} and \code{population} prediction intervals are Gaussian-based. 
#' 
#' @return Returns a data fram that inlcudes the ETA (and optionally the trips variance), with lower and upper prediction intervals (optional), for each trip in the \code{newdata}.
#' @examples
#' \dontrun{
#'}
#' @import data.table
#' @export
predict.traveltimeCLT <- function(object, newdata, level = 0.95, ...){

    if(!'data.table' %in% class(newdata))  newdata = data.table(newdata)[order(tripID, entry_time)]

    if(grepl('trip-specific', object$model)){
        pred = newdata[, predict.traveltimeCLT.trip_specific(entry_time[1], linkID, distance_meters,
                                                             object$network_parameters, object$rho$average_correlation,
                                                             finaly.only = TRUE,
                                                             pred.type = object$estimate, ...
                                                             ),by = tripID]
        q = qnorm(level)
        if(grepl('both', object$estimate)){            
            v = sqrt(object$residual_variance)
            pred[, lwr := ETA - q * v * sqrt(variance)]
            pred[, upr := ETA + q * v * sqrt(variance)]
        }
    }
    if(grepl('population', object$model))
        pred = newdata[, predict.traveltimeCLT.population(.N, object, level = level, ...),
                       by = tripID]
    
    return(data.frame(pred))
}

