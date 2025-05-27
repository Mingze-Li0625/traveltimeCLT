#' Route Sampling Function
#' 
#' This function samples trips based on the distance range. 
#' If a single distance is provided, it will set the lower and upper bounds to 90% and 110% of the distance,
#' respectively. If a vector of two distances is provided, it will use the first distance as the lower bound
#'  and the second distance as the upper bound. The function returns a data.table containing the sampled 
#' trips within the distance range.
#' 
#' @param distance numeric/numeric vector. The distance range.
#'  If a single distance is provided, it will set the lower and upper bounds to 90% and 110% of the distance, respectively.
#'  If a vector of two distances is provided, it will use the first distance as the lower bound and the second distance as the upper bound.
#' @param trips data.table. Trips data, with columns trip, time, length.
#' @param r numeric. Number of trips that wish to sample.
#' @return a data.table that contains sampled trips within given distance range.
#' @author Mingze Li <mingzeli7@cmail.carleton.ca>
#' @examples
#' sample_route(1000, trips, 10)
#' @export
sample_route <- function(distance,trips, r) {
    if(length(distance) == 1) {
       lower_distance <- distance *0.9
       upper_distance <- distance *1.1
    }else{
       lower_distance <- distance[1] 
       upper_distance <- distance[2] 
    }
    # Find trips length
    real_length <- trips[, .(distance = sum(length)), trip]
    real_length$time <- as.numeric(trips[, .(difftime(time[.N], time[1], units = "secs")), trip]$V1)
    # Filter trips based on distance range
    filtered_trips <- real_length[distance >= lower_distance & distance <= upper_distance]
    # Sample trips based on distance range
    sampled_trips <- filtered_trips[sample(1:nrow(filtered_trips), r, replace = TRUE)]
    return(sampled_trips)
}
