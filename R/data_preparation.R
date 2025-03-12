#' @import data.table
NULL
# -------------functions -------------------------------------------------------


#' Generate time-bin \eqn{\times} edge statistics table from trip data
#'
#' This function processes the trip data to statistics (mean and sd of log duration, frequency, length) 
#' categorized by edges and time bins. It accept both vector input and data table (`trips`) 
#'
#' @param trips A `data.table` containing trips data. Column names are case-insensitive.
#'             Expected it contains: trip, linkID, length; time, or duration/logduration+timeBin. See details.
#' @param tripID Vector of trip ID (if `trips` is not provided).
#' @param linkId Vector of edge ID.
#' @param length Vector of edge lengths.
#' @param timeBin Vector of pre-computed time bins. We recommand use the function \code{\link{time_bins_readable}}.
#' @param time Vector of time the car enter the edge.
#' @param duration Vector of measured durations of finish the edge.
#' @param log_duration Vector of log durations.
#'
#' @return A `data.table` with columns:
#' \itemize{
#'   \item \code{linkId}: Identifier for the edge.
#'   \item \code{timeBin}: Time period the trip is in. The "Global" timeBin means 
#'   Use all observations passed the edge.
#'   \item \code{mean}: Mean log-duration.
#'   \item \code{sd}: Standard deviation of log-duration (returns 0 for single observations).
#'   \item \code{frequency}: Number of observations.
#'   \item \code{length}: The edge length, calculated by mode.
#'   \item \code{ID}: Unique identifier for the edge and timeBin.
#' }
#'
#' @details
#' - If `trips` is provided: Column names are case-insensitive.
#' - If `trips` is not provided: All vector parameters must be equal-length
#' - This function will use `time` if provided. Otherwise it will use `log_duration`, and then `duration`.
#' 
#' The input need to fulfill:
#' - Time parameters: `timeBin` + (`duration` or `log_duration`), OR `time`.
#' This function will use time if provided.
#' - Trip parameters: `trip`, `linkId`, `length`. Column `trip` in trips refer the trip ID.
#' @seealso \code{\link{get_timeBin_x_connections}} Generate time-bin \eqn{\times} connection statistics table from trip data
#' @examples
#' #Use a data table
#' data(trips)
#' names(trips)<-c("trip","linkid","timebin","speed","duration","length","time")
#' stat<-get_timeBin_x_edges(trips)
#' View(stat)
#' # Using raw parameters
#' stat2=get_timeBin_x_edges(tripID=trips$trip,time=trips$time,linkId=trips$linkid,length = trips$length)
#' View(stat2)
#' @author Mingze Li <mingzeli7@cmail.carleton.ca>
#' @export
get_timeBin_x_edges <- function(trips=NULL,tripID=NULL,linkId=NULL,length=NULL,
                                timeBin=NULL,time=NULL,duration=NULL,log_duration=NULL){
  trip<-tripID
  frameAvailable <- !is.null(trips)
  if(frameAvailable){
    original_names <- names(trips)
    names(trips) <- tolower(names(trips))
    trips <- data.table(trips)
    tripParamsAvailable <- all(c("trip", "linkid", "length") %in% names(trips))
    timeParamsAvailable1 <- all(c("timebin", "duration") %in% names(trips))|all(c("timebin", "log_duration") %in% names(trips))
    timeParamsAvailable2 <- "time" %in% names(trips)
    setnames(trips, 
             old = c( "linkid", "timebin"),
             new = c( "linkId", "timeBin"),
             skip_absent = TRUE)
  } else{
    tripParamsAvailable <- !is.null(trip) & !is.null(linkId)& !is.null(length)
    timeParamsAvailable1 <- !is.null(timeBin) & (!is.null(duration) |!is.null(log_duration))
    timeParamsAvailable2 <- !is.null(time)
  }
  if(!tripParamsAvailable)stop("Either 'trip' ,'length', or 'linkId' is not provided.")
  if(!timeParamsAvailable1&!timeParamsAvailable2)stop("'time' or 'timeBin' and 'duration' is not provided.")
  if(!frameAvailable){
    if(!timeParamsAvailable2){
      if(!is.null(duration)){
        if (length(trip) != length(linkId) || length(trip) != length(duration)||
            length(trip) != length(timeBin) || length(trip) != length(length))
          stop("Parameter vectors for 'tripID', 'linkId','duration', 'length', and 'timeBin' are not equal in length!")
        trips<-data.table(trip=trip,linkId=linkId,timeBin=timeBin,log_duration=log(duration),length=length)
      }else{
        if (length(trip) != length(linkId) || length(trip) != length(log_duration)||
            length(trip) != length(timeBin)|| length(trip) != length(length))
          stop("Parameter vectors for 'tripID', 'linkId','log_duration','length', and 'timeBin' are not equal in length!")
        trips<-data.table(trip=trip,linkId=linkId,timeBin=timeBin,log_duration=log_duration,length=length)
      }
    }else{
      if (length(trip) != length(linkId) || length(trip) != length(time)|| length(trip) != length(length))
        stop("Parameter vectors for 'tripID', 'linkId', 'length', and 'time' are not equal in length!")
      trips<-data.table(trip=trip,linkId=linkId,time=time,length=length)
    }
  }
  if(timeParamsAvailable2){
    trips$time <- as.POSIXct( trips$time)
    trips$timeBin<-time_bins_readable(trips$time)
    trips[, duration := as.numeric(difftime(shift(time, type = "lead"), time, units = "secs")), by = trip]
    trips[, log_duration := log(duration)]
  }else  if(is.null(trips$log_duration)){
    trips[, log_duration := log(duration)]
  }
  trips <- na.omit(trips)
  timeBin_x_edges <- trips[,.(mean = mean(log_duration, na.rm = TRUE),
                              sd = sd_one_input_is_0(log_duration),
                              frequency = .N,
                              length = get_mode(length)),
                           by = .(linkId, timeBin)]
  global_stats <- trips[,.(mean = mean(log_duration, na.rm = TRUE),
                           sd = sd_one_input_is_0(log_duration),
                           frequency = .N,
                           length = get_mode(length)),
                        by = .(linkId)]
  global_stats[, timeBin := "Global"]
  timeBin_x_edges <- rbind(timeBin_x_edges, global_stats)
  timeBin_x_edges[, ID := 1:.N]
  timeBin_x_edges
}
#' Generate time-bin \eqn{\times} connection statistics table from trip data
#'
#' This function processes the trip data to statistics (mean and sd of log duration, frequency, length) 
#' categorized by connection and time bins. It accept both vector input and data table (`trips`) 
#'
#' @param trips A `data.table` containing trips data. Column names are case-insensitive.
#'             Expected it contains: trip, linkID, length; time, or duration/logduration+timeBin. See details.
#' @param tripID Vector of trip ID (if `trips` is not provided).
#' @param linkId Vector of edge ID.
#' @param length Vector of edge lengths.
#' @param timeBin Vector of pre-computed time bins. We recommand use the function \code{\link{time_bins_readable}}.
#' @param time Vector of time the car enter the edge.
#' @param duration Vector of measured durations of finish the edge.
#' @param log_duration Vector of log durations.
#'
#' @return A `data.table` with columns:
#' \itemize{
#'   \item \code{linkId}: Identifier the starting edge.
#'   \item \code{nextLinkId}: Identifier of the next edge. The travel is assumed to be
#'    from the beginning of starting edge to the beginning of the next edge.
#'   \item \code{timeBin}: Time period the trip is in. The "Global" timeBin means 
#'   Use all observations passed the edge.
#'   \item \code{one_way_mean}: Mean log-duration from the beginning of starting edge
#'    to the beginning of next edge.
#'   \item \code{one_way_sd}: Standard deviation of log-duration (returns 0 for single observations).
#'   \item \code{one_way_frequency}: Number of observations.
#'   \item \code{length}: The edge length, calculated by mode.
#'   \item \code{fictional}: If there is only observation of A -> B, we will also calculate the 
#'   fictional connection B -> A, and its stats is based on B -> all other edges.
#' }
#'
#' @details
#' - If `trips` is provided: Column names are case-insensitive.
#' - If `trips` is not provided: All vector parameters must be equal-length
#' - This function will use `time` if provided. Otherwise it will use `log_duration`, and then `duration`.
#' 
#' The input need to fulfill:
#' - Time parameters: `timeBin` + (`duration` or `log_duration`), OR `time`.
#' This function will use time if provided.
#' - Trip parameters: `trip`, `linkId`, `length`. Column `trip` in trips refer the trip ID.
#' @seealso \code{\link{get_timeBin_x_edges}} Generate time-bin \eqn{\times} edges statistics table from trip data
#' @examples
#' #Use a data table
#' data(trips)
#' names(trips)<-c("trip","linkid","timebin","speed","duration","length","time")
#' stat<-get_timeBin_x_connections(trips)
#' View(stat)
#' # Using raw parameters
#' stat2=get_timeBin_x_connections(tripID=trips$trip,time=trips$time,linkId=trips$linkid,length = trips$length)
#' View(stat2)
#' @author Mingze Li <mingzeli7@cmail.carleton.ca>
#' @export
#' 
get_timeBin_x_connections <- function(trips=NULL,tripID=NULL,linkId=NULL,length=NULL,
                                      timeBin=NULL,time=NULL,duration=NULL,log_duration=NULL){
  trip<-tripID
  frameAvailable <- !is.null(trips)
  if(frameAvailable){
    original_names <- names(trips)
    names(trips) <- tolower(names(trips))
    trips <- data.table(trips)
    tripParamsAvailable <- all(c("trip", "linkid", "length") %in% names(trips))
    timeParamsAvailable1 <- all(c("timebin", "duration") %in% names(trips))|all(c("timebin", "log_duration") %in% names(trips))
    timeParamsAvailable2 <- "time" %in% names(trips)
    setnames(trips, 
             old = c("linkid", "timebin"),
             new = c( "linkId", "timeBin"),
             skip_absent = TRUE)
  } else{
    tripParamsAvailable <- !is.null(trip) & !is.null(linkId)& !is.null(length)
    timeParamsAvailable1 <- !is.null(timeBin) & (!is.null(duration) |!is.null(log_duration))
    timeParamsAvailable2 <- !is.null(time)
  }
  if(!tripParamsAvailable)stop("Either 'trip' ,'length', or 'linkId' is not provided.")
  if(!timeParamsAvailable1&!timeParamsAvailable2)stop("'time' or 'timeBin' and 'duration' is not provided.")
  if(!frameAvailable){
    if(!timeParamsAvailable2){
      if(!is.null(duration)){
        if (length(trip) != length(linkId) || length(trip) != length(duration)||
            length(trip) != length(timeBin) || length(trip) != length(length))
          stop("Parameter vectors for 'tripID', 'linkId','duration', 'length', and 'timeBin' are not equal in length!")
        trips<-data.table(trip=trip,linkId=linkId,timeBin=timeBin,log_duration=log(duration),length=length)
      }else{
        if (length(trip) != length(linkId) || length(trip) != length(log_duration)||
            length(trip) != length(timeBin)|| length(trip) != length(length))
          stop("Parameter vectors for 'tripID', 'linkId','log_duration','length', and 'timeBin' are not equal in length!")
        trips<-data.table(trip=trip,linkId=linkId,timeBin=timeBin,log_duration=log_duration,length=length)
      }
    }else{
      if (length(trip) != length(linkId) || length(trip) != length(time)|| length(trip) != length(length))
        stop("Parameter vectors for 'tripID', 'linkId', 'length', and 'time' are not equal in length!")
      trips<-data.table(trip=trip,linkId=linkId,time=time,length=length)
    }
  }
  if(timeParamsAvailable2){
    trips$time <- as.POSIXct( trips$time)
    trips$timeBin<-time_bins_readable(trips$time)
    trips[, duration := as.numeric(difftime(shift(time, type = "lead"), time, units = "secs")), by = trip]
    trips[, log_duration := log(duration)]
  }else  if(is.null(trips$log_duration)){
    trips[, log_duration := log(duration)]
  }
  trips[, `:=`(nextLinkId, shift(linkId, type = "lead")), by = tripID]
  trips<-na.omit(trips)
  link_net_list<- trips[,c("log_duration","linkId","nextLinkId","timeBin","length")]
  names(link_net_list)<-c("log_duration","linkID","nextLinkID","timeBin","length")
  
  
  timeBin_stats <- link_net_list[, 
                                 .(one_way_mean = mean(log_duration, na.rm = TRUE),
                                   one_way_sd = sd_one_input_is_0(log_duration),
                                   one_way_frequency = .N),
                                 by = .(linkID, nextLinkID, timeBin)]
  length_stats <- link_net_list[, .(length = get_mode(length)), by = .(linkID, nextLinkID)]
  timeBin_stats <- merge(timeBin_stats, length_stats, by = c("linkID", "nextLinkID"))
  global_stats <- link_net_list[,
                                .(  one_way_mean = mean(log_duration, na.rm = TRUE),
                                    one_way_sd = sd_one_input_is_0(log_duration),
                                    one_way_frequency = .N,
                                    length = get_mode(length)),, 
                                by = .(linkID, nextLinkID)]
  global_stats[, timeBin := "Global"]
  stats1 <- rbind(timeBin_stats, global_stats)
  existing_pairs <- unique(link_net_list[, .(linkID, nextLinkID)])
  reverse_pairs <- existing_pairs[, .(linkID = nextLinkID, nextLinkID = linkID)]
  missing_reverse <- reverse_pairs[!existing_pairs, on = .(linkID, nextLinkID)]
  setnames(missing_reverse, c("B", "A"))
  
  fictional_data <- link_net_list[missing_reverse,
                                  on = .(linkID = B),
                                  allow.cartesian = TRUE][nextLinkID != A]
  timeBin_fictional <- fictional_data[,
                                      .(one_way_mean = mean(log_duration, na.rm = TRUE),
                                        one_way_sd = sd_one_input_is_0(log_duration),
                                        one_way_frequency = .N),
                                      by = .(linkID, A, timeBin)]
  timeBin_fictional[, nextLinkID := A][, A := NULL]
  global_fictional <- fictional_data[,
                                     .(one_way_mean = mean(log_duration, na.rm = TRUE),
                                       one_way_sd = sd_one_input_is_0(log_duration),
                                       one_way_frequency = .N),
                                     by = .(linkID, A)]
  global_fictional[, timeBin := "Global"][, nextLinkID := A][, A := NULL]
  fictional_stats <- rbind(timeBin_fictional, global_fictional, fill = TRUE)
  length_fictional <- fictional_data[, .(length = get_mode(length)), by = .(linkID, A)]
  length_fictional[, nextLinkID := A][, A := NULL]
  fictional_stats <- merge(fictional_stats, length_fictional, by = c("linkID", "nextLinkID"))
  
  fictional_stats[, fictional := TRUE]
  stats1[, fictional := FALSE]
  
  stats1 <- rbind(stats1, fictional_stats, fill = TRUE)
  setcolorder(stats1, c("linkID", "nextLinkID", "timeBin", "one_way_mean", 
                        "one_way_sd", "one_way_frequency", "length", "fictional"))
  stats1
}