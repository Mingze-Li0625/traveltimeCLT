#' Route Simulator and pressure test base on frequency
#'
#' This function simulates route travel times and lengths based on the log normal distributions
#' and information from time-bin \eqn{\times} edge statistics. It provides multiple simulation methods
#' including dependent, independent, first-order, and second-order correlated uniform simulation.
#'
#'
#' @param tripID Vector of trip identifiers to analyze. These trips will be used as templates
#'               for generating simulated routes.
#' @param trips Data.table containing trip information with the following columns:
#'              \itemize{
#'               \item{trip - Unique trip identifier}
#'               \item{linkId - Road segment identifier}
#'               \item{time - Timestamp in POSIXct format}
#'               \item{timeBin - Time period classification (optional, see\code{\link{time_bins_readable}})}
#'               \item{length - Road length}
#'              }
#' @param r Number of trips to simulate. If NULL, the number of trips to simulate is the same as the length of tripID in the input.
#'          Default is NULL.
#' @param timeBin_x_edges Precomputed edge statistics data.table (optional, generated from
#'  \code{\link{get_timeBin_x_edges}}). Contains:
#'              \itemize{
#'               \item{linkId - Road segment identifier}
#'               \item{timeBin - Time period classification}
#'               \item{mean - Log-transformed travel time mean}
#'               \item{sd - Log-transformed travel time standard deviation}
#'               \item{length - Road segment length}
#'               \item{frequency - Frequency of road segment usage in each time bin}
#'              }
#' @param rho Correlation coefficient (0-1) for dependent uniform generators. Controls the level
#'            of correlation between consecutive simulated travel times. Default=0.31
#' @param severity Proportion of abuse simulation (0-1). Default=0.
#'                   If greater than 0, only trips from the severity to 1 quantile of trips data will be used for sampling.
#'                   Must be between 0 and 1, otherwise an error will be thrown.
#' @param lambda Proportion of trips to apply abuse simulation. Default=0.
#' @return A list containing three elements:
#'         \itemize{
#'          \item{simulated_result - Summary statistics for simulated routes:
#'          \itemize{
#'           \item{dependent_time - Simulated time using correlated uniform sampling}
#'           \item{independent_time - Simulated time using independent uniform sampling}
#'           \item{first_order_time - Simulated time using first-order correlated uniform sampling}
#'           \item{second_order_time - Simulated time using second-order correlated uniform sampling}
#'           \item{simulated_length - Simulated total route length}
#'          }}
#'          \item{real_result - Summary statistics for real routes:
#'          \itemize{
#'           \item{real_time - Actual observed total travel time (seconds)}
#'           \item{real_length - Actual observed total route length}
#'          }}
#'          \item{simulated_data - Detailed link-level simulation data:
#'          \itemize{
#'           \item{trip - Trip identifier (1 to r)}
#'           \item{linkId - Sampled road segment identifier}
#'           \item{start_time - The start time of the trip}
#'           \item{length - Road segment length}
#'          }}
#'         }
#' @author Mingze Li <mingzeli7@cmail.carleton.ca>
#' @examples
#' # Load and prepare data
#' data(trips)
#' names(trips) <- c("trip", "linkid", "timebin", "speed", "duration", "length", "time")
#'
#' # Run simulation for specific trips
#' result <- frequency_route_fiction(c(2700, 2701, 2702, 2716, 2726, 2732, 2738, 2739, 2744, 2746, 2747, 2748, 2755, 2757, 2769, 2788, 2790), trips)
#'
#' # Run abuse test with quantile filtering (only calculates dependent_time and independent_time)
#' abuse_result <- frequency_route_fiction(c(2700, 2701, 2702, 2716, 2726, 2732, 2738, 2739, 2744, 2746, 2747, 2748, 2755, 2757, 2769, 2788, 2790),
#'   trips, r=50,
#'   severity = 0.3, lambda = 0.5
#' )
#' @seealso \code{\link{OnDemand_simulator}} for simulation with given routes,
#' \code{\link{dependent_uniform}} for correlated uniform random number generation,
#' \code{\link{first_order_uniform}} for first-order correlated uniform random number generation,
#' \code{\link{second_order_uniform}} for second-order correlated uniform random number generation,
#' \code{\link{time_bins_readable}} for time bin classification,
#' \code{\link{get_timeBin_x_edges}} for edge statistics calculation
#' @export
pressure_test1 <- function(tripID, trips, r = NULL, timeBin_x_edges = NULL, rho = 0.31, lambda = 0, severity = 0) {
  # Validate severity is between 0 and 1
  if (severity < 0 || severity > 1) {
    stop("severity must be between 0 and 1")
  }
  if (lambda < 0 || lambda > 1) {
   stop("lambda must be between 0 and 1") 
  }

  if (!data.table::is.data.table(trips)) {
    data.table::setDT(trips)
  }
  if (is.null(r)) r <- length(tripID)
  # Standardize column names
  names(trips) <- tolower(names(trips))
  setnames(trips,
    old = c("linkid", "timebin"),
    new = c("linkId", "timeBin"),
    skip_absent = TRUE
  )

  # Add time bins if not present
  if (is.null(trips$timeBin)) trips$timeBin <- time_bins_readable(trips$time)
  if (is.null(trips$time)) stop("trips do not have time!")

  # Generate edge statistics if not provided
  if (is.null(timeBin_x_edges)) timeBin_x_edges <- get_timeBin_x_edges(trips[!trip %in% tripID, ])

  # Extract real trip statistics
  real_time <- as.numeric(trips[trip %in% tripID, .(difftime(time[.N], time[1], units = "secs")), trip]$V1)
  real_length <- trips[trip %in% tripID, .(sum(length)), trip]$V1
  train_edge_count <- trips[!(trip %in% tripID),.(length(time)), trip]
  real_edge_count <- trips[trip %in% tripID,.(length(time)), trip]$V1
  real_start_time <- trips[trip %in% tripID,.(time[1]), trip]$V1

  # Filter trips based on severity quantile
  train_edge_count[, quantile := rank(V1) / .N]
  # Calculate the number of abuse trips
  abuse_trip_num <- ceiling(lambda * length(tripID))

  # Filter trips based on severity quantile
  filtered_tripID <- train_edge_count[quantile >= severity, trip]

  # If no trips meet the criteria, stop with an error
  if (length(filtered_tripID) == 0) {
    stop("No trips found in the specified quantile range (severity to 1). Please adjust the severity parameter.")
  }
  filtered_edge_count <- trips[trip %in% filtered_tripID, .(length(time)), trip]$V1
  filtered_start_time <- trips[trip %in% filtered_tripID, .(time[1]), trip]$V1

  normal_edge_num <- trips[, .(
    trip = 1:(r-abuse_trip_num),
    len = sample(real_edge_count, r-abuse_trip_num, replace = TRUE)
  )]
  abuse_edge_num <- trips[,.(
    trip = (r-abuse_trip_num+1):r,
    len = sample(filtered_edge_count, abuse_trip_num, replace = TRUE)
  )]
  simulated_data <- rbind(normal_edge_num, abuse_edge_num)
  simulated_data <- simulated_data[, .(trip = rep(trip, len)), by = trip]

  # Assign random start times and time bins
  normal_start_time <- sample(real_start_time, r-abuse_trip_num, replace = TRUE)
  abuse_start_time <- sample(filtered_start_time, abuse_trip_num, replace = TRUE)
  simulated_start_time <- c(normal_start_time, abuse_start_time)
  simulated_data[, start_time := simulated_start_time[trip]]
  simulated_timeBin <- time_bins_readable(simulated_start_time)
  simulated_data[, 1] <- NULL

  simulated_data <- simulated_data[, timeBin := simulated_timeBin[trip], trip]

  # Sample link IDs based on frequency in each time bin
  simulated_data <- simulated_data[, sampled_linkId := {
    current_edges <- timeBin_x_edges[timeBin == .BY$timeBin]
    sample(current_edges$linkId, size = .N, prob = current_edges$frequency, replace = TRUE)
  }, by = timeBin]

  # Merge with edge statistics to get mean, sd, and length
  simulated_data <- merge(simulated_data, timeBin_x_edges,
    by.x = c("sampled_linkId", "timeBin"),
    by.y = c("linkId", "timeBin"),
    all.x = TRUE
  )



  # Generate simulated travel times using different correlation methods
  # In abuse testing mode, calculate dependent_time and independent_time for performance
  dependent_time <- simulated_data[, .(sum(exp(mean + sd * qnorm(dependent_uniform(.N, rho = rho))))), trip]$V1
  independent_time <- simulated_data[, .(sum(exp(mean + sd * qnorm(runif(.N))))), trip]$V1

  #first_order_time <- simulated_data[, .(sum(exp(mean + sd * qnorm(first_order_uniform(.N, rho = rho))))), trip]$V1
  #second_order_time <- simulated_data[, .(sum(exp(mean + sd * qnorm(second_order_uniform(.N, rho = rho))))), trip]$V1


  # Calculate simulated route lengths
  simulated_length <- simulated_data[, .(sum(length)), trip]$V1

  # Create summary results data.frame
  simulated_result <- data.frame(
    dependent_time = dependent_time,
    independent_time = independent_time,
    #first_order_time = first_order_time,
    #second_order_time = second_order_time,
    simulated_length = simulated_length
  )
  real_result <- data.frame(
    real_time = real_time,
    real_length = real_length
  )

  # Create detailed link-level data data.frame with start times only in first row of each trip
  simulated_data <- simulated_data[, .(
    trip = simulated_data$trip,
    linkId = simulated_data$sampled_linkId,
    start_time = simulated_start_time[trip],
    length = simulated_data$length
  )]

  return(list(
    simulated_result = simulated_result,
    real_result = real_result,
    simulated_data = simulated_data
  ))
}
