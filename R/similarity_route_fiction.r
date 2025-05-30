#' Route Simulation based on Statistical Similarity
#'
#' This function generates simulated routes by statistically similarity of edges
#' using Welch's t-test.
#'
#' @param tripID Vector of trip identifiers to use as templates for simulation
#' @param trips Data.table containing historical trip data with columns:
#' \itemize{
#'  \item{trip - Unique trip identifier}
#'  \item{linkId - Road segment identifier}
#'  \item{time - POSIXct timestamp}
#'  \item{timeBin - Time period classification (see \code{\link{time_bins_readable}})}
#'  \item{length - Road segment length}
#' }
#' @param rho Correlation coefficient (0-1) for sampling similarity threshold. Default=0.31
#' @param sigma_n Standard deviation of noise added to edge counts. Default=0
#' @param significance Significance level (0-1) for Welch's t-test similarity comparison.
#' Close to zero or one meaning include more trips. Default=0.
#' @return A data.table containing simulated routes with columns:
#' \itemize{
#'  \item{trip - Simulated trip identifier}
#'  \item{linkId - Sampled road segment ID}
#'  \item{timebin - Time period classification}
#'  \item{length - Road segment length}
#' }
#' @author Mingze Li <mingzeli7@cmail.carleton.ca>
#' @examples
#' data(trips)
#' names(trips) <- c("trip", "linkid", "timebin", "speed", "duration", "length", "time")
#' sim_data <- similarity_route_fiction(c(2700), trips, significance=0.05)
#' @seealso 
#' \code{\link{get_timeBin_x_edges}} for edge statistics calculation
#' @export
similarity_route_fiction <- function(tripID, trips, rho = 0.31, sigma_n = 0, significance = 0) {
  names(trips) <- tolower(names(trips))
  setnames(trips,
    old = c("linkid"),
    new = c("linkId"),
    skip_absent = TRUE
  )
  if(is.null(trips$time))stop("trips do not have time!")
  if(is.null(trips$timebin))trips$timebin <- time_bins_readable(trips$time)
  if(significance > 1 | significance < 0)stop("significance must be between 0 and 1")
  if(significance < 0.5) significance <- 1-significance
  timeBin_x_edges=get_timeBin_x_edges(trips)
  #remove the Global time bin
  timeBin_x_edges=timeBin_x_edges[timeBin!="Global"]
  multi_timeBin_x_edges=timeBin_x_edges[frequency>1]
  real_edge_count <- trips[trip %in% tripID,.(length(time)), trip]$V1
  simulated_edge_count <- real_edge_count+floor(rnorm(length(real_edge_count), mean = 0, sd = sigma_n))
  simulated_data <- trips[trip %in% tripID, .(linkId, timebin), trip][, {
    current_length <- .N
    target_length <- simulated_edge_count[match(trip[1], tripID)]

    # 内联抽样逻辑
    sampled_edges <- .SD[, {
      edge <- timeBin_x_edges[linkId == .BY$linkId & timeBin == .BY$timebin, ]
      fr <- edge$frequency[1]
      va <- edge$sd[1]^2
      me <- edge$mean[1]
      
      if(fr > 1) {
        multi_timeBin_x_edges[, c("df", "stat") := {
          numerator = (va/fr + (sd^2)/frequency)^2
          denominator = ((va/fr)^2/(fr-1)) + ((sd^2/frequency)^2/(frequency-1))
          stat_val = abs(me - mean)/sqrt(va/fr + (sd^2)/frequency)
          .(numerator/denominator, stat_val)
        }, by = .(timeBin, linkId)]
        
        similarID <- multi_timeBin_x_edges[abs(stat) < qt(significance, df = df), .(linkId,timeBin)]
        selected_edge <- similarID[sample(.N, 1)]
      } else {
        similarID <- timeBin_x_edges[abs(mean - me) < 0.1*abs(mean) & 
                                    timeBin == .BY$timebin & frequency==1, ]
        selected_edge <- similarID[sample(.N, 1)]
      }
      timeBin_x_edges[linkId == selected_edge$linkId & timeBin == selected_edge$timeBin, ]
    }, by = .(linkId, timebin)]
    
    if(current_length > target_length) {
      sampled_edges[1:target_length]
    } else if(current_length < target_length) {
      rbind(sampled_edges, sampled_edges[sample(.N, target_length - current_length, replace = TRUE)])
    } else {
      sampled_edges
    }
  }, by = trip]
  
  return(simulated_data)
}


