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
#' @import data.table
#' @export 
similarity_route_fiction <- function(tripID, trips, model = "normal", sigma_n = 0, significance = 0) {
  if(!model %in% c("normal","t")){stop("model must be normal or t")}
  model <- tolower(model)
    names(trips) <- tolower(names(trips))
  setnames(trips,
    old = c("linkid"),
    new = c("linkId"),
    skip_absent = TRUE
  )
      # 设置多线程
  setDTthreads(0)  # 自动使用所有CPU核心
  if (!data.table::is.data.table(trips)) {
    data.table::setDT(trips)
  }
  if(is.null(trips$time))stop("trips do not have time!")
  trips$timebin <- time_bins_readable(trips$time)
  if(significance > 1 | significance < 0)stop("significance must be between 0 and 1")
  if(significance < 0.5) significance <- 1-significance
  if(model == "t"){
    similarity_route_fiction_t(tripID, trips, sigma_n = sigma_n, significance = significance)
  }
  else if(model == "normal"){
    similarity_route_fiction_normal(tripID, trips, sigma_n = sigma_n, significance = significance)
  }
}
#' @export
similarity_route_fiction_normal <- function(tripID, trips, sigma_n = 0, significance = 0) {
  timeBin_x_edges=get_timeBin_x_edges(trips)
  #remove the Global time bin
  timeBin_x_edges=timeBin_x_edges[timeBin!="Global"]
  multi_timeBin_x_edges=timeBin_x_edges[frequency>1]
  real_edge_count <- trips[trip %in% tripID,.(length(time)), trip]$V1
  simulated_edge_count <- real_edge_count+floor(rnorm(length(real_edge_count), mean = 0, sd = sigma_n))
  thresholds <- qnorm(1-(1-significance)/2, mean = 0, sd = 1)
  simulated_data <- trips[trip %in% tripID, .(linkId, timebin), trip][, {
    target_length <- simulated_edge_count[match(trip[1], tripID)]
    sampled_edges <- .SD[, {
      edge <- timeBin_x_edges[linkId %in% .BY$linkId & timeBin %in% .BY$timebin, ]
      
      fr <- if(nrow(edge)>0) edge$frequency[1] else NA
      va <- edge$sd[1]^2
      me <- edge$mean[1]
      
      if(!is.na(fr))if(fr > 1) {

        similar_matrix <- abs(multi_timeBin_x_edges$mean - me) / sqrt((va/fr) + (multi_timeBin_x_edges$sd^2)/multi_timeBin_x_edges$frequency)
        similar_mask <- similar_matrix < thresholds

        valid_edges <- which(similar_mask)
        if(length(valid_edges) > 0) {
          selected_idx <- sample(valid_edges, 1)
          selected_edge <- multi_timeBin_x_edges[selected_idx, .(linkId, timeBin)]
        } else {
          similarID <- timeBin_x_edges[abs(mean - me) < (significance-0.5)*abs(mean) & 
                                          timeBin == .BY$timebin & frequency==1, ]
          selected_edge <- similarID[sample(.N, 1)]
        }
      } else {
        similarID <- timeBin_x_edges[abs(mean - me) < 0.1*abs(mean) & 
                                    timeBin == .BY$timebin & frequency==1, ]
        selected_edge <- similarID[sample(.N, 1)]
      }
      if(nrow(selected_edge)!=0) {
        timeBin_x_edges[linkId == selected_edge$linkId & timeBin == selected_edge$timeBin, ]
      }
      
    }, by = .(linkId, timebin)]
    current_length <- nrow(sampled_edges)
    if(current_length > target_length) {
      sampled_edges[1:target_length]
    } else if(current_length < target_length) {
      rbind(sampled_edges, sampled_edges[sample(.N, target_length - current_length, replace = TRUE)])
    } else {
      sampled_edges
    }
  }, by = trip]
  simulated_data[,c(2,3)] <- NULL
  return(simulated_data)
}

#' @export
similarity_route_fiction_t <- function(tripID, trips, sigma_n = 0, significance = 0) {

  timeBin_x_edges=get_timeBin_x_edges(trips)
  #remove the Global time bin
  timeBin_x_edges=timeBin_x_edges[timeBin!="Global"]
  multi_timeBin_x_edges=timeBin_x_edges[frequency>1]
  real_edge_count <- trips[trip %in% tripID,.(length(time)), trip]$V1
  simulated_edge_count <- real_edge_count+floor(rnorm(length(real_edge_count), mean = 0, sd = sigma_n))
  simulated_data <- trips[trip %in% tripID, .(linkId, timebin), trip][, {
    target_length <- simulated_edge_count[match(trip[1], tripID)]
    sampled_edges <- .SD[, {
      edge <- timeBin_x_edges[linkId %in% .BY$linkId & timeBin %in% .BY$timebin, ]
      
      fr <- if(nrow(edge)>0) edge$frequency[1] else NA
      va <- edge$sd[1]^2
      me <- edge$mean[1]
      
      if(!is.na(fr))if(fr > 1) {

        similar_matrix <- abs(multi_timeBin_x_edges$mean - me) / sqrt((va/fr) + (multi_timeBin_x_edges$sd^2)/multi_timeBin_x_edges$frequency)
        df_vals <- ((va/fr + multi_timeBin_x_edges$sd^2/multi_timeBin_x_edges$frequency)^2) / 
          ((va/fr)^2/(fr-1) + (multi_timeBin_x_edges$sd^2/multi_timeBin_x_edges$frequency)^2/(multi_timeBin_x_edges$frequency-1))
        thresholds <- qt(1 - (1 - significance)/2, df = df_vals)
        similar_mask <- similar_matrix < thresholds

        valid_edges <- which(similar_mask)
        if(length(valid_edges) > 0) {
          selected_idx <- sample(valid_edges, 1)
          selected_edge <- multi_timeBin_x_edges[selected_idx, .(linkId, timeBin)]
        } else {
          similarID <- timeBin_x_edges[abs(mean - me) < (significance-0.5)*abs(mean) & 
                                          timeBin == .BY$timebin & frequency==1, ]
          selected_edge <- similarID[sample(.N, 1)]
        }
      } else {
        similarID <- timeBin_x_edges[abs(mean - me) < 0.1*abs(mean) & 
                                    timeBin == .BY$timebin & frequency==1, ]
        selected_edge <- similarID[sample(.N, 1)]
      }
      if(nrow(selected_edge)!=0) {
        timeBin_x_edges[linkId == selected_edge$linkId & timeBin == selected_edge$timeBin, ]
      }
      
    }, by = .(linkId, timebin)]
    current_length <- nrow(sampled_edges)
    if(current_length > target_length) {
      sampled_edges[1:target_length]
    } else if(current_length < target_length) {
      rbind(sampled_edges, sampled_edges[sample(.N, target_length - current_length, replace = TRUE)])
    } else {
      sampled_edges
    }
  }, by = trip]
  simulated_data[,c(2,3)] <- NULL
  return(simulated_data)
}