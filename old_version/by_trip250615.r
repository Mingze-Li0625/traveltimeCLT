#' Route Simulation based on Statistical Similarity
#'
#' This function generates simulated routes using Welch's t-test for edge similarity comparison.
#'
#' @param tripID Vector of trip identifiers for simulation templates
#' @param trips Data.table containing historical trip data with columns:
#' \itemize{
#'  \item{trip - Unique trip identifier}
#'  \item{linkId - Road segment identifier}
#'  \item{time - POSIXct timestamp}
#'  \item{timeBin - Time period classification (see \code{\link{time_bins_readable}})}
#'  \item{length - Road segment length}
#' }
#' @param sigma_n Standard deviation of noise added to edge counts (default=0)
#' @param significance Significance level (0-1) for similarity comparison, 
#'        close to 1 or 0 increase trip inclusion (default=0)
#' @return Simulated routes data.table with columns:
#' \itemize{
#'  \item{trip - Simulated trip ID}
#'  \item{linkId - Sampled road segment ID}
#'  \item{timeBin - Time period classification}
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
similarity_route_fiction <- function(tripID, trips, sigma_n = 0, significance = 0) {
  names(trips) <- tolower(names(trips))
  setnames(trips,
    old = c("linkid"),
    new = c("linkId"),
    skip_absent = TRUE
  )
      # Enable parallel processing
  setDTthreads(0)  # Use all CPU cores
  if (!data.table::is.data.table(trips)) {
    data.table::setDT(trips)
  }
  if(is.null(trips$time))stop("trips do not have time!")
  trips$timebin <- time_bins_readable(trips$time)
  if(significance > 1 | significance < 0)stop("siganificance must be between 0 and 1")
  if(significance < 0.5) significance <- 1-significance
  timeBin_x_edges=get_timeBin_x_edges(trips)
  #remove the Global time bin
  timeBin_x_edges=timeBin_x_edges[timeBin!="Global"]
  multi_timeBin_x_edges=timeBin_x_edges[frequency>1 & sd > 0]
  single_timeBin_x_edges=timeBin_x_edges[frequency == 1]
  real_edge_count <- trips[trip %in% tripID,.(length(time)), trip]$V1
  simulated_edge_count <- real_edge_count+floor(rnorm(length(real_edge_count), mean = 0, sd = sigma_n))
  simulated_data <- trips[trip %in% tripID, .(linkId, timebin), trip][, {
    target_length <- simulated_edge_count[match(trip[1], tripID)]
    
    # Build statistical matrix for all edges
    edge_stats <- as.matrix(rbind(multi_timeBin_x_edges, single_timeBin_x_edges)[, .(ID, mean, sd, frequency)])
    rownames(edge_stats) <- rbind(multi_timeBin_x_edges, single_timeBin_x_edges)$ID
    
    # Get current trip's edge feature matrix
    current_edges <- unique(.SD[, .(linkId, timebin)])
    valid_ids <- timeBin_x_edges[linkId %in% current_edges$linkId, ID]
    valid_ids <- intersect(as.character(valid_ids), rownames(edge_stats))
    current_features <- edge_stats[as.character(valid_ids), , drop=FALSE]

    current_means <- matrix(current_features[, "mean"], ncol = 1)
    multi_means <- matrix(multi_timeBin_x_edges[, mean], nrow = 1)
    single_means <- matrix(single_timeBin_x_edges[, mean], nrow = 1)

    # Handle empty multi_timeBin case
    if(nrow(multi_timeBin_x_edges) > 0) {
      var_terms_multi <- (current_features[, "sd"]^2 / pmax(current_features[, "frequency"], 1)) %*% 
                      matrix(1, nrow = 1, ncol = ncol(multi_means)) +
                      matrix(1, nrow = nrow(current_means)) %*% 
                      (multi_timeBin_x_edges[, sd]^2 / pmax(multi_timeBin_x_edges[, frequency], 1))
    } else {
      var_terms_multi <- matrix(0, nrow = nrow(current_means), ncol = 0)
    }

    mean_diffs_single <- current_means %*% matrix(1, nrow = 1, ncol = ncol(single_means)) - 
                       matrix(1, nrow = nrow(current_means)) %*% single_means
    mean_diffs_multi <- current_means %*% matrix(1, nrow = 1, ncol = ncol(multi_means)) -
                      matrix(1, nrow = nrow(current_means)) %*% multi_means

    df_vals <- pmax(current_features[, "frequency"], 1) %*% matrix(1, nrow = 1, ncol = ncol(multi_means)) +
              matrix(1, nrow = nrow(current_means)) %*% (pmax(multi_timeBin_x_edges[, frequency], 1) - 2)

  t_critical <- qt(1 - (1 - significance)/2, df = df_vals)
  similarity_matrix <- matrix(FALSE, nrow = nrow(mean_diffs_multi), ncol = ncol(multi_means) + ncol(single_means))
  
  if(nrow(multi_timeBin_x_edges) > 0) {
    similarity_matrix[,1:ncol(multi_means)] <- (abs(mean_diffs_multi) / sqrt(var_terms_multi)) < t_critical
  }

  multi_timeBin_x_edges <- multi_timeBin_x_edges[frequency > 1 & sd > 0]

    single_mask <- matrix(single_timeBin_x_edges$frequency == 1, 
                        nrow = nrow(current_means), 
                        ncol = ncol(single_means), 
                        byrow = FALSE)
    if(ncol(single_means) > 0){
  similarity_matrix[,(ncol(multi_means) + 1):(ncol(multi_means) + ncol(single_means))] <- abs(mean_diffs_single) < (significance - 0.5)*current_means %*% matrix(1, nrow=1, ncol=ncol(mean_diffs_single))
}
    similarity_conditions <- matrix(FALSE, 
                                  nrow = nrow(current_features),
                                  ncol = ncol(multi_means) + ncol(single_means))
    multi_mask <- current_features[, "frequency"] > 1
    if(any(multi_mask)) {
      similarity_conditions[multi_mask, 1:ncol(multi_means)] <- similarity_matrix[multi_mask, 1:ncol(multi_means)]
    }
    
    single_mask <- !multi_mask
    if(any(single_mask)) {
      if(ncol(single_means) > 0) {
  similarity_conditions[single_mask, (ncol(multi_means)+1):(ncol(multi_means)+ncol(single_means))] <- similarity_matrix[single_mask, (ncol(multi_means)+1):(ncol(multi_means)+ncol(single_means))]
}
    }
    
    # Select valid columns by frequency type
    valid_cols <- which(colSums(similarity_conditions) > 0)
    valid_ids <- if(current_features[1, "frequency"] > 1) {
      multi_timeBin_x_edges[valid_cols, ID]
    } else {
      single_timeBin_x_edges[valid_cols, ID]
    }

    sampled_indices <- if(length(valid_ids) > 0) {
      sample(valid_ids, target_length, replace=TRUE)
    } else {
      sample(timeBin_x_edges$ID, target_length, replace=TRUE)
    }
    sampled_edges <- timeBin_x_edges[ID %in% sampled_indices, .(
      linkId, 
      timeBin,
      mean, 
      sd=sqrt(sd),
      frequency,
      length
    )]
    current_length <- nrow(sampled_edges)
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


