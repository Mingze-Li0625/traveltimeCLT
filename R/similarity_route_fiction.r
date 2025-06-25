#' Route Simulation based on Statistical Similarity
#'
#' This function generates simulated routes by statistically similarity of edges
#' using Normal or Welch's t-test.
#'
#' @param tripID Vector of trip identifiers to use as templates for simulation
#' @param trips Data.table containing historical trip data with columns:
#' \itemize{
#'  \item{trip - Unique trip identifier}
#'  \item{linkId - Road segment identifier}
#'  \item{time - POSIXct timestamp}
#'  \item{length - Road segment length}
#' }
#' @param r Number of simulation rounds (default=1). Only available for normal distribution.
#' @param model Statistical model to use - "normal" (default) or "t" distribution
#' @param sigma_n Standard deviation of noise added to edge counts. Default=0
#' @param significance Significance level (0-1) for similarity comparison.
#' Close to zero or one meaning include more trips. Default=0.
#' @return A data.table containing simulated routes with columns:
#' \itemize{
#'  \item{trip - Simulated trip identifier}
#'  \item{newtrip - Simulation round identifier}
#'  \item{linkId - Sampled road segment ID}
#'  \item{timeBin - Time period classification}
#'  \item{frequency - Edge usage frequency}
#'  \item{mean - Historical mean travel time}
#'  \item{sd - Historical standard deviation}
#' }
#' @author Mingze Li <mingzeli7@cmail.carleton.ca>
#' @examples
#' data(trips)
#' names(trips) <- c("trip", "linkid", "timebin", "speed", "duration", "length", "time")
#' # Basic simulation with normal distribution
#' sim_data <- similarity_route_fiction(c(2700), trips, r=5)
#' # t-distribution simulation with significance threshold
#' sim_t <- similarity_route_fiction(c(2700), trips, model="t", significance=0.05)
#' @seealso 
#' \code{\link{get_timeBin_x_edges}} for edge statistics calculation
#' @import data.table
#' @export 
similarity_route_fiction <- function(tripID, trips, r=1,model = "normal", sigma_n = 0, significance = 0) {
  
  model <- tolower(model)
  if(!model %in% c("normal","t")){stop("model must be normal or t")}
    names(trips) <- tolower(names(trips))
  setnames(trips,
    old = c("linkid"),
    new = c("linkId"),
    skip_absent = TRUE
  )

  setDTthreads(0)  
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
    similarity_route_fiction_normal(tripID, trips,r, sigma_n = sigma_n, significance = significance)
  }
}
#' @export
similarity_route_fiction_normal <- function(tripID, trips,r, sigma_n = 0, significance = 0) {
  timeBin_x_edges=get_timeBin_x_edges(trips)
  #remove the Global time bin
  timeBin_x_edges=timeBin_x_edges[timeBin!="Global"]
  multi_timeBin_x_edges=timeBin_x_edges[frequency>1 & sd!=0]
  real_edge_count <- trips[trip %in% tripID,.(length(time)), trip]$V1
  simulated_edge_count <- real_edge_count
  thresholds <- qnorm(1-(1-significance)/2, mean = 0, sd = 1)
  newtrips <- 1
  simulated_data <- trips[trip %in% tripID, .(linkId, timebin), trip][, {
    real_length <- simulated_edge_count[match(trip[1], tripID)]

    all_edges <- unique(.SD[, .(linkId, timebin)])
    setnames(all_edges, old = "timebin", new = "timeBin")
    all_edges <- merge(all_edges, timeBin_x_edges[,.(linkId, timeBin, frequency, mean, sd, ID)], by = c("linkId", "timeBin"), all.x = TRUE, all.y = FALSE)
    all_edges <- na.omit(all_edges)
    
    multi_timeBin_x_edges[, `:=`(sd2_freq = sd^2 / frequency)]
    all_edges[, `:=`(sd2_freq = sd^2 / frequency)]
    # virtual join to create a dummy key for cartesian product
    all_edges[, dummy := 1]
    multi_timeBin_x_edges[, dummy := 1]

    similarity_matrix <- multi_timeBin_x_edges[
      all_edges,
      on = .(dummy),
      allow.cartesian = TRUE,
      .(
        origin_ID = i.ID,
        candidate_ID = x.ID,
        similarity = abs(i.mean - x.mean) / sqrt((x.sd^2/x.frequency) + (i.sd^2/i.frequency))
      )
    ]
    all_edges[, dummy := NULL]
    multi_timeBin_x_edges[, dummy := NULL]
    re <- NULL
  for(i in 1:r){
    # sample edges based on similarity
    valid_edges <- similarity_matrix[similarity < thresholds]
    sampled_IDs <- valid_edges[, .(
      selected_ID = sample(candidate_ID, size = 1, replace = TRUE)
    ), by = origin_ID]

    # get statistics for selected edges
    sampled_edges <- timeBin_x_edges[
      sampled_IDs,
      on = .(ID = selected_ID)
    ][, .( newtrip = newtrips, linkId,timeBin,frequency, mean, sd)]
    newtrips <<- newtrips + 1
    current_length <- nrow(sampled_edges)
    target_length <- real_length + floor(rnorm(1, mean = 0, sd = sigma_n))
    while(target_length<=0){
      target_length <- real_length + floor(rnorm(1, mean = 0, sd = sigma_n))
    }
    if(current_length > target_length) {
      re <- rbind(re, sampled_edges[1:target_length])
    } else if(current_length < target_length) {
      re <- rbind(re,rbind(sampled_edges, sampled_edges[sample(.N, target_length - current_length, replace = TRUE)]))
    } else {
      re <- rbind(re, sampled_edges)
    }
  }
  re
  }, by = trip]
  return(simulated_data)
}

#' @export
similarity_route_fiction_t <- function(tripID, trips, sigma_n = 0, significance = 0) {
  timeBin_x_edges=get_timeBin_x_edges(trips)
  #remove the Global time bin
  timeBin_x_edges=timeBin_x_edges[timeBin!="Global"]
  multi_timeBin_x_edges=timeBin_x_edges[frequency>1 & sd != 0]
  real_edge_count <- trips[trip %in% tripID,.(length(time)), trip]$V1
  simulated_edge_count <- real_edge_count+floor(rnorm(length(real_edge_count), mean = 0, sd = sigma_n))
  newtrips <- 1
  simulated_data <- trips[trip %in% tripID, .(linkId, timebin), trip][, {
    target_length <- simulated_edge_count[match(trip[1], tripID)]
    sampled_edges <- .SD[, {
      edge <- timeBin_x_edges[linkId %in% .BY$linkId & timeBin %in% .BY$timebin, ]
      
      fr <- if(nrow(edge)>0) edge$frequency[1] else NA
      va <- edge$sd[1]^2
      me <- edge$mean[1]
      
      if(!is.na(fr))if(fr > 1) {
        # similarity calculation
        similar_matrix <- abs(multi_timeBin_x_edges$mean - me) / sqrt((va/fr) + (multi_timeBin_x_edges$sd^2)/multi_timeBin_x_edges$frequency)
        df_vals <- ((va/fr + multi_timeBin_x_edges$sd^2/multi_timeBin_x_edges$frequency)^2) / 
          ((va/fr)^2/(fr-1) + (multi_timeBin_x_edges$sd^2/multi_timeBin_x_edges$frequency)^2/(multi_timeBin_x_edges$frequency-1))
        thresholds <- qt(1 - (1 - significance)/2, df = df_vals)
        similar_mask <- similar_matrix < thresholds
        
        # sample
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
    sampled_edges$newtrip <- rep(newtrips, nrow(sampled_edges))
    newtrips <<- newtrips + 1
    sampled_edges$ID <- NULL
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