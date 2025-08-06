#' Route Simulation Using Statistical Similarity
#'
#' This function generates simulated routes by comparing edge statistics using
#' either Normal distribution(for a faster calculation) or Welch's t-test for similarity assessment.
#'
#' @param tripID Vector of trip identifiers to use as simulation templates
#' @param trips Data.table containing historical trip data with columns:
#' \itemize{
#'  \item{trip - Unique trip identifier}
#'  \item{linkId - Road segment identifier}
#'  \item{time - POSIXct timestamp}
#'  \item{length - Road segment length}
#' }
#' @param r Number of simulation rounds (default=1)
#' @param model Statistical model: "normal" (z-test) or "t" (Welch's t-test), default is "normal".
#' normal distribution is faster.
#' @param sigma_n Std dev of noise added to route length (default=2)
#' @param significance Significance level (α) for similarity threshold. 
#' Defaults is 0.05. A larger value will result in more posible choices with less similarity.
#' @return Data.table containing:
#' \itemize{
#'  \item{trip - Original trip ID}
#'  \item{newtrip - Simulation iteration ID}
#'  \item{linkId - Sampled road segment}
#'  \item{timeBin - Time period classification}
#'  \item{frequency - Historical edge usage count}
#'  \item{mean - Historical mean travel time}
#'  \item{sd - Historical travel time std dev}
#' }
#' @author Mingze Li <mingzeli7@cmail.carleton.ca>
#' @examples
#' data(trips)
#' names(trips) <- c("trip", "linkid", "timebin", "speed", "duration", "length", "time")
#' # Basic simulation with normal distribution
#' sim_normal <- similar_route(c(2700), trips, significance=0.05)
#' # t-distribution simulation with significance threshold
#' sim_t <- similar_route(c(2700), trips, model="t", significance=0.05)
#' @seealso 
#' \code{\link{get_timeBin_x_edges}} for edge statistics calculation
#' @import data.table
#' @export 
similar_route <- function(tripID, trips, r=1,model = "normal", sigma_n = 2, significance = 0.05) {
  
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
  if(model == "t"){
    similar_route.t(tripID, trips, sigma_n = sigma_n, significance = significance,r)
  }
  else if(model == "normal"){
    similar_route.normal(tripID, trips,r, sigma_n = sigma_n, significance = significance)
  }
}
#' @export
similar_route.normal <- function(tripID, trips,r, sigma_n = 0, significance = 0) {
  timeBin_x_edges=get_timeBin_x_edges(trips)
  #remove the Global time bin
  timeBin_x_edges=timeBin_x_edges[timeBin!="Global"]
  multi_timeBin_x_edges=timeBin_x_edges[frequency>1 & sd!=0]
  real_edge_count <- trips[trip %in% tripID,.(length(time)), trip]$V1
  thresholds <- qnorm(1-(1-significance)/2, mean = 0, sd = 1)
  newtrips <- 1
  simulated_data <- trips[trip %in% tripID, .(linkId, timebin), trip][, {
    real_length <- real_edge_count[match(trip[1], tripID)]

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
    # pick valid edges based on threshold on every origin_ID
    sampled_IDs <- similarity_matrix[,
      {
        candidates <- .SD[similarity <= thresholds, candidate_ID]
        if(length(candidates) == 0) {
          s <- origin_ID
        }else if (length(candidates) == 1) {
          s <- candidates
        }else  s = sample(candidates, 1)
        .(selected_ID = s)
      },
      by = origin_ID
    ]
    # get statistics for selected edges
    sampled_edges <- timeBin_x_edges[
      sampled_IDs,
      on = .(ID = selected_ID)
    ][, .( newtrip = newtrips, linkId,timeBin,frequency, mean, sd,length)]
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
similar_route.t <- function(tripID, trips, sigma_n = 0, significance = 0, r = 1) {
  timeBin_x_edges=get_timeBin_x_edges(trips)
  timeBin_x_edges=timeBin_x_edges[timeBin!="Global"]
  multi_timeBin_x_edges=timeBin_x_edges[frequency>1 & sd != 0]
  real_edge_count <- trips[trip %in% tripID,.(length(time)), trip]$V1
  newtrips <- 1
  simulated_data <- trips[trip %in% tripID, .(linkId, timebin), trip][, {
    real_length <- real_edge_count[match(trip[1], tripID)]
    all_edges <- unique(.SD[, .(linkId, timebin)])
    setnames(all_edges, old = "timebin", new = "timeBin")
    all_edges <- merge(all_edges, timeBin_x_edges[,.(linkId, timeBin, frequency, mean, sd, ID)], 
                      by = c("linkId", "timeBin"), all.x = TRUE, all.y = FALSE)
    all_edges <- na.omit(all_edges)
    
    multi_timeBin_x_edges[, dummy := 1]
    all_edges[, dummy := 1]
    
    similarity_matrix <- multi_timeBin_x_edges[
      all_edges,
      on = .(dummy),
      allow.cartesian = TRUE,
      .(
        origin_ID = i.ID,
        candidate_ID = x.ID,
        similarity = abs(i.mean - x.mean) / sqrt((x.sd^2/x.frequency) + (i.sd^2/i.frequency)),
        df = ((x.sd^2/x.frequency + i.sd^2/i.frequency)^2) / 
             ((x.sd^4)/(x.frequency^2*(x.frequency-1)) + (i.sd^4)/(i.frequency^2*(i.frequency-1)))
      )
    ]
    multi_timeBin_x_edges[, dummy := NULL]
    all_edges[, dummy := NULL]
    
    similarity_matrix[, threshold := qt(1 - (1 - significance)/2, df = df)]
    
    re <- NULL
    for(i in 1:r) {
      target_length <- real_length + floor(rnorm(1, mean = 0, sd = sigma_n))
      while(target_length <= 0) {
        target_length <- real_length + floor(rnorm(1, mean = 0, sd = sigma_n))
      }
      
      sampled_IDs <- similarity_matrix[,
        {
          candidates <- .SD[similarity <= threshold, candidate_ID]
          if(length(candidates) == 0) s <- origin_ID
          else if(length(candidates) == 1) s <- candidates
          else s <- sample(candidates, 1)
          .(selected_ID = s)
        },
        by = origin_ID
      ]
      
      sampled_edges <- timeBin_x_edges[
        sampled_IDs,
        on = .(ID = selected_ID)
      ][, .(newtrip = newtrips, linkId, timeBin, frequency, mean, sd)]
      
      current_length <- nrow(sampled_edges)
      if(current_length > target_length) {
        re <- rbind(re, sampled_edges[1:target_length])
      } else if(current_length < target_length) {
        re <- rbind(re, rbind(sampled_edges, sampled_edges[sample(.N, target_length - current_length, replace = TRUE)]))
      } else {
        re <- rbind(re, sampled_edges)
      }
      newtrips <<- newtrips + 1
    }
    re
  }, by = trip]
  return(simulated_data)
}