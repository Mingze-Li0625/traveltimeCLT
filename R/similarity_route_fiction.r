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
similarity_route_fiction <- function(tripID, trips, rho = 0.31, sigma_n = 0, significance = 0) {
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
  timeBin_x_edges=get_timeBin_x_edges(trips)
  #remove the Global time bin
  timeBin_x_edges=timeBin_x_edges[timeBin!="Global"]
  multi_timeBin_x_edges=timeBin_x_edges[frequency>1 & sd != 0]
  real_edge_count <- trips[trip %in% tripID,.(length(time)), trip]$V1
  simulated_edge_count <- real_edge_count+floor(rnorm(length(real_edge_count), mean = 0, sd = sigma_n))
  simulated_data <- trips[trip %in% tripID, .(linkId, timebin), trip][, {
    
    target_length <- simulated_edge_count[match(trip[1], tripID)]
    edge_stats <- as.matrix(multi_timeBin_x_edges[, .(mean, sd, frequency,ID)])
    # 根据linkId和timebin查找对应ID
    current_ids <- timeBin_x_edges[linkId %in% .SD$linkId & timeBin %in% .SD$timebin, ID]
    current_features <- edge_stats[edge_stats[,"ID"] %in% current_ids, ]
    
    mean_diff <- outer(current_features[, "mean"], edge_stats[, "mean"], "-")
    current_var <- current_features[, "sd"]^2/current_features[, "frequency"]
    edge_var <- edge_stats[, "sd"]^2/edge_stats[, "frequency"]
    
    combined_se <- sqrt(outer(current_var, edge_var, "+"))
    
    t_ratio <- abs(mean_diff) / combined_se
    df_matrix <- (outer(current_var, edge_var, "+"))^2 / 
                (outer(current_var^2/(current_features[, "frequency"]-1), 
                      edge_var^2/(edge_stats[, "frequency"]-1), "+"))
    
    # 预计算唯一自由度值的分位数
    unique_df_values <- unique(as.vector(df_matrix))
    quantile_values <- qt((1 + significance)/2, df = unique_df_values)
    
    # 创建分位数矩阵
    qt_matrix <- matrix(quantile_values[match(df_matrix, unique_df_values)], 
                       nrow = nrow(df_matrix), ncol = ncol(df_matrix))
    
    similarity_conditions <- t_ratio < qt_matrix
    
    # 重构有效边筛选逻辑
    valid_cols <- which(colSums(similarity_conditions) > 0)
    
    # 矩阵采样
    valid_ID <- multi_timeBin_x_edges[valid_cols, ID]
    
    sampled_indices <- if(length(valid_ID) > 0) {
      sample(valid_ID, target_length, replace=TRUE)
    }
    sampled_edges <- multi_timeBin_x_edges[ID %in% sampled_indices, .(
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