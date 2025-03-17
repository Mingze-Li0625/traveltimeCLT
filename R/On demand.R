#' On Demand Simulator
#' 
#' This function simulate the travel time with different correlation structures
#' base on the log normal distributions and information from 
#' time-bin \eqn{\times} edge statistics.
#' 
#' @param tripID Vector of trip identifiers to analyze
#' @param trips Data.table at least containing following trip information:
#'              \itemize{
#'               \item{trip - Trip identifier}
#'               \item{linkId - Road identifier}
#'               \item{time - Timestamp (POSIXct)}
#'               \item{timeBin - Time period classification (optional, see\code{\link{time_bins_readable}})}
#'              }
#' @param timeBin_x_edges Precomputed edge statistics data.table (optional, generated from
#'  see\code{\link{get_timeBin_x_edges}}). Should at least contain:
#'              \itemize{
#'               \item{linkId - Road identifier}
#'               \item{timeBin - Time period classification}
#'               \item{mean - Log-transformed travel time mean}
#'               \item{sd - Log-transformed travel time standard deviation}
#'              }
#' @param rho Correlation coefficient (0-1) for dependent uniform generators. Default=0.31
#' @return A data.frame contains real travel times and four simulated time:
#'         \itemize{
#'          \item{real_time - Actual observed travel time}
#'          \item{dependent_time - Correlated uniform simulation 
#'          (\eqn{rho}-controlled), see \code{\link{dependent_uniform}}}
#'          \item{independent_time - Independent uniform simulation}
#'          \item{first_order_time - First-order Correlated uniform simulation
#'          (\eqn{rho}-controlled), see \code{\link{first_order_uniform}}}
#'          \item{second_order_time - Second-order Correlated uniform simulation
#'          (\eqn{rho}-controlled), see \code{\link{second_order_uniform}}}
#'         }
#' @author Mingze Li <mingzeli7@cmail.carleton.ca>
#' @examples
#' data(trips)
#' names(trips)<-c("trip","linkid","timebin","speed","duration","length","time")
#' OnDemand_simulator(c(2700,2701,2702,2716,2726,2732,2738,2739,2744,2746,2747,2748,2755,2757,2769,2788,2790),trips)
#' @export
OnDemand_simulator<- function(tripID,trips,timeBin_x_edges=NULL,rho=0.31){
  if (!data.table::is.data.table(trips)) {
    data.table::setDT(trips)
  }
  names(trips) <- tolower(names(trips))
  if(is.null(trips$time))stop("trips do not have time!")
  if(is.null(timeBin_x_edges))timeBin_x_edges<-get_timeBin_x_edges(trips)
  setnames(trips, 
           old = c( "linkid", "timebin"),
           new = c( "linkId", "timeBin"),
           skip_absent = TRUE)
  if(is.null(trips$timeBin))trips$timeBin=time_bins_readable(trips$time)

  real_time <- as.numeric(trips[trip %in%tripID, .(time[.N]-time[1]),trip]$V1)
  sampled_trips <- trips[trip %in% tripID, c("trip", "linkId", "timeBin")]
  sampled_trips<-merge(sampled_trips, timeBin_x_edges, by = c("linkId", "timeBin"), all.x = TRUE)
  sampled_trips<- na.omit(sampled_trips)
  dependent_time<-sampled_trips[,.(sum(exp(mean+sd*qnorm(dependent_uniform(.N,rho = rho))))),trip]$V1
  independent_time<-sampled_trips[,.(sum(exp(mean+sd*qnorm(runif(.N))))),trip]$V1
  first_order_time<-sampled_trips[,.(sum(exp(mean+sd*qnorm(first_order_uniform(.N,rho = rho))))),trip]$V1
  second_order_time<-sampled_trips[,.(sum(exp(mean+sd*qnorm(second_order_uniform(.N,rho = rho))))),trip]$V1
  data.frame(cbind(real_time,dependent_time,independent_time,first_order_time,second_order_time))


}
