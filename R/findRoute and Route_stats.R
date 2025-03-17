#' Route finding function
#' 
#' This function can find the k shortest length route in the matrix graph. 
#' @param graphs List, containing two igraph object, one_way_map and two_way_map, 
#' produced by \code{\link{get_metric_graph}}. 
#' @param start the starting edge number, it could either be a string or an integer.
#' @param end the ending edge number, it could either be a string or an integer.
#' @param k Integer, the number of different routes needed. Default to be 1.
#' A large k may increase process time.
#' 
#' @return A list containing oneway and twoway path list. 
#' oneway is a igraph path set that uses one_way_map, and twoway uses two_way_map.
#' The routes are in acsending order based on their length.
#' 
#' @references 
#'   Yen, Jin Y
#'   {An algorithm for finding shortest routes from all source nodes to a given destination in general networks}. 
#'   \emph{Quarterly of applied mathematics}, 27(4):526–530,1970 
#'   \url{https://doi.org/10.1090/qam/253822}
#'   
#' @examples
#' data(trips)
#' names(trips)<-c("trip","linkid","timebin","speed","duration","length","time")
#' stat<-get_timeBin_x_connections(trips)
#' metric_graph <- get_metric_graph(stat)
#' routeset <- findRoute(metric_graph,1,31,3)
#' routeset
#' @seealso \code{\link{route_time}} the route time calculator.
#' \code{\link{route_length}} the route length calculator.
#' @author Mingze Li <mingzeli7@cmail.carleton.ca>
#' @export
#' @importFrom igraph k_shortest_paths E
findRoute <- function(graphs,start, end,k = 1) {
  g1<-graphs$one_way_map
  g2<-graphs$two_way_map
  start <- as.character(start)
  end <- as.character(end)
  paths1 <- igraph::k_shortest_paths(g1, from = start, to = end, k = k, mode = "out")$vpaths
  paths2 <- igraph::k_shortest_paths(g2, from = start, to = end, k = k, mode = "out")$vpaths
  return(list(oneway = paths1, twoway = paths2))
}
#' Route time calculator
#' 
#' This function calculates informations about the route time.
#' 
#' @param pathset list, it can be:
#' -a igraph path set produced by k_shortest_paths function in igraphs package.
#' -a list contain string or integer vectors represent the edge ID.
#' -either ~$oneway or ~$twoway path set, where ~ is produced from \code{\link{findRoute}} function.
#' @param dataset either a time-bin \eqn{\times} connection statistics table or 
#' a time-bin \eqn{\times} edge statistics table. See \code{\link{get_timeBin_x_connections}},\code{\link{get_timeBin_x_edges}}
#' @param time string of string vector, the starting time of trip(s). It could be time bin, we have
#' "EveningNight","EveningRush","Weekday","MorningRush","Weekendday", and "Global".
#' Or a specific time, like "2025/03/09 19:59:30". 
#' If input a vector, its length need to be larger or equal to the number of path in the pathset.
#' `time[i]` will be view as the starting time of `path[i]`.
#' @param simulator it could be "independent","dependent","first order",or "second order".
#' see \code{\link{dependent_uniform}}, \code{\link{first_order_uniform}}, and \code{\link{second_order_uniform}}.
#' @param rho numeric, the correlation coefficient.
#' 
#' @return A list with length equal to the number of paths in the pathset. 
#' Each entry `i` (corresponding to the `i`-th path) is a list containing:
#' \itemize{
#'   \item \code{expect_time}: Numeric. Expected duration (in seconds) to travel
#'    from the beginning of the start edge to the beginning of the end edge.
#'   \item \code{expected_arrivetime}: Numeric. Expected arrival time at the
#'    beginning of the end edge (calculated via expected durations).
#'   \item \code{simulate_time}: Numeric. Simulated duration (in seconds) for
#'    completing the route.
#'   \item \code{simulate_arrivetime}: Numeric. Simulated arrival time at the 
#'   beginning of the end edge.
#'   \item \code{timebin}: Named character vector. Time bins used for statistics 
#'   at each step. If no data exists for the current time bin, falls back to "Global". 
#'   Long trips may switch time bins, the switch is based on expected time.
#'   \item \code{fictional}: Named logical vector. `FALSE` if the step exists in 
#'   `one_way_map`; `TRUE` otherwise (fictional connection).
#'   \item \code{frequency}: Named numeric vector. Number of historical records 
#'   used for statistical calculations at each step.
#' }
#' @examples
#' data(trips)
#' names(trips)<-c("trip","linkid","timebin","speed","duration","length","time")
#' stat<-get_timeBin_x_connections(trips)
#' metric_graph <- get_metric_graph(stat)
#' routeset <- findRoute(metric_graph,1,31,3)
#' route_time(routeset$oneway,stat,"2025/03/09 19:59:30","dependent",0.2)
#' stat2<-get_timeBin_x_edges(trips)
#' route_time(list(c(1,2,3,4,6,8,11,12)),stat2,"2025/03/09 19:59:30","dependent",0.2)
#' @seealso \code{\link{findRoute}} Route finding function.
#' \code{\link{route_length}} the route length calculator.
#' @author Mingze Li <mingzeli7@cmail.carleton.ca>
#' @export
route_time<- function( pathset,dataset,time="Global",simulator="independent",rho=0.31) {
  simulator<-tolower(simulator)
  isTimeBin<-c(T)
  multitime<-T
  if(length(time)==1)multitime<-F
  else if(length(time)<length(pathset))stop("Length of 'time' must be either 1 or >= the number of paths in 'pathset'.")
  for (i in 1:length(pathset)) {
    if(multitime){
      if(!(time[i] %in% c("EveningNight", "EveningRush" , "Weekday"  ,    "MorningRush" , "Weekendday","Global"  ))){
        isTimeBin[i]<-F
      }else isTimeBin[i]<-T
    }else{
      if(!(time %in% c("EveningNight", "EveningRush" , "Weekday"  ,    "MorningRush" , "Weekendday","Global"  ))){
        isTimeBin[i]<-F
        start_time <- as.POSIXct(time)
      }else isTimeBin[i]<-T
    }
  }
  result <- vector("list", length(pathset)) 
  if(length(pathset)==0)stop("the path set is empty!")
  if(is.null(dataset$nextLinkID)){
    for (path_idx  in 1:length(pathset)) {
      path <- pathset[[path_idx]]
      path<- attr(path,"names")
      if(is.null(path))path <- pathset[[path_idx]]
      if(length(path)<2)stop("the path has edges less than 2!")
      l = length(path)-1
      if(isTimeBin[path_idx]){if(multitime)start_time<-time[path_idx]
      expect_time <- 0
      simulate_time<-0
      time_Bin<-time[path_idx]
      simulate_time_Bin<-time[path_idx]
      }else{if(multitime)start_time<-as.POSIXct(time[path_idx])
        expect_time<-start_time
      simulate_time<-start_time}
      if(simulator=="independent")U<-runif(l)
      else if(simulator=="dependent")U<-dependent_uniform(l,rho)
      else if(simulator=="first order")U<-first_order_uniform(l,rho)
      else if(simulator=="second order")U<-second_order_uniform(l,rho)
      else stop("The simulator is not supported!")
      fictional <-c()
      frequency <-c()
      label_list <-c()
      timebinlist<-c()
      realtimelist<-c()
      for(i in 1:l){
        if(!isTimeBin[path_idx]){ time_Bin<-time_bins_readable(expect_time)
        simulate_time_Bin<-time_bins_readable(simulate_time)}
        leave <- as.integer(path[i])
        arrive <- as.integer(path[i+1])
        edge_data <- dataset[linkId == leave  & timeBin == time_Bin,]
        simulate_edge_data <- dataset[linkId == leave  & timeBin == simulate_time_Bin,]
        timebinlist[i]<-time_Bin
        if (nrow(edge_data) == 0) {
          edge_data <- dataset[linkId == leave  & timeBin == "Global",]
          timebinlist[i]<-"Global"
        }
        if (nrow(simulate_edge_data) == 0) {
          edge_data <- dataset[linkId == leave  & timeBin == "Global",]
          #timebinlist[i]<-"Global"
        }
        if (nrow(edge_data) == 0)stop(paste("Cannot find statistics of edge ",leave))
        duration <- exp(edge_data$mean)
        simulate_duration <- exp(edge_data$mean+edge_data$sd*qnorm(U[i]))
        realtimelist[i] <- duration
        expect_time <- expect_time+duration
        simulate_time <- simulate_time+simulate_duration
        label_list <-c(label_list,paste(leave,"->",arrive))
        frequency<-c(frequency,edge_data$frequency)
        names(frequency)<-label_list
        names(timebinlist)<-label_list
        names(realtimelist)<-label_list
      }
      if(isTimeBin[path_idx]){
        result[[path_idx]]<-list(expect_time=expect_time,expected_arrivetime=NA,
                                 simulate_time=simulate_time,simulate_arrivetime=NA,
                                 timebin=timebinlist,fictional=NA,frequency=frequency)
      }else {
        duration<-as.numeric(difftime(expect_time,start_time,  units = "secs"))
        simulate_duration<-as.numeric(difftime(simulate_time,start_time,  units = "secs"))
        result[[path_idx]]<-list(expect_time=duration,expected_arrivetime=expect_time,
                                 simulate_time=simulate_duration,simulate_arrivetime=simulate_time,
                                 timebin=timebinlist,fictional=NA,frequency=frequency)
      }
    }
  }else{
    for (path_idx  in 1:length(pathset)) {
      path <- pathset[[path_idx]]
      path<- attr(path,"names")
      if(is.null(path))path <- pathset[[path_idx]]
      if(length(path)<2)stop("the path has edges less than 2!")
      l = length(path)-1
      if(isTimeBin[path_idx]){if(multitime)start_time<-time[path_idx]
      expect_time <- 0
      simulate_time<-0
      time_Bin<-time[path_idx]
      simulate_time_Bin<-time[path_idx]
      }else{if(multitime)start_time<-as.POSIXct(time[path_idx])
      expect_time<-start_time
      simulate_time<-start_time}
      if(simulator=="independent")U<-runif(l)
      else if(simulator=="dependent")U<-dependent_uniform(l,rho)
      else if(simulator=="first order")U<-first_order_uniform(l,rho)
      else if(simulator=="second order")U<-second_order_uniform(l,rho)
      else stop("The simulator is not supported!")
      fictional <-c()
      frequency <-c()
      label_list <-c()
      timebinlist<-c()
      for(i in 1:l){
        if(!isTimeBin[path_idx]){ time_Bin<-time_bins_readable(expect_time)
        simulate_time_Bin<-time_bins_readable(simulate_time)}
        leave <- as.integer(path[i])
        arrive <- as.integer(path[i+1])
        edge_data <- dataset[linkID == leave & nextLinkID == arrive & timeBin == time_Bin,]
        timebinlist[i]<-time_Bin
        if (nrow(edge_data) == 0) {
          edge_data <- dataset[linkID == leave & nextLinkID == arrive & timeBin == "Global",]
          timebinlist[i]<-"Global"
        }
        if (nrow(edge_data) == 0)stop(paste("Cannot find statistics from ",leave," to ",arrive))
        duration <- exp(edge_data$one_way_mean)
        simulate_duration <- exp(edge_data$one_way_mean+edge_data$one_way_sd*qnorm(U[i]))
        expect_time <- expect_time+duration
        simulate_time <- simulate_time+simulate_duration
        label_list <-c(label_list,paste(leave,"->",arrive))
        fictional<-c(fictional,edge_data$fictional)
        frequency<-c(frequency,edge_data$one_way_frequency)
        names(fictional)<-label_list
        names(frequency)<-label_list
        names(timebinlist)<-label_list
      }
      if(isTimeBin[path_idx]){
        result[[path_idx]]<-list(expect_time=expect_time,expected_arrivetime=NA,
                                 simulate_time=simulate_time,simulate_arrivetime=NA,
                                 timebin=timebinlist,fictional=fictional,frequency=frequency)
      }else {
        duration<-as.numeric(difftime(expect_time,start_time,  units = "secs"))
        simulate_duration<-as.numeric(difftime(simulate_time,start_time,  units = "secs"))
        result[[path_idx]]<-list(expect_time=duration,expected_arrivetime=expect_time,
                                 simulate_time=simulate_duration,simulate_arrivetime=simulate_time,
                                 timebin=timebinlist,fictional=fictional,frequency=frequency)
      }
    }
  }

  return(result)
}

#' Route length calculator
#' 
#' This function calculates informations about the route length.
#' 
#' @param pathset list, it can be:
#' -a igraph path set produced by k_shortest_paths function in igraphs package.
#' -a list contain string or integer vectors represent the edge ID.
#' -either ~$oneway or ~$twoway path set, where ~ is produced from \code{\link{findRoute}} function.
#' @param dataset either a time-bin \eqn{\times} connection statistics table or 
#' a time-bin \eqn{\times} edge statistics table. See \code{\link{get_timeBin_x_connections}},\code{\link{get_timeBin_x_edges}}
#' 
#' @return A list with length equal to the number of paths in the pathset. 
#' Each entry `i` (corresponding to the `i`-th path) contains:
#' \itemize{
#'   \item \code{total_length}: Numeric. Total driving distance from 
#'   the beginning of the start edge to the beginning of the end edge.
#'   \item \code{length_list}: Named numeric vector. Length of each individual step 
#'   in the path.
#'   \item \code{frequency}: Named numeric vector. Number of historical records 
#'   used for each step.
#' }
#' @examples
#' data(trips)
#' names(trips)<-c("trip","linkid","timebin","speed","duration","length","time")
#' stat<-get_timeBin_x_connections(trips)
#' metric_graph <- get_metric_graph(stat)
#' routeset <- findRoute(metric_graph,1,31,3)
#' route_length(routeset$oneway,stat)
#' stat2<-get_timeBin_x_edges(trips)
#' route_length(list(c(1,2,3,4,6,8,11,12)),stat2)
#' @seealso \code{\link{findRoute}} Route finding function.
#' \code{\link{route_time}} the route time calculator.
#' @author Mingze Li <mingzeli7@cmail.carleton.ca>
#' @export
route_length<- function( pathset,dataset) {
  result <- vector("list", length(pathset)) 
  if(length(pathset)==0)stop("the path set is empty!")
  if(is.null(dataset$nextLinkID)){
    for (path_idx  in 1:length(pathset)) {
      path <- pathset[[path_idx]]
      path<- attr(path,"names")
      if(is.null(path))path <- pathset[[path_idx]]
      if(length(path)<2)stop("the path has edges less than 2!")
      l = length(path)-1
      len=0
      lenlist=c()
      frequency <-c()
      label_list <-c()
      for(i in 1:l){
        leave <- as.integer(path[i])
        arrive <- as.integer(path[i+1])
        edge_data <- dataset[linkId == leave & timeBin=="Global" ]
        if (nrow(edge_data) == 0)stop(paste("Cannot find statistics of the edge ",leave))
        steplength=edge_data$length
        len <- len+steplength
        label_list <-c(label_list,paste(leave,"->",arrive))
        lenlist<-c(lenlist,steplength)
        frequency<-c(frequency,edge_data$frequency)
      }
      names(lenlist)<-label_list
      names(frequency)<-label_list
      result[[path_idx]]<-list(total_length=len,length_list=lenlist,frequency=frequency)
      
    }
    return(result)
  }else{
    for (path_idx  in 1:length(pathset)) {
      path <- pathset[[path_idx]]
      path<- attr(path,"names")
      if(is.null(path))path <- pathset[[path_idx]]
      l = length(path)-1
      len=0
      lenlist=c()
      frequency <-c()
      label_list <-c()
      for(i in 1:l){
        leave <- as.integer(path[i])
        arrive <- as.integer(path[i+1])
        edge_data <- dataset[linkID == leave & nextLinkID == arrive & timeBin=="Global"]
        if (nrow(edge_data) == 0)stop(paste("Cannot find statistics from ",leave," to ",arrive))
        steplength=edge_data$length
        len <- len+steplength
        label_list <-c(label_list,paste(leave,"->",arrive))
        lenlist<-c(lenlist,steplength)
        frequency<-c(frequency,edge_data$one_way_frequency)
      }
      names(lenlist)<-label_list
      names(frequency)<-label_list
      result[[path_idx]]<-list(total_length=len,length_list=lenlist,frequency=frequency)
    }
    return(result)
  }
  

}
