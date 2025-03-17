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
                                 timebin=timebinlist,frequency=frequency)
      }else {
        duration<-as.numeric(difftime(expect_time,start_time,  units = "secs"))
        simulate_duration<-as.numeric(difftime(simulate_time,start_time,  units = "secs"))
        result[[path_idx]]<-list(expect_time=duration,expected_arrivetime=expect_time,
                                 simulate_time=simulate_duration,simulate_arrivetime=simulate_time,
                                 timebin=timebinlist,frequency=frequency)
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
        edge_data <- timeBin_x_connections[linkID == leave & nextLinkID == arrive & timeBin == time_Bin,]
        timebinlist[i]<-time_Bin
        if (nrow(edge_data) == 0) {
          edge_data <- timeBin_x_connections[linkID == leave & nextLinkID == arrive & timeBin == "Global",]
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

#' @export
route_length<- function( pathset,dataset) {
  result <- vector("list", length(pathset)) 
  if(length(pathset)==0)stop("the path set is empty!")
  if(is.null(dataset$nextLinkID)){
    
  }else{
    
  }
  
  for (path_idx  in 1:length(pathset)) {
    path <- pathset[[path_idx]]
    path<- attr(path,"names")
    l = length(path)-1
    len=0
    lenlist=c()
    label_list <-c()
    for(i in 1:l){
      leave <- as.integer(path[i])
      arrive <- as.integer(path[i+1])
      edge_data <- dataset[linkID == leave & nextLinkID == arrive]
      if (nrow(edge_data) == 0)stop(paste("Cannot find statistics from ",leave," to ",arrive))
      steplength=edge_data[timeBin=="Global",length]
      len <- len+steplength
      label_list <-c(label_list,paste(leave,"->",arrive))
      lenlist<-c(lenlist,steplength)
    }
    names(lenlist)<-label_list
    result[[path_idx]]<-list(total_length=len,length_list=lenlist)
  }
  return(result)
}
