#' @import data.table
NULL
# -------------functions -------------------------------------------------------

#' Construct Metric Graphs from Connection Data
#'
#' Builds directed graph representations of road networks using time bin \eqn{\times} connection data, 
#' (see \code{\link{get_timeBin_x_connections}}).
#' 
#' @param timeBin_x_connections A data.table, containing road connection data. 
#' at least need following columns:
#'   \itemize{
#'     \item{linkID - Character/numeric, Origin edge identifier}
#'     \item{nextLinkID - Character/numeric, Destination road segment identifier (character/numeric)}
#'     \item{length - Numeric, The length of the llnk ID}
#'     \item{fictional - Boolean, indicating if connection is fictional}
#'   }
#'
#' @return A list containing two igraph objects. The one way map has non-fictional data only,
#' and the two way map allowing fictional data.
#' @examples
#' data(trips)
#' names(trips)<-c("trip","linkid","timebin","speed","duration","length","time")
#' stat<-get_timeBin_x_connections(trips)
#' metric_graph <- get_metric_graph(stat)
#' 
#' @export
#' @importFrom igraph graph_from_data_frame simplify
get_metric_graph <- function(timeBin_x_connections){
  trips <- timeBin_x_connections
  net <- unique(trips, by = c("linkID", "nextLinkID"))
  net <- net[,.(linkID,nextLinkID,length)]
  names(net)[3]<-"weight"
  filted_net <- trips[trips$fictional==F,]
  filted_net <- unique(filted_net, by = c("linkID", "nextLinkID"))
  filted_net <- filted_net[,.(linkID,nextLinkID,length)]
  names(filted_net)[3]<-"weight"
  g1 <- igraph::graph_from_data_frame(filted_net, directed = TRUE)
  g2 <- igraph::graph_from_data_frame(net, directed = T)
  g1 <- igraph::simplify(g1, remove.multiple = TRUE, remove.loops = TRUE)
  g2 <- igraph::simplify(g2, remove.multiple = F, remove.loops = TRUE)
  return(list(one_way_map=g1,two_way_map=g2))
}
#' Plot Metric Graphs from a raw trips data
#'
#' plot a simplified directed metric graph representations of road networks using the raw trips data. 
#' 
#' @param samledtrips A data.table, containing raw trips data. 
#' The column names are case-insensitive. See \code{\link{get_timeBin_x_connections}}) for more details.
#' @return A simplified metric graph.
#' The begging edge of trips would be labeled in green, and the ending node is red. 
#' If multiple trips used the same edge, such edge will be labeled in yellow. 
#' If there are more than 6 unlabeled continuous edges, this function will draw only 6 edges.
#' @details We don't recommend to include too many trips data, as the plot would be too complex, 
#'   and the process time would be too long.
#' The input need to fulfill:
#' - Time parameters: `timeBin` + (`duration` or `log_duration`), OR `time`.
#' - Trip parameters: `trip`, `linkId`, `length`. Column `trip` in trips refer the trip ID.
#' @examples
#' data(trips)
#' names(trips)<-c("trip","linkid","timebin","speed","duration","length","time")
#' small_trips <- trips[trips$trip%in%2700:2750,]
#' p=plot_metric_graph(small_trips)
#' p
#' @author Mingze Li <mingzeli7@cmail.carleton.ca>
#' @export
#' @importFrom ggraph ggraph geom_edge_link geom_node_point geom_node_text
#' @importFrom ggplot2 scale_size_manual scale_color_manual theme_void theme
#' @importFrom igraph graph_from_data_frame degree V all_simple_paths
#' @importFrom tidygraph as_tbl_graph

plot_metric_graph <- function(sampledtrips){
  sampledtrips <- data.table::data.table(sampledtrips)
  names(sampledtrips) <- tolower(names(sampledtrips))
  setnames(sampledtrips, 
           old = c( "linkid", "timebin"),
           new = c( "linkId", "timeBin"),
           skip_absent = TRUE)
  sampled_connection=get_timeBin_x_connections(sampledtrips)
  edges <- unique(sampled_connection, by = c("linkID", "nextLinkID"))
  edges <- edges[fictional==F,]
  g <- igraph::graph_from_data_frame(edges, directed = TRUE)
  tidy_g <- tidygraph::as_tbl_graph(g)
  edge_alpha <- 1
  filtered_trips <- sampledtrips
  start_nodes <- as.character(filtered_trips[, .( linkId[1]), by = trip]$V1)
  end_nodes <- as.character(filtered_trips[, .( linkId[length(linkId)]), by = trip]$V1)
  junction_nodes <- igraph::V(g)[degree(g, mode = "out") > 1 | degree(g, mode = "in") > 1]$name
  node_label <- ifelse(igraph::V(g)$name %in% c(junction_nodes,end_nodes,start_nodes), igraph::V(g)$name, NA)
  paths <- list()
  for (i in 1:length(start_nodes)) {
    paths <- c(paths, igraph::all_simple_paths(g, from = start_nodes[i], to = end_nodes[i]))
  }
  shorten_segment <- function(segment) {
    l<-length(segment)
    if (l > 6) {
      new_length <- 5
      segment <- c(segment[1:new_length],segment[l])
    }
    return(segment)
  }
  segmented_paths <- lapply(paths, function(path) {
    junctions_in_path <- intersect(names(path), junction_nodes)
    if (length(junctions_in_path) == 0) {
      return(shorten_segment(path))
    }
    segments <- list()
    start_index <- 1
    for (junction in junctions_in_path) {
      end_index <- which(names(path) == junction)
      segment <- path[start_index:(end_index-1)]
      segments <- c(segments, list(shorten_segment(segment)))
      start_index <- end_index
    }
    last_segment <- path[start_index:length(path)]
    segments <- c(segments, list(shorten_segment(last_segment)))
    return(unlist(segments))
  })
  new_edges <- do.call(rbind, lapply(segmented_paths, function(path) {
    path_names <- names(path)
    from <- path_names[-length(path_names)]
    to <- path_names[-1] 
    data.frame(from = from, to = to)
  }))
  
  g <- igraph::graph_from_data_frame(new_edges, directed = TRUE)
  tidy_g <- tidygraph::as_tbl_graph(g)
  node_label <- ifelse(igraph::V(g)$name %in% c(junction_nodes,end_nodes,start_nodes), igraph::V(g)$name, NA)
  p1 <- ggraph::ggraph(tidy_g, layout = "stress") +
    ggraph::geom_edge_link(
      ggplot2::aes(alpha = edge_alpha), 
      arrow = ggplot2::arrow(length = grid::unit(1.5, "mm")), 
      edge_color = "black"
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(
        color = ifelse(name %in% start_nodes, "Start", 
                       ifelse(name %in% end_nodes, "End", 
                              ifelse(name %in% junction_nodes, "Junction", "Normal"))),
        size = ifelse(name %in% start_nodes, "Start", 
                      ifelse(name %in% end_nodes, "End", 
                             ifelse(name %in% junction_nodes, "Junction", "Normal")))
      )
    ) +
    ggraph::geom_node_text(ggplot2::aes(label = node_label), size = 3, color = "darkblue", repel = TRUE, na.rm = TRUE) +
    ggplot2::scale_size_manual(values = c("Start" = 2, "End" = 2, "Junction" = 2, "Normal" = 0.01)) +
    ggplot2::scale_color_manual(values = c("Start" = "green", "End" = "red", "Junction" = "orange", "Normal" = "lightblue")) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")
}
