#' This function allows to create the graph of the network and run the algorithm on the train set to get the mean of the autocorrelation and the mean of the residuals.
#' @param data.train Train dataset
#' @param L A parameter allowing to remove edges that have not enough observations in the train set.
#' @param data.timebins A vector of strings that represent the different time-bins of the dataset.


#' @examples
#' create_graph(data.train = data,train, L = L, data.timebins = data.timebins)
#' @import data.table
#' @import traveltimeHMM
#' @export

create_graph <- function(data.train = NULL, L = NULL, data.timebins = NULL) {

  # A.0 We are starting to transform our data so that it takes on a network form.
  # So, we transform the linkId variable into two variables: LinkId.from and LinkId.to.
  # Since a link represents a segment of a road network,
  # we can thus know the origin and the destination of a vehicle for each link of this network.
  # We add a variable called N, which represents the number of times that an ijk link
  # (i = linkId.from, j = linkId.to etk = timebins) is present in our data set.
  # Since there is no destination j (linkId.to) for the last ijk link of the trip of a vehicle,
  # we consider its value as NA.
  data.train[, linkId.from := linkId, by = trip]
  data.train[, linkId.to := shift(linkId, type = 'lead'), by = trip]
  data.train[, N:=.N, by =list(linkId.from, linkId.to, timeBins) ]

  # A.1 Variance and mean velocity computation
  # mu and sd2: For each ijk link, we calculate the mean and the variance of the speed for all
  # the times the ijk link is taken. Consequently, we also obtain NA values
  # for the variance of the speed of the ijk links which were taken only 1 time (N = 1) in the data set.
  graph.stat = data.train[, .(mu = mean(1/speed),  sd2 = var(1/speed), N=N[1]) ,by = list(linkId.from, linkId.to, timeBins)]

  # We create the structure of the graph network
  full.graph = graph.stat[, .(timeBins = data.timebins) ,by = list(linkId.from, linkId.to)]

  # We merge mu and sd2 calculated above to the structure of our graph network
  graph.stat = merge(full.graph, graph.stat, by= c('linkId.from', 'linkId.to', 'timeBins'), all.x = TRUE)
  graph.stat$N[is.na(graph.stat$N)]<-0

  # mu.from.tb and sd2.from.tb: We calculate the mean and the variance of the speed
  # for all the times when the ik link (linkid.from and timebins) is borrowed more
  # than 10 times (without looking at the destination).
  # That is to say that the value of mu.from.tb and sd2.from.tb exists if and only if,
  # the link ik has a sum of N greater than 10 (where 10 is a hyperparameter )
  graph.stat.from.tb = data.train[, .(mu = mean(1/speed),
                                      sd2 = var(1/speed), N= .N) ,
                                  by = list(linkId.from, timeBins)]

  # mu.tb and sd2.tb: We calculate the mean and the variance of the speed for each timebins.
  graph.stat.tb = data.train[, .(mu = mean(1/speed),
                                 sd2 = var(1/speed), N= .N) ,by = list(timeBins)]

  # we combine the three in the same data frame
  graph.stat.full =  merge(graph.stat,
                           graph.stat.from.tb[N>L, .(mu.from.tb = mu,
                                                     sd2.from.tb = sd2, linkId.from, timeBins)],
                           all.x = TRUE,
                           by.x=c('linkId.from', 'timeBins'),
                           by.y = c('linkId.from', 'timeBins'))
  graph.stat.full =  merge(graph.stat.full,
                           graph.stat.tb[N>L, .(mu.tb = mu, sd2.tb = sd2, timeBins)],
                           all.x = TRUE, by.x=c('timeBins'), by.y = c('timeBins'))

  # A.2 For each ijk link, if the value of mu and / or sd2 is missing, then the variance and the mean
  # of the ijk link are the values of mu.from.tb and sd2.from.tb.
  # If these values are also missing, then the mean and the variance of the ijk link become
  # the values of mu.tb and sd2.tb.
  # ( Sd2 is actually the standard deviation so we calculate the square root of sd2 to find the variance.)
  graph.stat.full[, muspeed := ifelse(is.na(mu) | N < L, ifelse(is.na(mu.from.tb), mu.tb, mu.from.tb), mu)]
  graph.stat.full[,  imputed_mean := ifelse(muspeed== mu, FALSE, TRUE )]

  graph.stat.full[, sdspeed := ifelse(is.na(sd2), ifelse(is.na(sd2.from.tb), sd2.tb, sd2.from.tb), sd2)]
  graph.stat.full[, imputed_sd := ifelse(sdspeed == sd2  & !is.na(sd2) , FALSE, TRUE)]

  graph.stat.full = graph.stat.full[,.(linkId.from,
                                       linkId.to,
                                       timeBins,
                                       mean = muspeed,
                                       sd = sqrt(sdspeed),
                                       imputed_sd,
                                       imputed_mean )]
  # Merging with the train dataset
  data.train = merge(data.train, graph.stat.full, all.x = TRUE, by = c('linkId.from', 'linkId.to', 'timeBins'), sort = FALSE)[order(trip,time)]

  # Returning variables
  graph <- list(graph.stat.full = graph.stat.full, data.train = data.train)

  class(graph) = append(class(graph), "graph", after=0)

  graph
}
