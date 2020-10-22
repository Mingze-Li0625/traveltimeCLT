#' Travel time estimation modeling using Central Limit Theorem
#'
#' This function allows to calculate travel time estimation confidence intervals on the test set.
#' @param obj.traveltime Object coming from the function traveltimeCLT
#' @param data.test Test set.
#' @param bin Allows to select a specific timebin from the dataset.
#' @param rules Need to represent a list containing, start, end, days and tag for each timebin of the dataset (see example).
#' @examples
#' predict.traveltimeCLT(obj.traveltime = traveltimeCLT, data.test = test, bin = "MR", rules = list(list(start='6:30', end= '9:00', days = 0:6, tag='MR'),list(start='15:00', end= '18:00', days = 0:6, tag='ER')))
#' @import data.table
#' @import traveltimeHMM
#' @export

traveltimeCLT <- function(data.train = NULL, M = NULL, L = NULL, bin = NULL, rules = NULL, data.timebins = NULL){

  graph_obj <- create_graph(data.train, L, data.timebins)

  samp_obj <- sample_data(graph_obj$data.train, bin = bin, M = M)

  rho_obj <- get_rho(tt = samp_obj$tt, xx= samp_obj$xx)

  # A.6 Residual variance.
  # We create a residual variance function, allowing to supply the residual variance taking as input: DB (data), rho, etsamp = NULL.
  # This function uses the param_zeta function seen above.

  # First, it calculates obstt, which is the sum of tt (travel time) for each trip.
  # Then we add to the data D a column of the residues called res and calculated in the following way:
  if(is.null(samp_obj$samp)){
    A = graph_obj$data.train[order(trip, time)]
  } else{
    A = graph_obj$data.train[trip %in% samp_obj$samp][order(trip, time)]}

  B = A[, param_zeta(time[1], rho_obj$rho, linkId.from, linkId.to, length, rules = rules, graph.stat.full = graph_obj$graph.stat.full), by = trip]
  D = merge(B, A[, .(obstt= sum(tt)), trip])
  D[, res := (obstt - tt)/sd]
  res = list(db = D, res.sd = sd(D$res))
  v = res$res.sd
  print("This is the mean of the residuals"); print(v)

  #Returning variables
  traveltimeCLT_obj <- list(variance = v, rho = rho_obj$rho, graph.stat.full = graph_obj$graph.stat.full)

  class(traveltimeCLT_obj) = append(class(traveltimeCLT_obj), "traveltimeCLT_obj", after=0)

  invisible(traveltimeCLT_obj)
}
