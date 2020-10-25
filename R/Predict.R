#' This function allows to predict the travel time estimation on the test set.
#'
#' @param traveltimeCLT_obj
#' @param data.test
#' @param bin Allows to select a specific timebin from the dataset.
#' @param rules Need to represent a list containing, start, end, days and tag for each timebin of the dataset (see example).
#' @examples
#' predict.traveltimeCLT(traveltimeCLT_obj = ttCLTmodel, data.test = test, bin = "MR" , rules = list(list(start='6:30', end= '9:00', days = 0:6, tag='MR'),list(start='15:00', end= '18:00', days = 0:6, tag='ER'))
#' @import data.table
#' @import traveltimeHMM
#' @export

predict_traveltimeCLT <- function(traveltimeCLT_obj = NULL, data.test = NULL, bin = NULL , rules = NULL){

  # A.0 We are starting to transform our data so that it takes on a network form.
  # So, we transform the linkId variable into two variables: LinkId.from and LinkId.to.
  # Since a link represents a segment of a road network,
  # we can thus know the origin and the destination of a vehicle for each link of this network.
  # We add a variable called N, which represents the number of times that an ijk link
  # (i = linkId.from, j = linkId.to etk = timebins) is present in our data set.
  # Since there is no destination j (linkId.to) for the last ijk link of the trip of a vehicle,
  # we consider its value as NA.
  data.test[, linkId.from := linkId, by = trip]
  data.test[, linkId.to := shift(linkId, type = 'lead'), by = trip]
  data.test[, N:=.N, by =list(linkId.from, linkId.to, timeBins) ]


  # Sampling test data according to the model inputs
  samp.test = data.test[, timeBins[1], trip][V1 == bin , trip]

  # A.2 Residual variance.
  # We create a residual variance function, allowing to supply the residual variance taking as input: DB (data), rho, etsamp = NULL.
  # This function uses the param_zeta function seen above.

  # First, it calculates obstt, which is the sum of tt (travel time) for each trip.
  # Then we add to the data D a column of the residues called res and calculated in the following way:
  A = data.test[trip %in% samp.test, param_zeta(time[1], traveltimeCLT_obj$rho, linkId.from, linkId.to, length, rules = rules, graph.stat.full = traveltimeCLT_obj$graph.stat.full), by = trip]

  # We merge to be able to get the observed travel time, the real travel time, and the variance
  B = merge(data.test[, .(obstt = sum(tt)), trip], A)

  # We get the results using the mean of the residuals v
  print("Model estimation using mean of the residuals v")
  print(t(B[ , numerical_res(obstt, tt, sd *qnorm(0.975)*traveltimeCLT_obj$variance)]))
  # We get the results without using the mean of the residuals v
  print("Model estimation without using mean of the residuals v")
  print(t(B[ , numerical_res(obstt, tt, sd *qnorm(0.975))]))

}
