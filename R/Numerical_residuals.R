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

# A.3 We get the model estimations
numerical_res<-function(tt, est, sym.q){
  list(
    MAREgeo = 100*exp(mean(log(abs(tt - est)/tt))),
    RMSE = sqrt(mean((tt - est)^2)),
    MAE  = mean(abs(tt - est)),
    ME  = mean(tt- est),
    MAPE = 100*mean(abs(tt - est)/tt),
    emprical.cov = 100*mean(abs(tt-est)<=sym.q),
    PI.length = mean(2*sym.q),
    PI.rel.length = 100*mean(2*sym.q/tt)
  )
}
