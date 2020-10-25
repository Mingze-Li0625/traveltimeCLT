#' This function allows to calculate: MAREgeo, RMSE, MAE, ME, MAPE, empirical.cov, PI.length, PI.rel.length
#' @param tt
#' @param est
#' @param sym.q
#' @examples
#' numerical_res(obstt, tt, sd * qnorm(0.975) traveltimeCLT_obj$variance)
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
    empirical.cov = 100*mean(abs(tt-est)<=sym.q),
    PI.length = mean(2*sym.q),
    PI.rel.length = 100*mean(2*sym.q/tt)
  )
}
