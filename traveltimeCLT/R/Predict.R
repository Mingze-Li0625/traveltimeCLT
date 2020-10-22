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

predict_traveltimeCLT <- function(obj.traveltime = NULL, data.test = NULL, bin = NULL , rules = NULL){

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


  # A.1 Model computation through the param_zeta function:
  # Now, we create our model which is a function (param_zeta), allowing to calculate the cumulative
  # variance and taking as parameters: t0 (a real time which will be converted into time bins by a
  # function calling the package traveltimeHMM), rho, linkfrom, linkto, len and sequence = FALSE.

  # The param_zeta function uses 2 loops: The first calculates the cumulative variance for all links - 1
  # (without the last link which is a NA) per trip.
  # The second loop takes into account the missing value for the LinkId.to of the last link of ijk,
  # since there is no destination for the last link of the route.
  # The cumulative variance is calculated in each loop by this line of code:

  # Where vcumlative adjusts to each iteration of the loop; len [i] ^ 2 is the distance traveled for each
  # link ijk to the square; d $ sd2 is the standard deviation of the speed for each ijk link of each trip;
  # rho is the autocovariance of the speed calculated by the function get_rho; lenprev is the distance traveled
  # to the previous link (lenprev = 1, for the first ijk link of each trip); vprev is the variance of the previous
  # link (vprev = 1, for the first ijk link of each trip).

  # This function has 2 outputs:
  # tt (traveltime): It represents the distance for the link ijk * the average speed for the same link ijk. (t = 0, for the first ijk link of each trip).
  # S : It is the square root of the cumulative variance for each ijk link.
  param_zeta<-function(t0, rho,linkfrom, linkto, len, sequence = FALSE){
    t0 = as.POSIXlt(t0)
    time_bins <- rules2timebins(rules)
    tbin = time_bins(t0)
    g = obj.traveltime$graph.stat.full[linkId.from %in% linkfrom]
    t = 0                               # time (mean)
    vcumlative = 0                                 # variance
    vprev = 1
    lenprev = 1
    if(sequence){
      t.seq = v.seq = numeric(length(len))
    }
    for(i  in 1:(length(len) - 1)){
      d = g[linkId.from == linkfrom[i] & linkId.to == linkto[i] & timeBins == tbin]
      t = t + len[i]*d$mean
      tbin = time_bins(t0 + t)
      vcumlative = vcumlative + len[i]^2  * d$sd^2  + 2*rho* len[i]*lenprev * d$sd * vprev
      vprev = d$sd
      lenprev = len[i]
      if(sequence){
        v.seq[i] = vcumlative
        t.seq[i] = t
      }
    }
    ## last linkId.to is NA
    i = i+1
    if(is.na(linkto[i])){
      d = g[linkId.from == linkfrom[i] & timeBins == tbin & is.na(linkId.to)]
    }else{
      d = g[linkId.from == linkfrom[i] & linkId.to == linkto[i] & timeBins == tbin]
    }
    t = t + len[i]*d$mean
    vcumlative = vcumlative + len[i]^2 * d$sd^2 + 2* rho * len[i]*lenprev *d$sd *vprev
    if(sequence){
      v.seq[i] = vcumlative
      t.seq[i] = t
    }
    s = sqrt(vcumlative)
    if(sequence){
      list(tt =t, sd = s, v.seq = sqrt(v.seq), t.seq = t.seq)
    }else{
      list(tt =t, sd = s)
    }}


  # A.2 Residual variance.
  # We create a residual variance function, allowing to supply the residual variance taking as input: DB (data), rho, etsamp = NULL.
  # This function uses the param_zeta function seen above.

  # First, it calculates obstt, which is the sum of tt (travel time) for each trip.
  # Then we add to the data D a column of the residues called res and calculated in the following way:
  A = data.test[trip %in% samp.test, param_zeta(time[1], obj.traveltime$rho, linkId.from, linkId.to, length), by = trip]

  # We merge to be able to get the observed travel time, the real travel time, and the variance
  B = merge(data.test[, .(obstt = sum(tt)), trip], A)

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
  # We get the results using the mean of the residuals v
  print("Model estimation using mean of the residuals v")
  print(t(B[ , numerical_res(obstt, tt, sd *qnorm(0.975)*obj.traveltime$variance)]))
  # We get the results without using the mean of the residuals v
  print("Model estimation without using mean of the residuals v")
  print(t(B[ , numerical_res(obstt, tt, sd *qnorm(0.975))]))

}
