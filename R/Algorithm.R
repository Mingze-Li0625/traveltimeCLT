#' This function allows to calculate travel time estimation confidence intervals on the test set.
#' @param t0
#' @param rho
#' @param linkfrom
#' @param len
#' @param sequence
#' @param rules
#' @param graph.stat.full
#' @examples
#' param_zeta(time[1], rho_obj$rho, linkId.from, linkId.to, length, rules = rules, graph.stat.full = graph_obj$graph.stat.full)
#' @import data.table
#' @import traveltimeHMM
#' @export

# A.5 Model computation through the param_zeta function:
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

param_zeta<-function(t0, rho,linkfrom, linkto, len, sequence = FALSE, rules = NULL, graph.stat.full = NULL){
  t0 = as.POSIXlt(t0)
  time_bins <- rules2timebins(rules)
  tbin = time_bins(t0)
  g = graph.stat.full[linkId.from %in% linkfrom]
  t = 0
  vcumlative = 0
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
