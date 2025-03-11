#'Standard error calculator
#'
#'Omit Na value, and return zero if there is only one valid observation.
#'
#'@param x Numeric vector or an R object but not a factor.
#' @author Mingze Li <mingzeli7@cmail.carleton.ca>
#' @export
sd_one_input_is_0<-function(x){
  x=na.omit(x)
  if(length(x)==1)return(0)
  else return(sd(x))
}
#'Mode calculator
#'
#'Return the first value present the the most time in the vector.
#'
#'@param x Numeric vector or an R object but not a factor.
#' @export
get_mode <- function(x) {
  x=na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}