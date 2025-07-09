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

#' Calculate group means
#'
#' Compute means for numeric vectors grouped by specified size
#'
#' @param x Numeric vector
#' @param group_size Integer specifying number of observations per group
#' @author Mingze Li <mingzeli7@cmail.carleton.ca>
#' @export
group_mean <- function(x, group_size) {
    dt <- data.table(x)
    names(dt) <- "value"
    dt[, chunk := (seq_len(.N) - 1) %/% group_size]
    Kt_sum_dt <- dt[, .(mean_value = sum(value) / group_size), by = chunk]$mean_value
    return(Kt_sum_dt)
}