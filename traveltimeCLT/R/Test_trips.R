#' Travel time estimation modeling using Central Limit Theorem
#'
#' This function allows to easily split the dataset into a train and a test set.
#' @param M Number of test trips
#' @param db Dataset
#' @param min.n Number minimum of links allowed
#' @import data.table
#' @export


create_test_trips<-function(M = 500, db, min.n = 1){
  db[, linkId.from := linkId, by = trip]
  db[, linkId.to := shift(linkId, type = 'lead'), by = trip]
  db[, N:=.N, by =list(linkId.from, linkId.to, timeBins) ]
  A = db[, .(N = N[1]), by =list(linkId.from, linkId.to, timeBins) ]
  A[, N.left := 0 ]
  setkey(A, linkId.from, linkId.to, timeBins)

  bank = c()
  unique.trips =db[, min(N>1)  , trip][V1 ==1][, trip]

  repeat{
    n = length(unique.trips)
    car = unique.trips[sample.int(n,1)]
    k = db[trip==car, .(linkId.from, linkId.to, timeBins)]

    if(A[k, all(N -N.left - 1> 0 )]){
      bank = c(bank,car)              ## add the trip
      unique.trips = setdiff(unique.trips, bank) ## removing all smapled trips
      ## updating A
      A[k, N.left:= N.left + 1]
      ## break if number of samples reached
      if(length(bank) >=M)
        break
    }

  }
  bank
}
