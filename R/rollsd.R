#' Rolling standard deviation
#'
#' -A function for computing the rolling standard deviation (between two sds) for the time series epidemic data
#' @param cases numeric vector - number of cases per day (or any other time interval)
#' @param lag_t integer - number of consecutive rolling windows (minimum/default value = 7, maximum recommended value = 30)
#'
#' @examples
#' data("sub_Italy")
#' cases = mova(sub_Italy$ncases, r_a=7)
#' roll = rollsd(cases,lag_t)
#'
#'
#' @export
rollsd = function(cases, lag_t) {
  rollsd=rep(NA,length(cases))
  for (i in 1:lag_t){
    rollsd[i]=medvol(cases[1:i])
  }
  for (i in (lag_t+1):length(cases)){
    rollsd[i]=medvol(cases[(i-(lag_t-1)):i])
  }
  return(rollsd)
}

