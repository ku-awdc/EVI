#' Function that calculates the moving average of a series of data
#'
#' Example function and help file for mova.r
#' Moving Average
#' Calculate the moving average for the time series epidemic data
#'
#' @param cases time series data
#' @param r_a rolling window size/time interval on which the moving average will be calculated - number of consecutive observations per rolling window. Usually the 7-day moving average rather than the actually observed cases are analyzed
#'
#' @examples
#' data("sub_Italy")
#' 7-day moving average
#' mova(sub_Italy$ncases, r_a=7)
#' 14-day moving average
#' mova(sub_Italy$ncases, r_a=14)
#' @export
mova=function(cases, r_a=7){
  ncases=rep(NA, length(cases))
  for (i in 1:length(cases)){
    ncases[i]=mean(cases[((i+1)-min(r_a,i)):i])
  }
  return(ncases)
}


