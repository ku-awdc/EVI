#' Example function and help file
#'
#' @param cases time series data
#' @param int rolling window
#'
#' @examples
#' cases=c(2,2,1,4,9,10,23,10,9,10,14,12,10)
#' mova_cases=mova(cases, 7)
#'
#' @export
mova=function(cases, int=7){
  ncases=rep(NA, length(cases))
  for (i in 1:length(cases)){
    ncases[i]=mean(cases[((i+1)-min(int,i)):i])
  }
  return(ncases)
}

