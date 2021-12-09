#'  Indicator function
#'  ### The 0 and 1 outcomes from the status and indic function are used to create the 2x2 table - see pdf file
#' Returns the EVI index/model prediction - 1/0

#' @param evi numeric vector (obtained from the evi function)
#' @param cut threshold value 0<=c<=1 (to get a positive result)
#' @param cases number of new cases per day (or any other time interval)
#'
#' @examples
#' evi: as obtained from before
#' cut <- 0.01
#' cases <- rbinom(100,10,0.5)
#' indic(evi,cut,cases)
#' NA NA  0  0  0  0  0  1  0  0  …
#'
#'
#' @export
#' ind
indic = function(evi, cut, cases) {
  ind=rep(NA,length(evi))
  for (i in 3:length(evi)){
    if (evi[i]>=cut && cases[i]>mean(cases[i:(i-min(7,i))]))
    {ind[i]=1}
    else
    {ind[i]=0}
  }
  return(ind)
}
