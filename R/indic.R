#'  Function that produces the early warning
#'
#' Index = 1 when an ealy warning warning is issued and Index=0 when when an ealy warning warning is not issued

#' @param evi numeric vector - object (obtained from the evi function and stored as ev) that corresponds to the relative change in the standard deviation
#' @param cut threshold value (0<=c<=0.5) for issuing an early warning. If evi >= c an early warning is issued and otherwise is not.
#' @param cases numeric vector with a number of cases per unit of time (i.e., daily)
#'
#' @examples
#' data("Italy")
#' cases = mova(cases=Italy$Cases, r_a=7)
#' roll = rollsd(cases=cases, lag_t=7)
#' ev = evi(rollsd=roll)
#' ind=indic(evi=ev, cut=0.01, cases=cases)
#'
#' @export
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
