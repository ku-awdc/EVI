#'  Function that produces an indicator index
#'
#'  ### The 0 and 1 outcomes from the status and indic function are used to create the 2x2 table
#' Returns the EVI index/model prediction - 1/0

#' @param evi numeric vector - object (obtained from the evi function and stored as ev)
#' @param cut threshold value 0<=c<=1 - expetation in the future number of cases
#' @param cases moving average for the time series epidemic data - obtained and stored as cases from the mova function
#'
#' @examples
#' data("Italy")
#' cases = mova(Italy$Cases$ncases)
#' roll = rollsd(cases)
#' ev = evi(roll)
#' ind=indic(ev, 0.01, cases)
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
