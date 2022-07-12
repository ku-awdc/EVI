#' Function that produces the early warning based on cEVI
#'
#' Index = 1 when an early warning is issued and Index=0 when an early warning is not issued
#'
#' @param cevi numeric vector - object that corresponds to either a 0 for no an early warning or an 1 for an early warning based on cEVI.
#' @param cut threshold value (0 <= c <= 0.5) for issuing an early warning. If evi >= c an early warning is issued and otherwise is not.
#' @param cases numeric vector with the number of new cases per unit of time (i.e., daily).
#'
#' @examples
#' data("Italy")
#' cases = mova(cases=Italy$Cases, r_a=7)
#' ind=indic_cEVI(cevi=cev, cut=0.01, cases=cases)
#'
#' @export
indic_cEVI = function(cevi, cut, cases) {
  ind <- rep(NA,length(cevi))

  for (i in 3:length(cevi)) {
    if (!is.na(cevi[i]) && cevi[i] ==1 && (!is.na(cases[i]) & cases[i] > mean(cases[i:(i - min(7, i))]))){
      ind[i] <- 1
    }else{
      ind[i] <- 0
    }
  }
  return(ind)
}
