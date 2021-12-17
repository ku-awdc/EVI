#index calculation
indic_cEVI <- function(evi, cut, cases) {
  ind <- rep(NA,length(evi))
  for (i in 3:length(evi)) {
    if (evi[i] ==1 && cases[i] > mean(cases[i:(i - min(7, i))]))
    {ind[i] <- 1}
    else
    {ind[i] <- 0}
  }
  return(ind)
}
