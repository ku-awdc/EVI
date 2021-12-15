#' Epidemic Volatility Index calculation
#'
#' Calculates the relative change of standard deviation between two consecutive rolling windows

#' @param rollsd numeric vector - returned and stored as roll from the rollsd() function
#'
#'
#' @examples
#' data("sub_Italy")
#' cases = mova(sub_Italy$ncases)
#' roll = rollsd(cases)
#' ev = evi(roll)
#'
#' @export

evi = function(rollsd) {
  evi=rep(NA,length(rollsd))
  for (i in 2:length(rollsd)){
    evi[i]= (rollsd[i]-rollsd[i-1])/rollsd[i-1]
  }
  evi[is.nan(evi)]<-0
  return(evi)
}



