#'  Cases of the first 150 Covid-19 days of Italy.
#'
#' Dataframe containing the first 150 days of Covid-19 pandemic 
#' in Italy and the EVI analysis
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{dates}{ID of date}
#'   \item{Days}{Days from beginning of Covid-19 epidemic}
#'   \item{ncases}{Number of observed cases}
#'   \item{cum_ncases}{Cumulative number of observed cases}
#'   \item{EVI}{Epidemic Volatility Index}
#'   \item{Cases}{Moving average of number of cases}
#'   \item{Index}{Positive or negative warning} 
#'   \item{pvs}{Cumulative predictive value positive  of algorithm}
#'   \item{pvn}{Cumulative predictive value negative  of algorithm}
#'   \item{lag_all}{Current optimal lag at time (Days)}
#'   \item{c_all}{Current optimal cut of point at time (Days)}
#'   \item{se_all}{Cumulative sensitivity of algorithm}
#'   \item{sp_all}{Cumulative specificity of algorithm}
#'   \item{name}{Name of the Country}
#' }
#' @source \url{https://github.com/CSSEGISandData/COVID-19}
"sub_Italy"