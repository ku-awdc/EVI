#include <Rcpp.h>

#include "movea.h"

// Potentially susceptible to catastrophic cancellation but very fast:
Rcpp::NumericVector movea(const Rcpp::NumericVector& cases, const int r_a)
{
  // TODO: re-implement using running mean algorithm to avoid catastrophic cancellation
  
  Rcpp::NumericVector ncases(cases.length());
  double sum = cases[0L];
  ncases[0L] = cases[0L];
  
  for(int i=1L; i<ncases.length(); ++i)
  {
    if(i >= r_a)
    {
      sum -= cases[i-r_a];
    }
    sum += cases[i];
    
    ncases[i] = sum / static_cast<double>((i < r_a) ? (i+1L) : r_a);
  }
  
  return ncases;
}


// Alternative implementation that is not susceptible to catastrophic cancellation:
/*
Rcpp::NumericVector movea(const Rcpp::NumericVector& cases, const int r_a)
{
  
  Rcpp::NumericVector ncases(cases.length());
  
  for(int i=0L; i<ncases.length(); ++i)
  {
    double rmu = 0.0;
    int n = 0L;
    for(int j=((i < (r_a-1L)) ? 0L : i-r_a+1L); j<=i; ++j)
    {
      if(j >= 0L) rmu -= (rmu - cases[j]) / static_cast<double>(++n);
    }
    ncases[i] = rmu;
  }
  
  return ncases;
}
*/
