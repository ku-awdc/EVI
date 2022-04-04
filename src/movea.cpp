#include <Rcpp.h>

#include "movea.h"

// Reasonably fast implementation that is robust to catastrophic cancellation:
Rcpp::NumericVector movea(const Rcpp::NumericVector& cases, const int r_a)
{
  Rcpp::NumericVector ncases(cases.length());
  double rmu = cases[0L];
  ncases[0L] = cases[0L];
  
  for(int i=1L; i<ncases.length(); ++i)
  {
    if(i >= r_a)
    {
      rmu += (rmu - cases[i-r_a]) / static_cast<double>(r_a-1L);
      rmu -= (rmu - cases[i]) / static_cast<double>(r_a);
    }
    else
    {
      rmu -= (rmu - cases[i]) / static_cast<double>(i+1L);
    }
    
    ncases[i] = rmu;
  }
  
  return ncases;
}


// Alternative faster implementation that is potentially susceptible to catastrophic cancellation:
// Note: this would likely be even faster if cases and sum were int
/*
Rcpp::NumericVector movea(const Rcpp::NumericVector& cases, const int r_a)
{
  
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
*/


// Alternative slower implementation:
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
