#include <Rcpp.h>

#include "movea.h"

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
