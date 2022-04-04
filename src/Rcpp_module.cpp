#include <Rcpp.h>

#include "movea.h"

RCPP_MODULE(EVImodule){

	using namespace Rcpp;
	
	function("Rcpp_movea", &movea);

}
