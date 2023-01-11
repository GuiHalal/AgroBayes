#'
#' Allows normalization of generated data.
#'
#' Allows normalization of data using the $$(x−min)/(max−min)$$ equation.
#'
#' @param x the set of data to be normalized
#' @return set of data normalized
#' @examples
#' \donttest{
#' normalizerData = f_minmax(notNormalizerData)
#' }
#' @keywords internal
#' @export
#'
f_minmax <- function(x){
  minmax = (max(x)-min(x))
  if(minmax==0){
    return(min(x))
  }
  return((x - min(x))/minmax)
}

