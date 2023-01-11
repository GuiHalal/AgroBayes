
#'
#' TEST FUNCTION - test data generation function
#'
#' @keywords internal
#' @export
test_datagen <- function(){

  nHarvests = 1000
  nphases = 5
  nAreas = 6
  nVars = 3
  nClass = 5

  areas <- testRunDataGen(nHarvests, nphases, nAreas, nVars, nClass)
  return(areas)
}

#'
#' TEST FUNCTION - tests generation of static Bayesian networks
#'
#' @keywords internal
#' @export

test_createBN <- function(){
  nHarvests = 1000
  nphases = 5
  nAreas = 6
  nVars = 3
  nClass = 5

  areas <- testRunDataGen(nHarvests, nphases, nAreas, nVars, nClass)
  areas_raw <- areas[1:(length(areas)/2)]
  areas_dis <- areas[((length(areas)/2)+1):length(areas)]
  area1 <- testRunNetworks(areas_dis[[1]], 1)
  return(area1)
}

#'
#' TEST FUNCTION - tests generation of static Dynamic Bayesian networks
#'
#' @keywords internal
#' @export

test_createDBN <- function(){
  nHarvests = 1000
  nphases = 5
  nAreas = 6
  nVars = 3
  nClass = 5

  areas <- testRunDataGen(nHarvests, nphases, nAreas, nVars, nClass)
  areas_raw <- areas[1:(length(areas)/2)]
  areas_dis <- areas[((length(areas)/2)+1):length(areas)]
  area1 <- createDbn(areas_raw[[1]])
  return(area1)
}
