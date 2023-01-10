#' Executes \code{\link{createNetworks}} function to an area
#'
#' Executes the \code{\link{createNetworks}} function for all phenological
#' phases of an area. It also organizes the generated metrics in a dataframe.
#'
#' @param arealist list os dataframes of all phenological phase on
#' a specific area of the plantation
#' @param blacklist a dataframe of character string with two columns,
#' it is passed as a parameter to bnlearn learn functions in order to
#' avoid these arcs composing the final network
#' @param whitelist a dataframe of character string with two columns,
#' it is passed as a parameter to bnlearn learn functions in order to
#' guarantee these arcs composing the final network
#' @return A dataframe organizing the network metrics generated in the
#' \code{\link{createNetworks}} function. The lines represent the network
#' performance learned in each phenological phase of the area (\code{arealist})
#' @examples
#' \donttest{
#' blacklist = data.frame(
#' from = c("X_1","X_1",
#'          "X_2", "X_2",
#'          "X_3", "X_3",
#'          "harvest",  "harvest",  "harvest" ),
#' to = c("X_2", "X_3", #from v1
#'        "X_1", "X_3", #from v2
#'        "X_1", "X_2", #from v3
#'        "X_1", "X_2", "X_3")) #from harvest
#' whitelist = data.frame(
#' from = c("X_1", "X_2", "X_3"),
#' to = c("harvest", "harvest", "harvest"))
#' arealist <- list(area1)
#' metricsArea1 <- createNetworks (arealist, blacklist, whitelist)
#' }
#' @export
#'
runNetworks <- function(arealist, blacklist, whitelist){
  area <- list()
  out <- data.frame()
  name <- array()
  iname=1
  for(phase in 1:length(arealist)){
    area[[phase]] <- createNetworks(arealist[[phase]], blacklist, whitelist)

    out <- dplyr::bind_rows(out, area[[phase]][3])
    name[iname] <- paste("phase", phase, "hc_dag")
    iname = iname+1

    out <- dplyr::bind_rows(out, area[[phase]][6])
    name[iname] <- paste("phase", phase, "hc_dag_raw")
    iname = iname+1

    out <- dplyr::bind_rows(out, area[[phase]][9])
    name[iname] <- paste("phase", phase, "mmhc_dag")
    iname = iname+1

    out <- dplyr::bind_rows(out, area[[phase]][12])
    name[iname] <- paste("phase", phase, "mmhc_dag_raw")
    iname = iname+1
  }
  row.names(out$eval)<-name
  return(out$eval)
}

#'
#' Creates Bayesian networks for performance evaluation
#'
#' Using harvest data from a phenological phase of the cultivar,
#' from a specific area of the plantation, Bayesian network are generated
#' (using the \code{bnlearn::hc} and \code{bnlearn::mmhc} functions),
#' trained (using the \code{bnlearn::bn.fit} function)
#' and evaluated for performance (using \code{\link{validateNetwork}}).
#' Four networks are created, two from the pre-established topology and
#' two learned only from the presented data.
#'
#' @param areaphase the dataframe of discretized data to be used
#' @param blacklist a dataframe of character string with two columns,
#' it is passed as a parameter to bnlearn learn functions in order to
#' avoid these arcs composing the final network
#' @param whitelist a dataframe of character string with two columns,
#' it is passed as a parameter to bnlearn learn functions in order to
#' guarantee these arcs composing the final network
#' @return Network evaluation metrics,
#' as calculated in the \code{validateNetwork} function
#' @examples
#' \donttest{
#' metricsArea1Phase1 = list()
#' blacklist = data.frame(
#' from = c("X_1","X_1",
#'          "X_2", "X_2",
#'          "X_3", "X_3",
#'          "harvest",  "harvest",  "harvest" ),
#' to = c("X_2", "X_3", #from v1
#'        "X_1", "X_3", #from v2
#'        "X_1", "X_2", #from v3
#'        "X_1", "X_2", "X_3")) #from col
#' whitelist = data.frame(
#' from = c("X_1", "X_2", "X_3"),
#' to = c("harvest", "harvest", "harvest"))
#' areaphase = data.frame(area1_phase_1)
#' metricsArea1Phase1 = createNetworks (areaphase, blacklist, whitelist)
#' }
#' @export

createNetworks <- function (areaphase, blacklist, whitelist){

  sample = caTools::sample.split(areaphase, SplitRatio = 0.75)
  training = subset(areaphase, sample == TRUE, )
  rownames(training)<-NULL
  test  = subset(areaphase, sample == FALSE)
  rownames(test)<-NULL

  # hc_dag =      Hill-Climbing with network topology definition
  # hc_dag_raw =  Hill-Climbing without defining the network topology
  # mmhc_dag =    Max-Min Hill-Climbing with network topology definition
  # mmhc_dag_raw = Max-Min Hill-Climbing without defining the network topology

  # Score-based Learning Algorithm

  hc_dag <- bnlearn::hc(training,
              whitelist = whitelist,
              blacklist = blacklist,
              debug = FALSE)
  #plot(hc_dag)

  hc_dag_raw <- bnlearn::hc(training, debug = FALSE)
  #plot(hc_dag_raw)


  # Hybrid Learning Algorithm

  mmhc_dag <- bnlearn::mmhc(training,
                   whitelist = whitelist,
                   blacklist = blacklist,
                   debug = FALSE)
  #plot(mmhc_dag)

  mmhc_dag_raw <- bnlearn::mmhc(training, debug = FALSE)
  #plot(mmhc_dag_raw)

  # train networks
  hc_dag_fitted = bnlearn::bn.fit(hc_dag, training)
  hc_dag_raw_fitted = bnlearn::bn.fit(hc_dag_raw, training)

  mmhc_dag_fitted = bnlearn::bn.fit(mmhc_dag, training)
  mmhc_dag_raw_fitted = bnlearn::bn.fit(mmhc_dag_raw, training)


  # validation of networks
  return(validateNetwork(test, training, hc_dag_fitted,
                         hc_dag_raw_fitted, mmhc_dag_fitted,
                         mmhc_dag_raw_fitted))
}

#'
#' Generates Bayesian networks performance evaluation
#'
#' Using the functions available in the repository
#' \url{https://github.com/KaikeWesleyReis/bnlearn-multivar-prediction-metrics}
#' calculates the metrics of the four Bayesian networks generated in
#' the \code{\link{createNetworks}} function when executed in context
#' of \code{\link{runNetworks}} function.
#'
#' @param test dataframe to be used to test the Bayesian networks.
#' It is composed of a 25% portion of the original dataframe presented to
#' the \code{\link{createNetworks}} function.
#' @param train dataframe to be used to train the Bayesian networks.
#' It is composed of a 75% portion of the original dataframe presented to
#' the \code{\link{createNetworks}} function.
#'
#' @param dag_fitted1 Fitted Bayesian network to be tested
#' @param dag_fitted2 Fitted Bayesian network to be tested
#' @param dag_fitted3 Fitted Bayesian network to be tested
#' @param dag_fitted4 Fitted Bayesian network to be tested
#'
#' @return List of values returned from \code{bnMetricsMultiVarPrediction}.
#' See more in \url{https://github.com/KaikeWesleyReis/bnlearn-multivar-prediction-metrics#bnmetricsmultivarprediction}
#' @export
#'

validateNetwork <- function(test, train, dag_fitted1, dag_fitted2, dag_fitted3, dag_fitted4) {
  # Define Target variables (Variables to be predicted)
  pred <-'harvest'
  # Evidence variables
  #(Variables that you will give information to the BN to do the prediction)
  evid <- names(train)[!names(train) %in% pred]

  results1 <- bnMultiVarPrediction(bnFit = dag_fitted1,
                                   trainSet = train,
                                   testSet = test,
                                   to_predict = pred,
                                   to_evidence = evid,
                                   calcFunction = 'predict')

  results2 <- bnMultiVarPrediction(bnFit = dag_fitted2,
                                   trainSet = train,
                                   testSet = test,
                                   to_predict = pred,
                                   to_evidence = evid,
                                   calcFunction = 'predict')

  results3 <- bnMultiVarPrediction(bnFit = dag_fitted3,
                                   trainSet = train,
                                   testSet = test,
                                   to_predict = pred,
                                   to_evidence = evid,
                                   calcFunction = 'predict')

  results4 <- bnMultiVarPrediction(bnFit = dag_fitted4,
                                   trainSet = train,
                                   testSet = test,
                                   to_predict = pred,
                                   to_evidence = evid,
                                   calcFunction = 'predict')
  # Metrics Evaluation

  metrics1 <- bnMetricsMultiVarPrediction(reference = test[pred],
                                          prediction = results1$dominantList,
                                          predProbList = results1$probList)
  metrics2 <- bnMetricsMultiVarPrediction(reference = test[pred],
                                          prediction = results2$dominantList,
                                          predProbList = results2$probList)

  metrics3 <- bnMetricsMultiVarPrediction(reference = test[pred],
                                          prediction = results3$dominantList,
                                          predProbList = results3$probList)
  metrics4 <- bnMetricsMultiVarPrediction(reference = test[pred],
                                          prediction = results4$dominantList,
                                          predProbList = results4$probList)

  # cat("Confusion matrix dag 1: ", metrics1$cmList)

  return(c(metrics1, metrics2, metrics3, metrics4))
}
