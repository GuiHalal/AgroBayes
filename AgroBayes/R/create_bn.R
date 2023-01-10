runNetworks <- function(areadf, areatype){
  area <- list()
  out <- data.frame()
  name <- array()
  iname=1
  for(phase in 1:length(areadf)){
    area[[phase]] <- createNetworks(areadf[[phase]], areatype)

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

#' createNetworks
#' #' Creates, from a dataframes, four Bayesian networks for performance evaluation
#'
#' Using harvest data from a phenological phase of the cultivar,
#' from a specific area of the plantation, Bayesian network are generated
#' (using the bnlearn::hc and bnlearn::mmhc functions),
#' trained (using the bnlearn::bn.fit function) and evaluated for performance.
#' Four networks are created, two from the pre-established topology and
#' two learned only from the presented data.
#' @param areaphase the data.frame to be used
#' @param blacklist a dataframe of character string with two columns,
#' it is passed as a parameter to bnlearn learn functions in order to
#' avoid these arcs composing the final network
#' @param whitelist a dataframe of character string with two columns,
#' it is passed as a parameter to bnlearn learn functions in order to
#' guarantee these arcs composing the final network
#' @return Network evaluation metrics,
#' as calculated in the \code{validateNetwork} function
#' @keywords internal
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
