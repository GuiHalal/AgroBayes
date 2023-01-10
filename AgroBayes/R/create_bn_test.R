#'Creates, from a dataframes, four Bayesian networks for performance evaluation
#'
#'From n crops data of a phenological phase in a expecific area in the cropfield,
#'four
#'A partir de dados de colheitas de uma fase fenológica da cultivar, de uma área
#'especícica da plantação, são geradas  (utilizando as funções  bnlearn::hc e
#'bnlearn::mmhc), treinadas (utilizando bnlearn::bn.fit) e
#'avaliadas quanto a performance (validaRede()) quatro redes bayesianas
#'
#' @param areaphase the data.frame to be used
#' @param areatype integer to identify which type of area the dataframe belongs to

createNetworksTest <- function (areaphase, areatype){

  sample = caTools::sample.split(areaphase, SplitRatio = 0.75)
  training = subset(areaphase, sample == TRUE, )
  rownames(training)<-NULL
  test  = subset(areaphase, sample == FALSE)
  rownames(test)<-NULL


  blacklist = data.frame(from = c("X_1","X_1",
                                  "X_2", "X_2",
                                  "X_3", "X_3",
                                  "harvest",  "harvest",  "harvest" ),
                         to = c("X_2", "X_3", #from v1
                                "X_1", "X_3", #from v2
                                "X_1", "X_2", #from v3
                                "X_1", "X_2", "X_3")) #from col

  # builds whitelist according to area type
  if(areatype == 1){
    whitelist = data.frame(from = c("X_1", "X_2", "X_3"),
                           to = c("harvest", "harvest", "harvest"))
  }else if(areatype == 2){
    whitelist = data.frame(from = c("X_1"),
                           to = c("harvest"))
  }else if(areatype == 3){
    whitelist = data.frame(from = c("X_3"),
                           to = c("harvest"))
  }else if(areatype == 4){
    whitelist = data.frame(from = c("X_1", "X_3"),
                           to = c("harvest", "harvest"))
  }else if(areatype == 5){
    whitelist = data.frame(from = c("X_2"),
                           to = c("harvest"))
  }else if(areatype == 6){
    whitelist = data.frame(from = c("X_1"),
                           to = c("harvest"))
  }



  # hc_dag =      Hill-Climbing with network topology definition
  # hc_dag_raw =  Hill-Climbing without defining the network topology
  # mmhc_dag =    Max-Min Hill-Climbing with network topology definition
  # mmhc_dag_raw = Max-Min Hill-Climbing without defining the network topology

  # Score-based Learning Algorithm

  hc_dag <- bnlearn::hc(training,
              whitelist = whitelist,
              blacklist = blacklist,
              debug = FALSE)
  graphics::plot(hc_dag)

  hc_dag_raw <-bnlearn::hc(training, debug = FALSE)
  graphics::plot(hc_dag_raw)


  # Hybrid Learning Algorithm

  mmhc_dag <- bnlearn::mmhc(training,
                   whitelist = whitelist,
                   blacklist = blacklist,
                   debug = FALSE)
  graphics::plot(mmhc_dag)

  mmhc_dag_raw <- bnlearn::mmhc(training, debug = FALSE)
  graphics::plot(mmhc_dag_raw)

  # train networks
  hc_dag_fitted = bnlearn::bn.fit(hc_dag, training)
  hc_dag_raw_fitted = bnlearn::bn.fit(hc_dag_raw, training)

  mmhc_dag_fitted = bnlearn::bn.fit(mmhc_dag, training)
  mmhc_dag_raw_fitted = bnlearn::bn.fit(mmhc_dag_raw, training)


  # validation of networks
  return(validateNetworkTest(test, training, hc_dag_fitted,
                             hc_dag_raw_fitted, mmhc_dag_fitted,
                             mmhc_dag_raw_fitted))
}

runNetworksTest <- function(areadf, areatype){
  area <- list()
  out <- data.frame()
  name <- array()
  iname=1
  for(phase in 1:length(areadf)){
    area[[phase]] <- createNetworksTest(areadf[[phase]], areatype)

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

validateNetworkTest <- function(test, train, dag_fitted1, dag_fitted2, dag_fitted3, dag_fitted4) {
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
