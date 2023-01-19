#' Creates dynamic Bayesian networks for performance evaluation
#'
#' Using harvest data from all phenological phases of the cultivar,
#' from a specific area of the plantation, dynamic Bayesian networks
#' are generated (using the \code{ dbnR::learn_dbn_struc} functions with
#' \code{natPsoho and dmmhc} methods),
#' trained (using the \code{dbnR::fit_dbn_params} function)
#' and evaluated for performance (using \code{dbnR::forecast_ts} and
#' \code{ caret::defaultSummary}).
#' Two networks are created, one using \code{natPsoho} learning method and
#' other using \code{dmmhc} method.
#' The DBN are tested using \code{dbnR::forecast_ts} with \code{exact}
#' and \code{approx} methods then, from the return of these functions,
#' the RMSE, R-squared and MAE metrics are calculated and returned
#'
#' @param area A dataframe of continuous data, from a specific area
#' of the plantation to be used
#' @return A dataframe with the dynamics networks metrics (RMSE, R-squared, MAE)
#' @examples
#' \donttest{
#' metricsDbn = createDbn(area_1)
#' }
#' @export
#'
createDbn <- function(area){
  #area <- areas_raw[[1]]
  temp <- area[[length(area)]]
  nvar <- dim(temp)[2]
  size <- length(area)
  # Create the expanded df with the n of desired time-cuttings for the D-network
  df_fold <- dbnR::fold_dt(temp, size)
  rowsdf <- dim(df_fold)[1]

  # Filling expanded df with area phase values
  inc <- 1
  for(fase in size:1){
    df_fold[,(((nvar*inc)-nvar)+1):(nvar*inc)] <-
      area[[fase]][1:rowsdf,]
    inc <- inc+1
  }
  # Adding big noise to 'harvest' columns so they do not affect network learning

  for(i in 2:(size)){
    df_fold[,nvar*i] <- df_fold[,nvar*i] +
      matrix(stats::rnorm(dim(df_fold)[1], 0, 200), ncol = 1)
  }

  ## Separating into training and test

  spliter <- array(0, dim(df_fold)[1])
  sampleDBN = caTools::sample.split(spliter, SplitRatio = 0.75)
  trainingDBN = subset(df_fold, sampleDBN == TRUE, )
  rownames(trainingDBN)<-NULL
  testDBN  = subset(df_fold, sampleDBN == FALSE)
  rownames(testDBN)<-NULL

  # Learning the dbn

  dag_natPsoho <- dbnR::learn_dbn_struc(dt = trainingDBN,
                                        size = size,
                                        f_dt = trainingDBN,
                                        method = "natPsoho")

  dag_dmmhc <- dbnR::learn_dbn_struc(dt = NULL,
                                     size = size,
                                     f_dt = trainingDBN,
                                     method = "dmmhc",
                                     intra = F)

  # If existing, remove arcs from nodes 'harvest_t'
  for(i in 1:(size-1)){
    testing <-  paste("harvest_t_", i, sep ="")
    if(length(which(bnlearn::arcs(dag_natPsoho)[,"from"] == testing)) > 0){
      rows <- which(bnlearn::arcs(dag_natPsoho)[,"from"] == testing)
      bnlearn::arcs(dag_natPsoho) <- bnlearn::arcs(dag_natPsoho)[-rows,]
    }
  }

  for(i in 1:(size-1)){
    testing <-  paste("harvest_t_", i, sep ="")
    if(length(which(bnlearn::arcs(dag_dmmhc)[,"from"] == testing)) > 0){
      rows <- which(bnlearn::arcs(dag_dmmhc)[,"from"] == testing)
      bnlearn::arcs(dag_dmmhc) <- bnlearn::arcs(dag_dmmhc)[-rows,]
    }
  }

  # plot(dag_natPsoho)
  # plot(dag_dmmhc)


  # Training the network
  dag_natPsoho_fited <- dbnR::fit_dbn_params(dag_natPsoho, trainingDBN)

  dag_dmmhc_fited <- dbnR::fit_dbn_params(dag_dmmhc, trainingDBN)

  # dag_natPsoho
  predict_dag_natPsoho_approx <- dbnR::forecast_ts(testDBN, dag_natPsoho_fited,
                                                   obj_vars = "harvest_t_0",
                                                   rep = 1,mode = "approx",
                                                   ini = 1, len = length(testDBN),
                                                   print_res = F, plot_res = F)

  predict_dag_natPsoho_exact <- dbnR::forecast_ts(testDBN, dag_natPsoho_fited,
                                                  obj_vars = "harvest_t_0",
                                                  rep = 1, mode = "exact",
                                                  ini = 1, len = length(testDBN),
                                                  print_res = F, plot_res = F)
  #dag_dmmhc
  predict_dag_dmmhc_approx <- dbnR::forecast_ts(testDBN, dag_dmmhc_fited,
                                                obj_vars = "harvest_t_0",
                                                rep = 1, mode = "approx",
                                                ini = 1, len = length(testDBN),
                                                print_res = F, plot_res = F)

  predict_dag_dmmhc_exact <- dbnR::forecast_ts(testDBN, dag_dmmhc_fited,
                                               obj_vars = "harvest_t_0",
                                               rep = 1, mode = "exact",
                                               ini = 1, len = length(testDBN),
                                               print_res = F, plot_res = F)


  metrics_df_natPsoho_approx =
    data.frame(obs = predict_dag_natPsoho_approx$orig$harvest_t_0,
               pred = predict_dag_natPsoho_approx$pred$harvest_t_0)

  metrics_df_natPsoho_exact =
    data.frame(obs = predict_dag_natPsoho_exact$orig$harvest_t_0,
               pred = predict_dag_natPsoho_exact$pred$harvest_t_0)

  metrics_df_dmmhc_approx =
    data.frame(obs = predict_dag_dmmhc_approx$orig$harvest_t_0,
               pred = predict_dag_dmmhc_approx$pred$harvest_t_0)

  metrics_df_dmmhc_exact =
    data.frame(obs = predict_dag_dmmhc_exact$orig$harvest_t_0,
               pred = predict_dag_dmmhc_exact$pred$harvest_t_0)

  natPsoho_approx =  c(caret::defaultSummary(metrics_df_natPsoho_approx),
                       accurCalc(metrics_df_natPsoho_approx))

  natPsoho_exact =  c(caret::defaultSummary(metrics_df_natPsoho_exact),
                      accurCalc(metrics_df_natPsoho_exact))

  dmmhc_approx = c(caret::defaultSummary(metrics_df_dmmhc_approx),
                   accurCalc(metrics_df_dmmhc_approx))

  dmmhc_exact = c(caret::defaultSummary(metrics_df_dmmhc_exact),
                  accurCalc(metrics_df_dmmhc_exact))

  metrics_dags = data.frame(cbind(natPsoho_approx,
                                  dmmhc_approx,
                                  natPsoho_exact,
                                  dmmhc_exact))

  return(metrics_dags)
}
