criaRedesDinamc <- function(area){
  # area <- list()
  # area <- areas_raw[[1]]

  temp <- area[[length(area)]]
  nvar <- dim(temp)[2]
  size <- length(area)
  # CRIA O DF EXPANDIDO COM O N DE CORTES DE TEMPO DESEJADOS PARA A REDE DINÂMICA
  df_fold <- dbnR::fold_dt(temp, size)
  linhasdf <- dim(df_fold)[1]

  # PREENCHENDO DF EXPANDIDO COM VALORES DA FASES DA AREA
  inc <- 1
  for(fase in size:1){
    df_fold[,(((nvar*inc)-nvar)+1):(nvar*inc)] <-
      area[[fase]][1:linhasdf,]
    inc <- inc+1
  }
  # ADICIONANDO GRANDE RUIDO NAS COLUNAS COLHEITAS
  # PARA QUE NÃO AFETEM O APRENDIZADO DA REDE

  for(i in 2:(size)){
    df_fold[,nvar*i] <- df_fold[,nvar*i] +
      matrix(stats::rnorm(dim(df_fold)[1], 0, 200), ncol = 1)
  }

  ## SEPARAR EM TREINO E TESTE

  spliter <- array(0, dim(df_fold)[1])
  sampleDBN = caTools::sample.split(spliter, SplitRatio = 0.75)
  treinoDBN = subset(df_fold, sampleDBN == TRUE, )
  rownames(treinoDBN)<-NULL
  testeDBN  = subset(df_fold, sampleDBN == FALSE)
  rownames(testeDBN)<-NULL

  # APRENDENDO A DBN

  # dag_dbn <- dbnR::learn_dbn_struc(dt = treinoDBN,
  #                            size = size,
  #                            f_dt = treinoDBN,
  #                            method = "natPsoho")

  dag_dbn_2 <- dbnR::learn_dbn_struc(dt = NULL,
                               size = size,
                               f_dt = treinoDBN,
                               method = "dmmhc",
                               intra = F)

  # CASO EXISTA, REMOVER ARCOS PARTINDO DOS NÓS 'colheita_t'

  # # https://link.springer.com/chapter/10.1007/978-3-030-86271-8_14
  # for(i in 1:(size-1)){
  #    test <-  paste("colheita_t_", i, sep ="")
  #    if(length(which(bnlearn::arcs(dag_dbn)[,"from"] == test)) > 0){
  #      linhas <- which(bnlearn::arcs(dag_dbn)[,"from"] == test)
  #      bnlearn::arcs(dag_dbn) <- bnlearn::arcs(dag_dbn)[-linhas,]
  #    }
  # }
  #


  for(i in 1:(size-1)){
    test <-  paste("colheita_t_", i, sep ="")
    if(length(which(bnlearn::arcs(dag_dbn_2)[,"from"] == test)) > 0){
      linhas <- which(bnlearn::arcs(dag_dbn_2)[,"from"] == test)
      bnlearn::arcs(dag_dbn_2) <- bnlearn::arcs(dag_dbn_2)[-linhas,]
    }
  }
  #plot(dag_dbn)
  #geraIMGgraph(dag_dbn_2, file)
  # plot(dag_dbn_2)



  # TREINA A REDE
  # dag_dbn_fited <- dbnR::fit_dbn_params(dag_dbn, treinoDBN)
  dag_dbn_fited_2 <- dbnR::fit_dbn_params(dag_dbn_2, treinoDBN)

  # predict_dag_dbn_1 <- dbnR::forecast_ts(testeDBN, dag_dbn_fited,
  #                                 obj_vars = "colheita_t_0", rep = 1,
  #                                 mode = "approx", ini = 1, len = length(testeDBN),
  #                                 print_res = T, plot_res = F)

  predict_dag_dbn_2_a <- dbnR::forecast_ts(testeDBN, dag_dbn_fited_2,
                                     obj_vars = "colheita_t_0", rep = 1,
                                     mode = "approx", ini = 1, len = length(testeDBN),
                                     print_res = T, plot_res = F)

  predict_dag_dbn_2_e <- dbnR::forecast_ts(testeDBN, dag_dbn_fited_2,
                                     obj_vars = "colheita_t_0", rep = 1,
                                     mode = "exact", ini = 1, len = length(testeDBN),
                                     print_res = T, plot_res = F)

  # return(predict_dag_dbn_1)
  return(dag_dbn_2)
  # return(c(predict_dag_dbn_1, predict_dag_dbn_2))
}
