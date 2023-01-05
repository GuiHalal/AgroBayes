criaRedes <- function (areaphase, areatype){

  sample = sample.split(areaphase, SplitRatio = 0.75)
  treino = subset(areaphase, sample == TRUE, )
  rownames(treino)<-NULL
  teste  = subset(areaphase, sample == FALSE)
  rownames(teste)<-NULL


  blacklist = data.frame(from = c("X_1","X_1",
                                  "X_2", "X_2",
                                  "X_3", "X_3",
                                  "colheita",  "colheita",  "colheita" ),
                         to = c("X_2", "X_3", #from v1
                                "X_1", "X_3", #from v2
                                "X_1", "X_2", #from v3
                                "X_1", "X_2", "X_3")) #from col

  # constroi whitelist conforme tipo de área
  if(areatype == 1){
    whitelist = data.frame(from = c("X_1", "X_2", "X_3"),
                           to = c("colheita", "colheita", "colheita"))
  }else if(areatype == 2){
    whitelist = data.frame(from = c("X_1"),
                           to = c("colheita"))
  }else if(areatype == 3){
    whitelist = data.frame(from = c("X_3"),
                           to = c("colheita"))
  }else if(areatype == 4){
    whitelist = data.frame(from = c("X_1", "X_3"),
                           to = c("colheita", "colheita"))
  }else if(areatype == 5){
    whitelist = data.frame(from = c("X_2"),
                           to = c("colheita"))
  }else if(areatype == 6){
    whitelist = data.frame(from = c("X_1"),
                           to = c("colheita"))
  }



  # hc_dag =      Hill-Climbing COM definicao da topologia da rede
  # hc_dag_raw =  Hill-Climbing SEM definicao da topologia da rede
  # mmhc_dag =      Max-Min Hill-Climbing COM definicao da topologia da rede
  # mmhc_dag_raw =  Max-Min Hill-Climbing SEM definicao da topologia da rede

  # Score-based Learning Algorithm

  hc_dag <-hc(treino,
              whitelist = whitelist,
              blacklist = blacklist,
              debug = FALSE)
  plot(hc_dag)

  hc_dag_raw <-hc(treino, debug = FALSE)
  plot(hc_dag_raw)


  # Hybrid Learning Algorithm

  mmhc_dag <- mmhc(treino,
                   whitelist = whitelist,
                   blacklist = blacklist,
                   debug = FALSE)
  plot(mmhc_dag)

  mmhc_dag_raw <- mmhc(treino, debug = FALSE)
  plot(mmhc_dag_raw)

  # treina redes
  hc_dag_fitted = bn.fit(hc_dag, treino)
  hc_dag_raw_fitted = bn.fit(hc_dag_raw, treino)

  mmhc_dag_fitted = bn.fit(mmhc_dag, treino)
  mmhc_dag_raw_fitted = bn.fit(mmhc_dag_raw, treino)


  # validacao das redes
  return(validaRede(teste, treino, hc_dag_fitted, hc_dag_raw_fitted, mmhc_dag_fitted, mmhc_dag_raw_fitted))
}

rodaRede <- function(areadf, areatype){
  area <- list()
  out <- data.frame()
  nome <- array()
  inome=1
  for(fase in 1:length(areadf)){
    area[[fase]] <- criaRedes(areadf[[fase]], areatype)

    out <- bind_rows(out, area[[fase]][3])
    nome[inome] <- paste("fase", fase, "hc_dag")
    inome = inome+1

    out <- bind_rows(out, area[[fase]][6])
    nome[inome] <- paste("fase", fase, "hc_dag_raw")
    inome = inome+1

    out <- bind_rows(out, area[[fase]][9])
    nome[inome] <- paste("fase", fase, "mmhc_dag")
    inome = inome+1

    out <- bind_rows(out, area[[fase]][12])
    nome[inome] <- paste("fase", fase, "mmhc_dag_raw")
    inome = inome+1
  }
  row.names(out$eval)<-nome
  return(out$eval)
}
