BuildSimulationData <- function (nHarvests, nPhases, nAreas = NULL, ...) {


  areasList <- list()
  if(is.null(nAreas)){
    # por padrão gera-se 6 áreas
    # cada área tem uma relação entre as variáveis e a produção

    areanames = c("Area_1", "Area_2", "Area_3", "Area_4", "Area_5", "Area_6")
    areatype  = c(1,2,3,4,5,6)
    for (i in 1 : length(areanames)){
      areasList[[i]] <- c(areanames =  areanames[i], areatype  = areatype[i])
    }
  }else{
    # se for definido o número de áreas, os tipos são atribuidos aleatóriamente
    for (i in 1 : nAreas){
      tempType = sample(1:6, 1)
      #tempType = 5
      tempName <- paste("Area", i, sep = "_")
      areasList[[i]] = c(areaName = tempName, areatype = tempType)
    }
  }

  # definição das variáveis pela funcao defVars
  defProdVariables = defVars(...)

  completeSimValues = list()
  namesArea = array(dim = length(areasList))

  # Para cada área Calcula os valores das variáveis
  # e da producao pela função setSimVarValues

  for (a in 1:length(areasList)) {

    #cat("Calculando valores da área ",a,"\n")

    completeSimValues[[a]] = setSimVarValues(nHarvests, areasList[[a]][2],defProdVariables,nPhases)
    namesArea[a] = paste("Area", a, "type", areasList[[a]][2], sep = "_")
  }
  names(completeSimValues) = namesArea

  return (completeSimValues)
}

defVars <- function (n_var = NULL, type_var = NULL, name_var = NULL){
  prodVariables = list()

  # Se não for definido o número de variáveis o padrão são as 3 abaixo
  if(is.null(n_var)){
    # Definição das variáveis: nome, valor mínimo, valor máximo, tipo
    # type (up=1,osc=2,const=3)
    prodVariables[[1]] = c("PrecAcum",0,1,1)
    prodVariables[[2]] = c("Insol",0,1,2)
    prodVariables[[3]] = c("Compact",-1,1,3)

    return (prodVariables)
  }else
    if(!is.null(type_var) | !is.null(name_var)){
      # sendo informado o n de variáveis e o nome e o comportamento delas
      # as variáveis são geradas conforme abaixo:
      if(length(type_var)!= n_var | length(name_var)!= n_var){
        stop("parâmetros incorretos. Se informados, tipos e nomes de variáveis devem ser informados para todas as variáveis")
      }else{
        for (i in 1 : n_var){
          # type (up=1,osc=2,const=3)
          if(type_var[i] == 3){
            prodVariables[[i]] = c(name_var[i], -1, 1, type_var[i])
          }else{
            prodVariables[[i]] = c(name_var[i], 0, 1, type_var[i])
          }
        }
      }
      return (prodVariables)
    }else{
      # Sendo informado o n de variáveis, sem mais detalhes a geração
      # ocorre de maneira aleatória, conforme abaixo:
      for (i in 1 : n_var){
        tempType = sample(1:3, 1)
        tempName <- paste("x", i, sep = "_" )
        if(tempType == 3){
          prodVariables[[i]] = c(tempName, -1, 1, tempType)
        }else{
          prodVariables[[i]] = c(tempName, 0, 1, tempType)
        }
      }
      return (prodVariables)
    }
}

setSimVarValues <- function (harvests, areatype, prodvars, nPhases){
  colheitas <-list()
  names_colheitas <- array(dim = harvests)

  # define os nomes das variáveis
  for(i in 1: harvests){
    # pra cada colheita da área
    # cria uma matriz de 'numero de fases' linhas e
    # 'numero de variáveis' colunas
    # (sendo a v pridução nvariaveis independente +1)
    # aqui eu usei matrizes pq é mais fácil fazer operações matemáticas
    # nas matrizes
    fases <- matrix(nrow=nPhases, ncol= length(prodvars)+1)
    colnames(fases) <-  colnames(fases, do.NULL = FALSE, prefix = "X_")
    colnames(fases)[length(prodvars)+1] <-  "colheita"
    rownames(fases) <-  rownames(fases, do.NULL = FALSE, prefix = "fase_")
    colheitas[[i]] = fases
    names_colheitas[i] = paste("colheita", i, sep = "_")
  }
  names(colheitas) = names_colheitas

  # gera os valores das variáveis independentes
  for (var in 1 : length(prodvars)){# pra cada vaiável
    for (harv in 1:harvests) {# pra cada colheita
      random_value   = runif(1,
                             min=as.numeric(prodvars[[var]][2]),
                             max=as.numeric(prodvars[[var]][3]))
      last_value_var = 0
      const_value    = random_value
      for (pha in 1:nPhases){ #pra cada fase
        # type (up=1,osc=2,const=3)
        if (prodvars[[var]][4]==1){
          v_value = random_value + last_value_var
          last_value_var = v_value
        }else
          if (prodvars[[var]][4]==2){
            v_value = random_value
          }else
            if (prodvars[[var]][4]==3){
              # ADICIONANDO RUIDO À VARIÁVEL X3 PARA NÃO INTERFERIR NO APRENDIZADO DA REDE
              v_value = const_value + rnorm(1, 0, .15)
            }

        colheitas[[harv]][pha,var] = v_value
        random_value = runif(1,
                             min=as.numeric(prodvars[[var]][2]),
                             max=as.numeric(prodvars[[var]][3]))
      }#for pha in 1:nPhases
    }#for harv in 1:harvests
  }#for var in 1 : length(prodvars)

  # Normalizando (entre 0 e 1 ou 1- e 1, conforme o tipo de variável)
  # dos dados das variáves antes de calcular os valores de produção
  # DUVIDA: é a abordagem mais correta. (??)

  for(h in 1 : harvests){
    for(v in 1 : length(prodvars)){
      if(max(abs(colheitas[[h]][,v])) > 1){
        colheitas[[h]][,v] = f_minmax(colheitas[[h]][,v])

        # corrige valores extremos 0 e 1, adicionando/subtraindo
        # um valor aleatório na 3ª casa depois da vírgula
        colheitas[[h]][which.max(colheitas[[h]][,v]),v] = max(colheitas[[h]][,v]) - runif(1)/100
        colheitas[[h]][which.min(colheitas[[h]][,v]),v] = min(colheitas[[h]][,v]) + runif(1)/100

      }
    }
  }

  # calcula o valor da produção com base nas relações arbitradas com as variáveis


  ##### POR DEFAULT CONSIDERA-SE 3 VARIÁVES:
  if(length(prodvars)==3){
    #  print('AQUI')

    # Área 1: o peso de produção varia linearmente com os valores de todas as variáveis
    # nas duas primeiras fases fenológicas
    # Prod = (X11 + X12 + X21 + X22 + X31 + X32)
    if (areatype == 1) {

      for (h in 1: harvests){
        # cada colheita recebe o somatório de cada linha (fase)
        # length(prodvars) é a quantidade de variáveis independentes
        # lembrando que 'colheitas' é uma lista de matrizes, daí
        # colheitas[[h]][,1:length(prodvars)] é:
        # colheitas[[h]] a matriz da colheita h
        # [,1:length(prodvars)] pra todas as linas da coluna 1 até a penultima
        # a última -- [,length(prodvars)+1] -- é onde fica a var 'colheita'
        colheitas[[h]][,length(prodvars)+1] =
          sum(colheitas[[h]][,1:length(prodvars)])
      }
    }

    # Área 2: o peso de produção varia com o quadrado de X1
    # Prod = (X11^2 + X12^2 + X13^2 + X14^2 + X15^2)
    else if (areatype == 2) {
      for(h in 1:harvests) {
        colheitas[[h]][,length(prodvars)+1] = sum((colheitas[[h]][,1])^2)
      }
    }

    # Área 3: o peso de produção varia com o quadrado de X3
    else if (areatype == 3) {
      for(h in 1:harvests) {
        colheitas[[h]][,length(prodvars)+1] = sum((colheitas[[h]][,3])^2)
      }
    }

    # Área 4: o peso de produção é inversamente proporcional à soma de X1 com X3
    # Prod = 1/(X11+X13) + 1/(...)
    else if (areatype == 4) {
      for(h in 1:harvests) {
        producaoPhase = 0
        for(p in 1:nPhases){
          temp = 1/(colheitas[[h]][p,1]+colheitas[[h]][p,3])
          producaoPhase =  producaoPhase + temp
        }
        colheitas[[h]][,length(prodvars)+1] =  producaoPhase
      }
    }

    # Área 5: o peso da produção decresce com uma ponderação dos valores de X2:
    # Prod = 1* X21 + 0,8* X22 + 0,6*X23 + 0,4*X24 + 0,2*X25
    else if (areatype == 5) {
      for(h in 1:harvests) {
        producaoPhase = 0
        chunk = 100/nPhases
        for(p in 1:nPhases){
          temp = (colheitas[[h]][p,2])*(100-(chunk*p-1))/100
          producaoPhase =  producaoPhase + temp
        }
        colheitas[[h]][,length(prodvars)+1] =  producaoPhase
      }
    }

    # Área 6: o peso da produção cresce com uma ponderação dos valores de X1:
    # Prod = 0,2*X11 + 0,4* X12 + 0,6*X13 + 0,8*X14 + 1*X15
    else if (areatype == 6){
      for(h in 1:harvests) {
        producaoPhase = 0
        chunk = 100/nPhases
        for(p in 1:nPhases){
          temp = ((colheitas[[h]][p,1])*(chunk*p))/100
          producaoPhase =  producaoPhase + temp
        }
        colheitas[[h]][,length(prodvars)+1] =  producaoPhase
      }
    }
  }else{# se forem definidas mais de 3 variáveis

    #  print('ALI')

    # Área 1: o peso de produção varia linearmente com os valores
    # de todas as variáveis nas duas primeiras fases fenológicas
    # Prod = (X11 + X12 + X21 + X22 + X31 + X32)

    if (areatype == 1) {
      for (h in 1: harvests){
        colheitas[[h]][,length(prodvars)+1] = sum(colheitas[[h]][,1:length(prodvars)])
      }
    }

    # Área 2: o peso de produção varia com o quadrado das variáveis impares
    # Prod = Σ(Xi1^2 + Xi2^2 + Xi3^2 + Xi4^2 + Xi5^2) | i=(2k+1), k(0:infinito)
    else if (areatype == 2) {
      for(h in 1:harvests) {
        producaoVar = 0
        for (v in 1 : length(prodvars)){
          if(v %% 2 == 0){next}
          temp = sum((colheitas[[h]][,v])^2)
          producaoVar =  producaoVar + temp
        }
        colheitas[[h]][,length(prodvars)+1] = producaoVar
      }
    }

    # Área 3: o peso de produção varia com o quadrado das variáveis pares
    # Prod = Σ(Xi1^2 + Xi2^2 + Xi3^2 + Xi4^2 + Xi5^2) | i=(2k), k(0:infinito)
    else if (areatype == 3){
      for(h in 1:harvests) {
        producaoVar = 0
        for (v in 1 : length(prodvars)){
          if(v %% 2 != 0){next}
          temp = sum((colheitas[[h]][,v])^2)
          producaoVar =  producaoVar + temp
        }
        colheitas[[h]][,length(prodvars)+1] = producaoVar
      }
    }

    # Área 4: o peso de produção é inversamente proporcional à soma das variáveis impares
    # Prod = 1/(X11+X13) + 1/(...)
    else if (areatype == 4) {
      for(h in 1:harvests) {
        producaoPhase = 0
        for(p in 1:nPhases){
          denominTemp = 0
          for (v in 1 : length(prodvars)){
            if(v %% 2 == 0){next}
            temp = sum(colheitas[[h]][p,v])
            denominTemp =  denominTemp + temp
          }
          temp2 = 1/denominTemp
          producaoPhase =  producaoPhase + temp2
        }
        colheitas[[h]][,length(prodvars)+1] =  producaoPhase
      }
    }

    # Área 5: o peso da produção decresce com uma ponderação da soma das variáveis ímpares
    # Prod = 1* X21 + 0,8* X22 + 0,6*X23 + 0,4*X24 + 0,2*X25
    else if (areatype == 5) {
      for(h in 1:harvests) {
        for(p in 1:nPhases){
          producaoVar = 0
          for (v in 1 : length(prodvars)){
            if(v %% 2 == 0){next}
            temp = sum((colheitas[[h]][p,v]))
            producaoVar =  producaoVar + temp
          }
          producaoPhase = 0
          chunk = 100/nPhases
          temp2 = producaoVar*(100-(chunk*p-1))/100
          producaoPhase =  producaoPhase + temp2
        }
        colheitas[[h]][,length(prodvars)+1] =  producaoPhase
      }
    }

    # Área 6: o peso da produção cresce com uma ponderação dos valores das variáveis pares:
    # Prod = 0,2*X11 + 0,4* X12 + 0,6*X13 + 0,8*X14 + 1*X15
    else if (areatype == 6) {
      for(h in 1:harvests) {
        for(p in 1:nPhases){
          producaoVar = 0
          for (v in 1 : length(prodvars)){
            if(v %% 2 != 0){next}
            temp = sum((colheitas[[h]][p,v]))
            producaoVar =  producaoVar + temp
          }
          producaoPhase = 0
          chunk = 100/nPhases
          temp2 = producaoVar*(100-(chunk*p-1))/100
          producaoPhase =  producaoPhase + temp2
        }
        colheitas[[h]][,length(prodvars)+1] =  producaoPhase
      }
    }
  }#fim else

  return(colheitas)
}

criaDataFrames <- function (dados){
  fase <- list()
  area <- list()
  for(k in 1:length(dados)){
    for(j in 1:length(dados[[1]][[1]][,1])){
      area_phase <- matrix(0, nrow=length(dados[[1]]),ncol=length(dados[[1]][[1]][1,]))
      for(i in 1:length(dados[[1]])){
        area_phase[i,] <- dados[[1]][[i]][j,]
      }
      fase[[j]] <- data.frame(area_phase)
      colnames(fase[[j]]) <- colnames(dados[[1]][[1]])
      rownames(fase[[j]]) <- rownames(fase[[j]])
    }
    area[[k]] <- fase
  }
  return(area)
}
