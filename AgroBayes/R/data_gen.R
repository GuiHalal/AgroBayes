BuildSimulationData <- function (nHarvests, nPhases, nAreas = NULL, ...) {

  areasList <- list()
  if(is.null(nAreas)){
    # by default 6 areas are generated
    # each area has a relationship between variables and production

    areanames = c("Area_1", "Area_2", "Area_3", "Area_4", "Area_5", "Area_6")
    areatype  = c(1,2,3,4,5,6)
    for (i in 1 : length(areanames)){
      areasList[[i]] <- c(areanames =  areanames[i], areatype  = areatype[i])
    }
  }else{
    # if the number of areas is defined, the types are randomly assigned
    for (i in 1 : nAreas){
      tempType = sample(1:6, 1)
      #tempType = 5
      tempName <- paste("Area", i, sep = "_")
      areasList[[i]] = c(areaName = tempName, areatype = tempType)
    }
  }

  # definition of variables by function defVars
  defProdVariables = defVars(...)

  completeSimValues = list()
  namesArea = array(dim = length(areasList))

  # For each area Calculate the values of the variables
  # and production by function setSimVarValues

  for (a in 1:length(areasList)) {

    #cat("Calculating area values ",a,"\n")

    completeSimValues[[a]] = setSimVarValues(nHarvests, areasList[[a]][2],defProdVariables,nPhases)
    namesArea[a] = paste("Area", a, "type", areasList[[a]][2], sep = "_")
  }
  names(completeSimValues) = namesArea

  return (completeSimValues)
}

defVars <- function (n_var = NULL, type_var = NULL, name_var = NULL){
  prodVariables = list()

  # If the number of variables is not defined, the default is the 3 below
  if(is.null(n_var)){
    # Definition of variables: name, minimum value, maximum value, type
    # type (up=1,osc=2,const=3)
    prodVariables[[1]] = c("PrecAcum",0,1,1)
    prodVariables[[2]] = c("Insol",0,1,2)
    prodVariables[[3]] = c("Compact",-1,1,3)

    return (prodVariables)
  }else
    if(!is.null(type_var) | !is.null(name_var)){
      # being informed the number of variables and their name and behavior
      # the variables are generated as follows:
      if(length(type_var)!= n_var | length(name_var)!= n_var){
        stop("incorrect parameters. If informed, types and names of
              variables must be informed for all variables")
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
      # Being informed the n of variables, without further details the generation
      # occurs randomly, as follows:
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
  crops <-list()
  names_crops <- array(dim = harvests)

  # defines variable names
  for(i in 1: harvests){
    # for each crop in the area
    # creates a matrix of 'number of phases' rows and
    # 'number of variables' columns

    phases <- matrix(nrow=nPhases, ncol= length(prodvars)+1)
    colnames(phases) <-  colnames(phases, do.NULL = FALSE, prefix = "X_")
    colnames(phases)[length(prodvars)+1] <-  "crop"
    rownames(phases) <-  rownames(phases, do.NULL = FALSE, prefix = "phase_")
    crops[[i]] = phases
    names_crops[i] = paste("crop", i, sep = "_")
  }
  names(crops) = names_crops

  # generates the values of the independent variables
  for (var in 1 : length(prodvars)){# for each variable
    for (harv in 1:harvests) {# for each crop
      random_value   = stats::runif(1,
                             min=as.numeric(prodvars[[var]][2]),
                             max=as.numeric(prodvars[[var]][3]))
      last_value_var = 0
      const_value    = random_value
      for (pha in 1:nPhases){ #pfor eachphase
        # type (up=1,osc=2,const=3)
        if (prodvars[[var]][4]==1){
          v_value = random_value + last_value_var
          last_value_var = v_value
        }else
          if (prodvars[[var]][4]==2){
            v_value = random_value
          }else
            if (prodvars[[var]][4]==3){
              # adding noise to variable type 3
                # to not interfere with network learning
              v_value = const_value + stats::rnorm(1, 0, .15)
            }

        crops[[harv]][pha,var] = v_value
        random_value = stats::runif(1,
                             min=as.numeric(prodvars[[var]][2]),
                             max=as.numeric(prodvars[[var]][3]))
      }#for pha in 1:nPhases
    }#for harv in 1:harvests
  }#for var in 1 : length(prodvars)

  # Normalizing (between 0 and 1 or 1- and 1, depending on the type of variable)
  # of the variable data before calculating the production values
  for(h in 1 : harvests){
    for(v in 1 : length(prodvars)){
      if(max(abs(crops[[h]][,v])) > 1){
        crops[[h]][,v] = f_minmax(crops[[h]][,v])

        # fix extreme values 0 and 1 by adding/subtracting
        # a random value in the 3rd place after the decimal point
        crops[[h]][which.max(crops[[h]][,v]),v] =
          max(crops[[h]][,v]) - stats::runif(1)/100
        crops[[h]][which.min(crops[[h]][,v]),v] =
          min(crops[[h]][,v]) + stats::runif(1)/100

      }
    }
  }

  # calculates the production value based on
  # the arbitrated relations with the variables


  ##### POR DEFAULT CONSIDERA-SE 3 VARIÁVES:
  if(length(prodvars)==3){
    #  print('AQUI')

    # Area 1: production weight varies linearly with the values of all
    # variables in the first two phenological phases
    # Prod = (X11 + X12 + X21 + X22 + X31 + X32)
    if (areatype == 1) {

      for (h in 1: harvests){
        crops[[h]][,length(prodvars)+1] =
          sum(crops[[h]][,1:length(prodvars)])
      }
    }

    # Area 2: production weight varies with the square of X1
    # Prod = (X11^2 + X12^2 + X13^2 + X14^2 + X15^2)
    else if (areatype == 2) {
      for(h in 1:harvests) {
        crops[[h]][,length(prodvars)+1] = sum((crops[[h]][,1])^2)
      }
    }

    # Area 3: production weight varies with the square of X3
    else if (areatype == 3) {
      for(h in 1:harvests) {
        crops[[h]][,length(prodvars)+1] = sum((crops[[h]][,3])^2)
      }
    }

    # Area 4: the production weight is inversely proportional to the sum of X1 and X3
    # Prod = 1/(X11+X13) + 1/(...)
    else if (areatype == 4) {
      for(h in 1:harvests) {
        producaoPhase = 0
        for(p in 1:nPhases){
          temp = 1/(crops[[h]][p,1]+crops[[h]][p,3])
          producaoPhase =  producaoPhase + temp
        }
        crops[[h]][,length(prodvars)+1] =  producaoPhase
      }
    }

    # Area 5: the production weight decreases with a weighting of the X2 values:
    # Prod = 1*X21 + 0.8*X22 + 0.6*X23 + 0.4*X24 + 0.2*X25
    else if (areatype == 5) {
      for(h in 1:harvests) {
        producaoPhase = 0
        chunk = 100/nPhases
        for(p in 1:nPhases){
          temp = (crops[[h]][p,2])*(100-(chunk*p-1))/100
          producaoPhase =  producaoPhase + temp
        }
        crops[[h]][,length(prodvars)+1] =  producaoPhase
      }
    }

    # Area 6: the weight of production grows with a weighting of the values of X1:
    # Prod = 0.2*X11 + 0.4*X12 + 0.6*X13 + 0.8*X14 + 1*X15
    else if (areatype == 6){
      for(h in 1:harvests) {
        producaoPhase = 0
        chunk = 100/nPhases
        for(p in 1:nPhases){
          temp = ((crops[[h]][p,1])*(chunk*p))/100
          producaoPhase =  producaoPhase + temp
        }
        crops[[h]][,length(prodvars)+1] =  producaoPhase
      }
    }
  }else{# if more than 3 variables are defined

    # Area 1: the production weight varies linearly with the values
    # of all variables in the first two phenological phases
    # Prod = (X11 + X12 + X21 + X22 + X31 + X32)

    if (areatype == 1) {
      for (h in 1: harvests){
        crops[[h]][,length(prodvars)+1] = sum(crops[[h]][,1:length(prodvars)])
      }
    }

    # Area 2: the production weight varies with the square of the odd variables
    # Prod = sum(Xi1^2 + Xi2^2 + Xi3^2 + Xi4^2 + Xi5^2) | i=(2k+1), k(0:infinity)
    else if (areatype == 2) {
      for(h in 1:harvests) {
        producaoVar = 0
        for (v in 1 : length(prodvars)){
          if(v %% 2 == 0){next}
          temp = sum((crops[[h]][,v])^2)
          producaoVar =  producaoVar + temp
        }
        crops[[h]][,length(prodvars)+1] = producaoVar
      }
    }

    # Area 3: the production weight varies with the square of the even variables
    # Prod = sum(Xi1^2 + Xi2^2 + Xi3^2 + Xi4^2 + Xi5^2) | i=(2k), k(0:infinity)
    else if (areatype == 3){
      for(h in 1:harvests) {
        producaoVar = 0
        for (v in 1 : length(prodvars)){
          if(v %% 2 != 0){next}
          temp = sum((crops[[h]][,v])^2)
          producaoVar =  producaoVar + temp
        }
        crops[[h]][,length(prodvars)+1] = producaoVar
      }
    }

    # Area 4: the production weight is inversely proportional to the sum of the odd variables
    # Prod = 1/(X11+X13) + 1/(...)
    else if (areatype == 4) {
      for(h in 1:harvests) {
        producaoPhase = 0
        for(p in 1:nPhases){
          denominTemp = 0
          for (v in 1 : length(prodvars)){
            if(v %% 2 == 0){next}
            temp = sum(crops[[h]][p,v])
            denominTemp =  denominTemp + temp
          }
          temp2 = 1/denominTemp
          producaoPhase =  producaoPhase + temp2
        }
        crops[[h]][,length(prodvars)+1] =  producaoPhase
      }
    }

    # Area 5: the production weight decreases with a weighting of the sum of the odd variables
    # Prod = 1*X21 + 0.8*X22 + 0.6*X23 + 0.4*X24 + 0.2*X25
    else if (areatype == 5) {
      for(h in 1:harvests) {
        for(p in 1:nPhases){
          producaoVar = 0
          for (v in 1 : length(prodvars)){
            if(v %% 2 == 0){next}
            temp = sum((crops[[h]][p,v]))
            producaoVar =  producaoVar + temp
          }
          producaoPhase = 0
          chunk = 100/nPhases
          temp2 = producaoVar*(100-(chunk*p-1))/100
          producaoPhase =  producaoPhase + temp2
        }
        crops[[h]][,length(prodvars)+1] =  producaoPhase
      }
    }

    # Area 6: the production weight grows with a weighting of the values of the even variables:
    # Prod = 0.2*X11 + 0.4*X12 + 0.6*X13 + 0.8*X14 + 1*X15
    else if (areatype == 6) {
      for(h in 1:harvests) {
        for(p in 1:nPhases){
          producaoVar = 0
          for (v in 1 : length(prodvars)){
            if(v %% 2 != 0){next}
            temp = sum((crops[[h]][p,v]))
            producaoVar =  producaoVar + temp
          }
          producaoPhase = 0
          chunk = 100/nPhases
          temp2 = producaoVar*(100-(chunk*p-1))/100
          producaoPhase =  producaoPhase + temp2
        }
        crops[[h]][,length(prodvars)+1] =  producaoPhase
      }
    }
  }#fim else

  return(crops)
}

createDataFrames <- function (data){
  phase <- list()
  area <- list()
  for(k in 1:length(data)){
    for(j in 1:length(data[[1]][[1]][,1])){
      area_phase <- matrix(0, nrow=length(data[[1]]),ncol=length(data[[1]][[1]][1,]))
      for(i in 1:length(data[[1]])){
        area_phase[i,] <- data[[1]][[i]][j,]
      }
      phase[[j]] <- data.frame(area_phase)
      colnames(phase[[j]]) <- colnames(data[[1]][[1]])
      rownames(phase[[j]]) <- rownames(phase[[j]])
    }
    area[[k]] <- phase
  }
  return(area)
}
