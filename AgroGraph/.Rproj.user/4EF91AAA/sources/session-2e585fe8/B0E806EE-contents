iniciaTeste <- function(){
  file_path = "/home/gui/TCC/R/meu_pacote/teste1.csv"
  nHarvests = 1000
  nphases = 5
  nAreas = 2
  nVars = 3
  #class_names <- c("B", "MB", "M", "MA", "A")
  class_names <- c("B", "M", "A")
  # listas para armazenar os dados por área
  areas_list  = list()
  areas_list_disc  = list()
  areas_list  = BuildSimulationData(nHarvests, nphases)
  #areas_list  = BuildSimulationData(nHarvests, nphases, nAreas, 10)


  #para cada área, cria uma lista de dataframes
  #cada dataframe tem os dados de uma fase fenológica
  for(area in 1:length(areas_list)){
    areas_list[area] = criaDataFrames(areas_list[area])
    for(fase in 1:nphases){

      model <- preProcess(areas_list[[area]][[fase]], method="range")
      areas_list[[area]][[fase]] <- predict(model, areas_list[[area]][[fase]])
    }
  }
  areas_list_disc = areas_list
  #return(areas_list)

  for(area in 1:length(areas_list)){#para cada área
    for(fase in 1:length(areas_list[[1]])){#para cada fase da área
      areas_list_disc[[area]][[fase]] <- discretize(areas_list[[area]][[fase]],
                                                    #idisc = 'interval',
                                                    ordered = FALSE,
                                                    method = 'hartemink',
                                                    ibreaks = length(class_names)*5,
                                                    breaks = length(class_names))
      for(var in 1:dim(areas_list[[1]][[fase]])[2]){#para cada variável da área/fase
        levels(areas_list_disc[[area]][[fase]][,var]) <- class_names
      }
    }
  }
  return(c(areas_list, areas_list_disc))
}

##  Gera dados
## GERA O CONJUNTO DE DADOS DUAS VEZES,
##   SENDO A 1ª METADE OS DADOS "CRUS" (areas_raw)
##   E NA SEGUNDA METADE OS DADOS DISCRETIZADOS areas_dis
areas <- iniciaTeste()
areas_raw <- areas[1:(length(areas)/2)]
areas_dis <- areas[((length(areas)/2)+1):length(areas)]


# TESTE - REDES ESTÁTICAS (dblearn)

area1 <- rodaRede(areas_dis[[1]], 1)
geraGraph(area1, "/home/gui/TCC/R/meu_pacote/saidas/grafico_ESTATICA_A1.png", "Área 1")
write.csv(area1, file = "/home/gui/TCC/R/meu_pacote/saidas/out_area1.csv")

area2 <- rodaRede(areas_dis[[2]], 2)
geraGraph(area2, "/home/gui/TCC/R/meu_pacote/saidas/grafico_ESTATICA_A2.png", "Área 2")
write.csv(area2, file = "/home/gui/TCC/R/meu_pacote/saidas/out_area2.csv")

area3 <- rodaRede(areas_dis[[3]], 3)
geraGraph(area3, "/home/gui/TCC/R/meu_pacote/saidas/grafico_ESTATICA_A3.png", "Área 3")
write.csv(area3, file = "/home/gui/TCC/R/meu_pacote/saidas/out_area3.csv")

area4 <- rodaRede(areas_dis[[4]], 4)
geraGraph(area4, "/home/gui/TCC/R/meu_pacote/saidas/grafico_ESTATICA_A4.png", "Área 4")
write.csv(area4, file = "/home/gui/TCC/R/meu_pacote/saidas/out_area4.csv")

area5 <- rodaRede(areas_dis[[5]], 5)
geraGraph(area5, "/home/gui/TCC/R/meu_pacote/saidas/grafico_ESTATICA_A5.png", "Área 5")
write.csv(area5, file = "/home/gui/TCC/R/meu_pacote/saidas/out_area5.csv")

area6 <- rodaRede(areas_dis[[6]], 6)
geraGraph(area6, "/home/gui/TCC/R/meu_pacote/saidas/grafico_ESTATICA_A6.png", "Área 6")
write.csv(area6, file = "/home/gui/TCC/R/meu_pacote/saidas/out_area6.csv")

# TESTE - REDES DINAMICAS

x <- criaRedesDinamc(areas_raw[[1]])
# DAG_dinamica_A1.png
plot(x)
#natPsoho
# The average MAE per execution is: colheita_t_0: 0.2448 # approx
# The average MAE per execution is: colheita_t_0:  0.259 # exact
#dmmhc:
# The average MAE per execution is: colheita_t_0: 0.192 # approx
# The average MAE per execution is: colheita_t_0: 0.195 # exact

criaRedesDinamc(areas_raw[[2]])
#natPsoho
# The average MAE per execution is: colheita_t_0: 0.195 # approx
# The average MAE per execution is: colheita_t_0: 0.168 # exact
#dmmhc:
# The average MAE per execution is: colheita_t_0: 0.297 # approx
# The average MAE per execution is: colheita_t_0: 0.297 # exact
criaRedesDinamc(areas_raw[[3]])
# The average MAE per execution is: colheita_t_0: 0.216 # approx
# The average MAE per execution is: colheita_t_0: 0.278 # exact
criaRedesDinamc(areas_raw[[4]])
# The average MAE per execution is: colheita_t_0: 0.0058 # approx
# The average MAE per execution is: colheita_t_0: 0.0111 # exact
criaRedesDinamc(areas_raw[[5]])
# The average MAE per execution is: colheita_t_0: 0.106 # approx
# The average MAE per execution is: colheita_t_0: 0.2092 # exact
criaRedesDinamc(areas_raw[[6]])
# The average MAE per execution is: colheita_t_0: 0.252 # approx
# The average MAE per execution is: colheita_t_0: 0.2083 # exact

