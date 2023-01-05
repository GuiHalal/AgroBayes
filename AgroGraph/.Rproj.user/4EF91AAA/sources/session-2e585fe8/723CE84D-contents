f_minmax <- function(x){
  minmax = (max(x)-min(x))
  if(minmax==0){
    return(min(x))
  }
  return((x - min(x))/minmax)
}

geraGraph <-function(areaN, file, subtitulo){

  text_size <- 20

  areaN$redes <- rep(c("hc_dag", "hc_dag_raw", "mmhc_dag", "mmhc_dag_raw"),
                     each = 1)
  areaN$periodo <- rep(c("fase 1", "fase 2", "fase 3", "fase 4", "fase 5"),
                       each = 4)
  areaN$periodo <- factor(areaN$periodo, levels =
                            c("fase 1", "fase 2", "fase 3", "fase 4", "fase 5"))
  png(file, width = 1024, height = 768)

  plot(
    ggplot(areaN, aes(periodo, accuracy, fill = redes)) +
      scale_y_continuous(limits = c(0,100), breaks = seq(0,100,25)) +
      labs(
        # title = "Acurácia de Redes Estáticas por fase",
        subtitle = subtitulo
        # caption = "Fonte: Autor(2022)"
      )+
      scale_fill_brewer(palette = "Spectral") + # Spectral; Paired; Set3
      geom_col(position = "dodge",
               show.legend = TRUE, linetype = 1, alpha = .65) +
      geom_text(aes(label = accuracy), vjust = 1,
                position = position_dodge(.9),
                size = 5, color = "black")+
      theme(axis.ticks.length = unit(.3, "cm"),
            axis.text = element_text(size = text_size, colour = "black"),
            axis.title = element_text(size = text_size, colour = "black"),
            axis.ticks = element_line(colour = "black"),
            panel.border = element_rect(colour = "black",
                                        fill = NA, size = 0.5),
            legend.position="bottom",
            legend.text = element_text(colour="black",
                                       size=text_size, face="bold"),

            legend.title =  element_text(colour="black",
                                         size=text_size, face="bold"),

            plot.title.position = "panel",
            plot.caption.position = "panel",
            title = element_text(colour="black",
                                 size=text_size, face="bold")
            # panel.grid =  element_blank()
      )+
      geom_hline(yintercept = mean(areaN$accuracy),
                 linetype = "solid",
                 size = .2))

  dev.off()
}

validaRede <- function(test, train, dag_fitted1, dag_fitted2, dag_fitted3, dag_fitted4) {
  # Define Target variables (Variables to be predicted)
  pred <-'colheita'
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

  #print("metricas dag1: ")
  metrics1 <- bnMetricsMultiVarPrediction(reference = test[pred],
                                          prediction = results1$dominantList,
                                          predProbList = results1$probList)
  #print("metricas dag2: ")
  metrics2 <- bnMetricsMultiVarPrediction(reference = test[pred],
                                          prediction = results2$dominantList,
                                          predProbList = results2$probList)

  metrics3 <- bnMetricsMultiVarPrediction(reference = test[pred],
                                          prediction = results3$dominantList,
                                          predProbList = results3$probList)
  #print("metricas dag2: ")
  metrics4 <- bnMetricsMultiVarPrediction(reference = test[pred],
                                          prediction = results4$dominantList,
                                          predProbList = results4$probList)
  # print("Matriz de confusão dag 1:")
  # print(metrics1$cmList)
  # print("Matriz de confusão dag 2:")
  # print(metrics2$cmList)
  return(c(metrics1, metrics2, metrics3, metrics4))
}
