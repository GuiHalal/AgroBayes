#'
#' Allows normalization of generated data.
#'
#' Allows normalization of data using the $$(x−min)/(max−min)$$ equation.
#'
#' @param x the set of data to be normalized
#' @return set of data normalized
#' @examples
#' \donttest{
#' normalizerData = f_minmax(notNormalizerData)
#' }
#' @keywords internal
#' @export
#'
f_minmax <- function(x){
  minmax = (max(x)-min(x))
  if(minmax==0){
    return(min(x))
  }
  return((x - min(x))/minmax)
}

#'
#' Generate graphs of static bayesian networks metrics
#'
#' generate graphs of the accuracy of the four static Bayesian networks
#' generated in the \code{\link{testCreateNetworks}} and
#' \code{\link{createNetworks}} functions. The generated graphs will be
#' grouped by phenological phase.
#'
#' @param areaN dataframe with the data to be plotted on the chart
#' @param file string that indicates the path where the graph will be saved
#' @param subtitle string of the term that will be placed at top-left of the graph
#' @examples
#' \donttest{
#' file = path_to_where_to_save_the_graph
#' area1bn <- testRunNetworks(areas_dis[[1]],1)
#' geraGraph(area1bn, file, "Area 1")
#' }
#' @keywords internal
#' @export
#'

geraGraph <-function(areaN, file, subtitle){

  text_size <- 20

  areaN$networks <- rep(c("hc_dag", "hc_dag_raw", "mmhc_dag", "mmhc_dag_raw"),
                     each = 1)
  areaN$season <- rep(c("phase 1", "phase 2", "phase 3", "phase 4", "phase 5"),
                       each = 4)
  areaN$season <- factor(areaN$season, levels =
                            c("phase 1", "phase 2", "phase 3", "phase 4", "phase 5"))

  grDevices::png(file, width = 1024, height = 768)

  graphics::plot(
    ggplot2::ggplot(areaN, ggplot2::aes(season, accuracy, fill = networks)) +
      ggplot2::scale_y_continuous(limits = c(0,100), breaks = seq(0,100,25)) +
      ggplot2::labs(
        subtitle = subtitle
        )+
      ggplot2::scale_fill_brewer(palette = "Spectral") + # Spectral; Paired; Set3
      ggplot2::geom_col(position = "dodge",
                        show.legend = TRUE, linetype = 1, alpha = .65) +
      ggplot2::geom_text(ggplot2::aes(label = accuracy), vjust = 1,
                         position = ggplot2::position_dodge(.9),
                         size = 5, color = "black")+
      ggplot2::theme(axis.ticks.length = ggplot2::unit(.3, "cm"),
                     axis.text =
                       ggplot2::element_text(size =
                                               text_size, colour = "black"),
                     axis.title =
                       ggplot2::element_text(size =
                                               text_size, colour = "black"),
                     axis.ticks =
                       ggplot2::element_line(colour = "black"),
                     panel.border =
                       ggplot2::element_rect(colour =
                                               "black", fill = NA, size = 0.5),
                                             legend.position="bottom",
                     legend.text = ggplot2::element_text(colour="black",
                                                         size=text_size, face="bold"),

                     legend.title =  ggplot2::element_text(colour="black",
                                                           size=text_size, face="bold"),

                     plot.title.position = "panel",
                     plot.caption.position = "panel",
                     title = ggplot2::element_text(colour="black",
                                                   size=text_size, face="bold")
                     # panel.grid =  element_blank()
      )+
      ggplot2::geom_hline(yintercept = mean(areaN$accuracy),
                          linetype = "solid", size = .2))

  grDevices::dev.off()
}

#'
#' Defines classes for discretization of results from dynamic Bayesian networks
#'
#' @keywords internal
#' @export
#'
defClasses <- function(n_classes, df_nome) {
  val_max <- max(df_nome, na.rm = TRUE)
  val_min <- min(df_nome, na.rm = TRUE)
  amp_total <- val_max - val_min
  amp_classe <- amp_total / n_classes
  classes <- vector()
  classes <- val_min + amp_classe
  for (i in 1:(n_classes -1)) {
    classes[i+1] <- classes[i] + amp_classe
  }
  return(classes)
}

#'
#' Replace values with classes generated in the
#'  \code{\link{defClasses}} function
#'
#' @keywords internal
#' @export
#'
classficator <- function(input, varclass, class_names) {
  input <- replace(input, input <= varclass[1], class_names[1])
  for (i in 2 : length(varclass)){
    input <- replace(input, input > varclass[i-1] &
                       input <= varclass[i], class_names[i])
  }
  return(input)
}

#'
#' calculate accuracy from the predicted and observed values in
#' the prediction functions of dynamic Bayesian networks
#'
#' @keywords internal
#' @export
#'
accurCalc <- function(df){
  #classificando
  #class_names <- c("L", "ML", "M", "MH", "H")
  class_names <- c("L", "M", "H")
  class_x1 <- defClasses(3, df$obs)
  class_x1[length(class_x1)] <- max(df)
  x1_class <- classficator(df, class_x1, class_names)
  x1_class$obs <- factor(x1_class$obs)
  x1_class$pred <- factor(x1_class$pred)

  return((caret::confusionMatrix(x1_class$pred, x1_class$obs))$overall[1])
}

