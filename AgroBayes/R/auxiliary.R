f_minmax <- function(x){
  minmax = (max(x)-min(x))
  if(minmax==0){
    return(min(x))
  }
  return((x - min(x))/minmax)
}

# geraGraph <-function(areaN, file, subtitulo){
#
#   text_size <- 20
#
#   areaN$redes <- rep(c("hc_dag", "hc_dag_raw", "mmhc_dag", "mmhc_dag_raw"),
#                      each = 1)
#   areaN$periodo <- rep(c("fase 1", "fase 2", "fase 3", "fase 4", "fase 5"),
#                        each = 4)
#   areaN$periodo <- factor(areaN$periodo, levels =
#                             c("fase 1", "fase 2", "fase 3", "fase 4", "fase 5"))
#   grDevices::png(file, width = 1024, height = 768)
#
#   graphics:plot(
#     ggplot2:ggplot(areaN, ggplot2::aes(periodo, accuracy, fill = redes)) +
#       scale_y_continuous(limits = c(0,100), breaks = seq(0,100,25)) +
#       labs(
#         subtitle = subtitulo
#       )+
#       scale_fill_brewer(palette = "Spectral") + # Spectral; Paired; Set3
#       geom_col(position = "dodge",
#                show.legend = TRUE, linetype = 1, alpha = .65) +
#       geom_text(ggplot2::aes(label = accuracy), vjust = 1,
#                 position = position_dodge(.9),
#                 size = 5, color = "black")+
#       theme(axis.ticks.length = unit(.3, "cm"),
#             axis.text = element_text(size = text_size, colour = "black"),
#             axis.title = element_text(size = text_size, colour = "black"),
#             axis.ticks = element_line(colour = "black"),
#             panel.border = element_rect(colour = "black",
#                                         fill = NA, size = 0.5),
#             legend.position="bottom",
#             legend.text = element_text(colour="black",
#                                        size=text_size, face="bold"),
#
#             legend.title =  element_text(colour="black",
#                                          size=text_size, face="bold"),
#
#             plot.title.position = "panel",
#             plot.caption.position = "panel",
#             title = element_text(colour="black",
#                                  size=text_size, face="bold")
#             # panel.grid =  element_blank()
#       )+
#       geom_hline(yintercept = mean(areaN$accuracy),
#                  linetype = "solid",
#                  size = .2))
#
#   dev.off()
# }
