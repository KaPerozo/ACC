################################################################################
# # WrightMapICFES.R
# # R Versions: 3.2.0
# #
# # Author(s): Dora Suarez
# #
# # SABER PRO
# # Description: Grafica el mapa de items personas
# #
# # Inputs: infoItem y infoCal data frames con las habilidades y las calibraciones
# #         de los items
# # Outputs: Grafico de items personas
# #
# # File history:
# #   20150922: Creation
# #   20151024: Creación de la función y inclusión de otros parametros 
# #             para el grafico creado en ggplot
# #   20170606: Actualización del paquete ggplot2
################################################################################

################################################################################
# # Carga de librerias
################################################################################

WrightMapICFES <- function(infoItem, infoCal, colHab, colDiff, 
                           file = NULL, Title = NULL) {
  require(dplyr)
  require(grid)     
  require(ggplot2)
  require(reshape2)
  require(plyr)
  require(RColorBrewer)
  require(gridExtra)
 # # Contrucción de la base
  measure <- as.numeric(c(infoCal[[colHab]], infoItem[[colDiff]]))
  grupos  <- c(rep("PERS", nrow(infoCal)), rep("ITEM", nrow(infoItem)))
  data    <- data.frame(measure , grupos)
  m1 <- m2 <- 1

  # # Histogramas y graficos de densidades (Se voltean las coordenadas)
  secu    <- round(c(seq(from= m1 , to= 0, -m1/5), seq(from=m2/5, to= m2, m2/5) ),2)
  p <- ggplot(data, aes(x = measure , fill = grupos, colour = grupos))
  p <- p + geom_histogram(data = subset(data, grupos == "PERS"), aes(y = - ..density..)) 
  p <- p + geom_density(data = subset(data, grupos == "PERS"), aes(y = - ..density..), alpha = 0.1) 
  p <- p + coord_flip() + geom_histogram(data = subset(data, grupos == "ITEM"), aes(y = ..density..))
  p <- p + geom_density(data = subset(data, grupos == "ITEM"), aes(y = ..density..), alpha = 0.1)
  
  # # Calcular las medias, minimo, maximo en Y

  pg     <- suppressWarnings(ggplot_build(p)$layout$panel_ranges[[1]]$x.range)
  medias <- tapply(measure, grupos, mean)
  medias <- data.frame(t(medias), 'PERS2'=pg[1], 'ITEM2' = pg[2])
  
  p <- p + scale_y_continuous(breaks = seq(-1, 1, length=length(secu)), labels= as.character(secu), 
                              limits = c(pg[1], pg[2]))
  p <- p + geom_segment(data = medias, aes(y = PERS2, x = PERS, xend= PERS, fill = "PERS", yend =0), linetype="dashed", size=0.7, colour=4)
  p <- p + geom_segment(data = medias, aes(y = 0, x = ITEM, xend= ITEM, fill = "ITEM", yend = ITEM2), linetype="dashed", size=0.7, colour="red")
  
  # # Personalización del grafico
  p <- p + theme_bw() + scale_colour_brewer(palette = "Set1") + xlab("Habilidad") + ylab("Densidad") 
  p <- p + theme(legend.position="top")
  if (!is.null(Title)){ 
    p <- p + ggtitle(paste0("Mapa Items-Personas ", Title, "")) + 
         theme(plot.title = element_text(lineheight=1.1, face="bold"))
  } else {
    p <- p + ggtitle("Mapa Items-Personas") + 
         theme(plot.title = element_text(lineheight=1.1, face="bold"))
  }
  # # Identificando Extremos
  infoExtrem       <- boxplot(infoItem[[colDiff]], plot = FALSE)$stats
  infoItem[["vj"]] <- rep(0, length.out=nrow(infoItem))
  infoItem[["vj"]][infoItem[[colDiff]] <= infoExtrem[1, 1]] <- 1.2
  infoItem[["vj"]][infoItem[[colDiff]] >= infoExtrem[5, 1]] <- -1.2

  infoExtrem <- infoItem[[colDiff]] > infoExtrem[1, 1] & 
                infoItem[[colDiff]] < infoExtrem[5, 1]
  infoItem[["ITEM"]][infoExtrem] <- ""

  # # Graficando dificultades ordenadas
  infoItem <- infoItem[order(infoItem[[colDiff]]), ]
  infoItem[["Sort"]] <- order(infoItem[[colDiff]])

  #infoItem[["hj"]] <- rep(c(-0.3, -0.2), length.out=nrow(infoItem))
  p2    <- ggplot(infoItem, aes(x = dif_NEW, y = Sort, label = ITEM)) + geom_point() 
  p2    <- p2 + geom_text(aes(label = ITEM, vjust = vj), size = 2.6, angle = 90) + theme_bw()
  p2    <- p2 + geom_vline(xintercept = medias[1, "ITEM"]) + coord_flip() + xlab("") + ylab("Orden")
  
  # # Crando el nuevo grafico 
  lay  <- rbind(c(1,1,1,1,1,2))
  pFin <- suppressWarnings(gridExtra::arrangeGrob(grobs = list(p, p2), layout_matrix = lay))
  
  if (!is.null(file)) {
    #ggsave(filename = file,  plot = grid::grid.draw(pFin), width = 5, height = 5, dpi = 1200)
    png(file, width = 1372, height = 686, res = 95, antialias = "cleartype")
    suppressWarnings(grid.draw(pFin))
    dev.off() 
  }
 
  return(list(p, p2))
} 
