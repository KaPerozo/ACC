################################################################################
# # RunDIF.R
# # R Versions: 3.3.1
# #
# # Author(s): Jorge Mario Carrasco, Edwin Cuellar
# #
# # SABER 5° y 9° Citizenship competencies 
# # Description: Creates xlsx output files of DRFIT analysis for each 
# #              anchor items 
# #
# #
# # File history:
# #   20140401: Creation
# #   20160826: Adaptation for S4 Clases (Nelson Rodriguez and Jorge Carrasco)
################################################################################

################################################################################
# # Definition of class and parameters
################################################################################

# # Heritage class Analysis
DRIFT <- setClass("DRIFT", contains = "Analysis")
setMethod("initialize", "DRIFT", function(.Object, ..., param) {
  .Object@outFile$pathRdata <- "../Output/09DRIFT/outList_DRIFT.Rdata"
  .Object <- callNextMethod()
})

DRIFT <- function(test, paramExp = NULL){
  paramDefault <- list(anchorPath = "", anchorForm = "",
                       driftSeed = format(Sys.time(), "%d%m%Y"),
                       nReplicates = 1000, irtModel = "3pl", 
                       alphaCut = 0.05)
  if (!is.null(paramExp)) {
    isDefault <- setdiff(names(paramDefault), names(paramExp))
    paramExp  <- c(paramExp, paramDefault[isDefault])
  } else {
    paramExp <- paramDefault
  }

  if (paramExp$anchorPath == "" | paramExp$anchorForm == "") {
    stop("ERROR ... Se debe definir 'anchorPath' y 'anchorForm' en los parametros.\n")
  }
  cat("-----> Se correra un analisis DRIFT con los siguientes parametros: \n")
  print(paramExp) 
  object <- new("DRIFT", test = test, param = paramExp, 
                verSalida = auxVerSalida)
  object <- filterAnalysis(object)
  return(object)
}


################################################################################
# # Definition of codeAnalysis Method
################################################################################

# # Heritage class Analysis
DRIFT <- setClass("DRIFT", contains = "Analysis")

setMethod("codeAnalysis", "DRIFT",
  analDRIFT <- function(object){
    cat("O.o--Inicio la corrida de DRIFT--o.O\n")
    pathIRT  <- getOutFile(object@test, "IRT")$pathRdata
    outPath <- file.path(outPath, "09DRIFT")
    dir.create(outPath, recursive = TRUE, showWarnings = FALSE)

    ################################################################################
    # # Load libraries
    ################################################################################
    library(plink)
    library(xlsx)
    library(DFIT)
    library(ggplot2)

    ###############################################################################
    # # Load data base
    ################################################################################
    # # Current aplication
    statApp1 <- getAnchors(pathIRT, names(object@datAnalysis))
    setnames(statApp1, c("disc", "dif", "azar"), c("a", "b", "c"))    
    statApp1 <- statApp1[!(is.na(a) & is.na(b) & is.na(c)), ]
    
    # # Last aplication
    if (!is.null(object@param$anchorPath)) {   
      statApp2 <- getAnchors(object@param$anchorPath, object@param$anchorForm)
      setnames(statApp2, c("disc", "dif", "azar"), c("a", "b", "c"))      
      statApp2 <- statApp2[!(is.na(a) & is.na(b) & is.na(c)), ]
    }   
    ###############################################################################
    # # Combine two data base of items
    ################################################################################

    statApp1[ , "pos_App1"] <- 1:nrow(statApp1)
    statApp2[ , "pos_App2"] <- 1:nrow(statApp2)
    statComp <- merge(statApp1, statApp2, by = "item", suffixes = c("_App1", "_App2"))
    statComp <- data.frame(statComp)
    statComp[ , "seqItem"] <- 1:nrow(statComp)
    statComp[ , "kRatio"]  <- floor(statComp[ , "TRIED_App2"] / statComp[ , "TRIED_App1"])
    
    # Tamaños muestrales
    iItems <- ddply(statComp, .(TRIED_App1, TRIED_App2), summarize, nItems = length(item))
    
    # Ítema iguales con cambios
    idenItem <- data.frame(item = statComp[ , "item"])
    idenItem[ , "itemI"] <- substr(as.character(idenItem[ , "item"]), 1, 7)
    iItemsChan <- ddply(idenItem, .(itemI), summarize, nItems = length(itemI))

    # Posiciones Items Comunes
    posItemCom <- data.frame(app1 = statComp[ , "pos_App1"], app2 = statComp[ , "pos_App2"])
    # Listas
    param <- list(app1 = subset(statApp1, select = c(a, b, c)), 
                  app2 = subset(statApp2, select = c(a, b, c)))
    # Item comunes
    itemCom <- intersect(statApp1[, "item"], statApp2[, "item"])
    
    ################################################################################
    # # plink - Rescale Stocking-Lord
    ################################################################################
    nitem1 <- nrow(statApp1)
    nitem2 <- nrow(statApp2)  
    # Items
    pm1 <- as.poly.mod(nitem1)
    pm2 <- as.poly.mod(nitem2)
    x <- as.irt.pars(param, posItemCom,
                     cat=list(rep(2, nitem1), rep(2, nitem2)), 
                     poly.mod = list(pm1, pm2))
    out <- plink(x, rescale = "SL", base.grp = 2, D = 1.7)

    ################################################################################
    # # DIFT
    ################################################################################
    pars.out <- link.pars(out)
    
    focalParam <- pars.out$group1[statComp[ , "pos_App1"], ]
    referParam <- pars.out$group2[statComp[ , "pos_App2"], ]
    
    threePlParameters <- list(     focal= focalParam,
                              reference = referParam)
    
    nItems <- nrow(threePlParameters[['focal']])

    ################################################################################
    # # Calculos
    ################################################################################
    ## NCDIF, CDIF and DFT
    ncdif3pl <- Ncdif(itemParameters = threePlParameters, irtModel = object@param@irtModel, focalAbilities = NULL,
                      focalDistribution = "norm", subdivisions = 5000, logistic = FALSE)
    
    cdif3pl <- Cdif(itemParameters = threePlParameters, irtModel = object@param@irtModel, focalAbilities = NULL,
                    focalDistribution = "norm", subdivisions = 5000, logistic = FALSE)
    
    (dtf3plWithCdif <- Dtf(cdif = cdif3pl))
    
    ## Mantel-Haenszel
    
    # # groupRatio programaticamente el calculo
    mh3Pl <- IrtMh(itemParameters = threePlParameters, irtModel = object@param@irtModel,
                   focalDistribution = "norm", referenceDistribution = "norm",
                   focalDistrExtra = list(mean = 0, sd = 1),
                   referenceDistrExtra = list(mean = 0, sd = 1), groupRatio = 87, logistic = FALSE)
    
    delta3Pl <- DeltaMhIrt(mh3Pl)

    ################################################################################
    # # Ncdif - The noncompensatory DIF index.
    ################################################################################
    # Ncdif
    threePlNcdifIpr <- IprNcdif(itemParameterList = threePlIpr, 
                                irtModel = object@param@irtModel, logistic = TRUE)    
    signifAux      <- 1 - object@param$alphaCut
    cutoff3plNcdif <- CutoffIpr(quantiles = c(signifAux), iprStatistics = threePlNcdifIpr,
                                itemParameterList = threePlIpr, itemParameters = nullParameters,
                                itemCovariances = threePlAse,
                                irtModel = object@param@irtModel, statistic = "ncdif")
    
    #################################################################################
    ## # Mantel-Haenszel
    #################################################################################
    ## Mh
    threePlMhIpr <- IprMh(itemParameterList = threePlIpr, 
                          irtModel = object@param@irtModel, logistic = TRUE)
    
    # incluir alphaCut
    signifAux   <- c(object@param$alphaCut / 2, 1 - object@param$alphaCut / 2) 
    cutoff3plMh <- CutoffIpr(quantiles = signifAux, iprStatistics = threePlMhIpr,
                             itemParameterList = threePlIpr, itemParameters = nullParameters,
                             itemCovariances = threePlAse, irtModel = object@param@irtModel, statistic = "mh")

    ################################################################################
    # # Consolida salidas
    ################################################################################
    # Estadísticos
    infoStat <- data.frame(ncdif = ncdif3pl, 
                           cutoffNcdif = cutoff3plNcdif$quantile,
                           cdif = cdif3pl, delta = delta3Pl, dtf = dtf3plWithCdif,
                           mhLow = cutoff3plMh$quantiles[1,], mh = mh3Pl, 
                           mhSup = cutoff3plMh$quantiles[2,])
    
    infoStat[ , "IndDif_Ncdif"] <- ifelse(infoStat[ , "ncdif"] > 
                                   infoStat[ , "cutoffNcdif"], 1, 0)
    infoStat[ , "IndDif_Mh"] <- ifelse(infoStat[ , "mh"] > infoStat[ , "mhSup"] | 
                                infoStat[ , "mh"] < infoStat[ , "mhLow"], 1, 0)
    # Item
    infoItem <- data.frame(focal = focalParam, refer = referParam)
    infoItem <- cbind(infoItem, statComp[, c("item", "seqItem", "TRIED_App1", 
                                         "TRIED_App2")])
    infoItem <- rename(infoItem, c("item" = "Item", "seqItem" = "nItem",
                                   "focal.1" = "a.focal", "focal.2" = "b.focal", 
                                   "focal.3" = "c.focal", "refer.1" = "a.refer", 
                                   "refer.2" = "b.refer", "refer.3" = "c.refer",
                                   "TRIED_App1" = "nSample.focal", 
                                   "TRIED_App2" = "nSample.refer"))
    infoItem[ , "focal"] <- "20161"
    infoItem[ , "refer"] <- "20162"
    
    # Diferencias
    infoItem[, "rfdif.a"] <- round(infoItem[, "a.refer"] - infoItem[, "a.focal"], 2)
    infoItem[, "rfdif.b"] <- round(infoItem[, "b.refer"] - infoItem[, "b.focal"], 2)
    infoItem[, "rfdif.c"] <- round(infoItem[, "c.refer"] - infoItem[, "c.focal"], 2)
    
    # Exporta
    infoExp <- cbind(infoItem, infoStat)
    
    varOrd <- c("focal", "refer", "nItem", "Item", "a.focal", "b.focal", 
                "c.focal", "a.refer", "b.refer", "c.refer", "rfdif.a", "rfdif.b", 
                "rfdif.c", "nSample.focal", "nSample.refer", "ncdif", 
                "cutoffNcdif", "IndDif_Ncdif", "cdif", "delta", "dtf", "mhLow",
                "mh", "mhSup", "IndDif_Mh")
    infoExp <- infoExp[, varOrd]
    infoExp

})

################################################################################
# # Definition of output files
################################################################################

# # Definition of .xlsx output files
setMethod("outXLSX", "DRIFT", 
function(object, srcPath = "."){
  cat("Aún no se encuentra disponible la salida en XLSX----Coming Soon!!")
  outPathPba <- file.path(srcPath, outPath, "03DRIFT")
  # load(file.path(srcPath, object@outFile$pathRdata))
  # pruebasRead <- names(object@datAnalysis)
  # keyPba      <- gsub("(.+)::(.+)", "\\1", pruebasRead)
  # pruebasRead <- split(pruebasRead, f = keyPba)

  # for (prueba in names(pruebasRead)) {
  #   # # version with dict V00 and data _2014_01_28_17_10_35
  #   versionOutput  <- object@verSalida
  #   versionComment <- paste0("Corrida Análisis TCT --",  object@test@nomTest, "--") 

  #   # # guardar xlsx
  #   outFile <- file.path(outPathPba, paste("TCT_V", versionOutput, "_", 
  #                        prueba, ".xlsx", sep = ''))
  #   wb      <- createWorkbook()

  #   # # estilo de las celdas
  #   # # estilo del encabezado
  #   csEnc <- CellStyle(wb) + Font(wb, isBold = TRUE) +
  #            Border(pen = "BORDER_DOUBLE") + Alignment(h = "ALIGN_CENTER")
  #   # # estilo de columnas que reportan porcentajes
  #   csPor <- CellStyle(wb) + DataFormat("0.0%")
  #   # # estilo de columnas que reportan la desviación estándar del por
  #   csDs <- CellStyle(wb) + DataFormat("(0.0%)") +
  #           Alignment(v = "VERTICAL_CENTER") + Border()
  #   csDsE <- CellStyle(wb) + Alignment(v = "VERTICAL_CENTER", wrapText = TRUE) +
  #            Border()
  #   # # estilo de columnas que reportan n
  #   csN <- CellStyle(wb) + DataFormat("#,##0.00") + Font(wb, isItalic = TRUE)
  #   csNE <- CellStyle(wb) + Font(wb, isItalic = TRUE)
    
  #   # # borde
  #   csPC <- CellStyle(wb) + Border() +
  #           Alignment(v = "VERTICAL_CENTER", wrapText = TRUE)
    
  #   # # fuente en negrilla
  #   csNeg   <- CellStyle(wb) + Font(wb, isBold = TRUE)
  #   for (kk in pruebasRead[[prueba]]) {
  #      # # creación de una hoja
  #      auxPru       <- gsub("(::|\\s)","_", kk)      
  #      namesSheet   <- auxPru
  #      corItemIndex <- listResults[[auxPru]][["resulTCT"]]
  #      exGraphPath  <- unique(as.character(corItemIndex[, "pathCMC"]))
  #      textoAlerta  <- listResults[[auxPru]]$txtAlerta
  #      exGraphPath  <- file.path(srcPath, exGraphPath)
  #      assign(namesSheet, xlsx::createSheet(wb, sheetName = auxPru))
     
  #      # # poner el data.frame en la hoja
  #      colTomar <- names(corItemIndex)[names(corItemIndex) != "pathCMC"]
  #      addDataFrame(corItemIndex[, colTomar], sheet = get(namesSheet), startRow = 5,
  #                   startColumn = 1, row.names = FALSE,
  #                   col.names = TRUE, colnamesStyle = csEnc,
  #                   colStyle = list('4' = csN, '5' = csN, '6' = csN,
  #                                   '7' = csN, '8' = csN))
     
  #      # # Agrega el grafico CMC al excel
  #      addPicture(exGraphPath, sheet = get(namesSheet), scale=1,
  #                 startRow=5, startColumn=10)
     
  #      # # Descripcion de los items
  #      pruebasDesc <- object@datAnalysis[[kk]]$dictionary
  #      namesPrueba <- data.frame('Codigo_prueba' = unique(pruebasDesc[, "codigo_prueba"]),
  #                                'Prueba' = versionComment, 
  #                                'Descripción' = gsub("^(.*)(::)(.*)","\\3", kk),
  #                                'nItems' = listResults[[auxPru]][["nObs"]])
     
  #      addDataFrame(t(namesPrueba), sheet = get(namesSheet), startRow = 1,
  #                   startColumn = 1, row.names = TRUE,
  #                   col.names = FALSE, rownamesStyle = csNeg)
     
  #      xlsx::setColumnWidth(get(namesSheet), 1, 15)
  #      xlsx::setColumnWidth(get(namesSheet), 2, 15)
  #      xlsx::setColumnWidth(get(namesSheet), 3, 10)
  #      xlsx::setColumnWidth(get(namesSheet), 4, 10)
  #      xlsx::setColumnWidth(get(namesSheet), 5, 10)
  #      xlsx::setColumnWidth(get(namesSheet), 6, 10)
  #      xlsx::setColumnWidth(get(namesSheet), 7, 10)
  #      xlsx::setColumnWidth(get(namesSheet), 8, 10)
  #      xlsx::setColumnWidth(get(namesSheet), 9, 5)
  #      listResults[[auxPru]]$fileXLSX <- outFile
  #      saveResult(object, listResults, srcPath)
  #   }
  #   saveWorkbook(wb, file = outFile)
  #   cat("Termino Salida: ", outFile, "\n")
  # }
})

# # Definition of .html output files

setMethod("outHTML", "DRIFT", 
function(object, srcPath = "."){
  cat("Aún no se encuentra disponible la salida en HTML")
  # # Identificando Pruebas
  # outPathPba <- file.path(srcPath, outPath, "03TCT")  
  # load(file.path(srcPath, object@outFile$pathRdata))
  # pruebasRead <- names(object@datAnalysis)
  # pruebasRead <- split(pruebasRead, f = gsub("(.+)::(.+)", "\\1", pruebasRead)) 
  # auxPru      <- lapply(pruebasRead, function(x) gsub("(::|\\s)","_", x))
  # auxNombres  <- names(listResults)

  # # # Identificando archivos en excel
  # listXLSX  <- lapply(listResults, function(x) x$fileXLSX)
  # listXLSX  <- lapply(auxPru, function(x) unique(unlist(listXLSX[x])))
  # listALERT <- lapply(listResults, function(x) x$txtAlerta)  

  # # # Juntando subConjunto de una prueba
  # listResults <- lapply(names(listResults), function(x){ 
  #                       return(cbind('pba_subCon' = x, listResults[[x]]$resulTCT))})
  # names(listResults) <- auxNombres
  # listResults <- lapply(auxPru, function(x) do.call(rbind, listResults[x]))
  # cat("<h2> An&aacute;lisis TCT de la prueba:", object@test@nomTest, "</h2>") 

  # cat('<p>El objetivo de este análisis es identificar si los ítems que hacen parte del constructo tienen propiedades, desde el punto de vista de la medición, que permiten ser una fuente de medición óptima para la estimación de un atributo determinado (escala). En particular, a partir de la TCT se establecen dos características importantes que deben ser observadas en los ítems:</p>
  # <ol style="list-style-type: decimal">
  # <li>Confiabilidad: esta hace referencia a la consistencia a través de un conjunto de ítems que se espera que midan el mismo constructo o dimensión teórica.</li>
  # <ul>
  # <li>La medida de la fiabilidad mediante el alfa de Cronbach asume que los ítems (medidos en escala tipo Likert) miden un mismo constructo y que están altamente correlacionados. La fiabilidad de la escala debe obtenerse siempre con los datos de cada muestra para garantizar la medida fiable del constructo en la muestra concreta de investigación.</li>
  # </ul>
  # </ol>
  # <ol start="2" style="list-style-type: decimal">
  # <li>De acuerdo a los análisis de pilotaje se establecieron los siguientes criterios:</li>
  # <ul>
  # <li>Confiabilidades menores a 0.6 son bajas y pueden sugerir dificultades para la estimación de la escala propuesta.</li>
  # <li>Confiabilidades entre 0.6 y 0.75 son bajas pero aceptables.</li>
  # <li>Confiabilidades entre 0.75 y 0.95 son las esperadas.</li>
  # <li>Confiabilidades mayores a 0.95 sugieren redundancia de los ítems que conforman la escala.</li>
  # <li>KR20. Kuder y Richardson desarrollaron un procedimiento basado en los resultados obtenidos con cada ítem. Básicamente, este procedimiento establece la confiabilidad sobre el constructo cuando se elimina cada ítem de manera particular. <strong>Es necesario identificar los ítems para los cuales aumenta la confiabilidad una vez se elimina el ítem (Comentario) </strong></li>
  # <li>Validez. Para aproximarnos a la validez de los ítems dentro del constructo (escala) se analiza la correlación punto biserial (ítems dicotómicos) y poliserial (politómicos). Estas medidas se utilizan para identificar la correlación existente entre dos variables, particularmente se estiman dos tipos de correlación ítem-índice e ítem-prueba. <strong>De acuerdo a los análisis de pilotaje, se estableció el criterio mínimo de correlación de 0.2, tanto para índices polítomicos como dicotómicos. Esto debido a que los análisis de pilotaje mostraron que este es el valor del umbral mínimo tolerable a partir del cual los ítems comienzan a reducir la confiabilidad de las escalas. </strong></li>
  # </ul>
  # </ol>')

  # for (result in names(listResults)){
  #   #x = listResults[[result]]; codPrueba = result; pathExcel = listXLSX[[result]]
  #   textoAlerta <- listALERT[[result]]
  #   tabHtml <- reportTCT(listResults[[result]], codPrueba = result, pathExcel = listXLSX[[result]], 
  #                        alertText = textoAlerta)
  #   cat(as.character(htmltools::tagList(tabHtml)))
  #   totAlpha    <- unique(listResults[[result]][, "alphaTotal"])
    

  #   cat("<b>El coeficiente &alpha; de Cronbach (KR-20) para el total de preguntas de la prueba es de ", 
  #       round(totAlpha, 2), ".", sep = "")
  #   cat("</b>Las figuras presentadas anteriormente corresponden", 
  #       "a la(s) curva(s) de Cronbach-Mesbah para la prueba",
  #       "o subconjuntos de la prueba", sep = "")
  #   cat(" (la cual muestra el valor máximo del coeficiente que se obtiene al
  #     eliminar un ítem sucesivamente).")
  # }

})