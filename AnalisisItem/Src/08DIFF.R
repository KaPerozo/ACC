################################################################################
# # RunDIF.R
# # R Versions: 3.3.1
# #
# # Author(s): Fabio Tejedor
# #
# # SABER 5° y 9° Citizenship competencies 
# # Description: Creates xlsx output files of DIF analysis for each block
# #              of items
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
DIFF <- setClass("DIFF", contains = "Analysis")
setMethod("initialize", "DIFF", function(.Object, ..., param) {
  .Object@outFile$pathRdata <- "../Output/08DIFF/outList_DIFF.Rdata"
  .Object <- callNextMethod()
})

DIFF <- function(test, paramExp = NULL){
  paramDefault <- list(kOmissionThreshold = 1,
                       flagOri = FALSE, flagTotal = TRUE,
                       flagSubCon = FALSE, orderedDat = TRUE,
                       catToNA = c('No Presentado', 'NR', 'Multimarca'),
                       useCor = "pairwise.complete.obs")
                       # kThereLoadi = 0.15, 
                       # kThresItemCor = 0.2, 
                       # isCheckKeys = FALSE,)
                       # ,flagOri = FALSE, s = TRUE,
                       # flagSubCon = TRUE, orderedDat = FALSE,
                       # idNoPKey = c('O', 'M'), constDmodel = 1.7,
                       # isCheckKeys = FALSE, kThresItemCorrDic = 0.2,
                       # kThresItemCorrOrd = 0.2, espSd = 1, espMean = 0, 
                       # AnclaRdata = NULL, formAncla = "", flagSPrior = FALSE)
  if ("verSalida" %in% names(paramExp)){
    auxVerSalida <- paramDefault$verSalida
  } else {
    auxVerSalida <- 1
  }

  if (!is.null(paramExp)) {
    isNew     <- names(paramExp)[names(paramExp) %in% names(paramDefault)]
    isDefault <- names(paramDefault)[!names(paramDefault) %in% names(paramExp)]
    paramExp  <- c(paramExp[isNew], paramDefault[isDefault])
  } else {
    paramExp <- paramDefault
  }
  cat("-----> Se correra un analisis DIFF con los siguientes parametros: \n")
  print(paramExp)
  object <- new("DIFF", test = test, param = paramExp, 
                verSalida = auxVerSalida)
  object <- filterAnalysis(object)
  return(object)
}


################################################################################
# # Definition of codeAnalysis Method
################################################################################

setMethod("codeAnalysis", "DIFF",
  analDIFF <- function(object){
    cat("O.o--Inicio la corrida de DIFF--o.O\n")
    binPath <- file.path("..", "Src", "bin")
    outPath <- file.path(outPath, "08DIFF")
    dir.create(outPath, recursive = TRUE, showWarnings = FALSE)

    ################################################################################
    # # Load libraries
    ################################################################################
    require(psych)      # # 1.1.10
    require(ggplot2)    # # 0.9.3.1
    require(gtools)     # # 3.2.1
    require(LaF)        # # 0.5
    require(data.table) # # 1.8.10
    require(xlsx)       # # 0.5.5
    require(xtable)  
    require(ggplot2)  
    require(polycor)  
    require(psych)    
    require(car)
    require(sem)
    require(semTools)
    require(polycor)
    require(eRm) # # Version 0.13-0 0.14-0
    require(mirt) # # 0.6.0
    require(GPArotation) # # 0.31
    require(homals) # # 1.0-3
    require(FactoMineR) # # 1.24
    require(plyr) # # 
    require(lavaan)
    require(nnet)

    ###############################################################################
    # # Load wrapWS.R functions
    ################################################################################
    source(file.path(funPath, "log.R"))  # # log
    source(file.path(funPath, "MeasurementModelsWithLavaan.R"))
    source(file.path(funPath, "univariateFunctions01.R"))
    source(file.path(funPath, "DIFAnalysis.R"))
    source(file.path(funPath, "rlV02.R"))
    load(file.path(binPath, "cortesDIF.Rdata"))

    # # version with object parameter
    versionOutput <- object@verSalida
    versionComment <- paste0("Corrida Análisis Funcionamiento Diferencial de los Ítems (DIFF) --",  object@test@nomTest, "--") 

    # # version of input dictionary
    verDataIn <- object@test@verInput

    # # File with forms and SNP's info
    datGroup <- file.path("..", "Output", "outIdentifica.Rdata")

    load(datGroup)

    # # Define la base de datos
    auxPru  <- gsub("\\.con", "", names(object@test@datBlock))
    kkBlock <- as.data.frame(object@datAnalysis[[auxPru]]$datos)
    
    # # Trae diccionario
    fullDictionary <- object@datAnalysis[[auxPru]]$dictionary
    confkkBlockOri <- merge(x = kkBlock,  y = baseIdenti, by = "SNP", all.x = TRUE)
    rm(baseIdenti)

    pru    <- paste0("Test", gsub("(.+)(\\d{3})(.+)", "\\2", auxPru))
    formas <- sort(unique(confkkBlockOri[, pru]))
    # print(head(confkkBlock))
    pruebasToDiff <- t(combn(formas, 2))[,] #-c(1:6)
    print(pruebasToDiff)
    listResults <- list()

    for(ii in 1:nrow(pruebasToDiff)){
      datPru <- subset(confkkBlockOri, eval(parse(text=pru)) %in% pruebasToDiff[ii, ])
      nomIt  <- grep("\\d{6}.+", names(datPru), value = TRUE)
      
      itCompa <- c()
      aa <- sapply(names(datPru[, nomIt]), function(z){
        nomTab <- names(table(as.numeric(as.character(datPru[, z])), useNA = "if"))
        if (all(is.na(nomTab))) {
          # cat(z, "No pertenece a ninguna forma\n")
        } else if (all(nomTab %in% c("0", "1"))){
          # cat(z, "Es un ítem compartido\n")
          itCompa <<- rbind(itCompa, z)
        } else if (all(c("0", "1", NA) %in% nomTab)) {
          # cat(z, "Pertenece a una de las formas\n")
          # datatoDicDif[!is.na(datatoDicDif[, z]), c(z, pru)]
        }
        
        # table(as.numeric(as.character(datatoDicDif[, z])), useNA = "if")
        datPru[, z] <<- as.numeric((datPru[,z]))
      })
      rm(aa)

      itCompa    <- c(itCompa)
      grouptoDif <- as.numeric(factor(datPru[, pru]))

      namDiff         <- paste(gsub("(.+)(\\.con)", "\\1", pruebasToDiff[ii, ]), collapse="--DIFF--")
      blockStrucUbyIn <- data.frame(indicator = paste0("V", itCompa), factor = namDiff)

      listVaInterest      <- pru
      # dictVarPruebaIndSub <- subset(fullDictionary, id %in% itCompa)
      dictVarPruebaIndSub <- fullDictionary
      # # Crea variable con el nombre, sí no viene originalmente
      if (!"prueba" %in% names(dictVarPruebaIndSub)) {
        dictVarPruebaIndSub[, "prueba"] <- object@test@nomTest 
      }
      # confkkBlock <- datPru
      # rm(datPru)
      # # 
      # cat(print(cutoffDIF[[1]][1,]))
      DIFAnalysis(confkkBlock = datPru, blockStrucUbyIn, 
                  listVaInterest, dictVarPruebaIndSub, 
                  cortesDiff = cutoffDIF, auxiPru = auxPru, itCompa)
      cat("Finalice el DIFF para --> ", namDiff, "comparten ", length(itCompa), "\n")
      gc(reset = TRUE)
    }      
  })

################################################################################
# # Definition of output files
################################################################################

# # Definition of .xlsx output files
setMethod("outXLSX", "DIFF", 
function(object, srcPath = "."){
  cat("Aún no se encuentra disponible la salida en XLSX----Coming Soon!!")
  # outPathPba <- file.path(srcPath, outPath, "03TCT")
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

setMethod("outHTML", "DIFF", 
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