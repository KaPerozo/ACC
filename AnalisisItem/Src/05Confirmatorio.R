################################################################################
# # dimensionalityCC.Rnw
# # R Versions: R version 3.02
# #
# # Author(s): María Fernanda Zárate Jiménez
# #
# #
# # Process: SABER 5° y 9° Factores Asociados
# # Description: Genera salidas en excel para llevar a cabo análisis
# #              confirmatorios de dimensionalidad
# # Inputs: datBlock, dictionaryList, confirmatorySample, corConfirmatory.
# #
# # Outputs: confirmatorySample.R, exploratorySample.R, corExploratory.R,
# #          corConfirmatory.R, 05Confirmatorio.xlsx
# #
# # File history:
# #   20111129: Creation
# #   20121126: Lecutura Prueba actividades 5. Paola Marin
# #   20121217: Tablas con diferentes medidas de ajuste para comparacion
# #   de modelos.Paola Marin
# #   20130107: Adaptación para correr FA
# #   20140505: Adaptación para generar salidas en Excel
# #   20150505: Inclución de procesos para poder correer SABER 3, 5 y 9 junto con
# #             SABER 11 y SABER PRO. Jorge Mario Carrasco.
###############################################################################
# setwd("\\\\icfesserv5/academica$/SABER/SABER_2013/20132-EJERCICIO_ANALITEM359-JUNIO2")
options(encoding = "UTF-8")

source(file.path(funPath, "MeasurementModelsWithLavaan.R"))
source(file.path(funPath, "corItem.R"))
source(file.path(funPath, "log.R"))  # # log
source(file.path(funPath, "exploratoryFunctions.R")) 

################################################################################
# # Definition of class and parameters
################################################################################

# # Heritage class Analysis
Confirmatory <- setClass("Confirmatory", contains = "Analysis")
setMethod("initialize", "Confirmatory", function(.Object, ..., param) {
  .Object@outFile$pathRdata <- "../Output/05Confirmatorio/resulCONF.Rdata"
  .Object <- callNextMethod()
})


Confirmatory <- function(test, paramExp = NULL) { 
  paramDefault <-  list(kOmissionThreshold = 0.5,
                        flagOri = FALSE, flagTotal = TRUE,
                        flagSubCon = TRUE, orderedDat = TRUE,
                        catToNA = c('No Presentado', 'NR', 'Multimarca'),
                        seqFactors = NULL, rotation = 'oblimin',
                        semilla = format(Sys.time(), "%d%m%Y"),
                        useCor = "pairwise.complete.obs", 
                        tamSize = 0.5, flagUni = TRUE, flagMultiC  = TRUE,
                        flagMultiNC = TRUE, flagBiFac  =TRUE)
  if (!is.null(paramExp)) {
    isDefault <- setdiff(names(paramDefault), names(paramExp))
    paramExp  <- c(paramExp, paramDefault[isDefault])
  } else {
    paramExp <- paramDefault
  }
  cat("----->Se correra un analisis exploratorio con los siguientes parametros: \n \n")
  print(paramExp)
  object <- new("Confirmatory", test = test, param = paramExp)
  object <- filterAnalysis(object)
  return(object)
}


################################################################################
# # global definitions
################################################################################
setMethod("codeAnalysis", "Confirmatory",
          function(object){
            #object <- filterAnalysis(object) # Organizando filtros
            
            ###############################################################################
            # # Load libraries
            ################################################################################
            require(data.table)   # # 1.5-6
            require(xtable)   # # 1.5-6
            require(polycor)  # # 0.7-8
            require(mc2d)     # # 0.1-12
            require(ggplot2)  # # 0.8.9
            require(lavaan)   # # 0.4-10
            require(semTools) # # 0.2-8
            require(car)
            require(semPlot)  # # 0.2-8
            require(GPArotation)
            require(xlsx)
            require(plyr)
            require(reshape)
            
            ################################################################################
            # # Load sourcefiles
            ################################################################################
            source(file.path(funPath, "MeasurementModelsWithLavaan.R"))
            source(file.path(funPath, "corItem.R"))
            source(file.path(funPath, "confirmatoryFunctions.R"))            
            
            # # Directory Definition
            outPath  <- file.path(outPath, "05Confirmatorio")
            dir.create(outPath)
            
            # # version of input dictionary
            verDataIn <- object@test@verInput
            
            ###########################################################################
            # # Other Functions
            ###########################################################################
            listResults <- list()
            
            for (kk in names(object@datAnalysis)) {
              # # create folder and routes to save results
              # #  carpetas por prueba
              
              auxPru    <- gsub("(::|\\s)","_", kk)
              outPathPba <<- file.path(outPath, auxPru)
              
              # #  carpetas de muestras por prueba
              outPathSamPba   <- file.path(outPath, "../Muestras", auxPru)
              outPathPbaGraph <- file.path(outPathPba, "graficas")
              
              if(!file.exists(file.path(outPath, "../Muestras"))) {
                cat("Se creo la carpeta muestras en el output!!!!!\n")
              } 
              dir.create(outPathSamPba, recursive = TRUE, showWarnings = FALSE)
              dir.create(outPathPbaGraph, recursive = TRUE, showWarnings = FALSE)
              
              # # keep items that aren't eliminated
              dictVarPrueba <- object@datAnalysis[[kk]]$dictionary
              varId         <- dictVarPrueba[, 'id']             
              if (is.null(object@datAnalysis[[kk]])) {
                warning('No tiene Datos para hacer confirmatorio', kk)
                next
              }
              
              if (!'subCon' %in% names(dictVarPrueba)) {
                stop("Falta agregar 'indiceInfo' y 'infoItem'")
              }
              # # variables by index
              dictKk <- subset(dictVarPrueba, select = c(id, subCon))             
              if (nrow(dictKk) == 0) {
                stop('No tiene Items para hacer la corrida en PBA', nomKK,
                     'Revise si todos los Items estan eliminados o si tiene alguna
                     escala diferente a NI')
              }
              
              # # Definir la base de datos
              kkBlock  <- as.data.frame(object@datAnalysis[[kk]]$datos)
              
              # # Eliminar ítems sin afirmación 
              indAfirmacion <- dictVarPrueba[, "subCon"] == "NOFIND"
              if (any(indAfirmacion)) {
                dictVarPrueba <- subset(dictVarPrueba, !indAfirmacion)
                indAfirmacion <- subset(dictVarPrueba, indAfirmacion, select = "id")
                indAfirmacion <- names(kkBlock)[!names(kkBlock) %in% indAfirmacion]
                kkBlock <- kkBlock[,  indAfirmacion]
                
              }
              
              # # Number of observations
              nObsConfirmatory <- nrow(kkBlock)
              
              # # Obtain correlation matrix
              corBlock <- MakeCorrelation(kkBlock, outPathSamPba, verDataIn, auxPru, 
                                          semilla = object@param$semilla, 
                                          tamMue = object@param$tamSize, 
                                          flagExplo = FALSE,
                                          varId = varId, useCor = object@param$useCor)
              
              if(class(corBlock) == "try-error"){
                cat("_____ No se estimo la matriz tetracorica", 
                    "/policorica inicial _______\n\n")
                next
              }
              
              # # Run structural equation model
              vecFlags  <- c('flagUni' = object@param$flagUni,
                             'flagMultiC' = object@param$flagMultiC,
                             'flagMultiNC' = object@param$flagMultiNC,
                             'flagBiFac' = object@param$flagBiFac)
              isUniItem <- length(unique(dictVarPrueba$subCon)) == 1
              if (isUniItem) {
                vecFlags[-1] <- FALSE
              }
              
              # # Ajustes para que en el modelo no confunda los interceptos
              corConfBlockX <- corBlock$corBlock$correlations
              codItemR      <- rownames(corConfBlockX)
              
              for(ii in codItemR) {
                Idx <- paste0('IT', codItemR)
                rownames(corConfBlockX) <- colnames(corConfBlockX) <- Idx
              }
              dictVarPrueba[, 'idx'] <- paste('IT', dictVarPrueba[, 'id'], sep =  '')
              nomAuxPru <- gsub("-", "_", kk)
              for (flag in names(vecFlags[vecFlags])){
                listResults[[auxPru]][[flag]] <- genConfirmatory(dictVarPrueba, corConfBlockX,
                                                                 nObsConfirmatory, nameTest = nomAuxPru, 
                                                                 typeSem = flag, outPathPbaGraph, kk, 
                                                                 verDataIn)
              }
              
              # # Guardando resultados 
              saveResult(object, listResults)
              }
            return(object)    
          })

################################################################################
# # Apply the confirmatory function to each test in controlData
################################################################################
setMethod("outXLSX", "Confirmatory", 
          function(object, srcPath = "."){
            outPath  <- file.path(outPath, "05Confirmatorio")
            wb <<- createWorkbook()
            
            # # version of input dictionary
            verDataIn <- object@test@verInput 
            load(file.path(srcPath, object@outFile$pathRdata))
            pruebasRead <- names(object@datAnalysis)
            
            for (kk in pruebasRead) {
              
              auxPru <- gsub("(::|\\s)","_", kk)
              
              # # Number of observations
              nObsConfirmatory <- nrow(object@datAnalysis[[kk]]$datos)
              
              # # version comentario
              versionComment <- paste0("Corrida Analisis Confirmatorio --", 
                                       object@test@nomTest, "--\n")   
              namesSheet <- c("flagUni" = 'Unidimensional', "flagMultiC" = 'Multidimensional',
                              "flagMultiNC" = 'MultidimensionalNC', "flagBiFac" = 'Bifactorial')             
              
              # # Excluir modelos NO convergencia
              indModelos <- sapply(listResults[[kk]], function(x) class(x$outFitMeas)) != "try-error"
              listResults[[kk]] <- listResults[[kk]][indModelos]
              
              # # Guardando Salidas en Excel
              summarys  <- lapply(listResults[[kk]], function(x) x$sumModMult)
              outGraphs <- sapply(listResults[[kk]], function(x) x$outGraph)
              outFits   <- data.frame(lapply(listResults[[kk]], function(x) x$outFitMeas))
              namesSheet <- namesSheet[names(summarys)]
              createExcelCon('Items', summarys, outGraphs, outFits, model = FALSE,
                             nObsConfirmatory = nObsConfirmatory, items = TRUE, outfit = FALSE, 
                             object = object, kk=kk, versionComment= versionComment)
              mapply(createExcelCon, namesSheet,  summarys, outGraphs, outFits,
                     MoreArgs = list(model = TRUE, items = FALSE, outfit = FALSE, object = object, 
                                     kk=kk, nObsConfirmatory = nObsConfirmatory, 
                                     versionComment= versionComment))
              createExcelCon('Fits',  summarys, outGraphs, outFits, model = FALSE, tColumns = namesSheet,
                             items = FALSE, outfit = TRUE, object = object, kk=kk,
                             nObsConfirmatory =nObsConfirmatory, versionComment= versionComment)
              outFile <- file.path(outPathPba, paste("05Confirmatorio_", kk,"_V", 
                                                     verDataIn, ".xlsx", sep = ''))
              xlsx::saveWorkbook(wb, file = outFile)
              cat("..... Salida en ", outFile, '\n')
            }
          })

setMethod("outHTML", "Confirmatory", 
          function(object, srcPath = "."){
            print("En construcción")
          })