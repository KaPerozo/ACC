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
                 useCor = "pairwise.complete.obs"; 
                 tamSize = 0.5)
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

setGeneric(name = "confirmatAnalysis", def = function(object, ...){standardGeneric("confirmatAnalysis")})

# object <- controlData[[prueba]]
# object <- controlData[['./SABER9/SA20131/JP_M10_M11_M15_M16_M17_M19_M20_M21_M22_M23_M24_M3/PBAM10']]
setMethod("confirmatAnalysis", "Test",
          function(object, auxAnalisis){
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
            
            useCor = "pairwise.complete.obs", 
            useMatrixCorr = 'policoricas', 
            kOmissionThreshold  = 0.8, 
            codesRasch = c('01', '02'), 
            kCodNElim = '06',
            kCodPar   = '01',
            kCodMod   = '00',

            flagUni     <- TRUE, flagMultiC  <- TRUE,
            flagMultiNC <- TRUE, flagBiFac   <- TRUE
                                  
            ###########################################################################
            # # Other Functions
            ###########################################################################
                                             
            versionComment <- paste0("Corrida Análisis Confirmatorio --", 
                                     object@test@nomTest, "--\n")
            listResults <- list()
          
            for (kk in names(object@datAnalysis)) {
              # # create folder and routes to save results
              # #  carpetas por prueba
          
              auxPru    <- gsub("(::|\\s)","_", kk)
              outPathPba <- file.path(outPath, auxPru)
          
              # #  carpetas de muestras por prueba
              outPathSamPba   <- file.path(outPath, "../Muestras", auxPru)
              outPathPbaGraph <- file.path(outPath, "graficas")

              if(!file.exists(file.path(outPath, "../Muestras"))) {
                cat("Se creo la carpeta muestras en el output!!!!!\n")
              } 
              dir.create(outPathSamPba, recursive = TRUE, showWarnings = FALSE)
              dir.create(outPathPbaGraph, recursive = TRUE, showWarnings = FALSE)
              
              # # keep items that aren't eliminated
              dictVarPrueba <- object@datAnalysis[[kk]]$dictionary
              varId         <- dictVarPrueba[, 'id']             
              if (is.null(object@datAnalysis[[kk]])) {
                  warning('No tiene Datos para hacer exploratory', kk)
                  next
              }
              
              # # variables by index
              dictKk <- subset(dictVarPrueba, select = c(id, indice))             
              if (nrow(dictKk) == 0) {
                stop('No tiene Items para hacer la corrida en PBA', nomKK,
                      'Revise si todos los Items estan eliminados o si tiene alguna
                      escala diferente a NI')
              }
              
              # # Definir la base de datos
              kkBlock  <- as.data.frame(object@datAnalysis[[kk]]$datos)
              
              # # Number of observations
              nObsConfirmatory <- nrow(kkBlock)
              
              # # Obtain correlation matrix
              corBlock <- MakeCorrelation(kkBlock, outPathSamPba, verDataIn, auxPru, 
                                          semilla = object@param$semilla, 
                                          tamMue = object@param$tamSize, 
                                          flagExplo = FALSE,
                                          varId = varId, useCor = useCorExp)

              if(class(corBlock) == "try-error" | isUniItem){
                  cat("_____ No se estimo la matriz tetracorica", 
                      "/policórica inicial _______\n\n")
                  next
              }

              # # Run structural equation model
              vecFlags  <- c('flagUni' = object@param$flagUni,
                             'flagMultiC' = object@param$flagMultiC,
                             'flagMultiNC' = object@param$flagMultiNC,
                             'flagBiFac' = object@param$flagBiFac)

              
              isUniItem <- length(unique(dictVarPrueba$indice)) == 1
              if (isUniItem) {
                vecFlags[-1] <- FALSE
              }
              
              # # Ajustes para que en el modelo no confunda los interceptos
              corConfBlockX <- corBlock$cor
              codItemR      <- rownames(corConfBlockX)
        
              for(ii in codItemR) {
                Idx <- paste0('IT', codItemR)
                rownames(corConfBlockX) <- colnames(corConfBlockX) <- Idx
              }

              dictVarPrueba[, 'idx'] <- paste('I', dictVarPrueba[, 'id'], sep =  '')
              


              return(auxAnalisis)
            }
          }
     return(object)    
    })

################################################################################
# # Apply the confirmatory function to each test in controlData
################################################################################
setMethod("outXLSX", "Exploratory", 
function(object, srcPath = "."){
   outPath  <- file.path(srcPath, outPath, "04Exploratorio")
   wb <<- createWorkbook()
   # # header style
   csEnc <- CellStyle(wb) + Font(wb, isBold = TRUE) +
   Border(pen = "BORDER_DOUBLE") + Alignment(h = "ALIGN_CENTER")
   
   # # percentages style
   csPor <- CellStyle(wb) + DataFormat("0.0%")
   # # estilo de columnas que reportan la desviación estándar del por
   csDs <- CellStyle(wb) + DataFormat("(0.0%)") +
           Alignment(v = "VERTICAL_CENTER") + Border()
   csDsE <- CellStyle(wb) + Alignment(v = "VERTICAL_CENTER", wrapText = TRUE) +
            Border()              
   # # estilo de columnas que reportan n
   csN  <- CellStyle(wb) + DataFormat("0.000") + Font(wb, isItalic = TRUE)
   csNE <- CellStyle(wb) + Font(wb, isItalic = TRUE)
                 # # borde
   csPC <- CellStyle(wb) + Border() +
           Alignment(v = "VERTICAL_CENTER", wrapText = TRUE)
   # # fuente en negrilla
   csNeg <- CellStyle(wb) + Font(wb, isBold = TRUE)
   # # Columna centrada
   cen  <- CellStyle(wb) + Alignment(h="ALIGN_CENTER")              
   namesSheet <- c('Unidimensional', 'Multidimensional',
                   'MultidimensionalNC', 'Bifactorial')             
   
   # # Guardando Salidas en Excel
   summarys  <- list(sumModUni = sumModUni, sumModMult = sumModMult,
                     sumModMultNR = sumModMultNR, sumModBifa = sumModBifa)
   outGraphs <- c(outGraphUni, outGraphMult, outGraphMultNR, outGraphBifa)
   outFits   <- data.frame(outFitMeasUni, outFitMeasMult,
                          outFitMeasMultNR, outFitMeasBifa)
   CreateExcel('Items', summarys, outGraphs, outFits, model = FALSE,
               items = TRUE, outfit = FALSE)
   mapply(CreateExcel, namesSheet,  summarys, outGraphs, outFits,
           model = TRUE, items = FALSE, outfit = FALSE)
   CreateExcel('Fits',  summarys, outGraphs, outFits, model = FALSE,
               items = FALSE, outfit = TRUE)
   
   outFile <- file.path(outPathPba, ".xlsx", sep = '')
   xlsx::saveWorkbook(wb, file = outFile)
   cat("..... Salida en ", outFile, '\n')
})

setMethod("outHTML", "Exploratory", 
function(object, srcPath = "."){
  print("En construcción")
})