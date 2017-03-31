################################################################################
# # RunDimensionalityCC.R
# # R Versions: 3.02
# #
# # Author(s): Maria Fernanda Zarate Jimenez
# #
# # SABER
# # Description: Creates Excel outputs of dimensionality exporatory analysis for each
# #              block of items
# #
# # Inputs: datBlock and dictionaryList
# #
# # Outputs: 04Exploratorio PBA V.xlsx
# #          c("Rnw", "tex")) files
# #
# # File history:
# #   20111205: Creation
# #   20130125: Modification to implement in factors associated
# #   20140502: Modification to generated Excel outputs
# #   20160106: Adaptation for S4 Clases (William Acero and Jorge Carrasco) 
################################################################################
# # Definition of input and output paths
################################################################################
#options(encoding = "UTF-8")

################################################################################
# # Load sourcefiles
################################################################################

source(file.path(funPath, "MeasurementModelsWithLavaan.R"))
source(file.path(funPath, "corItem.R"))
source(file.path(funPath, "log.R"))  # # log
source(file.path(funPath, "exploratoryFunctions.R")) 

################################################################################
# # Definition of class and parameters
################################################################################

# # Heritage class Analysis
Exploratory <- setClass("Exploratory", contains = "Analysis")
setMethod("initialize", "Exploratory", function(.Object, ..., param) {
	.Object@outFile$pathRdata <- "../Output/04Exploratorio/resulEXPO.Rdata"
	.Object <- callNextMethod()
  })


Exploratory <- function(test, paramExp = NULL) { 
  paramDefault <-  list(kOmissionThreshold = 0.5,
						flagOri = FALSE, flagTotal = TRUE,
						flagSubCon = TRUE, orderedDat = TRUE,
						catToNA = c('No Presentado', 'NR', 'Multimarca'),
						seqFactors = NULL, rotation = 'oblimin',
						semilla = format(Sys.time(), "%d%m%Y"),
						tamSize = 0.5)
  if (!is.null(paramExp)) {
    isDefault <- setdiff(names(paramDefault), names(paramExp))
    paramExp  <- c(paramExp, paramDefault[isDefault])
  } else {
    paramExp <- paramDefault
  }
  cat("----->Se correra un analisis exploratorio con los siguientes parametros: \n \n")
  print(paramExp)
  object <- new("Exploratory", test = test, param = paramExp)
  object <- filterAnalysis(object)
  return(object)
}

# # use parameter for correlations
useCorExp      <<- "pairwise.complete.obs"
kThereLoadiExp <<- 0.15
nReplicatesExp <<- 200

# # Puntos de corte para colorear las comunalidades
kThresholdLoadSupExp <<- 0.60
kThresholdLoadMidExp <<- 0.30
kThresholdLoadInfExp <<- 0.15

# # Puntos de corte para colorear las cargas factoriales
kThresholdLoadNCExp  <<- 0.10
kThresholdComSupExp  <<- 0.60
kThresholdComMidExp  <<- 0.30
kThresholdComInfExp  <<- 0.15


###############################################################################
# # Definition of Analysis
################################################################################
setMethod("codeAnalysis", "Exploratory", 
function(object){
    #object <- filterAnalysis(object) # Organizando filtros

    # # Load libraries
    require(xtable)   # # 1.5-6
    require(polycor)  # # 0.7-8
    require(mc2d)     # # 0.1-12
    require(ggplot2)  # # 0.8.9
    require(car)
    require(GPArotation)
    require(xlsx)
    require(pcaPA)
    require(reshape)
    require(RColorBrewer) # # 1.0-5
    #require(kernlab)
    #require(HistogramTools)
    #require(cluster)

	source(file.path("Function", "exploratoryFunctions.R"))
    outPath  <- file.path(outPath, "04Exploratorio")
    dir.create(outPath, showWarnings = FALSE)

	# # version with object parameter
	versionOutput <- object@verSalida

	# # version of input dictionary
	verDataIn <- object@test@verInput

	# # Dimensiones que quiere explorar el usuario
	flagUser <- !is.null(object@param$seqFactors) 
	flagUser <- flagUser | length(object@param$seqFactors) != 0

	# # Crear carpeta de muestras
	outPathSamp <- file.path(outPath, "../Muestras")
  	if(!file.exists(outPathSamp)){
    	dir.create(outPathSamp)
    	cat("Se creo la carpeta muestras en el output!!!!!\n")
  	}

	################################################################################
	# # Analisis de Dimensionalidad
	################################################################################
    auxRdata <- object@outFile$pathRdata

    if (file.exists(auxRdata)){
      load(auxRdata)
      readyList <- unlist(lapply(listResults, names))
      readyList <- paste0(names(readyList), readyList)
    } else {
      readyList <- NULL
    }

	# # create list to save results
	listResults <- list()
	pruebasRead <- names(object@datAnalysis)

	for (kk in pruebasRead) {
	  # # create folder and routes to save results
	  # #  carpetas por prueba

	  auxPru 	 <- gsub("(::|\\s)","_", kk)
	  outPathPba <- file.path(outPath, auxPru)

	  # #  carpetas de muestras por prueba
	  outPathSamPba   <- file.path(outPathSamp, auxPru)
	  outPathPbaGraph <- file.path(outPath, "graficas")
	  dir.create(outPathSamPba, recursive = TRUE, showWarnings = FALSE)
	  dir.create(outPathPbaGraph, recursive = TRUE, showWarnings = FALSE)
    
	  # # keep items that aren't eliminated
	  dictVarPrueba <- object@datAnalysis[[kk]]$dictionary
  	  varId         <- dictVarPrueba[, 'id']
	  
	  if (is.null(object@datAnalysis[[kk]])) {
   	  	warning('No tiene Datos para hacer exploratory', kk)
   	  	next
  	  }

  	  # # Definir la base de datos
  	  kkBlock  <- as.data.frame(object@datAnalysis[[kk]]$datos)
    	 
  	  # # Calcular matrix de correlación
  	  corBlock <- MakeCorrelation(kkBlock, outPathSamPba, verDataIn, auxPru, 
  	                			  semilla = object@param$semilla, 
  	 	            			  tamMue = object@param$tamSize, 
  	 	            			  varId = varId, useCor = useCorExp)
      
      corExpBlock  <- corBlock$corBlock
      expkkBlock   <- corBlock$sampleBlock
  
  
	  ###################################################
	  ### Obtain parallel analysis with exploratory data
	  ##################################################
      if (!paste0(auxPru, "paBlock") %in% readyList) {
      	dir.create(file.path(outPath, "bckPA"), 
      		       showWarnings = FALSE)
      	auxBCKPA <- file.path(outPath, "bckPA",
      		                  paste0("bckPA_", auxPru, "_V",
	                                 versionOutput, ".Rdata"))
      	if (!file.exists(auxBCKPA)){
	       paBlock <- PA(expkkBlock, percentiles = 0.95,
	                     nReplicates = nReplicatesExp, type = "ordered",
	                     use = useCorExp, algorithm = 'polychoric')
	       save(paBlock, file = auxBCKPA)
	    } else {
	       cat(".....Cargando PA previo\n")
	       load(auxBCKPA)
	    }

	    nFactors <- CountEigen.PA(paBlock)
  	    listResults[[auxPru]][["paBlock"]]  <- paBlock
  	    listResults[[auxPru]][["nFactors"]] <- nFactors

	    ###################################################
	    ### # # Obtain parallel analysis plot
	    ###################################################
    
	    paPlotFile <- file.path(outPath, 'graficas', paste("paParalel_", auxPru, "_V",
	                            versionOutput, ".png", sep = ""))
	    paPlot <- plot(paBlock, groupLabel = "",
	                   observed = "Observados",
	                   percentile = " percentil",
	                   xlab = "Eigenvalores ordenados",
	                   ylab = "Eigenvalores", main = "")
        auxLabel    <- c(1:nFactors, rep("", 2 * ncol(expkkBlock) - nFactors))
	    paPlot$data <- cbind(paPlot$data, label = auxLabel)
	    paPlot <- paPlot + geom_text(aes(label = label), size = 2)
	    ggsave(paPlotFile)
	    listResults[[auxPru]][["paPlotFile"]] <- paPlotFile
    	    
	    ###################################################
	    ### code chunk number 7: optionalMultExpl
	    ###################################################
    
	    # # Sugerido por los eigenvalues
	    if(!flagUser){
	  	   cat('---------"seqFactors"... Analisis Paralelo\n')
	       if(nFactors == 1){
	          seqFactors <- c(nFactors, nFactors + 1)
	          resultsExp <- lapply(seqFactors, function(x)
	                            MakeExploratory(x, object@param$rotation,
	                        				    dictVarPrueba, corExpBlock))
	       } else{	     	
	          seqFactors <- c(nFactors - 1, nFactors, nFactors + 1)
	          resultsExp <- lapply(seqFactors, function(x)
	                            MakeExploratory(x, object@param$rotation,
	                        				    dictVarPrueba, corExpBlock))
	       }
	       names(resultsExp) <- paste(seqFactors, 'Factores')
	    }
  
	    # Las dimensiones que quiera explorar el usuario
	    if(flagUser){
	  	  cat('---------"seqFactors"... Definidos por el Usuario\n')
	      if(nFactors == 1){
	         resultsExp <- lapply(seqFactors, function(x) 
	     					    MakeExploratory(x, object@param$rotation,
	     									    dictVarPrueba, corExpBlock) )
	      } else{
	        resultsExp <- lapply(seqFactors, function(x)
	                            MakeExploratory(x, object@param$rotation,
	                        				    dictVarPrueba, corExpBlock) )
	      }
	    }
	    listResults[[auxPru]][["resultsExp"]] <- resultsExp
	    listResults[[auxPru]][["seqFactors"]] <- seqFactors
	   } 	
	   # # Guardando resultados 
       saveResult(object, listResults)
     }
     return(object)
})
################################################################################
# # Definition of output files
################################################################################

setMethod("outXLSX", "Exploratory", 
function(object, srcPath = "."){
	outPath  <- file.path(srcPath, outPath, "04Exploratorio")
	
	#####################################################
	# # Function to Make Cabezotes
	#####################################################
	flagUser <- !is.null(object@param$seqFactors)
	flagUser <- flagUser | length(object@param$seqFactors) != 0
	PutCabezote <-  function(nameSheet, object, kk, isCensal = TRUE){
	  codigo_prueba      <- gsub("^(.*)(::)(.*)","\\1", kk)
	  nItems <- nrow(object@datAnalysis[[kk]]$dictionary)
	  kOmissionThreshold <- object@param$kOmissionThreshold	
	  versionComment 	 <- gsub("^(.*)(::)(.*)","\\3", kk)
	  rotation           <- object@param$rotation	
	  nObsExploratory    <- nrow(object@datAnalysis[[kk]]$datos)
	  codigo_prueba2     <- object@test@nomTest

      # # Cabezote Exploratorio
	  isOmissDel <- kOmissionThreshold < 1 & kOmissionThreshold > 0
	  criterios1 <- c('Código de Prueba', 'Datos de Análisis',
	                  'Uso criterio Omisiones', 'Número de ítems',
	                  'Tipo de Análisis')

	  valores1 <-  data.frame(valor = c(codigo_prueba,
	                    ifelse(isCensal, 'Censal', 'Muestra Nacional'),
	                    ifelse(isOmissDel, 'Sí', 'No'),
	                    nItems, 'policoricas'))

	  cabBlock1 <- data.frame(criterios1, valores1)

	  addDataFrame(cabBlock1, sheet = get(nameSheet), startRow = 1,
	               startColumn = 1, row.names = FALSE,
	               col.names = FALSE, colStyle = list('1' = csNeg))

	  criterios2 <- c('Prueba','n Análisis',
	                  'Criterio para tratamiento de omisiones',
	                  'Comentario', 'Rotación')

	  valores2   <-  data.frame(valor = c(codigo_prueba2,
	                            nObsExploratory, kOmissionThreshold,
	                            versionComment, rotation))

	  cabBlock2  <- data.frame(criterios2, valores2)

	  addDataFrame(cabBlock2, sheet = get(nameSheet), startRow = 1,
	               startColumn = 3, row.names = FALSE,
	               col.names = FALSE, colStyle = list('1' = csNeg) )

	  criterios3 <- c('Selección de número de dimensiones',
	                  'Tratamiento de valores ausentes')

	  valores3   <- data.frame(valor = c(ifelse(flagUser, 'Usuario', 'Paralelo'),
	                          useCorExp))

	  cabBlock3  <- data.frame(criterios3, valores3)

	  addDataFrame(cabBlock3, sheet = get(nameSheet), startRow = 1,
	               startColumn = 5, row.names = FALSE,
	               col.names = FALSE, colStyle = list('1' = csNeg))

	  criterios4 <- data.frame(loads = c(
	                           paste(kThresholdLoadSupExp, ' < |load| < 1.00'),
	                           paste(kThresholdLoadMidExp, ' < |load| < ', kThresholdLoadSupExp),
	                           paste(kThresholdLoadInfExp, ' < |load| < ',
	                                 kThresholdLoadMidExp)))

	  addDataFrame(criterios4, sheet = get(nameSheet), startRow = 3,
	               startColumn = 5, row.names = FALSE, col.names = FALSE)

	  colRetain  <- 5
	  rowRetain  <- 3:5
	  rows       <- xlsx::getRows(sheet = get(nameSheet), rowIndex = rowRetain )
	  cells      <- xlsx::getCells(rows, colIndex = colRetain)

	  posiciones <- outer(rowRetain, colRetain, FUN = "paste", sep = '.')
	  pcl1       <- posiciones[1]
	  pcl2       <- posiciones[2]
	  pcl3       <- posiciones[3]

	  xlsx::setCellStyle(cells[[pcl1]], csL4)
	  xlsx::setCellStyle(cells[[pcl2]], csL3)
	  xlsx::setCellStyle(cells[[pcl3]], csL2)


	  criterios5 <- data.frame(commu = c(
	                           paste(kThresholdComSupExp, ' < comm < 1.00'),
	                           paste(kThresholdComMidExp, ' < comm < ',
	                                 kThresholdComSupExp),
	                           paste(kThresholdComInfExp, ' < comm < ',
	                                 kThresholdComMidExp)))


	  addDataFrame(criterios5, sheet = get(nameSheet), startRow = 3,
	               startColumn = 6, row.names = FALSE, col.names = FALSE)

	  colRetain  <- 6
	  rowRetain  <- 3:5
	  rows       <- xlsx::getRows(sheet = get(nameSheet), rowIndex = rowRetain )
	  cells      <- xlsx::getCells(rows, colIndex = colRetain)

	  posiciones <- outer(rowRetain, colRetain, FUN = "paste", sep = '.')
	  pcl1       <- posiciones[1]
	  pcl2       <- posiciones[2]
	  pcl3       <- posiciones[3]

	  xlsx::setCellStyle(cells[[pcl1]], csC4)
	  xlsx::setCellStyle(cells[[pcl2]], csC3)
	  xlsx::setCellStyle(cells[[pcl3]], csC2)

	}
	load(file.path(srcPath, object@outFile$pathRdata))
	pruebasRead <- names(object@datAnalysis)
	
	for (kk in pruebasRead) {
	  auxPru <- gsub("(::|\\s)","_", kk)
	  # # create folder and routes to save results
	  paBlock    <- listResults[[auxPru]]$paBlock
	  nFactors   <- listResults[[auxPru]]$nFactors
	  paPlotFile <- listResults[[auxPru]]$paPlotFile
	  resultsExp <- listResults[[auxPru]]$resultsExp
	  finseqFact <- listResults[[auxPru]]$seqFactors

      ########################################################
	  # #  SALIDAS A EXCEL
	  ########################################################
	  width              <- list()
	  colorLoadings      <- brewer.pal(3, "Blues")
	  colorCommunalities <- brewer.pal(3, "Greens")
	  colStyle           <- list()

	  wb <- xlsx::createWorkbook()

	  # # cell style
	  # # header style
	  csEnc <- CellStyle(wb) + Font(wb, isBold = TRUE) +
	            Border(pen = "BORDER_DOUBLE") + Alignment(h = "ALIGN_CENTER")
	  # # merge style
	  csEnM <- CellStyle(wb) + Font(wb, isBold = TRUE) +
	            Alignment(h = "ALIGN_CENTER")
	  # # percentages style
	  csPor <- CellStyle(wb) + DataFormat("0.00%")
	  # # estilo de columnas que reportan la desviación estándar del por
	  csDs  <- CellStyle(wb) + DataFormat("(0.0%)") +
	            Alignment(v = "VERTICAL_CENTER") + Border()
	  csDsE <- CellStyle(wb) + Alignment(v = "VERTICAL_CENTER", wrapText = TRUE) +
	            Border()
	  # # estilo de columnas que reportan n
	  csN   <- CellStyle(wb) + DataFormat("0.000") + Font(wb, isItalic = TRUE)
	  csD   <- CellStyle(wb) + DataFormat("0.000")

	  csNE  <- CellStyle(wb) + Font(wb, isItalic = TRUE)
	  # # borde
	  csPC  <- CellStyle(wb) + Border() +
	            Alignment(v = "VERTICAL_CENTER", wrapText = TRUE)
	  # # fuente en negrilla
	  csNeg <- CellStyle(wb) + Font(wb, isBold = TRUE)
	  # # Columna centrada
	  cen   <- CellStyle(wb) + Alignment(h="ALIGN_CENTER")

	  # # estilo de loadings rango 0
	  csL0  <- CellStyle(wb) + DataFormat("0.000")
	  
	  # # estilo de loadings rango 1
	  csL1  <- CellStyle(wb) + DataFormat("0.000") + Font(wb, color = 9)
	  # # estilo de loadings rango 2
	  csL2  <- CellStyle(wb) + DataFormat("0.000") +
	             Fill(foregroundColor=colorLoadings[1])
	  # # estilo de loadings rango 3
	  csL3  <- CellStyle(wb) + DataFormat("0.000") +
	            Fill(foregroundColor=colorLoadings[2])
	  # # estilo de loadings rango 4
	  csL4  <- CellStyle(wb) + DataFormat("0.000") +
	            Fill(foregroundColor=colorLoadings[3])

	  # # estilo de comunalidad rango 1
	  csC1  <- CellStyle(wb) + DataFormat("0.000")
	  # # estilo de comunalidad rango 2
	  csC2  <- CellStyle(wb) + DataFormat("0.000") +
	             Fill(foregroundColor=colorCommunalities[1])
	  # # estilo de comunalidad rango 3
	  csC3  <- CellStyle(wb) + DataFormat("0.000") +
	            Fill(foregroundColor=colorCommunalities[2])
	  # # estilo de comunalidad rango 4
	  csC4  <- CellStyle(wb) + DataFormat("0.000") +
	            Fill(foregroundColor=colorCommunalities[3])


	  # # Salida de resúmen de los items
	  print(names(object@datAnalysis))
	  auxEtiq <- object@datAnalysis[[kk]]$dictionary 
      if (!"etiqu" %in% auxEtiq) {
      	 auxEtiq <- auxEtiq[, "subCon"]
         object@datAnalysis[[kk]]$dictionary[, "etiqu"] <- auxEtiq
      }	  
      varKeepDict   <- c('id', 'subCon', 'etiqu')
	  expItemIndex <- object@datAnalysis[[kk]]$dictionary[, varKeepDict]
      isVacio <- expItemIndex[, 'etiqu'] == '' | is.na(expItemIndex[, 'etiqu'])
	  if (any(isVacio)) {
	      expItemIndex[isVacio, 'etiqu'] <- expItemIndex[isVacio, 'instr']
	  }
      
      nameSheet     <- 'Items'
	  assign(nameSheet, xlsx::createSheet(wb, sheetName = nameSheet))
	  addDataFrame(expItemIndex, sheet = get(nameSheet), startRow = 7,
	               startColumn = 1, row.names = FALSE,
	               col.names = TRUE, colnamesStyle = csEnc)

	  xlsx::setColumnWidth(get(nameSheet), 1, 23)
	  xlsx::setColumnWidth(get(nameSheet), 2, 20)
	  xlsx::setColumnWidth(get(nameSheet), 3, 23)
	  xlsx::setColumnWidth(get(nameSheet), 4, 22)
	  xlsx::setColumnWidth(get(nameSheet), 5, 25)
	  xlsx::setColumnWidth(get(nameSheet), 6, 25)

	   
	  PutCabezote(nameSheet, object = object, kk = kk, isCensal = TRUE)

	  # # Grafica de sedimentación

	  if(!flagUser){
	     nameSheet <- 'Paralelo'
	  } else {
	    nameSheet <- 'Sedimentación'
	  }

	  assign(nameSheet , xlsx::createSheet(wb, sheetName = nameSheet ))
	  PutCabezote(nameSheet, object = object, kk = kk, isCensal = TRUE)

	  titleEigenvalues <- data.frame(tve = 'Eigenvalues')
	  addDataFrame(titleEigenvalues, sheet = get(nameSheet),
	  		       startRow = 7, startColumn = 2,
	               row.names = FALSE, col.names = FALSE,
	               colStyle = list('1' = csEnM))

	  addMergedRegion(sheet = get(nameSheet),
	                  startRow = 7, endRow = 7,
	                  startColumn = 2, endColumn = 6)

	  delta             <- as.numeric(c(' ', -diff(paBlock$observed$eigenValues, 1)))
	  varianceExplained <- paBlock$observed$eigenValues/
	                       sum(paBlock$observed$eigenValues)

	  eigenTable <- data.frame(Observed  = paBlock$observed$eigenValues,
	                           Simulated = paBlock$percentiles$eigenValues,
	                           Delta     = delta,
	                           VarianceExplained = varianceExplained,
	                           VarianceExplainedCum = cumsum(varianceExplained))
      names(eigenTable) <- c('Observed', 'Simulated', 'Delta',
                             '% Retained Variance', '% of cumulative variance retained')

	  addDataFrame(eigenTable, sheet = get(nameSheet), startRow = 9,
	               startColumn = 2, row.names = FALSE,
	               col.names = TRUE, colStyle = list('1' = csN, '2' = csN,
	               '3' = csN, '4' = csPor, '5'= csPor), colnamesStyle = csEnc)

	  paPlotPath <- file.path(outPath, "graficas", paste("paParalel_", auxPru, "_V",
	                          object@verSalida, ".png", sep = ""))
	  addPicture(file = paPlotPath,
	             sheet = get(nameSheet), scale = 1.0,
	             startRow = 9, startColumn = 7)
	  nFact <- data.frame(numF = 'nFactors', nFactors)
	  addDataFrame(nFact, sheet = get(nameSheet), startRow = 19,
	             startColumn = 20, row.names = FALSE,
	             col.names = FALSE,   colStyle = list('1' = csEnc))
	  for(ll in 1:6){
	      width[[ll]]    <- paste("xlsx::setColumnWidth(get(nameSheet),", ll, ",25)", sep='')
	      eval(parse(text = width[[ll]]))
	  }

	  ######################################################
	  # # Salida de los análisis por cada dimension
	  ######################################################

      loadingsList      <- list()
      communalitiesList <- list()
      correlationList   <- list()
      varExplainedList  <- list()
      seqRowVarExp      <- NULL
      nDims             <- length(finseqFact)
      widthRow          <- list()

	  for(jj in 1:nDims){
        nDim           <- finseqFact[jj]
        namesSheet     <- paste(nDim, "Dim", sep = '')
        assign(namesSheet, xlsx::createSheet(wb, sheetName = namesSheet))
        PutCabezote(namesSheet, object = object, kk = kk, isCensal = TRUE)
        varExplained  <- resultsExp[[jj]]$VarExplained
        correlation   <- resultsExp[[jj]]$Correlation
        loadings      <- resultsExp[[jj]]$Loadings
        communalities <- resultsExp[[jj]]$Communalities
        nRowOutVar <- nrow(varExplained)
        nColOutVar <- ncol(varExplained)

	    # # Varianza explicada

	    titleVarExplained <- data.frame(tve = 'Retained Variance')
	    addDataFrame(titleVarExplained, sheet = get(namesSheet),
			         startRow = 7, startColumn = 2,
	                 row.names = FALSE, col.names = FALSE,
	                 colStyle = list('1' = csEnM))

	    addMergedRegion(sheet = get(namesSheet),
	                    startRow = 7, endRow = 7,
	                    startColumn = 2, endColumn = 2 + nColOutVar - 1)

	    addDataFrame(varExplained, sheet = get(namesSheet), startRow = 9,
	                 startColumn = 2, row.names = FALSE,
	                 col.names = TRUE, colnamesStyle = csEnc,
	                 colStyle = list('1' = cen, '2' = csPor))

	    # # Correlaciones
        nRowOutCor <- 9 + nRowOutVar + 2
        nColOutCor <- ncol(correlation) + 1
        titleCorrelation <- data.frame(tc = 'Component Correlation Matrix')
        addDataFrame(titleCorrelation, sheet = get(namesSheet), startRow = nRowOutCor,
                     startColumn = 2, row.names = FALSE, col.names = FALSE,
                     colStyle = list('1' = csEnM))
        addMergedRegion(sheet = get(namesSheet),
                        startRow = nRowOutCor, endRow = nRowOutCor,
                        startColumn = 2, endColumn = ncol(correlation) + 2)

	    # # Agregar formato
	    temp <- 'eliminar'
	    for(ll in 1:ncol(correlation)){
	         colStyle[[ll]] <- paste("'", ll, "'=csC1", sep = '')
	         temp           <- paste(temp, colStyle[[ll]], sep=",")
	    }
	    temp1       <- "list("
	    temp2       <- ")"
	    temp3       <- gsub('eliminar,', '', temp)
	    colStyleDef <- paste(temp1, temp3, temp2)
	    addDataFrame(correlation, sheet = get(namesSheet),
	                startRow = nRowOutCor + 2, startColumn = 2,
	                row.names = TRUE, col.names = TRUE ,
	                colStyle = eval(parse(text = colStyleDef)) )
	    xlsx::setColumnWidth(get(namesSheet), 1, 23)
	    for(ll in 1:max(nColOutVar, nColOutCor)+1){
	       width[[ll]]    <- paste("xlsx::setColumnWidth(get(namesSheet),", ll, ",23)", sep='')
	       eval(parse(text = width[[ll]]))
	    }

	   # # Cargas factoriales
	   nColOutLoa    <- 2 + max(nColOutVar, nColOutCor) + 1
	   titleLoadings <- data.frame(tc = 'Rotate Loadings')
	   addDataFrame(titleLoadings, sheet = get(namesSheet), startRow = 7,
	                startColumn = nColOutLoa, row.names = FALSE,
	                col.names = FALSE, colStyle = list('1' = csEnM))
	   addMergedRegion(sheet = get(namesSheet),
	                   startRow = 7, endRow = 7,
	                   startColumn = nColOutLoa,
	                   endColumn = nColOutLoa + ncol(loadings) - 1)
	   addDataFrame(loadings, sheet = get(namesSheet), startRow = 9,
	                startColumn = nColOutLoa, row.names = FALSE,
	                col.names = TRUE, colnamesStyle = csEnc)

	   # # Agregar color a las cargas
 
	   loadingsI  <- subset(loadings, select = -Item)
	   colRetain  <- seq(nColOutLoa + 1, nColOutLoa + nDim )
	   rowRetain  <- seq(10, 10 + nrow(loadings) - 1)
	   rows       <- xlsx::getRows(sheet = get(namesSheet),
	                             rowIndex = rowRetain )
	   cells      <- xlsx::getCells(rows, colIndex = colRetain)
	   posiciones <- outer(rowRetain, colRetain, FUN = "paste", sep = '.')
	   ld0 <- abs(loadingsI) < kThresholdLoadInfExp & abs(loadingsI) > kThresholdLoadNCExp
	   ld1 <- abs(loadingsI) < kThresholdLoadNCExp
	   ld2 <- abs(loadingsI) > kThresholdLoadInfExp & abs(loadingsI) < kThresholdLoadMidExp
	   ld3 <- abs(loadingsI) > kThresholdLoadMidExp & abs(loadingsI) < kThresholdLoadSupExp
	   ld4 <- abs(loadingsI) >  kThresholdLoadSupExp
	   pcl0 <- posiciones[ld0]
	   pcl1 <- posiciones[ld1]
	   pcl2 <- posiciones[ld2]
	   pcl3 <- posiciones[ld3]
	   pcl4 <- posiciones[ld4]
	   lapply(pcl0, function(x) xlsx::setCellStyle(cells[[x]], csL0))
	   lapply(pcl1, function(x) xlsx::setCellStyle(cells[[x]], csL1))
	   lapply(pcl2, function(x) xlsx::setCellStyle(cells[[x]], csL2))
	   lapply(pcl3, function(x) xlsx::setCellStyle(cells[[x]], csL3))
	   lapply(pcl4, function(x) xlsx::setCellStyle(cells[[x]], csL4))

	   # # Comunalidades

	   nColOutCom  <- nColOutLoa + ncol(loadings) + 1
	   titleCommun <- data.frame(tcm = 'Communalities')
	   addDataFrame(titleCommun, sheet = get(namesSheet), startRow = 7,
	                startColumn = nColOutCom, row.names = FALSE,
	                col.names = FALSE, colStyle = list('1' = csEnM),
	                colnamesStyle = csEnM)

	   addMergedRegion(sheet = get(namesSheet),
	                   startRow = 7, endRow = 7,
	                   startColumn = nColOutCom,
	                   endColumn = nColOutCom + ncol(communalities) - 1)

	   addDataFrame(communalities, sheet = get(namesSheet), startRow = 9,
	                startColumn = nColOutCom, row.names = FALSE,
	                col.names = TRUE, colnamesStyle = csEnc )

	   # # Agregar color a las comunalidades

	   communalitiesI <- subset(communalities, select = -Item)
	   colRetain      <- nColOutCom + 1
	   rowRetain      <- seq(10, 10 + nrow(communalities) - 1)
	   rows           <- xlsx::getRows(sheet = get(namesSheet),
	                                   rowIndex = rowRetain )
	   cells          <- xlsx::getCells(rows, colIndex = colRetain)
	   posiciones     <- outer(rowRetain, colRetain, FUN = "paste", sep = '.')
	   
	   cg1  <- communalitiesI   < kThresholdComInfExp
	   cg2  <- kThresholdComInfExp < communalitiesI & communalitiesI <
	           kThresholdComMidExp
	   cg3  <- kThresholdComMidExp < communalitiesI & communalitiesI <
	           kThresholdComSupExp
	   cg4  <- kThresholdComSupExp < communalitiesI
	   pcg1 <- posiciones[cg1]
	   pcg2 <- posiciones[cg2]
	   pcg3 <- posiciones[cg3]
	   pcg4 <- posiciones[cg4]
       lapply(pcg1, function(x) xlsx::setCellStyle(cells[[x]], csC1) )
       lapply(pcg2, function(x) xlsx::setCellStyle(cells[[x]], csC2) )
       lapply(pcg3, function(x) xlsx::setCellStyle(cells[[x]], csC3) )
       lapply(pcg4, function(x) xlsx::setCellStyle(cells[[x]], csC4) )
       loadingsList[[jj]]      <- resultsExp[[jj]]$Loadings
       communalitiesList[[jj]] <- resultsExp[[jj]]$Communalities
       correlationList[[jj]]   <- resultsExp[[jj]]$Correlation
       varExplainedList[[jj]]  <- resultsExp[[jj]]$VarExplained
	 }



	  #################################################################
	  # # Salida de los resumenes de todas las dimensiones
	  #################################################################

	   nameSheet     <- 'Extracciones'
	   assign(nameSheet, xlsx::createSheet(wb, sheetName = nameSheet))
	   PutCabezote(nameSheet, object = object, kk = kk, isCensal = TRUE)

	   # # Varianzas Explicadas

	   nColOutCom    <- nColOutLoa + ncol(loadings) + 1
	   titleVariance <- data.frame(tvm = '% Retained Variance')
	   addDataFrame(titleVariance, sheet = get(nameSheet), startRow = 7,
	                startColumn = 2, row.names = FALSE,
	                col.names = FALSE, colStyle = list('1' = csEnM) )

	   addMergedRegion(sheet = get(nameSheet),
	                   startRow = 7, endRow = 7,
	                   startColumn = 2, endColumn = 3)

	   nColVarExp      <- 2

	   varExplained    <- as.data.frame(t(sapply(varExplainedList, cbind)))
	   varExplained    <- apply(varExplained, 2, as.numeric)
	   colnames(varExplained) <- c('Component', 'Retained Variance')
	   nColVarExpCon   <- nrow(varExplained)

	   addDataFrame(varExplained, sheet = get(nameSheet),
	                 startRow = 9, startColumn = 2, row.names = FALSE,
	                 col.names = TRUE, colnamesStyle = csEnc,
	                 colStyle = list('2' = csPor) )


	    # # Correlaciones

	   seqRowCorr       <- sapply(correlationList, nrow)
	   seqColCorr       <- sapply(correlationList, ncol)
	   seqRowCorrCum    <- cumsum(seqRowCorr)
	   starRowCorr      <- 9 +  nColVarExpCon + 2

	   titleCorrelation <- data.frame(tvm = 'Component Correlation Matrix')
	   addDataFrame(titleCorrelation, sheet = get(nameSheet),
	                startRow = starRowCorr,
	                startColumn = 2, row.names = FALSE,
	                col.names = FALSE, colStyle = list('1' = csEnM) )

	   addMergedRegion(sheet = get(nameSheet),
	                  startRow = starRowCorr, endRow = starRowCorr,
	                  startColumn = 2,
	                  endColumn = 2 + max(unlist(seqRowCorr)) )

	   for(jj in 1:nDims){

	       temp <- 'eliminar'

	        for(ll in 1:ncol(correlation)){
	            colStyle[[ll]] <- paste("'", ll, "'=csC1", sep = '')
	            temp           <- paste(temp, colStyle[[ll]], sep=",")
	        }

	          temp1       <- "list("
	          temp2       <- ")"
	          temp3       <- gsub('eliminar,', '', temp)
	          colStyleDef <- paste(temp1, temp3, temp2)


	      addDataFrame(correlationList[[jj]], sheet = get(nameSheet),
	                 startRow = starRowCorr  + seqRowCorrCum[jj] + jj,
	                 startColumn = 2, row.names = FALSE,
	                 col.names = TRUE, colnamesStyle = csEnc,
	                 colStyle = eval(parse(text = colStyleDef))
	                )

	   }


	  # # Comunalidades

	   nColOutComStart <- 1 +  max(seqColCorr[nDims], nColVarExp) + 2
	   nColOutComEnd   <- nColOutComStart + nDim
	   titleCommun     <- data.frame(tcm = 'Communalities')
	   addDataFrame(titleCommun, sheet = get(nameSheet), startRow = 7,
	                startColumn = nColOutComStart, row.names = FALSE,
	                col.names = FALSE, colStyle = list('1' = csEnM) )

	   addMergedRegion(sheet = get(nameSheet),
	                   startRow = 7, endRow = 7,
	                   startColumn = nColOutComStart,
	                   endColumn = nColOutComEnd )

	   sumCommu   <- as.data.frame(communalitiesList)
	   sumCommuD  <- subset(sumCommu, select =
	                 grep('Extraction', colnames(sumCommu)))
	   colnames(sumCommuD) <- paste(finseqFact, 'Dimension', sep = '.')

	   addDataFrame(sumCommuD, sheet = get(nameSheet),
	                startRow = 9,
	                startColumn = nColOutComStart, row.names = TRUE,
	                col.names = TRUE, colnamesStyle = csEnc)

	   # # Agregar color a las comunalidades

        colRetain <- seq(nColOutComStart + 1,  nColOutComEnd)
        rowRetain <- seq(10, 10 + nrow(sumCommuD) - 1)
        rows      <- xlsx::getRows(sheet = get(nameSheet),
                                   rowIndex = rowRetain )
        cells     <- xlsx::getCells(rows, colIndex = colRetain)
        posiciones <- outer(rowRetain, colRetain, FUN = "paste", sep = '.')

        cg1 <- sumCommuD  < kThresholdComInfExp
        cg2 <- kThresholdComInfExp < sumCommuD & sumCommuD < kThresholdComMidExp
        cg3 <- kThresholdComMidExp < sumCommuD & sumCommuD < kThresholdComSupExp
        cg4 <- kThresholdComSupExp < sumCommuD

        pcg1 <- posiciones[cg1]
        pcg2 <- posiciones[cg2]
        pcg3 <- posiciones[cg3]
        pcg4 <- posiciones[cg4]

        try(lapply(pcg1, function(x) xlsx::setCellStyle(cells[[x]], csC1) ), TRUE)
        try(lapply(pcg2, function(x) xlsx::setCellStyle(cells[[x]], csC2) ), TRUE)
        try(lapply(pcg3, function(x) xlsx::setCellStyle(cells[[x]], csC3) ), TRUE)
        try(lapply(pcg4, function(x) xlsx::setCellStyle(cells[[x]], csC4) ), TRUE)



	  for(ll in 1:nColOutComEnd){
	      width[[ll]]    <- paste("xlsx::setColumnWidth(get(nameSheet),", ll, ",18)", sep='')
	      eval(parse(text = width[[ll]]))
	  }


	   #############################################################
	   # # Ultima hoja: Resúmen de cargas factoriales
	   #############################################################

	   nameSheet <- 'Loadings'
	   assign(nameSheet, xlsx::createSheet(wb, sheetName = nameSheet))

	 # #  Agregar el titulo

	   loadingsTot <- as.data.frame(loadingsList)
	   expression  <- grep('Item\\.', colnames(loadingsTot))
	   loadingsTot[, expression] <- ''
	   names(loadingsTot) <- gsub("Item.[1-9]", ' ', names(loadingsTot))
	   names(loadingsTot) <- gsub("Factor\\.(\\d)(\\.\\d)*", "Factor \\1", names(loadingsTot))
	   names(loadingsTot) <- car::recode(names(loadingsTot), "NA = '' ")
	   
	      
	   auxTable    <- expItemIndex[, c("id", "etiqu")]
	   names(auxTable) <- gsub("^id$", 'Item', names(auxTable))
	   loadingsTot <- merge(auxTable, loadingsTot, by = "Item")


	   nColOutLoad   <- ncol(loadingsTot) + 1
	   titleLoadings <- data.frame(tlm = 'Rotate Loadings')
	   addDataFrame(titleLoadings, sheet = get(nameSheet), startRow = 7,
	                 startColumn = 2, row.names = FALSE,
	                 col.names = FALSE, colStyle = list('1' = csEnM) )

	   addMergedRegion(sheet = get(nameSheet),
	                   startRow = 7, endRow = 7,
	                   startColumn = 2, endColumn = nColOutLoad )


	   addDataFrame(loadingsTot, sheet = get(nameSheet),
	                startRow = 9, startColumn =  2,
	                row.names = FALSE, col.names =  TRUE,
	                colnamesStyle = csEnc, showNA = FALSE)

	   PutCabezote(nameSheet, object = object, kk = kk, isCensal = TRUE)

   	   # # Agregar color a las cargas

	    loadingsTotI <- loadingsTot[, -1]

		colRetain <- seq(3, ncol(loadingsTot) + 1)
		rowRetain <- seq(10, 10 + nrow(loadings) - 1)
		rows      <- xlsx::getRows(sheet = get(nameSheet),
	                      rowIndex = rowRetain )
		cells     <- xlsx::getCells(rows, colIndex = colRetain)

		posiciones <- outer(rowRetain, colRetain, FUN = "paste", sep = '.')

        ld0  <- (loadingsTotI > -kThresholdLoadInfExp & loadingsTotI < -kThresholdLoadNCExp) |
	             (loadingsTotI > kThresholdLoadNCExp   & loadingsTotI < kThresholdLoadInfExp)

	      ld1  <-  loadingsTotI < kThresholdLoadNCExp & loadingsTotI >
	                 -kThresholdLoadNCExp

	      ld2  <- (loadingsTotI <  kThresholdLoadMidExp & loadingsTotI >
	               kThresholdLoadInfExp) |
	             (loadingsTotI < -kThresholdLoadInfExp & loadingsTotI > -kThresholdLoadMidExp)

	      ld3  <- (loadingsTotI <  kThresholdLoadSupExp & loadingsTotI >
	                kThresholdLoadMidExp) |
	              (loadingsTotI < -kThresholdLoadMidExp & loadingsTotI >
	                -kThresholdLoadSupExp)
	 	ld4  <-  loadingsTotI < -kThresholdLoadSupExp | loadingsTotI > kThresholdLoadSupExp
	    ld5  <-  loadingsTotI == ''

		pcl0 <- posiciones[ld0]
		pcl1 <- posiciones[ld1]
	    pcl2 <- posiciones[ld2]
		pcl3 <- posiciones[ld3]
		pcl4 <- posiciones[ld4]
	    pcl5 <- posiciones[ld5]

	    try(lapply(pcl0, function(x) xlsx::setCellStyle(cells[[x]], csL0) ), TRUE)
	    
	
	    try(lapply(pcl1, function(x) xlsx::setCellStyle(cells[[x]], csL1) ), TRUE)
	    try(lapply(pcl2, function(x) xlsx::setCellStyle(cells[[x]], csL2) ), TRUE)
	    try(lapply(pcl3, function(x) xlsx::setCellStyle(cells[[x]], csL3) ), TRUE)
	    try(lapply(pcl4, function(x) xlsx::setCellStyle(cells[[x]], csL4) ), TRUE)
	    try(lapply(pcl5, function(x) xlsx::setCellStyle(cells[[x]], csL1) ), TRUE)



	    for(ll in 1:(max(nColOutLoad, 6)) ){
	      width[[ll]]    <- paste("xlsx::setColumnWidth(get(nameSheet),", ll, ",18)", sep='')
	      eval(parse(text = width[[ll]]))
	    }

	  outFile <- file.path(outPath,
	                       paste("04Exploratorio_", auxPru,"_V", object@verSalida,
	                             ".xlsx", sep = ''))
	  xlsx::saveWorkbook(wb, file = outFile)
	  listResults[[auxPru]]$fileXLSX <- outFile
	  cat("Termino Salida: ", outFile, "\n")
	}
    saveResult(object, listResults, srcPath)
})


setMethod("outHTML", "Exploratory", 
function(object, srcPath = "."){
	
	load(file.path(srcPath, object@outFile$pathRdata)) # load listResults
	nomPrueba <- object@test@nomTest
	
	cat("<h2>An&aacute;lisis exploratorio de la prueba:", nomPrueba, "</h2>\n")

	cat("A continuación se muestra(n) el(los) gráfico(s) de sedimentación
      para las correlaciones (tetracóricas) de la prueba, así como los
      percentiles 95 estimados mediante análisis paralelo.\n", sep = "")
 
  	cat("El gráfico de sedimentación presenta los valores propios asociados a
      la matriz de correlaciones estimada de los ítems de la prueba, y el
      percentil 95 de los valores propios correspondientes a matrices de
      correlación del mismo tipo de la prueba, pero en
      donde los ítems no se relacionan entre sí.\n\n",
      sep  = "")

  	cat("El gráfico representa cuan fuertes son las asociaciones entre los ítems,
      cuántas dimensiones pueden necesitarse para describir aproximadamente
      estas asociaciones y cuán diferentes son los valores propios, de los que
      se observarían en el caso de que los ítems no se asocien entre sí.\n",
      sep  = "")
  	
  	cat("En este sentido, el gráfico permite identificar la cantidad de dimensiones
      que se desean interpretar; en especial, permite observar si algún conjunto
      de valores propios, a pesar de superar el percentil definido, son muy
      similares a lo que se esperarían cuando los ítems no se asocian y así
      explorar un menor número de dimensiones.\n", sep  = "")

	cat("<table bgcolor=\"#FFFFFF\" border=\"0\" cellpadding=\"2\" cellspacing=\"2\" width=\"100%\" class=\"table-bordered table-condensed\"><tbody>")

     nomSubPru <- names(object@datAnalysis)
     
     for(ii in nomSubPru){
     	
	     nomAux   <- gsub("::|\\s", "_", ii) 
	     pathImg  <- listResults[[nomAux]]$paPlotFile
	     nomSub   <- gsub("^(.+)(::)(.+)", "\\3", ii)
	     nFactors <- listResults[[nomAux]]$nFactors
	     nFactAux <- paste(nFactors,"Factores")
	     expVar1  <- listResults[[nomAux]]$resultsExp[[nFactAux]]$VarExplained[,2]
    
    simpleCap <- function(x) {
      s <- strsplit(x, " ")[[1]]
      paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
    }

    cat("<tr>",
        "<td width=\"80%\">",	     
        "<center>",
        "<b>",
        "<h3 id=\"Exploratory_Header_tab\">",
        "An&aacute;lisis exploratorio de:", nomSub, 
        "</h3>", "</b>", "</center>",
        "</td>", "</tr>")
         cat('<tr>')    
         cat('<td width="100%">')
		 cat("Al realizar un análisis factorial exploratorio se encontró que los primeros ",
             nFactors, " dimensiones recogen el ", round(100 * expVar1, 1), "% de la varianza. \n", sep = "")
         
         cat("Se realizo una rotación ", simpleCap(object@param$rotation), ", la cual redistribuye la varianza explicada por cada dimensión
             buscando que cada ítem pese fuertemente en una sola de las dimensiones
             conservadas, con lo cual se facilita la interpretación de las mismas.\n", sep = "")

		 cat("<center><a href=\"javascript:void(0)\" onclick=\"PopupCenter('", pathImg, 
		 	 "', 'Exploratory Analysis', '944', '900')\"><img src=\"", 
		 	 pathImg,'" alt="alt text" style = "width:472px;height:450px"></a>
		     </center></td>', sep = "")		 
		 cat(paste0('<td> <li class="linkxlscol"><a href="', listResults[[nomAux]]$fileXLSX, 
		 	        '"> Descargar <br> informe Excel </a></li>', 
                    '<li class="linkxlscol"><a href="../../../../Manuales/02_AnalisisDimensionalidad.docx">', 
                    '<b> Manual de <br> interpretación </b></a></li>',
		 	        '</td>'))
	     cat('</tr>\n')
     
     }
     cat('</tbody></table>')
})