################################################################################
# # pruebaClass.R
# # R Versions: R version 3.0.0 i386
# #
# # Author(s): Jorge Mario Carrasco
# #
# # SABER 359, SABER PRO, SABER 11 y ACC
# # Description: Función creada para definar la clase Test, la clase Analysis y
# #              reporte test para las definir las funciones de análisis de item.
# #
# # Outputs: Funciones para crear objetos y metodos
# #
# # File history:
# #   20150914: Creation
# #   20160516: Modificación de las clases test y Análisis
# # ToDo:
# #       Inclución de FlagCensal para el caso de 359 y ACC
################################################################################

################################################################################
# # Definición clase Test
################################################################################

Test <- setClass("Test", 
 		# # Definir la estructura
 		slots = c(path           = 'character',  # Ruta de la carpeta 
 			        dictionaryList = 'list',		   # Diccionario de variables
 			        datBlock       = 'list',  	   # Datos leidos
 			        exam           = 'character',  # (SABER359, SABER11, SABERPRO)
              periodo        = 'character',  # (AC20142, EK2015)
	            verInput       = 'numeric',    # Version con el que se genera diccionarios
	            nomTest        = 'character',  # Nombre para impresion en los reportes
	            paramLect      = 'list',       # Parametros de lectura de la prueba
	            codMod         = 'character',
              listAnal       = 'list',
              isUpdate       = 'logical'), # Codigos del modelo de la prueba

 		# # Definir los valores por defecto
 		prototype = list(verInput = 1, path = "", exam = "", codMod = "02",
					  	       nomPrueba = "", paramLect = list(conDirs = ""), 
                     isUpdate = TRUE), 
 		validity  = function(object){ 			 			
      auxModelos <- data.frame('codMod' = c("00", "01", "02", "03", "04", "05", "06", "07"),
                               'model'  = c("noModel", "PCM", "RSM", "GRM", "MRD", "1PL", "2PL", "3PL"))		
 			if(!object@codMod %in% auxModelos[, "codMod"]){
 				cat("No existe el codigo del modelo: ", object@codMod)
 				print(auxModelos)
				stop("\nSeleccionar entre los modelos anteriores")
			}
 			if(object@path == "")
 				stop("Se debe definir el directorio del examen")
 			if(object@paramLect$conDirs == "")
 				stop("El parametro 'conDirs' debe contener almenos un .con")
 			return(TRUE)
 		})

# # Methods's Test Class
setGeneric(name = "readSupplies", def = function(object){standardGeneric("readSupplies")})

setMethod("readSupplies", "Test",
function(object){
 
  ################################################################################
  # # Definition of input and output paths
  ################################################################################
  outPath <- file.path("..", "Output", "00Crear")
  dir.create(outPath, showWarnings = FALSE, recursive = TRUE)

  ################################################################################
  # # Source of scripts with functions
  ################################################################################
  source(file.path(funPath, "partirComas.R"), local = TRUE)  
  source(file.path(funPath, "readFilesAI02.R"), local = TRUE)   
  
  ################################################################################
  # # Validation for reading a Test
  ################################################################################
	controlPrueba <- object
	controlAnal   <- object@paramLect

    # # Parameters validation
    if (controlPrueba@exam == "" | is.na(controlPrueba@exam))
	    stop("**ERROR** No se espesifico el nombre de la prueba que se quiere procesar")
    if (controlPrueba@path == "" | is.na(controlPrueba@path))
	    stop("**ERROR** Se debe especificar una ruta para cada prueba")
	  if (!file.exists(file.path(inPath, controlPrueba@path)))
		  stop("**ERROR** El directorio de la prueba no existe:", file.path(inPath, controlPrueba@path))
    if (is.na(controlPrueba@verInput))
	    stop("**ERROR** Se debe especificar la versión de entrada")
    if (!"conDirs" %in% names(controlAnal))
	    stop("**ERROR** Se debe especificar el vector de .con que se quieren Leer")
    if (length(controlAnal$conDirs) == 0)
	    stop("**ERROR** Se debe especificar almenos un .con en los parametros de lectura")
    if (controlPrueba@exam == "ACC"){
	    if (!"Estructura" %in% names(controlAnal))
		    stop("**ERROR** Para procesar una prueba NO cognitiva se debe asignar un diccionario")
	    if (length(controlAnal$Estructura) == 0)
		    stop("**ERROR** Se debe especificar almenos un .con en los parametros de lectura")
    } else {
	    if (!"Estructura" %in% names(controlAnal))
		    warning("\n>>>O_O>>> No se especifico el archivo de Estructura\n",
			        "          el diccionario se creara segun la descarga.\n")
    }

    ################################################################################
    # # global definitions
    ################################################################################

    # # name of Rdata files

    if (controlPrueba@exam == "ACC"){
      datDictionary <- file.path(outPath, 
                           paste("dictionaryList_V",
                                 controlPrueba@verInput, ".RData", sep = ""))
      datReadBlock  <- file.path(outPath, 
                           paste("datBlock_V",
                                 controlPrueba@verInput, ".RData", sep = ""))
    } else {
      auxExamen     <- gsub("(.+)?(exam\\d+).+", "\\2", controlPrueba@path)
      auxExamen     <- ifelse(nchar(auxExamen) > 0, auxExamen, controlPrueba@periodo)
      print(auxExamen)
      datDictionary <- file.path(outPath,
                               paste("dictionaryList_",
                                     gsub(".+(\\\\|\\/)(.+)$", "\\2", controlPrueba@path), 
                                     "_", auxExamen, "_V",
                                     controlPrueba@verInput, ".RData", sep = ""))
      datReadBlock  <- file.path(outPath,
                               paste("datBlock_",
                                     gsub(".+(\\\\|\\/)(.+)$", "\\2", controlPrueba@path), 
                                     "_", auxExamen, "_V",
                                     controlPrueba@verInput, ".RData", sep = ""))
    }

    ################################################################################
    # #  reading  dictionary and DB
    ################################################################################
    inFolder <- controlPrueba@path

    if (controlPrueba@exam == "ACC") {
    	if (!file.exists(datDictionary)){
		    cat("Nombre del diccionario - ->", controlAnal$Estructura, "\n")
		    # # Reading the project dictionary
		    dictionaryList <- ReadDict(fileName = controlAnal$Estructura,
                                   variables = controlAnal$nameSheet, 
                                   categories = "OpcResp", model = "model",
	            	                   index = "escalas", collapse = "colapsa",
	                	               desElim = "elimina")
        attr(dictionaryList, "fileRdata") <- datDictionary
		    save(dictionaryList, file = datDictionary)
		  } else {
			  cat("Cargando el diccionario - ->", controlAnal$Estructura, "\n")
		   	load(datDictionary)
		  } 
		  datBlock <- ReadDataAI(folderName = inFolder, dict = dictionaryList,
	                           multiMarkOmiss = FALSE, verbose = TRUE,
	                           eliminatedVars = FALSE)
		  save(datBlock, file = datReadBlock)
    } else {
      # # Reading the project dictionary
      if (!file.exists(datDictionary)){
      	dictionaryList <- con2Dict(object)
        attr(dictionaryList, "fileRdata") <- datDictionary
        save(dictionaryList, file = datDictionary)

      } else {
      	load(datDictionary)
      }
      # # Reading the DB using generic dictionary (only for dichotomous items)
      if (is.null(object@paramLect$valMUO))
        object@paramLect$valMUO <- 9
	      datBlock <- ReadGeneric(object, dict = dictionaryList, verbose =  FALSE, 
                                valMUO = object@paramLect$valMUO, 
                                eliminatedVars = FALSE, multiMarkOmiss = FALSE)
	    save(datBlock, file = datReadBlock)
    }    
    # # Save directions of Rdata and datDictionary    
    object@dictionaryList <- dictionaryList
    object@datBlock       <- datBlock

    return(object)
  })


setMethod("initialize", "Test", function(.Object, ..., isUpdate = TRUE) {    
    .Object <- callNextMethod()
   	if (length(.Object@path) != 0){
    		if (length(.Object@dictionaryList) == 0 & length(.Object@datBlock) == 0){
          if (isUpdate){
            .Object <- readSupplies(.Object)  
          } else {
            cat("#############################################################\n",
                "se uso el parametro 'testUpdate' no se correra ", .Object@nomTest,
                "\n #############################################################\n\n")
          }        
    		}    	
    }
    .Object
})

################################################################################
# # Definición Clase Analysis
################################################################################

Analysis <- setClass("Analysis",  					  
 					    # # Definir la estructura                
 					    slots = c(
 					  	   test        = 'Test',
 					  	   datAnalysis = 'list',
 					  	   param       = 'list', 
 					  	   inputFile   = 'list',
 					  	   outFile     = 'list', 
 					  	   verSalida   = 'numeric', 
                 pathCor     = 'character', 
                 pruebasRead = 'character'),  					  
 					    # # Definir los valores por defecto
 					    prototype = list(test = NULL, scripName = "", param = list(), 
 					                    inputFile = list(), outFile = list(pathRdata = "")), 
 					    validity  = function(object){ 					  	 					  	
 					  	   if (object@outFile$pathRdata == "")
 					  		  return("Se debe definir un directorio de salida de resultados (Rdata)")
 					      return(TRUE)})

setMethod("initialize", "Analysis", function(.Object, ..., test, param, verSalida) {
	.Object <- callNextMethod()
    if(missing(test)){
      stop("Se debe especificar un objeto 'test = '??? ")
    }
    if(missing(param)){
      stop("Se debe definir los parametros de la clase --> ", class(.Object))
    } else {
      .Object@param <- param    
    }
    if(missing(verSalida)){
      .Object@verSalida <- 1
    }
    .Object@test <- test    
    auxClase        <- class(.Object)
    methodAnalysis  <- attr(methods(class = auxClase), "info")[, "generic"]
    if (!"codeAnalysis" %in% methodAnalysis)
      stop("-- Se debe definir el metodo 'codeAnalysis' para la clase --> ", auxClase)
    if (!"outXLSX" %in% methodAnalysis)
      stop("-- Se debe definir el metodo 'outXLSX' para la clase --> ", auxClase)
    if (!"outHTML" %in% methodAnalysis)
      stop("-- Se debe definir el metodo 'outHTML' para la clase --> ", auxClase)
    .Object
  })

# # Metodos de la clase Análisis
setGeneric(name = "getParams", def = function(object){standardGeneric("getParams")})
setMethod("getParams", "Analysis", function(object){return(object@param)})

setGeneric(name = "getInput", def = function(object){standardGeneric("getInput")})
setMethod("getInput", "Analysis", function(object){return(object@inputFile)})

setGeneric(name = "getOutput", def = function(object){standardGeneric("getOutput")})
setMethod("getOutput", "Analysis", function(object){return(object@outFile)})

setGeneric(name = "filterSubsets", 
           def = function(object, recodeNA = FALSE, catToNA = NULL, kOmissionThreshold = 1, 
                          orderedDat = FALSE, kCodNElim = "06", kCodPar = NULL, kCodMod = NULL, 
                          columnId = "SNP", flagNI = FALSE, filPrueba = NULL , 
                          filIndex = NULL, ...){standardGeneric("filterSubsets")})
setMethod("filterSubsets", "Analysis", 
  function(object, ...){
    # # Package in R
    require(plyr)
    require(data.table)

    # # filtering dictionary
    for(blockDat in names(object@datAnalysis)){
      dictionaryList <- object@datAnalysis[[blockDat]]$dictionary
      # # Default values of some fields
      if(!"paralelo" %in% names(dictionaryList)){
        dictionaryList$paralelo <- "01"        
      }

      if(!"subCon" %in% names(dictionaryList)){
        dictionaryList$subCon <- dictionaryList$codigo_prueba        
      } 

      # # Filtering items
      isNoElim   <- dictionaryList[, 'elimina' ] == kCodNElim  

      if (!is.null(kCodMod)){
        isCodMod <- dictionaryList[, 'codMod'] == kCodMod     
      } else {
        isCodMod <- rep(TRUE, nrow(dictionaryList))
      }      
      if (!is.null(kCodPar)){
        isCodPar <- dictionaryList[, 'paralelo'] == kCodPar     
      } else {
        isCodPar <- rep(TRUE, nrow(dictionaryList))
      }   

      if (object@test@exam == "ACC") {
        if (flagNI){
          isNI     <- dictionaryList[, 'subCon'] != 'NI' 
          isPrueba <- isCodMod & isNoElim & isCodPar & isNI
        }
      } else {
        isPrueba   <- isCodMod & isNoElim & isCodPar
      }     
  
      # # Filtering items by 'codigo_prueba' and 'subCon'
      if (!is.null(filPrueba)) {
        flagPru  <- dictionaryList[, 'codigo_prueba'] %in% filPrueba        
        isPrueba <- isPrueba & flagPru
      }
      if (!is.null(filIndex)) {
        flagIndex <- dictionaryList[, 'subCon'] %in% filIndex
        isPrueba  <- isPrueba & flagIndex
      }
      dictionaryList <- dictionaryList[isPrueba, ]
      object@datAnalysis[[blockDat]]$dictionary <- dictionaryList
    }

    # # Conserved data from sample
    if(object@test@exam %in% c("ACC", "SABER359")){
      if (is.null(object@param$kApli)) {
        stop("ERROR_____ Definir kApli en los parametros (.json)")
      }
      object@datAnalysis <- lapply(object@datAnalysis, function(x){
                                   x$datos <- subset(x$datos, x$datos$tipoApli %in% object@param$kApli)
                                   x$datos <- data.frame(x$datos)
                                   names(x$datos) <- gsub("X(\\d+)", "\\1", names(x$datos))
                                   return(x)})
    } else {
      object@datAnalysis <- lapply(object@datAnalysis, function(x) {
                                   x$datos <- data.frame(x$datos)
                                   names(x$datos) <- gsub("X(\\d+)", "\\1", names(x$datos))
                                   return(x)})
    }

    RecodeToNA <- function (variable, categories){
     # # Recode the categories of variable to NA
     # #
     # # Arg:
     # #  variable[character|factor]: variable to recode
     # #  categories[character]: levels of variable recoding to NA
     # #
     # # Ret:
     # #  the variable after recode
     # # levels of variable
         if (is.null(categories)) 
           stop("... Se debe definir el parametro 'catToNA'")
         levelsVar <- levels(variable)
         levelsVar <- levelsVar[!(levelsVar %in% categories)]
         isCat     <- variable %in% categories
         if (any(isCat)) {
           variable[isCat] <- NA
         }
         variable <- ordered(variable, levels = levelsVar)
         return(variable)
    } 

    # # Recode 'Multimarca' 'No aplica' 'NR' for NA
    recodeLA <- function(subDat){
        varId  <- intersect(subDat$dict$id, names(subDat$datos))
        if (length(varId) == 0){
          return(NULL)
        } 
        dataBlo <- data.table(subDat$datos[, c(columnId, varId)])       
        if (recodeNA) {          
          if (object@test@exam == "ACC") {
            dataBlo[, ] <- lapply(dataBlo, RecodeToNA, catToNA)  
          }                
        }  
        if (orderedDat){
          dataBlo[, ] <- lapply(dataBlo[, ], function(x) ordered(x))
        }
        subDat$datos <- dataBlo      
        return(subDat)
    }
    object@datAnalysis <- lapply(object@datAnalysis, recodeLA)
        
    # # conserve rows with less than kOmissionThreshold NR data
    isOmissDel <- kOmissionThreshold < 1 & kOmissionThreshold > 0
    if (isOmissDel) {
      delOmiss <- function(data){
        if (is.null(data$datos)){
          return(NULL)
        } else {
          if (nrow(data$datos) == 0)
             return(NULL)
        }
        misRow  <- rowMeans(data$datos == "")
        isKeep  <- misRow <= kOmissionThreshold
        data$datos    <- data$datos[isKeep, ]
        return(data)       
      }
      object@datAnalysis <- lapply(object@datAnalysis, delOmiss)
    }
    return(object)
})

setGeneric(name = "buildSubsets", 
           def = function(object, flagTotal = TRUE, flagSubCon = FALSE, 
                          flagOri = FALSE, fileExt = NULL, 
                          infoItem = list('sheetExt' = NULL, 
                          'id' = NULL, 'id_Subset' = NULL), ...){standardGeneric("buildSubsets")})
setMethod("buildSubsets", "Analysis", 
  function(object, ...){
    auxDictionary  <- object@test@dictionaryList$variables
    auxParGet      <- ifelse(flagOri, "oriBlock", "calBlock")
	  if(!is.null(fileExt)){
		  if(class(fileExt))
			  stop("ERROR.... O_O Se debe definir la ruta de un archivo de Excel (parametro 'fileExt')")
		  if(!gsub("(.+)\\.(.+)", "\\2", fileExt) %in% c("xlsx", "xls"))
			  stop("ERROR.... O_O Su archivo no es de Excel (parametro 'fileExt')")
		  if(is.null(infoItem$sheetExt))
			  stop("ERROR.... O_O Se debe definir la hoja del archivo (parametro 'infoItem$sheetExt')")
		  if(is.null(infoItem$id) | is.null(infoItem$id_Subset))
			  stop("ERROR.... O_O Se debe elegir las columnas a tomar (parametro 'infoItem')")
		  if(!file.exists(fileExt))	
			  stop("ERROR.... O_O No se encontro el archivo")
		  require(XLConnect)
		  channel   <- XLConnect::loadWorkbook(fileExt)
      auxSheets <- XLConnect::getSheets(channel)
      if (!infoItem$sheetExt %in% auxSheets) {
    	  stop("ERROR.... O_O  Some tables are not found in the file: \n",
         	   fileExt)
  		}

      # # Find max number of columns
      auxNcol <- apply(object@test@datBlock, ncol)
      isMax   <- which(auxNcol == max(auxNcol))[1]
      maxCon  <- names(object@test@datBlock)[isMax]
      cat("--- Se construira los subConjuntos con: ", maxCon, "\n")
      object@datAnalysis <- list()
      if (!flagTotal){
         warning("___Ojo___  No se correra el análisis con todos los ítems del:", maxCon,  
               "\n           Se tendra solo encuenta el archivo", fileExt, "\n", 
               "cambiar el parametro flagTotal")
      }
      filConP <- subset(auxDictionary, codigo_prueba == auxPrueba)
      if (flagTotal){        
        auxPrueba <- gsub("\\.con", "", test)
        object@datAnalysis[[auxPrueba]] <- list(
          'datos' = object@test@datBlock[maxCon][[1]][[auxParGet]],
          'dictionary' = filConP)
      }
      if (flagSubCon){
        warning("___Ojo___ No se tendra encuenta el subCon del diccionario, 
                se construye los bloques con el archivo", fileExt)
      }
	  } else {
		  object@datAnalysis <- list()
		  for(iiTest in names(object@test@datBlock)){
			  auxPrueba <- gsub("\\.con", "", iiTest)
        if(!flagTotal & !flagSubCon){
          stop("___Error___ Se debe definir si se quiere hacer análisis\n", 
               "con Todo los ítems (flagTotal) o por SubConjunto (flagSubCon)")
        }
        filConP <- subset(auxDictionary, codigo_prueba == auxPrueba)
        if(flagTotal) {          
			    object@datAnalysis[[auxPrueba]] <- list(
				    'datos'      = object@test@datBlock[iiTest][[1]][[auxParGet]],
				    'dictionary' = filConP)
        }

        if(flagSubCon & length(unique(filConP$subCon)) > 1) {
          for(jjSubCon in unique(filConP$subCon)){             
             filterDict <- subset(filConP, subCon == jjSubCon)
             colGet     <- names(object@test@datBlock[iiTest][[1]][[auxParGet]])
             colGet     <- c(grep("^([A-Z]|[a-z]){3,}", colGet, value = TRUE), filterDict[, "id"])
             auxKey     <- paste0(auxPrueba, "::", jjSubCon)
             object@datAnalysis[[auxKey]] <- list(
                'datos'      = object@test@datBlock[iiTest][[1]][[auxParGet]][, colGet, with = FALSE],
                'dictionary' = filterDict)

          }
        }
		  }
	  }
    #return(object)
    object <- filterSubsets(object, ...)
    object@datAnalysis <- structure(object@datAnalysis, parameters = match.call(),
                                    date = format(Sys.time(), "%m_%d_%Y %X"))
    return(object)
})


# # Definition of filters
setGeneric(name = "filterAnalysis", def = function(object){standardGeneric("filterAnalysis")})
setMethod("filterAnalysis", "Analysis", 
          function(object){
            auxParam  <- getParams(object)
            allParams <- names(formals(buildSubsets))
            allParams <- union(allParams, names(formals(filterSubsets)))
            allParams <- intersect(allParams, names(auxParam))
            do.call(buildSubsets, c(list(object), auxParam[allParams]))
          })

# # Save result of Analysis

joinListResult <- function(listResults, dataRead, outRdata, flagActualizar = TRUE) {
    auxResult <- listResults
    auxDataRe <- dataRead
    load(file = outRdata)
    for (resul in names(auxResult)){
      if (flagActualizar) {
        listResults[[resul]] <- auxResult[[resul]]
      } else {
        if (!resul %in% names(listResults)) {
          listResults[[resul]] <- auxResult[[resul]]
        }
      }
    }     
    dataRead <- unique(rbind(dataRead, auxDataRe))
    save(dataRead, listResults, file = outRdata)
}

setGeneric(name = "saveResult", def = function(object, ...){standardGeneric("saveResult")})
setMethod("saveResult", "Analysis", function(object, listResults, srcPath = "."){
     dataRead <- data.frame('nomTest' = object@test@nomTest, 
                            'codSalida' = names(listResults))
     outRdata <- file.path(srcPath, object@outFile$pathRdata)
     if (file.exists(outRdata)){
        joinListResult(listResults, dataRead, outRdata)
     }
     save(dataRead, listResults, file = outRdata)
})

# # Definición Metodos necesarios para definir una clase
setGeneric(name = "codeAnalysis", def = function(object, ...){standardGeneric("codeAnalysis")})
setGeneric(name = "outXLSX", def = function(object, ...){standardGeneric("outXLSX")})
setGeneric(name = "outHTML", def = function(object, ...){standardGeneric("outHTML")})

################################################################################
# # Metodos para correr varias pruebas con varios analisis
################################################################################

analyzeTests <- function(vecJson, anUpdate = NULL, testUpdate = NULL, 
                         getDatBlocks = FALSE){
  # # inicializa la clase Test para cada prueba en la entrada
  # #
  # # Arg:
  # #  fileJson: [character] la ruta del archivo de parametros
  # #  anUpdate: [vector] lista de analisis para correr
  # #  testUpdate: [vector] lista de prubas-formas a correr
  # #  getDatBlocks: [boolean] se quiere tener en memoria los datos de la prueba
  # # Ret:
  # #  listTests: [list-Test] lista con todas las pruebas analizadas
  
  # # Load  scripts
  require(jsonlite)
  listTests <- list() 
  for (fileJson in vecJson) {
     readJson <- fromJSON(fileJson, simplifyVector = TRUE, 
                          simplifyDataFrame = FALSE, 
                          simplifyMatrix = FALSE)
     # # Comprobando si existe nombre para la salida
     if (! "labelHtml" %in% names(readJson)){
       stop('___ERROR___ Se debe definir en el archivo de parametros "labelHtml"')
     }
     readJson <- readJson[names(readJson) != "labelHtml"]    
   
     for (test in names(readJson)){
       jsonLec   <- readJson[[test]]$paramLect
       if (!is.null(jsonLec$infoItem) & !is.null(jsonLec$subConInfo)){
         paramLect <- list(infoItem = jsonLec$infoItem, 
                           conDirs = jsonLec$conDirs, 
                           valMUO = jsonLec$valMUO, 
                           subConInfo = jsonLec$subConInfo)      
       } else {
         paramLect <- list(conDirs = jsonLec$conDirs, 
                           valMUO = jsonLec$valMUO)            
       }
       
       # # Creando nueva prueba
       isTestUpdate = (is.null(testUpdate) | test %in% testUpdate)
       auxTest <- new('Test', path = jsonLec$path, exam = jsonLec$exam, 
                      codMod = jsonLec$codMod, verInput = jsonLec$verInput, 
                      periodo = jsonLec$periodo,
                      nomTest = jsonLec$nomTest, paramLect = paramLect, 
                      isUpdate = TRUE)#isTestUpdate)       
       
       if ("Filtros" %in% names(readJson[[test]])){
         auxFiltros <- Filtros(auxTest, readJson[[test]]$Filtros)  
         auxTest    <- codeAnalysis(auxFiltros)
       }
       # # Parametro para correr una forma en particular
       if (isTestUpdate){
          anUpdateaux <- anUpdate
       } else {        
          anUpdateaux <- c("")
       }

       auxTest <- runAnalysis(object   = auxTest,
                              jsonTest = readJson[[test]]$Analisis, 
                              anUpdate = anUpdateaux)
       
       if (!getDatBlocks){
         auxTest@datBlock <- list()
         gc(reset = TRUE)
       }

       if ("Filtros" %in% names(readJson[[test]])){
         auxTest@listAnal <- c('Filtros' = auxFiltros, 
                               auxTest@listAnal) 
         rm(auxFiltros)
       }
       listTests[[test]] <- auxTest 
       rm(auxTest)
       gc(reset = TRUE)
     }
  }
  return(listTests)
}

################################################################################
# # Función para armarIdentifica (DIFF)
################################################################################

armaIdentifica <- function(listTests, patternExclu = "(-21|01JN|-06|-07|(pbaF\\d{3}(06|07).+))\\.con"){
    # # inicializa la clase Test para cada prueba en la entrada
    # #
    # # Arg:
    # #  fileJson: [character] la ruta del archivo de parametros
    # #  anUpdate: [vector] lista de analisis para correr
    # # Ret:
    # #  listTests: [list-Test] lista con todas las pruebas analizadas
    fileIdentifica <- file.path(outPath, "outIdentifica.Rdata")
    baseIdenti     <- NULL
    for (test in listTests) {
        auxFormas  <- names(test@datBlock)
        auxFormas  <- auxFormas[!grepl(patternExclu, auxFormas)]
        if (length(auxFormas) > 0) {
          getIDFORMA <- lapply(auxFormas, function(x) {
                              auxTable <- test@datBlock[[x]][['oriBlock']]
                              auxTable <- cbind(SNP = auxTable[, SNP], 'FORMA' = x)
                              })
          getIDFORMA <- data.frame(do.call(rbind, getIDFORMA))
          baseIdenti <- rbind(baseIdenti, getIDFORMA)      
        }
    }
    baseIdenti[, "FORMA_BASE"] <- gsub("(pba|PBA|pbaF|UOF)(\\d{3})(.+)?", "\\2",
                                        baseIdenti[, "FORMA"])
    baseIdenti[, "FORMA_BASE"] <- paste0("Test", baseIdenti[, "FORMA_BASE"])
    if (any(duplicated(baseIdenti[, c("SNP", "FORMA_BASE")]))) {
      stop("Hay mas de una forma base en listTests (Separe las pruebas en el json)")
    }
    
    baseIdenti <- dcast(baseIdenti, SNP ~ FORMA_BASE, value.var = "FORMA")
    baseIdenti[, "SNP"] <- as.character(baseIdenti[, "SNP"])
    baseIdenti <- data.table(baseIdenti)
    if (file.exists(fileIdentifica)) {
      newBase <- data.table(baseIdenti)
      load(fileIdentifica)
      baseIdenti <- merge(baseIdenti, newBase, by = "SNP", all = TRUE)
    }
    
    save(baseIdenti, file = fileIdentifica)
    write.table(baseIdenti, sep = "\t", file = gsub("\\.Rdata", ".txt", fileIdentifica), row.names = FALSE, quote = FALSE)
    return(cat("-----Archivo para DIFF generado!!-----\n"))
}


setGeneric(name = "runAnalysis", def = function(object, ...){standardGeneric("runAnalysis")})
setMethod("runAnalysis", "Test",
function(object, jsonTest, anUpdate = NULL){
  # # Ejecuta los analisis con los parametros del archivo .json
  # #
  # # Arg:
  # #  jsonTest: [list] filtro del archivo con los parametros de la prueba
  # #
  # # Ret:
  # #  object: [Analysis] objecto prueba guardando todos los analisis
  for(ii in 1:length(jsonTest)){
     # # Verificando si codeAnalysis existe para ese analisis
     analisis  <- names(jsonTest[[ii]])
     
     # # Definiendo analisis
     exprAnalysis <- paste0(analisis, "(test = object, paramExp = jsonTest[[ii]][[1]])")
     auxAnalysis  <- try(eval(parse(text = exprAnalysis)))         
     
     # # Corriendo analisis
     if (file.exists(auxAnalysis@outFile$pathRdata)){
        load(auxAnalysis@outFile$pathRdata)
        isExecuted <- all(names(auxAnalysis@datAnalysis) %in% names(listResults))
     } else {
        isExecuted <- FALSE
     }
     if (analisis %in% anUpdate | (is.null(anUpdate) & !isExecuted)){
         auxAnalysis <- codeAnalysis(auxAnalysis)  
         outXLSX(auxAnalysis, srcPath = ".")
     } else {
         cat("-----> Cargando los resultados de la clase '", analisis, "'\n")
     }

     # # Guardando análisis en la prueba
     if (class(auxAnalysis) != "try-error"){
       auxAnalysis@test@datBlock       <- list()
       auxAnalysis@test@dictionaryList <- list()
       auxAnalysis@datAnalysis <- lapply(auxAnalysis@datAnalysis, 
                                         function(x){
                                          x$datos = x$datos[0, ]
                                          return(x) })
       
       object@listAnal  <- c(object@listAnal, auxAnalysis)
       names(object@listAnal)[length(object@listAnal)] <- analisis
     }
     
  }
  return(object)
})

jointReports <- function(listTests, vecJson, pathJS = 'lib', flagView = FALSE){
  # # Crear el reporte html para 1 o varias pruebas
  # #
  # # Arg:
  # #  listTests: [list-Test] lista con todas las pruebas analizadas
  # #
  # # Ret:
  # #  archivos html con el nombre asignado en json$labelHtml
  for (fileJson in vecJson){
    options(encoding = "UTF-8")
    readJson  <- fromJSON(fileJson, simplifyVector = TRUE, 
                         simplifyDataFrame = FALSE, 
                         simplifyMatrix = FALSE)
    if (!file.exists(docPath)) dir.create(docPath)
    options(encoding = "native.enc")
    rmarkdown::render('.\\Sweave\\reportAnaItem.Rmd', 'knitrBootstrap::bootstrap_document', 
                      output_file = readJson$labelHtml[["html"]],  output_dir = docPath, 
                      encoding = "utf-8")
    salPathDoc <- file.path(docPath, readJson$labelHtml[["html"]])
  
    # # Incluyendo archivos .js necesarios para las tablas
    jsonLib <- c(paste0('  <script src="', pathJS,'/htmlwidgets-0.5/htmlwidgets.jsp"></script>'),
                 paste0('  <script src="', pathJS,'/jquery-1.11.1/jquery.min.jsp"></script>'),
                 paste0('  <script src="', pathJS,'/datatables-binding-0.1/datatables.jsp"></script>'),
                 paste0('  <script src="', pathJS,'/datatables-1.10.7/jquery.dataTables.min.jsp"></script>'),
                 paste0('  <script src="', pathJS,'/nouislider-7.0.10/jquery.nouislider.min.css"></script>'),
                 paste0('  <script src="', pathJS,'/nouislider-7.0.10/jquery.nouislider.min.jsp"></script>'),
                 paste0('  <script src="', pathJS,'/selectize-0.12.0/selectize.bootstrap3.css"></script>'),
                 paste0('  <script src="', pathJS,'/selectize-0.12.0/selectize.min.jsp"></script>'),               
                 paste0('  <script src="', pathJS,'/popUPGR.jsp"></script>'),
    #            paste0('  <script src="lib/FancyZoom_1.1/js-global/FancyZoom.jsp" type="text/javascript"></script>',
    #            paste0('  <script src="lib/FancyZoom_1.1/js-global/FancyZoomHTML.jsp" type="text/javascript"></script>',
                 paste0('  <link href="', pathJS,'/datatables-default-1.10.7/dataTables.extra.css" rel="stylesheet" />'),
                 paste0('  <link href="', pathJS,'/datatables-default-1.10.7/jquery.dataTables.min.css" rel="stylesheet" />'),
                 paste0('<link href="', pathJS,'/../css/linkDownload.css" rel="stylesheet" />'))
                 
    archHtml  <- readLines(salPathDoc)
    archCSS   <- grep("<!-- jQuery -->", archHtml)
    indCSStab <- grep("/\\* style tables \\*/", archHtml)
    #archHtml  <- gsub("font-weight: bold;", "font-weight: bold; \n color: red; \n background-image: url(\"FondoPresentacionesICFES01.png\");" , archHtml)
    archHtml  <- gsub("font-weight: bold;", "font-weight: bold; \n background-image: url(\"../../../../FondoPresentacionesICFES01.png\");" , archHtml)
    archHtml  <- gsub("<body>", "<body onload=\"setupZoom()\">" , archHtml)  
    #archHtml  <- archHtml[-seq(indCSStab, indCSStab + 1)]
    archHtml[indCSStab + 1]  <- "$('table3').addClass('table-bordered table-condensed');"
    
    archHtml <- c(archHtml[1:(archCSS + 2)], jsonLib, archHtml[(archCSS + 3):length(archHtml)])
    cat(archHtml, file = salPathDoc, sep = "\n")
    if (flagView) {
      browseURL(normalizePath(salPathDoc))
    }
    options(encoding = "UTF-8")
  }
}


publishRepo <- function(vecJson, pathDest, flagActualizar = FALSE){
  # # Lectura de json 
  options(encoding = "UTF-8")
  for(fileJson in vecJson){
     readJson <- jsonlite::fromJSON(fileJson, simplifyVector = TRUE, 
                          simplifyDataFrame = FALSE, 
                          simplifyMatrix = FALSE)
     jsonHtml <- file.path(pathDest, "index.json")
     if (file.exists(jsonHtml)){
       ppJson  <- gsub("var indexData\\s+=\\s+", "", readLines(jsonHtml, warn = FALSE))
       auxJson <- jsonlite::fromJSON(ppJson, simplifyVector = TRUE, 
                          simplifyDataFrame = FALSE, 
                          simplifyMatrix = FALSE)
     } else {
       auxJson <- list()
     }
     auxJson <- unlist(auxJson)
     auxJson <- sapply(auxJson, function(x) basename(x))
   
     # # Creando estructura de Json
     outExam  <- unlist(sapply(readJson, function(x) x$paramLect$exam))
     outYear  <- rep(readJson$labelHtml[["year"]], length(outExam))
     outStage <- rep(readJson$labelHtml[["mode"]], length(outExam))
     outName  <- unlist(sapply(readJson, function(x) x$paramLect$nomTest))
     outName  <- gsub(".+\\((.+)\\)", "\\1", outName)
     outList  <- rep(readJson$labelHtml[["html"]], length(outExam))
     names(outList) <- paste(outExam, outYear, outStage, outName, sep = ".")
   
     for (value in names(unlist(outList))){
       auxJson[value] <- unlist(outList)[value]
     }
   
     # Creación de carpetas
     foldIni <- unique(file.path(pathDest, outExam, outYear, outStage))  
     for (folder in foldIni){
       dir.create(folder, recursive = TRUE, showWarnings = FALSE)
       foldOut <- file.path(folder, c("Doc", "Output"))
       for (out in foldOut){
         dir.create(out, recursive = TRUE, showWarnings = FALSE)      
       }
       
       require(tools)
       # # Encontrando archivos en comun
       liDestino <- list.files(file.path(folder, "Output"), recursive = TRUE)
       liOrigen  <- list.files(outPath, recursive = TRUE)
       liInterse <- intersect(liDestino, liOrigen)

       if (length(liInterse) ==  0) {
         file.copy(outPath, folder, recursive = TRUE, overwrite = flagActualizar)
       } else {
         # # Listando Rdatas paraUnir
         liInterse <- liInterse[toupper(file_ext(liInterse)) == "RDATA"]
         liInterse <- grep("((outList)|(conteoFiltros))", liInterse, value = TRUE)
         for (file in liInterse){
           load(file.path(outPath, file))
           fileAux <- file.path(folder, "Output", file)
           joinListResult(listResults, dataRead, fileAux, flagActualizar)
           rm(listResults, dataRead); gc(reset = TRUE)
         }
         # # Copiando el resto de archivos
         liUnion <- setdiff(union(liDestino, liOrigen), liInterse)
         dirCrear <- gsub("(.+)(/pba.+/)(.+)", "\\1\\2", grep("06IRT", file.path(folder, "Output", liUnion), value = TRUE))
         dirCrear <- unique(dirCrear[!grepl(".xlsx", dirCrear)])
         dirCrear <- dirCrear[!dir.exists(dirCrear)]
         lapply(dirCrear, function(x){dir.create(x, recursive = TRUE)})
         file.copy(file.path(outPath, liUnion), file.path(folder, "Output", liUnion), 
                   overwrite = flagActualizar, recursive = TRUE)
       }

       # # Copiando otros archivos
       file.copy(docPath, folder, recursive = TRUE, overwrite = flagActualizar)
       
     }
      
     # # Creando Lista 
     consNode <- function(label, child = list()){
         nodeAux <- list(jj = child)
         names(nodeAux) <- label
         return(nodeAux)
     }
   
     indexJson  <- list()
     labelList  <- strsplit(names(auxJson), "\\.")

     # # llenando lista
     for (ii in 1:length(auxJson)){
       list     <- labelList[[ii]]
       if (!list[1] %in% names(indexJson)) {
         indexJson[[list[1]]] <- list()
       }
       if (!list[2] %in% names(indexJson[[list[1]]])) {
         indexJson[[list[1]]][[list[2]]] <- list()
       }
       if (!list[3] %in% names(indexJson[[list[1]]][[list[2]]])){
         indexJson[[list[1]]][[list[2]]][[list[3]]] <- list()
       }
       auxHtml <- file.path(list[1], list[2], list[3], "Doc", auxJson[[ii]]) 
       indexJson[[list[1]]][[list[2]]][[list[3]]][[list[4]]] <- auxHtml
     }
     # # Imprimir lista
     cat(jsonlite::toJSON(indexJson, pretty = TRUE), file = jsonHtml)
     ppJson    <- readLines(jsonHtml, warn = FALSE)
     ppJson[1] <- paste0("var indexData = ", ppJson[1]) 
     cat(ppJson, file = jsonHtml, sep = "\n")
  }
}

