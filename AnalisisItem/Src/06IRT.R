###############################################################################
# # filename: 06IRT.R
# # R Versions: R version 3.0.2
# #
# # Author(s): Ronald Herrera, Álvaro Uzaheta and Víctor H. Cervantes
# #            (original runs Rnw files)
# #
# # SABER 5° y 9° Citizenship competencies
# # Description: Code to run IRT (Item Response Theory) models (GRM and
# #              2PL) through Parscale for blocks of items (index) for test
# #
# # Inputs: Dictionary and list of data.frames with data from
# #         00CrearRdata.R
# #
# # Outputs: If isReporte = FALSE file in xlsx with:
# #          - identification of items by block
# #          - table and graphic of Cronbach-Mesbah Curve
# #          - graphic of Item Characteristic Curve (ICC)
# #          - graphic of Items/Index Information Curve and
# #            tables with the approximate areas of the curves
# #          - graphic person-item map and table with
# #            the distribution of people by ability
# #          - table with estimated parameters
# #            and measures of goodness of fit
# #          If isReporte = TRUE previous graphs in outPath and
# #            file in txt with table with estimated parameters
# #            and measures of goodness of fit
# #
# # File history:
# #   20111214: Creation
# #   20130222: Adaptation for FA subCons (Mario Carrasco)
# #   20140425: Adaptation for output in xlsx (Sandra Ropero)
# #   20140530: Adaptation for graphics and tables additional (Sandra Ropero)
# #   20140609: Adaptation for 2PL models, indexes with just three items
# #             and output when exists problems estimability of the parameters
# #             (Fabio Tejedor and Sandra Ropero)
# #   20151124: Adaptation for 2PL models with Bilog,
# #             and for Test of SABER 3, 5, 7, 9
# #             (Jorge Mario Carrasco Ortiz)
# #   20160620: Adaptation for S4 Clases
# #
# # ToDo: - implement to run models PCM, RSM, MRD
# #       - implement confidence intervals in graphic ICC
################################################################################
# # global options
###############################################################################
#options(encoding = "UTF-8")

################################################################################
# # Definition of class and parameters
################################################################################

# # Heritage class Analysis
IRT <- setClass("IRT", contains = "Analysis")

setMethod("initialize", "IRT", function(.Object, ..., param) {
  .Object@outFile$pathRdata <- "../Output/06IRT/outList_IRT.Rdata"
  .Object <- callNextMethod()
})

IRT <- function(test, paramExp = NULL){
  paramDefault <- list(kOmissionThreshold = 0.5,
                       flagOri = FALSE, flagTotal = TRUE,
                       flagSubCon = TRUE, orderedDat = FALSE,
                       idNoPKey = c('O', 'M'), constDmodel = 1.7,
                       isCheckKeys = FALSE, kThresItemCorrDic = 0.2,
                       kThresItemCorrOrd = 0.2, espSd = 1, espMean = 0, 
                       AnclaRdata = NULL, formAncla = "", flagSPrior = FALSE, 
                       mCentrar = NULL, sdCentrar = NULL)
  if ("verSalida" %in% names(paramExp)){
    auxVerSalida <- paramDefault$verSalida
  } else {
    auxVerSalida <- 1
  }
  if (!is.null(paramExp)) {
    isDefault <- setdiff(names(paramDefault), names(paramExp))
    paramExp  <- c(paramExp, paramDefault[isDefault])
  } else {
    paramExp <- paramDefault
  }
  cat("----->Se correra un analisis IRT con los siguientes parametros: \n")
  print(paramExp)
  object <- new("IRT", test = test, param = paramExp, 
                verSalida = auxVerSalida)
  object <- filterAnalysis(object) # Organizando filtros            
  return(object)
}

################################################################################
# # Función escalamiento 
################################################################################

funRescal <- function(datFrame, colName, meanHab = 0, sdHab = 1, meanFin = 0, 
                      sdFin = 1, flagIverse = FALSE, flagEE = FALSE){ 
  if (!flagIverse){
     if (!flagEE) {
       datFrame[, colName] <- ((datFrame[, colName] - meanHab) / sdHab) * sdFin + meanFin
     } else {
       datFrame[, colName] <- (sdFin / sdHab) * (datFrame[, colName])
     }
  } else {
    if (!flagEE) {
       datFrame[, colName] <- ((datFrame[, colName] - meanFin) / sdFin) * sdHab + meanHab
    } else {
       datFrame[, colName] <- (sdHab / sdFin) * (datFrame[, colName])
    }
  }
  return(datFrame)
}

################################################################################
# # Definition of codeAnalysis Method
################################################################################

setMethod("codeAnalysis", "IRT",
          analIRT <- function(object){
  #object <- filterAnalysis(object) # Organizando filtros            
  binPath <- file.path("..", "Src", "bin")
  outPath <- file.path(outPath, "06IRT")
  dir.create(outPath, recursive = TRUE, showWarnings = FALSE)    

  # # Load libraries
  require(psych)  # # 1.1.10
  require(ggplot2)  # # 0.9.3.1
  require(gtools)  # # 3.2.1
  require(scales)  # # 0.2.3
  require(car)  # # 2.0-19
  require(LaF)  # # 0.5
  require(data.table)  # # 1.8.10
  require(RColorBrewer) # # 1.0-5
  require(gridExtra)

  # # load functions
  source(file.path(funPath, "univariateFunctions01.R"))
  source(file.path(funPath, "log.R"))
  source(file.path(funPath, "wrapWS.R"))
  source(file.path(funPath, "wrapPS.R"))
  source(file.path(funPath, "wrapBL.R"))
  source(file.path(funPath, "plotICCP.R"))
  source(file.path(funPath, "ggplot_dual_axis.R"))
  source(file.path(funPath, "plotPImap.R"))
  source(file.path(funPath, "plotPImap.R"))
  source(file.path(funPath, "WrightMapICFES.R"))
  source(file.path(funPath, "gvisUtils.R"))
  source(file.path(funPath, "tablasHtml.R"))

  # # models
  codesRasch           <- c("01", "02", "04")
  names(codesRasch)    <- c("PCM", "RSM", "MRD")
  codesNotRasch        <- c("03", "05")
  names(codesNotRasch) <- c("GRM", "2PL")

  # # version with dict V03
  versionOutput  <- object@verSalida
  versionComment <- paste0("Corrida Análisis Exploratorio --",  object@test@nomTest, "--") 

  # # see ?psych::alpha
  isCheckKeys <- FALSE

  # # Cargando archivo/s de anclas
  if (!is.null(object@param$AnclaRdata)) {          
     fileAncla <- object@param$AnclaRdata
     formAncla <- object@param$formAncla

     # # Comprobacion de parametros
     if (length(fileAncla) != length(formAncla)) {
       stop("La longitud de 'AnclaRdata' y de 'formAncla' no es la misma")
     } 

     # # Armando data frame     
     listResultsAN <- NULL
     for (jj in seq(length(fileAncla), 1)) {
        if (file.exists(fileAncla[jj])){
          load(fileAncla[jj])  
        } else {
          stop("ERROR...... No encontre 'AnclaRdata': ", fileAncla[jj])
        }
 
        if (formAncla[jj] %in% names(listResults)) {
          cat("...... Lecutura de la forma", formAncla[jj], 
              "\n...... del archivo:", fileAncla[jj], "\n")
          auxCalibra <- listResults[[formAncla[jj]]]$tablaFin
          if (!is.null(listResultsAN)) {
            isNewCal   <- !auxCalibra[, item] %in% listResultsAN[, item]
            auxCalibra <- auxCalibra[isNewCal, ]
            auxCalibra <- subset(auxCalibra, select = intersect(names(auxCalibra), 
                                 names(listResultsAN)))
            listResultsAN <- subset(listResultsAN, select = intersect(names(auxCalibra), 
                                 names(listResultsAN)))
          }
          listResultsAN <- rbind(listResultsAN, auxCalibra)
        } else {
          stop("ERROR...... No se encontro la forma... ", formAncla[jj], 
               "en el archivo 'AnclaRdata': ", fileAncla[jj])
        }
        # # Parametros de transformacion
        posBuscar <- ifelse(!is.null(object@param$posMeSig), 
                            object@param$posMeSig, length(fileAncla))
        if (jj == posBuscar) {
            cat("Transformacion con :", fileAncla[jj], "\n")
            meanAbilParam <- listResults[[formAncla[jj]]]$meanAbil
            sdAbilParam   <- listResults[[formAncla[jj]]]$sdAbil
        }
        rm(listResults)         
     }
  } else {
     listResultsAN <- NULL
  }

  # # Especificando la transformacion para las dificultades
  if (!is.null(object@param$mCentrar)) {
    cat("Centrando de acuerdo a parametros mCentrar:", 
       object@param$mCentrar, "\n")
    meanAbilParam <- object@param$mCentrar
  }

  if (!is.null(object@param$sdCentrar)) {
    cat("Centrando de acuerdo a parametros sdCentrar:", 
       object@param$sdCentrar, "\n")
    sdAbilParam <- object@param$sdCentrar
  }

  # # create list to save results
  listResults <- list()
  pruebasRead <- names(object@datAnalysis)

  for (kk in pruebasRead) { 
      cat(".... Ejecutando estimación para -->", kk, "\n")
      basePru <- gsub("(.+)::.+", "\\1", kk)
      auxPru  <- gsub("(::|\\s)","_", kk)
      subComm <- gsub("^(.*)(::)(.*)","\\3", kk)
      # # Create Folders      
      auxOutPath <- outPath
      outPath    <- file.path(outPath, auxPru)
      dir.create(outPath, recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(outPath, "salidas"), recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(outPath, "graficos"), recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(outPath, "corridas"), recursive = TRUE, showWarnings = FALSE)

      # # Define Dictionary
      dictVarPrueba <- object@datAnalysis[[kk]]$dictionary
      varId         <- dictVarPrueba[, 'id']

      if (object@test@exam != "ACC"){
        if (!"instr" %in% names(dictVarPrueba))
          dictVarPrueba[, 'instr'] <-  "No disponible"
        if (!"etiqu" %in% names(dictVarPrueba))
          dictVarPrueba[, 'etiqu'] <-  dictVarPrueba[, "subCon"]
      }
          
      dicRasch     <- subset(dictVarPrueba, codMod %in% codesRasch)[, c("id", "subCon")]
      isIndexRasch <- nrow(dicRasch) > 0

      # # item that are part of the index
      indexItems    <- dictVarPrueba[, 'id']
      isIDStudent   <- names(object@datAnalysis[[kk]]$datos)
      isIDStudent   <- isIDStudent[!isIDStudent %in% indexItems]
      personDataBlo <- object@datAnalysis[[kk]]$datos[, isIDStudent, with = FALSE]
      dataCor       <- object@datAnalysis[[kk]]$datos[, indexItems, with = FALSE]

      if (object@test@exam == "ACC"){
         # # correlations item
         itemCorrelations <- psych::alpha(data.frame(dataCor), check.keys = isCheckKeys)$item.stats
         itemCorrelations <- data.frame('item' = row.names(itemCorrelations),
                                        'correlation' = itemCorrelations[, "r.drop"])
         isMissingHalf <- rowSums(!is.na(dataCor)) < max(length(indexItems) %/%  2, 3)
         # # calibration criterion
         indexreliability <- psych::alpha(dataCor)
         alphaCoef        <- indexreliability$total[, "raw_alpha"]
         nRatio <- 3000 / nrow(dataCor[!isMissingHalf, indexItems])
         kThresItemChiHigh  <- 1 + (2/nRatio)
         indexData <- paste("ind", auxPru, "_V", versionOutput, sep = "")
      }

      if (object@test@exam == "SABER359"){
         orderSB <- subset(dictVarPrueba, id %in% indexItems, select = c("id", "SUBBLOQUE"))
         dataCor <- dataCor[, orderSB[order(orderSB$SUBBLOQUE), "id"], with = FALSE]         
      }

      if (object@test@exam %in% c("SABER359", "SABERPRO", "SABER11", "SABERTYT")) {
        isMissingHalf <- rep(FALSE, nrow(dataCor))
        indexData     <- paste(auxPru, "_V", versionOutput, sep = "")
      }

      # # Check the grouping in RSM blocks
      rsmType             <- LETTERS[1:length(indexItems)]
      codModels           <- subset(dictVarPrueba, select =  c("id", "codMod"))
      rownames(codModels) <- codModels[, "id"]
      codModels           <- codModels[indexItems, "codMod"]
      names(codModels)    <- indexItems

      # # Unique code model
      if (length(unique(codModels)) > 1) {
         stop("%%% Para este subCon hay varios códigos de modelos")
      }

      rsmType[codModels == codesRasch["RSM"]] <- 1

      # # Data block to obtain results
      resBlock <- dataCor[!isMissingHalf, indexItems, with = FALSE]

      if (isIndexRasch) {
        # #     ##################
        # #     #### OJO HASTA AQUI VA RASH
        # #     ##################
      }
      auxCodModel <- unique(codModels)
      if (auxCodModel %in% c("00", "05", "06", "07")){
        auxPath <- getwd()
        runPath <- file.path(outPath, 'corridas')

        auxNPAR <- ifelse((auxCodModel == "05") | (auxCodModel == "00"), 1, 
                          ifelse(auxCodModel == "06", 2, 3))
        
        # # Create .blm and .dat file
        personDataBlo <- personDataBlo[!isMissingHalf]        
        RunBilog(responseMatrix = resBlock,
                 runName = indexData,  srcPath = auxPath,
                 outPath = file.path("..", 'salidas'),
                 personIds = as.character(personDataBlo[[1]]), 
                 itemIds = paste0("I", dictVarPrueba$id), binPath = binPath,
                 runPath = file.path(outPath, 'corridas'),
                 verbose = TRUE, runProgram = TRUE, nQuadPoints = 40,
                 commentFile = indexData, NPArm = auxNPAR, thrCorr = 0.05, 
                 datAnclas = listResultsAN, 
                 flagSPrior = object@param$flagSPrior)
        
        # Reading results of chi square test 
        itemPH2File <- paste(indexData, ".PH2", sep = "")
        itemPH2     <- try(readPH2CHI(itemPH2File, runPath))

        # Reading results of parameters model
        itemDiffFile   <- paste("salidas/", indexData, ".PAR", sep = "")
        itemParameters <- try(ReadBlParFile(itemDiffFile, outPath))
        
        # # Reading TCT results Original
        itemTCTFile  <- paste("salidas/", indexData, "_0.TCT", sep = "")
        tctParam_ORI <- try(ReadBlTCTFile(itemTCTFile, outPath))

        # # Reading TCT results Final
        itemTCTFile <- paste("salidas/", indexData, ".TCT", sep = "")
        tctParam    <- try(ReadBlTCTFile(itemTCTFile, outPath))
        
        # # Filtrando datos
        indexItemsFin <- intersect(indexItems, gsub("^I", "", tctParam[, "item"]))

        # # Ajustando archivo final TCT
        tctParam    <- merge(tctParam_ORI, tctParam[, c("item", "BISERIAL")], 
                             by = "item", all.x = TRUE)
        tctParam[, "BISERIAL"] <- ifelse(is.na(tctParam[, "BISERIAL.y"]), 
                                         tctParam[, "BISERIAL.x"], 
                                         tctParam[, "BISERIAL.y"])
        tctParam[, "BISERIAL.x"] <- tctParam[, "BISERIAL.y"] <- NULL

        # # Resultados estimación de habilidades
        personAbilFile  <- paste("salidas/", indexData, ".SCO", sep = "")
        outFileAbili    <- file.path(outPath, "salidas",
                                     paste0("personAbilities_V",
                                           versionOutput, ".RData"))
        if (!file.exists(outFileAbili)) {
           personAbilities <- try(ReadBlScoFile(personAbilFile, outPath,
                                  lengthIds = nchar(personDataBlo[1])))
           if (nrow(personAbilities) > 0) {
             save(personAbilities, file = outFileAbili)
           }
        } else {
           load(outFileAbili)
        }

        # # habilidades por TCT
        if (auxCodModel == "00") {
          itemParameters <- merge(itemParameters, 
                                  tctParam[, c("item", "TRIED", "RIGHT")], 
                                  by = "item")
          itemParameters[, 'dif']      <- (1 - itemParameters[, 'RIGHT'] / 
                                          itemParameters[, 'TRIED']) * 100
          itemParameters[, 'TRIED'] <- NULL
          itemParameters[, 'RIGHT'] <- NULL
          personAbilities[, 'ABILITY'] <- personAbilities[, 'RIGHT'] / 
                                          personAbilities[, 'TRIED'] * 100
        }
 
        # # Usando promedio de anclaje
        if (!is.null(object@param$AnclaRdata)) { # Indicadora de Anclas
          meanAbil <- meanAbilParam
          sdAbil   <- sdAbilParam
        } else {
          meanAbil <- mean(personAbilities$ABILITY)
          sdAbil   <- sd(personAbilities$ABILITY)         
        }

        # cat(meanAbil)
        # cat(sdAbil)

        # # Transformando dificultad y habilidad
        personAbilities[, "ABILITY_NEW"] <- personAbilities[, "ABILITY"]
        itemParameters[, "dif_NEW"]      <- itemParameters[, "dif"]
        itemParameters[, "eedif_NEW"]    <- itemParameters[, "eedif"]

        if (all(c("espMean", "espSd") %in% names(object@param))){
          personAbilities <- funRescal(personAbilities, "ABILITY_NEW", meanHab = meanAbil, 
                                       sdHab = sdAbil, meanFin = object@param$espMean, 
                                       sdFin =  object@param$espSd, flagEE = FALSE)
          itemParameters  <- funRescal(itemParameters, "dif_NEW", meanHab = meanAbil, 
                                       sdHab = sdAbil, meanFin = object@param$espMean, 
                                       sdFin =  object@param$espSd, flagEE = FALSE)
          itemParameters  <- funRescal(itemParameters, "eedif_NEW", meanHab = meanAbil, 
                                       sdHab = sdAbil, meanFin = object@param$espMean, 
                                       sdFin =  object@param$espSd, flagEE = TRUE)
        } else {
          cat(">>>>> No se especifico Media y Varianza Esperada NO se hizo tranformación")
          meanAbil <- 0
          sdAbil   <- 1
        }

        # # Comprobación de errores
        if (class(itemParameters) == "try-error" |
            class(tctParam) == "try-error" |
            class(personAbilities) == "try-error" | 
            any(class(itemPH2) == "try-error")) {
           warning("Error en Bilog no se pudo leer alguno",
                   "de estos archivos (.PAR, .TCT, .SCO)")
           next
        }

        # # Capturando base de datos original
        datBase  <- grep(paste0(basePru, "\\.con"), 
                         names(object@test@datBlock), value = TRUE)
        if (length(datBase) > 1) {
          stop("_____ Se encontro mas de una prueba base:", datBase)
        }
        #print(datBase)
        #print(names(object@test@datBlock))
        resBlockOri <- object@test@datBlock[[datBase]]$oriBlock
        isIDgood    <- resBlockOri[[isIDStudent]] %in% personDataBlo[[1]]
        resBlockOri <- subset(object@test@datBlock[[datBase]]$oriBlock, 
                              isIDgood, select = indexItems)
        resBlockOri[, "iSubject"] <- 1:nrow(resBlockOri)

        # # Curvas de respuestas
        dirPlotOPpng <- paste0("graficos/plotOP-", indexData, ".png")           
        dirPlotOPpng <- file.path(outPath, dirPlotOPpng)
        keyData      <- subset(dictVarPrueba, id %in% indexItems,
                               select = c("id", "keyItem"))
        dirCatFre <- file.path(outPath, "salidas",
                               paste0("catFreq_V",
                               versionOutput, ".RData"))
        cat("...... Guardado informacion de opciones de respuesta .\n")
        #save(resBlockOri, personAbilities, keyData, dirPlotOPpng, 
        #     dirCatFre, indexItems, file = "auxiliar.Rdata")
        listOP <- responseCurve(resBlockOri, personAbilities,
                                methodBreaks = "Sturges",
                                keyData = keyData,
                                dirPlot = dirPlotOPpng, plotName =
                                "Opciones Item", dirCatFreq = dirCatFre, 
                                indexItems = indexItems)
        load(dirCatFre)  # This load tablaRep Constructed by responseCurve

        # # ICC curve
        resBlock[,"consLect"]  <- personDataBlo[!isMissingHalf]
        resBlock[, "iSubject"] <- 1:nrow(resBlock)
        resBlockFin   <- resBlock[, c(indexItemsFin, "consLect", "iSubject"), with = FALSE]

        dirPlotICCpng <- paste0("graficos/plotICC-", indexData, ".png")
        dirPlotICCpng <- file.path(outPath, dirPlotICCpng)

        cat("......Guardado ICC. \n")  
        #source(file.path(funPath, "plotICCP.R"))
        # save(itemParameters, resBlockFin, personAbilities, object@param, subComm, 
        #      dirPlotICCpng, kk, outPath, indexItemsFin, meanAbil, sdAbil, 
        #      file = "aux.Rdata")
        listICC <- plotICCB(itemParameters, resBlockFin, personAbilities,
                   scaleD = object@param$constDmodel, flagGrSep = TRUE,
                   methodBreaks = "Sturges", namesubCon = subComm,
                   dirPlot = dirPlotICCpng, plotName =
                   paste("ICC para", object@nomPrueba), prueba = kk, 
                   dirSalida = outPath, indexItems = indexItemsFin, 
                   codModel = object@test@codMod, meanHab = meanAbil, 
                   sdHab = sdAbil, meanFin = object@param$espMean, 
                   sdFin =  object@param$espSd, flagEE = FALSE)
        infTest <- listICC[[2]]
        listICC <- listICC[[1]]        
        if (all(c("espMean", "espSd") %in% names(object@param))){
          infTest <- funRescal(infTest, "x", meanHab = meanAbil, 
                               sdHab = sdAbil, meanFin = object@param$espMean, 
                               sdFin =  object@param$espSd, flagEE = FALSE)
        }
        # # Items - Person Curve and Test Information Curve
        dirPerItem <- paste0("graficos/personItem-", indexData, ".png")                                 
        dirPerItem <- file.path(outPath, dirPerItem)
        pathGraph  <- object@test@nomTest
        pathGraph  <- gsub("\\)", paste0(" - ", subComm, ")"), pathGraph)

        cat("......Guardado WrightMap. \n")
        itHaMap    <- WrightMapICFES(infoItem = itemParameters, 
                                     infoCal = personAbilities,
                                     colHab = "ABILITY_NEW", 
                                     colDiff = "dif_NEW", 
                                     file = dirPerItem,
                                     Title = gsub("(\\s+)?SABER 3,\\s?5 y 9 ", "", pathGraph))       
        listResults[[auxPru]][["itHaMap"]]  <- itHaMap
        listResults[[auxPru]][["plotInfo"]] <- infTest

        # # Information of block
        # if (object@test@exam == "SABER359") {
        #    infoBloque <- data.table(dictVarPrueba)
        #    infoBloque <- infoBloque[id %in% indexItems, ]
        #    infoBloque <- infoBloque[match(indexItems, infoBloque$id),
        #                             list(codigo_prueba, COMPONENTE,
        #                                  COMPETENCIA, keyItem, 'item_cod' = id,
        #                                  'item' = paste0(SUBBLOQUE, sprintf("%.2d", 1:length(id)))),
        #                                   by = "SUBBLOQUE"]
        # } else {
          infoBloque <- data.table(dictVarPrueba)
          infoBloque <- infoBloque[id %in% indexItems, ]
          infoBloque <- infoBloque[match(indexItems, infoBloque$id),
                                   list('item_cod' = paste0("ITEM", sprintf("%.4d", 1:length(id))),
                                        'item' = paste0("I", id), codigo_prueba, keyItem)]

        # }

        # # Codigos de los subloques
        itemParameters <- merge(itemParameters, infoBloque, by = "item", all = TRUE)
        setnames(itemParameters, "item_cod", "item_blq")
        selecColTCT    <- names(tctParam)
        tctParam       <- merge(tctParam, infoBloque, by = "item", all.x = TRUE)
        setnames(tctParam, "item_cod", "item_blq")
        tctParam       <- tctParam[, selecColTCT]

        # # Construct Flag Proportions and flag of habilities
        tablaRep[, item := paste0("I", item)]
        tablaFlags  <- merge(data.table(itemParameters)[, list(item, keyItem)],
                             tablaRep, by = "item", all.y = TRUE)
        # # Change key categoria
        if (any(tablaFlags[, item] %like% "^IA.+")){
          tablaFlags[!keyItem %in% LETTERS, keyItem := "Correcta"]
        }
        #save(tablaFlags, file = "aux.Rdata")
        tablaFlags[keyItem == categoria, keyAbility := mAbility]
        #print(str(tablaFlags))
        tablaFlags[, keyAbility := na.omit(keyAbility)[1], by = "item"]        
        tablaFlags  <- tablaFlags[!categoria %in% object@param$idNoPKey,
                                  list('FLAGPROP' = ifelse(min(prop) < 0.1 | max(prop) >= 0.9, 1, 0),
                                       'FLAGKEY2'  = ifelse(sum(mAbility > keyAbility), 1, 0)), by = "item"]

        # # Construct Data Consolidation
        ldirOP   <- data.table('item' = paste0("I", names(listOP)), 'dir_OP' = sapply(listOP, function(x) x$dir))
        ldirICC  <- data.table('item' = paste0("I", names(listICC)), 'dir_ICC' = sapply(listICC, function(x) x$dir), 
                               'maxINFO' = sapply(listICC, function(x) x$maxINFO))
        tablaRep <- melt(tablaRep, id = 1:2, measure = 3:4)
        tablaRep <- dcast.data.table(tablaRep, item ~ categoria + variable, fun = sum,
                                     value.var = c("value"))
        tablaFin <- merge(merge(ldirOP, ldirICC, by = "item", all = TRUE), tablaRep, by = "item", all = TRUE)

        # # Cruce con Anclas
        if (!is.null(listResultsAN)) { # Indicadora de Anclas
          itemAnclas     <- subset(listResultsAN, select = c("item"))
          itemAnclas     <- cbind(itemAnclas, 'Ancla' = 1)
          itemParameters <- merge(itemParameters, itemAnclas, by = "item", all.x = TRUE)
          isNOAncla      <- is.na(itemParameters[["Ancla"]])
          itemParameters[["Ancla"]][isNOAncla] <- 0
        } else {
          itemParameters <- cbind(itemParameters, 'Ancla' = 0)
        }

        # # Construct Data Consolidation
        tablaFin <- merge(tablaFin, itemParameters, by = "item", all = TRUE)
        tablaFin <- merge(tablaFin, tctParam, by = "item", all = TRUE)
        tablaFin <- merge(tablaFin, tablaFlags, by = "item", all = TRUE)
        tablaFin <- merge(tablaFin, itemPH2, by = "item", all = TRUE)


        # # Fill other columns in the report
        colFix <- c("CORRELACION", "PCT", "disc", "dif", "azar","INFIT", "OUTFIT", 
                    "BISERIAL", "item_blq", "TRIED", "SUBBLOQUE", "COMPONENTE", "COMPETENCIA", 
                    "chi2", "gl_chi2", "p_val_chi2", "M_prop", "M_mAbility")
        colFix <- colFix[!colFix %in% names(tablaFin)]
        for(col in colFix){
           tablaFin <- cbind(tablaFin, 'newCol' = "No aplica")
           setnames(tablaFin, 'newCol', col)
        }

        # # Other Flags
        #tablaFin[, indPos := ifelse(TRIED <= 200, 1, ifelse(TRIED > 100000, 4, ifelse(TRIED > 500, 3, 2))), by = item]
        #tablaFin[, minOutms := c(0.7, 0.75, 0.8, 0.9)[indPos]]
        #tablaFin[, maxOutms := c(1.3, 1.25, 1.2, 1.1)[indPos]]

        # # Cambio en el se de la dificultad
        tablaFin[, eedif_NEW := eedif_NEW / abs(dif_NEW) * 100]

        # # Apagando lo no necesario en el caso de reporte TCT
        if (auxCodModel == "00") {
          tablaFin[, c('p_val_chi2', 'chi2', 'gl_chi2',
                       'maxINFO', 'disc', 'azar', 'eeazar', 'eedif_NEW', 
                       'dir_ICC') := list(NA, NA, NA, NA, NA, NA, NA, NA, '')]
        }

        # # Alertas
        tablaFin <- cbind(tablaFin, tablaFin[, list(
                          'codMOD' = unique(codModels),
                          'FLAGMEAN' = ifelse((PCT >90) | (PCT < 10), 1, 0),
                          'FLAGCORR' = ifelse(CORRELACION < 0.1, 1, 0),
                          'FLAGA'    = ifelse(disc < 0.5 , 1, 0),
                          'FLAGB'    = ifelse(auxCodModel != "00", ifelse(dif > 3 | dif < -3, 1, 0), 0),
                          'FLAGBISE' = ifelse(BISERIAL < 0.1, 1, 0), 
                          'FLAGINFIT' = 0,   #ifelse((INFIT < minOutms[indPos]) | (INFIT > maxOutms[indPos]), 1, 0),
                          'FLAGOUTFIT' = 0,  #ifelse((OUTFIT < minOutms[indPos]) | (OUTFIT > maxOutms[indPos]), 1, 0), 
                          'FLAGKEY1' = 0, 'FLAGKEY3' = 0,
                          'FLAGDIFDIS' = ifelse(abs(dif) > 3 & disc > 0.5, 1, 0),
                          'FLAGAZAR'  = ifelse(azar > 0.25 | eeazar > 0.15, 1, 0), 
                          'FLAGCHI2' = 0, #ifelse(p_val_chi2 > 0.1, 1, 0), 
                          'FLAGINFO' = ifelse(max(maxINFO, na.rm = TRUE) == maxINFO, 1, 0), 
                          'FLAGCV' = ifelse(eedif_NEW > 30, 1, 0))])
        tablaFin[, eedif_NEW := paste0(sprintf("%.2f", eedif_NEW), "%")]
        listResults[[auxPru]][["tablaFin"]] <- tablaFin

        # # Guardando media y Varianza
        listResults[[auxPru]][["meanAbil"]] <- meanAbil
        listResults[[auxPru]][["sdAbil"]]   <- sdAbil
      }
      outPath <- auxOutPath
  }
  saveResult(object, listResults)
  return(object)
})

################################################################################
# # Definition of output files
################################################################################

setMethod("outXLSX", "IRT", 
function(object, srcPath = "."){
  require(xlsx)
  load(file.path(srcPath, object@outFile$pathRdata))
  pruebasRead <- names(object@datAnalysis)
  for (kk in pruebasRead) {
     auxPru  <- gsub("(::|\\s)","_", kk)
     wb <- xlsx::createWorkbook()
     # # header style
     csEnc <- CellStyle(wb) + Font(wb, isBold = TRUE) + Fill(foregroundColor = brewer.pal(3, "Greys")[1])+ 
              Border(pen = "BORDER_DOUBLE") + Alignment(h = "ALIGN_CENTER")
     csL2  <- CellStyle(wb) +
              Fill(foregroundColor = brewer.pal(3, "Greys")[1])
                
     # # Hoja de parametros
     nameSheet <- 'Parametros'
     assign(nameSheet, xlsx::createSheet(wb, sheetName = nameSheet))
     auxSalida <- listResults[[kk]]$tablaFin

     # # Eliminando columnas
     colRemove <- c("prueba", "item_blq", "p_val_chi2", 
                    "chi2", "gl_chi2", "FLAGCHI2", "INFIT", "OUTFIT", 
                    "FLAGINFIT", "FLAGOUTFIT", "indPos", 
                    "minOutms", "maxOutms", "codMOD", "intercepto", 
                    "eeinter", "cons")
     auxSalida[, (colRemove) := NULL]
     addDataFrame(auxSalida, 
                  sheet = get(nameSheet), startRow = 1,
                  startColumn = 1, row.names = FALSE,
                  col.names = TRUE, colnamesStyle = csEnc)
     outFile <- file.path(outPath,
                         paste("06IRT_", auxPru,"_V", object@verSalida,
                               ".xlsx", sep = ''))
     

     # # correlación Diccionario

     labelVariable <- c("item", "dir_OP", "dir_ICC", "maxINFO", "A_prop", "A_mAbility", 
                        "B_prop", "B_mAbility", "C_prop", "C_mAbility", "D_prop", "D_mAbility", "E_prop", 
                        "E_mAbility", "F_prop", "F_mAbility", "G_prop", "G_mAbility", "H_prop", "H_mAbility", 
                        "M_prop", "M_mAbility", "O_prop", "O_mAbility", "disc", "eedisc", "dif", "eedif", 
                        "dispersion", "eedisp", "azar", "eeazar", "drift", "eedrift", "dif_NEW", "eedif_NEW", 
                        "codigo_prueba", "keyItem", "Ancla", "order", "TRIED", "RIGHT", "PCT", "LOGIT", "PEARSON", 
                        "BISERIAL", "FLAGPROP", "FLAGKEY2", "CORRELACION", "SUBBLOQUE", "COMPONENTE", "COMPETENCIA", 
                        "FLAGMEAN", "FLAGCORR", "FLAGA", "FLAGB", "FLAGBISE", "FLAGKEY1", "FLAGKEY3", 
                        "FLAGDIFDIS", "FLAGAZAR", "FLAGINFO", "FLAGCV")

     varDescrip <- c("Código del ítem","Dirección de la grafica de opciones de respuesta", 
                    "Dirección de la grafica de la curva caracteristica","Punto de máxima información del ítem",
                    "Proporción de respuesta de la opción A","Promedio de habilidad de los que respondieron la opción A ",
                    "Proporción de respuesta de la opción B","Promedio de habilidad de los que respondieron la opción B",
                    "Proporción de respuesta de la opción C","Promedio de habilidad de los que respondieron la opción C",
                    "Proporción de respuesta de la opción D","Promedio de habilidad de los que respondieron la opción D",
                    "Proporción de respuesta de la opción E","Promedio de habilidad de los que respondieron la opción E",
                    "Proporción de respuesta de la opción F","Promedio de habilidad de los que respondieron la opción F",
                    "Proporción de respuesta de la opción G","Promedio de habilidad de los que respondieron la opción G",
                    "Proporción de respuesta de la opción H","Promedio de habilidad de los que respondieron la opción H",
                    "Proporción de Multimarcas del ítem","Promedio de habilidad de los que respondieron multimarca",
                    "Proporción de Omisiones del ítem","Promedio de habilidad de los que respondieron omisión","Parámetro de discriminación",
                    "Error de estimación del parámetro de discriminación","Parámetro de dificultad","Error de estimación del parámetro de dificultad",
                    "Representa la carga en el primer factor ","Error de estimación  del parámetro dispersión","Parámetro de azar",
                    "Error estimado del parámetro de azar","Medida de funcionamiento diferencial del ítem",
                    "Medida de funcionamiento diferencial del ítem en el tiempo","Parámetro de dificultad en la escala de la prueba",
                    "Coeficiente de variación del parámetro de dificultad en la escala de la prueba","Código de la prueba",
                    "Clave","Indicadora de anclaje de los ítems","Posición del ítem en la corrida","Numero de personas que abordan el ítem",
                    "Numero de personas que responden correctamente el ítem","Porcentaje de respuestas correctas del ítem",
                    "Porcentaje de respuestas correctas del ítem en Logit","Correlación de pearson","Correlación punto biserial",
                    "La proporción de respuesta de alguna opción es menor al 10% o mayor al 90%",
                    "Si la habilidad media de la clave es menor a la habilidad media de otra opción",
                    "Correlación punto biserial excluyendo al ítem","Subloque al que pertenece el ítem",
                    "Componente asociado al ítem","Competencia asociado al ítem","Porcentaje de respuestas correctas del ítem es menor al 10% o mayor al 90%",
                    "Correlación excluyendo al ítem sea menor a 0.1","Si la discriminación es menor a 0.5",
                    "Si la dificultad original es mayor a 3 o menor que -3", "Si la correlación Biserial es menor que 0.1","","",
                    "Si la dificultad en valor absoluto es mayor a 3 y la discriminación es menor 0.5",
                    "Si el azar es mayor a 0.25 o el error de estimación del azar > 0.15",
                    "Indicadora del ítem con la 'mayor' información", "Si el coeficiente de variación es mayor a 30")

     tablaDescri <- data.frame('V1' = labelVariable, 'V2' = varDescrip)
     names(tablaDescri) <- c('Variable', 'Descripción')
     # # Hoja de Diccionario
     nameSheet <- 'Diccionario'
     assign(nameSheet, xlsx::createSheet(wb, sheetName = nameSheet))
     addDataFrame(tablaDescri, 
                  sheet = get(nameSheet), startRow = 1,
                  startColumn = 1, row.names = FALSE,
                  col.names = TRUE, colnamesStyle = csEnc)
     xlsx::setColumnWidth(get(nameSheet), 1, 14)
     xlsx::setColumnWidth(get(nameSheet), 2, 72)
     rows   <- xlsx::getRows(sheet = get(nameSheet),
                             rowIndex = seq(2, 100))
     cells  <- xlsx::getCells(rows, colIndex = 1)
     lapply(names(cells), 
            function(x) xlsx::setCellStyle(cells[[x]], csL2))

     outFile <- file.path(outPath, "06IRT",
                         paste("06IRT_", auxPru,"_V", object@verSalida,
                               ".xlsx", sep = ''))
     xlsx::saveWorkbook(wb, file = outFile)
     listResults[[auxPru]]$fileXLSX <- outFile
  }
  saveResult(object, listResults, srcPath)
})

setMethod("outHTML", "IRT", 
function(object, srcPath){
  require(gridExtra)
  require(ggplot2)
  source(file.path(srcPath, "Function", "gvisUtils.R"))
  load(file.path(srcPath, object@outFile$pathRdata)) # load listResults
  nomPrueba <- object@test@nomTest
  
  cat("<h2> Estad&iacute;sticas por &iacute;tem:", nomPrueba, "</h2>\n")
  # # Imprimiento descripciones
  cat('<p>A continuación se presentan los datos de identificación: código, posición, 
      y clave de los items de la prueba. Estadísticos relevantes para la toma de decisiones 
      como cantidad de personas que contestaron, cantidad y porcentaje de respuestas correctas, 
      dificultad estimada mediante el modelo de 3PL, correlación punto biserial excluyendo al ítem. 
      Además, la gráfica de la curva característica del ítem (ICC) y la gráfica de opciones.</p>')
  cat('<p>Se definieron señales de aviso que indican mal funcionamiento del ítem en cuanto a dificultad, 
      correlación, porcentaje, infit, outfit, pendiente de la clave y promedio de habilidad de la clave.</p>')
  
  # # Cargando parametros
  liDiff    <- object@param$espMean - 3 * object@param$espSd  # -3
  lsDiff    <- object@param$espMean + 3 * object@param$espSd  # 3

  cat("<p>Las señales generadas fueron las siguientes:</p>",
       "<center>",
       "<table style=\"width:83%;\">",
       "<colgroup>",
       "<col width=\"5%\" />",
       "<col width=\"77%\" />",
       "</colgroup>",
       "<thead style=\"color: rgb(255, 255, 255); background-color: rgb(0, 0, 0);\">",
       "<tr class=\"header\">",
       "<th align=\"left\">Señal</th>",
       "<th align=\"left\">Descripción</th>",
       "</tr>",
       "</thead>",
       "<tbody>",
       "<tr role=\"row\" class=\"odd\">",
       "<td align=\"left\">1</td>",
       "<td align=\"left\">La correlación ítem-prueba excluyendo al ítem es menor a <span class=\"math inline\">0.1</span>.</td>",
       "</tr>",
       "<tr role=\"row\" class=\"even\">",
       "<td align=\"left\">2</td>",
       "<td align=\"left\">Las pendientes de las curvas empíricas de dos o más opciones son mayores a 0.05.</td>",
       "</tr>",
       "<tr role=\"row\" class=\"odd\">",
       "<td align=\"left\">3</td>",
       "<td align=\"left\">La pendiente de la curva empírica de la clave del ítem es negativa.</td>",
       "</tr>",
       "<tr role=\"row\" class=\"even\">",
       "<td align=\"left\">4</td>",
       "<td align=\"left\">La pendiente de la curva empírica de la clave del ítem es negativa.El promedio de habilidad de la clave es inferior al promedio de habilidad de alguna de las otras opciones de respuesta.</td>",
       "</tr>",
       "<tr role=\"row\" class=\"odd\">",
       "<td align=\"left\">5</td>",
       "<td align=\"left\">El porcentaje de respuestas por opción es menor a <span class=\"math inline\">10%</span> o mayor a <span class=\"math inline\">90%</span>.</td>",
       "</tr>",
       "<tr role=\"row\" class=\"odd\">",
       "<td align=\"left\">6</td>",
       "<td align=\"left\">La dificultad del ítem, bajo el modelo, es aproximadamente menor a ", liDiff, " o mayor a ", lsDiff, "</td>",
       "</tr>",
       "<tr role=\"row\" class=\"even\">",
       "<td align=\"left\">7</td>",
       "<td align=\"left\">El porcentaje de respuestas correctas es menor a <span class=\"math inline\">10%</span> o mayor a <span class=\"math inline\">90%</span>.</td>",
       "</tr>",
       "<tr role=\"row\" class=\"even\">",
       "<td align=\"left\">8</td>",
       "<td align=\"left\">El porcentaje de respuestas correctas es menor a <span class=\"math inline\">10%</span> o mayor a <span class=\"math inline\">90%</span>.</td>",
       "</tr>",
       "</tbody>",
       "</table>",
       "</center>")
  nomSubPru <- names(object@datAnalysis)

  for(ii in nomSubPru){
    nomAux <- gsub("::|\\s", "_", ii) 
    nomSub <- gsub("^(.+)(::)(.+)", "\\3", ii)
    if (nomAux %in% names(listResults)){    
      if (nomSub == ii) {
        nomSub <- "Calibración Global de la Prueba"
      } else {
        nomSub <- paste0("An&aacute;lisis para ", nomSub)
      }
      cat('<h3 id="IRT_Header_tab"> Mapa Ítems Personas</h3>\n\n')
      lay  <- rbind(c(1,1,1,1,1,2))
      pFin <- suppressWarnings(grid.arrange(grobs = list(listResults[[nomAux]][["itHaMap"]][[1]], 
                               listResults[[nomAux]][["itHaMap"]][[2]]), layout_matrix = lay))
      # # Mapa items personas
      suppressWarnings(grid::grid.draw(pFin))
      
      # # Curva de informacion
      pathGraph  <- object@test@nomTest
      pathGraph  <- gsub("\\)", paste0(" - ", nomSub, ")"), pathGraph)      
      plotInfo <- ggplot(listResults[[nomAux]][["plotInfo"]], aes(x = x, y = y)) + geom_blank() + 
                  geom_line(colour = "red") +
                  ggtitle(paste0("Curva de Información ", 
                            gsub("(\\s+)?SABER 3,\\s?5 y 9 ", "", 
                            pathGraph), "")) +
                  ylab("Información") + xlab("Habilidad") +
                  theme_bw(base_size = 12) 
      if (object@test@codMod != "00") {
        plot(plotInfo)
      }      
      cat('<h3 id="IRT_Header_tab">\n', nomSub, '</h3>\n\n')
      tabHtml <- reporteItem(listResults[[nomAux]]$tablaFin, idPrueba = nomAux)    
      cat(as.character(htmltools::tagList(tabHtml)))
      cat(paste0('<td> <li class="linkxlscol"><a href="', listResults[[nomAux]]$fileXLSX, 
        '"> Descargar <br> informe Excel </a></li>', 
        '</td>'))
    }    
  }
})
