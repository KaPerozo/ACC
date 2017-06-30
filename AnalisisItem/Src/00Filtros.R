################################################################################
# # 03TCT.R
# # R Versions: R version 3.0.2 i386
# #
# # Author(s): Areli Moreno y Adriana Clavijo
# # SABER 
# # Compute clasic statistics from TCT
# #
# # Inputs: Dictionary and list of data.frames with data from
# #         00CrearRdata.R
# #
# # Outputs: A xlsx file with the statistics from TCT
# #
# # File history:
# #   20111123: Creation
# #   20140211: Update
# #   20160106: Adaptation for S4 Clases (Jorge Carrasco and Nelson Rodriguez) 
################################################################################

################################################################################
# # Definition of class and parameters
################################################################################
# # Heritage class Analysis
Filtros <- setClass("Filtros", contains = "Analysis")

setMethod("initialize", "Filtros", function(.Object, ..., param) {
  .Object@outFile$pathRdata <- "../Output/00Crear/conteoFiltros.Rdata"
  .Object <- callNextMethod()
})

Filtros <- function(test, paramExp = NULL){
  paramDefault <- list(fileCopy = NULL, 
  	                   indiInfo = NULL, 
  	                   kOmissionThreshold = 0.5, 
                       flagConjunta = FALSE, 
                       flagNOESTU = FALSE) 
  if (!is.null(paramExp)) {
    isDefault <- setdiff(names(paramDefault), names(paramExp))
    paramExp  <- c(paramExp, paramDefault[isDefault])
  } else {
    paramExp <- paramDefault
  }
  
  if (is.null(paramExp$fileCopy)){
  	stop("_____ERROR_____ Especifica el archivo de copietas 'fileCopy'")
  }

  if (is.null(paramExp$indiInfo) & test@exam %in% c("SABERPRO", "SABERTYT")){
  	stop("_____ERROR_____ Especifica el archivo de informacion basica 'indiInfo'") 	
  }
  cat("----->Se haran los filtros con los siguientes parametros: \n")
  print(paramExp)
  object <- new("Filtros", test = test, param = paramExp)
  return(object)
}

################################################################################
# # Definition of codeAnalysis Method
################################################################################

setMethod("codeAnalysis", "Filtros",
 function(object){
    # # Crear lista para guardar informacion
    listResults <- list()
    pruebasRead <- names(object@test@datBlock)
    
    # # Lectura de copia
    pathCopy    <- file.path(inPath, getParams(object)$fileCopy)
    if (!file.exists(pathCopy)){
      stop("_____ERROR_____ No se encuentra el archivo de copia :'( :'( :'(")  
    }
    sospechosos <- read.delim(pathCopy, sep ="\t", header=TRUE, 
                              colClasses = "character")

    # # Verificando forma del SNP
    if (object@test@exam %in% c("SABERPRO", "SABERTYT")){    
      sospechosos[,"APLICACION"] <- substr(sospechosos[,"SNP"],1,7)
      isSNPALL <- !grepl("\\w{2}\\d{5}.+", object@test@datBlock[[1]][["oriBlock"]][[1]])
      if (all(isSNPALL)) {
        sospechosos[,"SNP1"] <- substr(sospechosos[,"SNP"], 8, 15)
      } else {
        sospechosos[,"SNP1"] <- sospechosos[,"SNP"]
        cat("... Se trabajara con el SNP completo\n")
      }
    }

    if (object@test@exam == "SABER11"){
       sospechosos <- plyr::rename(sospechosos, c("APLICACIÓN" = "APLICACION"))
       sospechosos[,"SNP1"] <- sospechosos[,"SNP"]
    }

    # # Lectura de Individual Basicos
    if (object@test@exam %in% c("SABERPRO", "SABERTYT")){
      pathInfo <- file.path(inPath, getParams(object)$indiInfo)
      infoINR  <- gsub("(.+)\\..+", "\\1.Rdata", pathInfo)
      if (!file.exists(infoINR)) {
          datInfo  <- read.delim(pathInfo, sep ="\t", header=TRUE, 
                           colClasses = "character")
          datInfo  <- subset(datInfo, select = c("CITA_SNEE", "GRRE_ID", 
                                                 "GRRE_NOMBRE"))
          datInfo  <- data.table(datInfo)
          datInfo[, SNP := substr(CITA_SNEE, 8, 14)]
          save(datInfo, file = infoINR)
      } else {
        load(infoINR)
      }
      if (nrow(datInfo) == 0) {
        stop("-----ERROR-LECTURA----- Cambiar Encoding a UTF-8 o revisar archivo:\n", pathInfo)
      }
    }

    # # Extrae info sobre grado and test   
    for (kk in pruebasRead) {
      datSblq       <- object@test@datBlock[[kk]]$oriBlock 
      auxSelected   <- names(datSblq)
      datSblqCal    <- object@test@datBlock[[kk]]$calBlock
      dictVarPrueba <- object@test@dictionaryList$variables
      dictVarPrueba <- subset(dictVarPrueba, elimina == "06")
  	  varId <- dictVarPrueba[, "id"]
      varId <- intersect(names(datSblq), varId)

      # # Parametro de conjunta
      flagConjunta <- object@param$flagConjunta

      # # Seleccion copia
      auxForma <- gsub("(pba|PBA|UOF|pbaF)", "", gsub("\\.con", "", kk))
      if (object@test@exam == "SABER11"){
         auxForma <- gsub("(\\d{3})(.+)?", "\\1", auxForma)
      }

      if (flagConjunta){
        baseName <- strsplit(gsub("(.+)(JN)$", "\\1", auxForma), "")[[1]]
        baseName <- paste(baseName[1:(length(baseName)-2)], collapse = "")
        auxForma <- paste0(baseName, ".+")
      } 

      print(str(sospechosos))
      print(auxForma)
      if (!flagConjunta){
	      sospPrueb <- subset(sospechosos, 
                            PRUE_CODIGOICFES %like% auxForma & 
	  	                      APLICACION == object@test@periodo)
      } else{
        sospPrueb <- sospechosos
      }
      print(str(sospPrueb))
      
      # # Exclusion de colados 
      datSblq[, indColados := 0]
      if ("colados" %in% names(getParams(object))) {
        pathColados <- file.path(inPath, getParams(object)$colados)
        colados <- read.delim(pathColados, colClass="character")
        datSblq[SNP %in% colados[,"SNP"], indColados := 1]
      } 
      
      datSblq[!SNP %in% sospPrueb[,"SNP1"], indCopia := 0]
      datSblq[SNP %in% sospPrueb[,"SNP1"], indCopia := 1]
     
      errBaseCop <- sospPrueb[,"SNP1"][!sospPrueb[,"SNP1"] %in% datSblq[, SNP]] 
      
      if (any(!c("Tipo_de_Evaluado", "Estado_Final") %in% names(datSblq))){
        datSblq[, Tipo_de_Evaluado := "1"]
        datSblq[, Estado_Final := "1"]
      }

      # # Seleccion No estudiante

      datSblq[, indNE := ifelse(Tipo_de_Evaluado != "1" & !flagConjunta, 1, 0)]  	  
      
      # # Seleccion No presentes
   	  datSblq[, indNP := ifelse(!Estado_Final %in% c("1", "8") & !flagConjunta, 1, 0)]
	
      # # Seleccion de Omisiones
      koPram <- object@param$kOmissionThreshold
      indOmi <- rowMeans(datSblq[, varId, with= FALSE] == "O") > koPram
      datSblq[, indOMI := ifelse(indOmi & !flagConjunta, 1, 0)]
    
	    # # Seleccion No estudiante No presentes
      datSblq[, indNENP := ifelse(Tipo_de_Evaluado != "1" & indNP == 1 & !flagConjunta, 1, 0)]

      # # Extremos Inferiores
      indExtL <- rowMeans(datSblqCal[, varId, with= FALSE], na.rm = TRUE) == 0
      indExtL <- indExtL & !indOmi
      datSblq[, indEXTI := ifelse(indExtL == 1, 1, 0)]

      # # Extremos Superiores
      indExtS <- rowMeans(datSblqCal[, varId, with= FALSE], na.rm = TRUE) == 1
      datSblq[, indEXTS := ifelse(indExtS, 1, 0)]

      # # Indicadora global
      if (!object@param$flagNOESTU){ 
        datSblq[, indTOTAL := ifelse(indCopia + indNENP + indOMI + indNP + indNE + indColados > 0, 1, 0)]
      } else {
        datSblq[, indTOTAL := ifelse(indCopia + indNENP + indOMI + indNP + indColados> 0, 1, 0)]
      }
      
      datSblq[, indInclu := ifelse(indTOTAL == 0, 1, 0)]
      datSblq[, indTodo := indTOTAL + indInclu]

      if (datSblq[, sum(indTodo)] != nrow(datSblq)){
      	stop(">>>>>>>>>> :'( Algo malo en los conteos :'(:'(")
      }
      # # Tablas data.tabla  
     
      tabResumen <- datSblq[, lapply(.SD,sum), .SDcols = grep("ind.+", 
      	                   names(datSblq), value = TRUE)]
      tabResumen <- t(data.frame(tabResumen))
      tabResumen <- data.frame("Descripción" = c(
                      "Evaluados que no pertenecen al Examen",      	              
                      "Evaluados Sospechosos de Copia",
      	              "Evaluados que no son estudiantes",
                      "Evaluados que no estuvieron presentes",
                      "Evaluados que no respondieron al menos el 50% de la prueba",                      
                      "Evaluados que no son estudiantes presentes",
                      "Evaluados que respondieron incorrectamente todas las preguntas",
                      "Evaluados que respondieron correctamente todas las preguntas",
                      "Total de evaluados excluidos del análisis",
                      "Total de evaluados incluidos en el análisis",
                      "Total de evaluados en la prueba"), 
                      "Cantidad" = tabResumen[, 1], stringsAsFactors = FALSE)
      
      totalEvaluados <- tabResumen[nrow(tabResumen), "Cantidad"]
      totalAnalisis  <- tabResumen[nrow(tabResumen) - 1, "Cantidad"]

      # # Carreta del inicio
      nomPrueba <- gsub(".+\\((.+)\\)", "\\1", object@test@nomTest)
      nomPrueba <- gsub("(\\(|\\))", "", nomPrueba)
    
      textoIni <- paste("La prueba de", nomPrueba, "consta de", length(varId), 
      	                "ítems de selección múltiple con única respuesta y tiene", 
      	                totalEvaluados, "registros leídos, la tabla 1 muestra la distribución", 
      	                "de las exclusiones realizadas para el análisis.")

      textoGREE <- paste("En total, se incluyeron", totalAnalisis, "personas para el análisis. La tabla 2",
                         "muestra la cantidad de registros por grupo de referencia.")

      # # Conteos por grupo de referecia (TyT Pro)
      if (object@test@exam %in% c("SABERPRO", "SABERTYT")){
        if (any(duplicated(datInfo[["SNP"]]))) {
          stop("ERROR TECNOLOGIA------- IndividualBasicos con duplicados")
        }

        datSblq <- merge(datSblq, datInfo, by = "SNP", all.x = TRUE)
        datSblq[is.na(GRRE_NOMBRE), GRRE_NOMBRE := "SIN GRUPO DE REFERENCIA"]
        tabResumen2 <- datSblq[, sum(indTodo), by = c("GRRE_NOMBRE", "indInclu")]      
        tabResumen2 <- reshape2::dcast(tabResumen2, GRRE_NOMBRE ~ indInclu, 
                                       value.var = "V1", fill = 0)
        if (!"0" %in% names(tabResumen2)){
          tabResumen2[, "0"] <- 0
        }
        tabResumen2[, "Total"] <- tabResumen2[, "0"] + tabResumen2[, "1"]
        names(tabResumen2) <- c("Grupo de referencia", "Excluidos", "Incluidos", "Total")
        tabResumen2 <- tabResumen2[, c("Grupo de referencia", "Incluidos", "Excluidos", "Total")]

        listResults[[kk]] <- list('parrafo' = textoIni, 'tabla' = tabResumen, 
                                  'parrafo2' = textoGREE, 'tablaGRRE' = tabResumen2)
      } else {
     	listResults[[kk]] <- list('parrafo' = textoIni, 'tabla' = tabResumen, 
                                'parrafo2' = textoGREE)
      }
      saveResult(object, listResults)

      # # Filtros de la pruebas
      snps <- subset(datSblq, indInclu == 1)[, SNP]
      object@test@datBlock[[kk]]$oriBlock <- subset(object@test@datBlock[[kk]]$oriBlock, 
      	                                            SNP %in% snps, select = auxSelected)
      object@test@datBlock[[kk]]$calBlock <- subset(object@test@datBlock[[kk]]$calBlock, 
      	                                            SNP %in% snps)
   }
   return(object@test)
})

################################################################################
# # Definition of outXLSX y outHTML Method
################################################################################

setMethod("outXLSX", "Filtros", 
function(object, srcPath = "."){
})


setMethod("outHTML", "Filtros", 
function(object, srcPath = "."){
  require(xtable)
  require(DT)
  load(file.path(srcPath, object@outFile$pathRdata)) # load listResults
  nomPrueba <- object@test@paramLect$conDirs  
  for (kk in nomPrueba) {
  	cat(listResults[[kk]][["parrafo"]], "<br><br>")
  	# # Estilo de la tabla
    cat("<style>",
    "#conteos {",
        "font-family: \"Trebuchet MS\", Arial, Helvetica, sans-serif;",
        "border-collapse: collapse;",
        "width: 100%;",
    "}",
    "#conteos td, #conteos th {",
        "border: 1px solid #ddd;",
        "text-align: left;",
        "padding: 8px;",
    "}",
    "#conteos td:nth-child(even){",
        "text-align: center;",
    "}",
    "#conteos tr:hover {background-color: #ddd;}",
    "#conteos th {",
        "padding-top: 12px;",
        "padding-bottom: 12px;",
        "background-color: #044C92;",
        "color: white;",
    "}",
    "#conteos th:nth-child(even){",
        "text-align: center;",
    "}",
    "</style>\n\n<center>\n\n")
   
   bold.somerows <- function(x) gsub('BOLD\\((.*)\\)',paste('<b> \\1 <\\b>'),x)

    for(ii in 1:nrow(listResults[[kk]][["tabla"]])){
       auxLabel <- listResults[[kk]][["tabla"]][ii, "Descripción"]
       isTotal  <- grepl("Total.+", auxLabel)
       #auxColor <- ifelse(isTotal, "bgcolor = \"81A5C8\"", "")
       auxNegrita <- ifelse(!isTotal, "", 'BOLD(')
       auxFin     <- ifelse(!isTotal, "", ')')
       listResults[[kk]][["tabla"]][ii, "Descripción"] <- paste0(auxNegrita, auxLabel, auxFin)
       listResults[[kk]][["tabla"]][ii, "Cantidad"]    <- paste0(auxNegrita, listResults[[kk]][["tabla"]][ii, "Cantidad"], auxFin)
    }

    listResults[[kk]][["tabla"]][, "Cantidad"] <- as.character(listResults[[kk]][["tabla"]][, "Cantidad"])
    tableHtml <- xtable(listResults[[kk]][["tabla"]], caption = "Tabla 1: Distribución de Exclusiones")
    print(tableHtml, type = "html", html.table.attributes = "id=\"conteos\" style=\"width:65%;\"", 
          include.rownames = FALSE, sanitize.text.function = bold.somerows)    
	  cat("</tbody></table>")
    cat("</center>\n", "<br><br>")  

    if (object@test@exam %in% c("SABERPRO", "SABERTYT")){
      htlmTab <- datatable(listResults[[kk]][["tablaGRRE"]], caption = "Tabla 2: Distribución de registros por grupo de referencia", 
                           rownames = FALSE)    
      cat(listResults[[kk]][["parrafo2"]], "<br><br>")
      cat(as.character(htmltools::tagList(htlmTab)))
    }

  }

})