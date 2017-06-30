################################################################################
# # pruebaClass.R
# # R Versions: R version 3.0.0 i386
# #
# # Author(s): Jorge Mario Carrasco
# #
# # SABER 11	
# # Description: Función creada para definar la clase prueba, la clase análisis y
# #              reporte prueba para las definir las funciones de análisis de item.
# #
# # Outputs: Funcio nes para crear objetos y metodos
# #
# # File history:
# #   20160302: Creation
# #   
# # ToDo:
# #       
#################################################################################

################################################################################
# # Global Definitions
################################################################################
inPath  <- file.path("..","Input")
srcPath <- file.path("..","Src")
docPath <- file.path("..","Doc")
funPath <- file.path("Function")
outPath <- file.path("..", "Output")
logPath <- file.path("..", "Log")

options(encoding = "UTF-8")
source(file.path(funPath, "tablasHtml.R"))
source(file.path(funPath, "pruebaClass.R"))
source("00Filtros.R")
source("03TCT.R")
source("04Exploratorio.R")
source("06IRT.R") 

################################################################################
# # Execute 
################################################################################

dirPandoc <- file.path(Sys.getenv("APPDATA"), "..", "Local\\Pandoc") 
Sys.setenv(RSTUDIO_PANDOC = dirPandoc)
#Sys.setenv(RSTUDIO_PANDOC = "C:\\Program Files (x86)\\Pandoc")

vecJson  <- list.files(inPath, "\\.json", full.names = TRUE)
for (fileJson in vecJson){
  if ('listTests' %in% ls()) {
  	rm(listTests)
    gc(reset = TRUE)
  }
  try(listTests <- analyzeTests(fileJson, getDatBlocks = TRUE))
  #armaIdentifica(listTests)
  
  if ("listTests" %in% ls()){
    jointReports(listTests, fileJson, pathJS = "../../../../lib", 
                 flagView = FALSE)
  }
}

publishRepo(vecJson, pathDest = "\\\\icfesserv5\\Analisisitems$", 
            flagActualizar = TRUE)

################################################################################
# # Depuración metodo 
################################################################################
# prueba0 <- listTests[["Test1"]]
# prueba0 <- readSupplies(prueba0)
# object  <- IRT(test = prueba0, paramExp = list("kOmissionThreshold" = 1))

#object  <- listTests[["TestNOES"]]@listAnal[["IRT"]]
#codeAnalysis(object)
#outHTML(object)

# prueba0 <- new('Test', path = "JUNTURAS/EK20161/exam717/PBAF000401JN", 
# 	           exam = "SABERTYT", codMod = "07", verInput = 1, 
#                periodo = "EK20161", nomTest = "SABER T&T(Lectura Crítica Conjunta)", 
#                paramLect = list("conDirs" = "pbaF000401JN.con"))
# object <- IRT(test = prueba0, paramExp = list("kOmissionThreshold" = 1))
# codeAnalysis(object)
# outHTML(object)