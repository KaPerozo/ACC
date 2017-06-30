library(data.table)
library(WriteXLS)

ruta1 <- "C:\\Users\\jcarrasco\\Documents\\2017\\SABER11\\20171_3PL_Metodo2_EMPERICAL\\output\\calibracion\\01_Estudiantes\\JUNTAS\\SERIE-AC20171-AC20171-TODOS\\MOD-CN"
file1 <- "CALIBRACION.PBA083CN.PAR"
nameActual <- "PBA083CN"


parHisto  <- try(ReadBlParFile(file1, ruta1))
itemHis        <- data.table(parHisto[,c("item", "dif", "disc", "azar")])
itemDiffFile   <- paste("salidas/", nameActual, ".PAR", sep = "")
parActual <- data.table(try(ReadBlParFile(itemDiffFile, ruta1)))


  parActual <- merge(itemHis, parActual, all.y = TRUE, by = "item", 
                     suffixes = c(".base",".new"))
  parActual[, A := disc.new / disc.base ]
  parActual[, B := dif.base - A * dif.new]
  parActual[, A := parActual[!is.na(A), mean(A)]]
  parActual[, B := parActual[!is.na(B), mean(B)]]

  # # Escalar parametros y fijar items de anclaje
  parActual[, disc.equa := disc.new / A]
  parActual[, dif.equa  := A * dif.new + B]
  parActual[, azar.equa := azar.new]
  obsAnclas <- parActual[, sum(abs(dif.equa - dif.base) + 
                         abs(disc.equa - disc.base), na.rm = TRUE)]
  if (obsAnclas > 0.001) {
    stop("Existe unas diferencias en los parametros de anclas de ", str(obsAnclas))
  }
  parActual[!is.na(disc.base), disc.equa := disc.base]
  parActual[!is.na(dif.base),  dif.equa := dif.base]
  parActual[!is.na(disc.base), azar.equa := azar.new]

  # # Lectura de archivo .PAR original
  auxParEMPI <- readLines(itemDiffFile)

  # # Consolidar nuevo archivo de parametros
  for(column in c('disc.new', 'dif.new', 'azar.new', 'disc.equa', 'dif.equa', 'azar.equa')){
    if (any(is.na(parActual[, column]))) {
      stop("El calculo o algunos de los parametros es NA ")      
    }
    if (column %in% c('disc.equa', 'dif.equa', 'azar.equa')) {
      charCol <- sprintf("%.5f", as.numeric(parActual[[column]]))
    } else {
      charCol <- sprintf("%.5f", abs(as.numeric(parActual[[column]])))
    }
    parActual[, column := charCol, with = FALSE]
  }
  parActual[, item := paste0("(", item, ")")]
  parActual <- data.frame(parActual)
  parActual[, "pat"]  <- apply(parActual[, c('item', 'disc.new', 'dif.new', 'azar.new')], 
                               1, paste, collapse = "(.+)(\\s|\\-)")  
  fixSigno <- ifelse(substr(parActual[, "dif.equa"], 1,1) == "-", "", " ")
  parActual[, "gsub"] <- paste0('\\1\\2 ', parActual[, "disc.equa"], '\\4', 
                               fixSigno, parActual[, "dif.equa"], '\\6 ', 
                               parActual[, "azar.equa"])     
  for (ii in 1:nrow(parActual)) {
    auxParEMPI <- gsub(parActual[ii, "pat"], parActual[ii, "gsub"], auxParEMPI)
  }
  file.copy(itemDiffFile, gsub("\\.PAR", ".EMPIRICAL", itemDiffFile))
  cat(auxParEMPI, file = itemDiffFile, sep = "\n")
  
  # # Comprobar anclas
  comparaFile <- file.path(outPath, paste("salidas/", nameActual, "_valida.xlsx", sep = ""))
  parActual <- try(ReadBlParFile(itemDiffFile, outPath))
  comparaANC(parHisto = parHisto, parActual = parActual, outPath = comparaFile)