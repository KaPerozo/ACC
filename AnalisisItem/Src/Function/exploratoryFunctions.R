################################################################################
# # univariateFunctions.R
# # R Versions: 3.2.3 x64
# #
# # Author(s): Jorge Mario Carrasco and William Acero
# #
# # SB ALL
# # Description: Function to compute polychoric correlation of items and
# #              make parallel analysis
# #
# # Inputs: NA
# #
# # Outputs: NA
# #
# # File history:
# #   20160601: Creation, adpatación codigos de 04Exploratorio
# #   20140201: 
################################################################################

##############################################################
# # Function to make parallel analysis
##############################################################

MakeExploratory <- function(kFactors, rotation, dictVarPrueba, corExpBlock){
  Items <- dictVarPrueba[, 'id']
  blockPCA   <- princomp(covmat = corExpBlock$cor)
  
  if(kFactors == 1){
    rotation = ''
  }

  # # Factorial loadings
  blockEFAP  <- paste(rotation,"(blockPCA$loadings[, 1:kFactors])", sep='')
  blockEFA   <- eval(parse(text = blockEFAP))
  if(kFactors == 1){
    cargasFA   <- blockEFA
  } else{
    cargasFA   <- unclass(blockEFA$loadings)
  }
  cargasFAdf <- data.frame(Item = Items, cargasFA = cargasFA)
  colnames(cargasFAdf) <- c('Item', paste("Factor", 1:kFactors))

  # # Correlation
  if(kFactors == 1){
      compCorrelation <- as.matrix(1)
  } else {
    if (rotation == "oblimin"){
      compCorrelation <- blockEFA$Phi
    } 
    if (rotation == "Varimax"){
      compCorrelation <- diag(kFactors)
    }
  }

  rownames(compCorrelation) <- paste("Comp.", 1:kFactors, sep = '')
  colnames(compCorrelation) <- paste("Comp.", 1:kFactors, sep = '')

  # # Communalities
  if(kFactors == 1){
      communalities <- as.vector(cargasFA^2)
  } else{
      common          <- blockEFA$loadings %*% t(blockEFA$loadings)
      communalities   <- diag(common)
  }
  communalitiesdf <- data.frame(Item = Items, comunalidades = communalities)
  colnames(communalitiesdf) <- c('Item', 'Extraction')

  # # Variance Explained 
  component <- kFactors
  blockDesv <- blockPCA$sdev^2
  blockVar  <- blockDesv[kFactors]
  var       <- (blockDesv / sum(blockDesv))[kFactors]
  cumVar    <- (cumsum(blockDesv)/sum(blockDesv))[kFactors]
  expVar    <- data.frame(component, cumVar)
  colnames(expVar) <- c('Component', '% Retained Variance') 
  parList <- list(Loadings = cargasFAdf,
                  Communalities = communalitiesdf,
                  Correlation = compCorrelation,
                  VarExplained = expVar)   
  return(parList)
}

##############################################################
# # Function to make Confirmatory and Exploratory analysis
##############################################################

MakeCorrelation <- function(kkBlock, outPathSamPba, verDataIn, 
                            auxPru, semilla, tamMue, flagExplo = TRUE,
                            varId, useCor){  
   require(polycor)
   # # Obtain sample
   auxNom  <- ifelse(flagExplo, "Exploratory", "Confirmatory")
   auxData <- file.path(outPathSamPba,
                        paste0("sample", auxNom, "_", auxPru, "_V",
                        verDataIn, ".RData"))
   if (!file.exists(auxData)) {  
      # # Select Exploratory or Confirmatory
      set.seed(as.numeric(semilla))
      if (tamMue != 1) {
        isExploratory <- sample(x = rownames(kkBlock), 
                                size = ceiling(nrow(kkBlock) * tamMue))
        if (flagExplo) {
            indSelected <- as.numeric(isExploratory)
        } else {
            indSelected <- -as.numeric(isExploratory)
        } 
      } else {
        indSelected   <- 1:nrow(kkBlock)
      }
      kkBlockSAM    <- kkBlock[indSelected, varId]
      rownames(kkBlockSAM) <- kkBlock[indSelected, "SNP"]
      nObs       <- nrow(kkBlockSAM)
      kkBlockSAM <- structure(kkBlockSAM)
      save(kkBlockSAM, nObs, file = auxData)    
    } else {
      load(auxData)
    }

    # # Obtain correlation matrix
    corData <- file.path(outPathSamPba,
                         paste("corExploratory_", auxPru, "_V",
                         verDataIn, ".RData", sep = ""))
    if (!file.exists(corData)) {
      corBlock  <- try(hetcor(kkBlockSAM, pd = TRUE, use = useCor, 
                       std.err = FALSE, ML = FALSE))
      if(class(corBlock) != "try-error"){
        save(corBlock, file = corData)
        cat("Listo correlación ", auxNom, " :", auxPru , "\n")
      } 
    } else {
      load(corData)
    }
    return(list(corBlock = corBlock, sampleBlock = kkBlockSAM))
}
