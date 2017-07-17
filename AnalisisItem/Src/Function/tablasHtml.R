reportTCT <-  function(x, codPrueba, subPrueba = "SubConjunto", pathExcel = NULL, 
                       alertText = "") {
  require(DT)
  #  x <- listResults[[1]]
  if (!"etiqu" %in% names(x)){
    x[, "etiqu"] <- x[, "subCon"]
  }
  x <- data.table(x)

  # # Organizando resultados
  x[, ind_Sub := as.double(1:nrow(.SD)), by = c("pba_subCon")]
  maxItems <- max(x[, ind_Sub])
  x[, nItems := sum(unique(nItems)), by = c("pba_subCon")]
  colsUni <- c("subCon", "etiqu", "alphaTotal", "nItems", "pathCMC")
  iniCol  <- x[, lapply(.SD, unique), .SDcols = colsUni, by = c("pba_subCon")]
  iniCol  <- iniCol[!duplicated(pba_subCon), ]
  cols   <- c("alphaTotal", "raw_alpha", "corIt", "corItSub")
  x[,  (cols) := round(.SD, 3), .SDcols = cols]
  colMeasure <- c("id", "raw_alpha", "corIt", "corItSub")
  isTest <- x[, all(corIt == corItSub)] # Si por subconjunto y glogal se la misma confiabilidad
  if (isTest){
    x[, corItSub := NULL]
    colMeasure <- colMeasure[colMeasure != "corItSub"]
  }
  x <- suppressWarnings(melt(data.table(x), id = c("ind_Sub", "pba_subCon"), 
                             measure = colMeasure))
  
  # # Representando grado-area-SubConjunto en una sola fila
  x <- as.data.frame(x)
  dcastCont <- dcast.data.table(data.table(x), pba_subCon ~ ind_Sub + variable, 
                                fun.aggregate = list, value.var = c("value"), drop = "FALSE")
  dcastCont <- merge(iniCol, dcastCont, by = c("pba_subCon")) 
  dcastCont[pba_subCon == codPrueba, subCon := codPrueba]
  dcastCont[pba_subCon == codPrueba, etiqu  := codPrueba]
  dcastCont[, pba_subCon := NULL]
  
  # # Ordenando columnas
  colName <- lapply(1:maxItems, function(x) paste0(x, "_", colMeasure))
  names(colName) <- as.character(1:maxItems)
  colPos   <- lapply(colName, function(x) sapply(x, function(z) which(z == names(dcastCont))))
  finTable <- ""
                
  for (ww in names(colPos)) {
    iterInd <- sapply(colPos[[ww]], function(x)  
                      paste0("            '<td>'+ d[", x + 1, "] +'</td>'+\n"))
    auxRow  <- paste0("if (d[", min(colPos[[ww]]) + 1 , '] != "" && d[',  
                      min(colPos[[ww]]) + 1, '] != null ', ") {    ", 
                      "auxTable = auxTable  + '<tr>'+\n            '<td>", ww , "</td>'+\n",
                      paste(iterInd, collapse = ""), "        '</tr>';\n }")
    finTable <- paste(finTable, auxRow, sep = "\n")
  }

  # # Renombrando primeras columnas
  dcastCont <- data.frame(dcastCont)
  names(dcastCont)[1:4] <- c("Codigo", "subCon", "&alpha;", 
                             "No_Items")
  dcastCont[, "&alpha;"] <- round(as.numeric(dcastCont[, "&alpha;"]), 3)
  if (all(as.character(dcastCont$Codigo) == as.character(dcastCont$subCon))) {
    nColIni               <- c(0, 2, 6:(ncol(dcastCont) + 1))  
  } else {
    nColIni               <- c(0, 6:(ncol(dcastCont) + 1))  
  }
  linkExcel <- ''
  if (!is.null(pathExcel)){
    linkExcel <- paste0('<td colspan="3"><li class="linkxlscol"><a href="', pathExcel, 
                 '"> Descargar <br> informe Excel </a></li></td>', 
                 '<td colspan="2"><li class="linkxlscol"><a href="../../../../Manuales/01_EsquemadeAnalisis.docx">', 
                 '<b> Manual de <br> interpretación </b></a></li></td>')
  }
  save(dcastCont, file = "dcastCont.Rdata")
  # # Tablas en Html de los items
  htmlTab1 <- datatable(cbind(' ' = '', dcastCont), escape = FALSE,
    options = list(
      columnDefs = list(
        list(visible = FALSE, targets = nColIni),
        list(orderable = FALSE, className = 'details-control', targets = 1)
      ),
    initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}")
    ),  
    callback = JS(
    paste0("table.column(1).nodes().to$().css({cursor: 'pointer'});    
    var format = function(d) {",
    "var auxTable = ''; \n", 
    finTable,
    "\n return '<table border=\"0\" cellpadding=\"5\" cellspacing=\"0\" style=\"padding-left:95px;\" width=\"100%\" >'+\n",
              "'<tr>' + 
    '<th>Id</th>' + 
    '<th>Item</th>' + 
    '<th>&alpha; - <br>Excluyendo el ítem</th>' +", 
    "'<th>Correlaci&oacute;n<br>- Bloque</th>' + ", 
    ifelse(isTest, "", "'<th>Correlaci&oacute;n<br>- &Iacute;ndice</th>' + "),
    "'<th  colspan=\"4\" rowspan=\"", length(colPos), 
    "\" align = \"center\">", alertText, 
    "<a href=\"javascript:void(0)\" onclick=\"popup(\\'' + d[6].replace('..', '') +'\\')\">",
    "<img align=\"middle\" style=\"width:660px;height:550px;\" src=\"'+ 
    d[6] + '\"></a></th>' + auxTable + '", linkExcel, "</tr></table>'};
    table.on('click', 'td.details-control', function() {
    var tr = $(this).closest('tr');
        var row = table.row( tr );
 
        if ( row.child.isShown() ) {
            
            // This row is already open - close it
            row.child.hide();
            tr.removeClass('shown');
            table.fnAdjustColumnSizing();
        }
        else {
            
            // Open this row
            row.child( format(row.data()) ).show();
            tr.addClass('shown');            
        }
    } );"
  )))
  return(htmlTab1)
}

reporteItem <-  function(x, idPrueba, carNR = c("O", "M"), dirBase = getwd()) {
  require(DT)
  require(googleVis)

  # # Diagrama de opciones de respuesta
  # x <- tablaFin; idPrueba <- "f123"
  itObs <- grep("(_mAbility|_prop)", names(x), value = TRUE)
  itObs <- x[, c("item", itObs, "keyItem"), with = FALSE]
  keyIt <- cbind(x[, c("item", "keyItem"), with = FALSE], 'flagKey' = FALSE)
  setnames(keyIt, "keyItem", "categoria")
  itObs <- melt(itObs, id = 1, measure = 2:(ncol(itObs) - 1))
  itObs <- itObs[, list(item, 
                'categoria' = gsub("(\\w)_.+", "\\1", variable), 
                'variable' = gsub("\\w_(.+)", "\\1", variable), value)]
  itObs <- merge(itObs, keyIt, by = c("item", "categoria"), all.x = TRUE)
  itObs[is.na(itObs$flagKey), flagKey := TRUE]

  # # Dividir para cada uno de los items
  itObs <- split(itObs, f = itObs$item)
  itObs <- lapply(itObs, function(xx) {
                  auxX <- data.table(dcast(xx, item + categoria + flagKey ~ variable))
                  auxX <- cbind(auxX, 'Habilidad Promedio' = round(auxX$mAbility, 3))
                  auxX[, mAbility := paste0("Promedio de habilidad: ", round(mAbility, 3))]            
                  setnames(auxX, c("prop", "categoria", "mAbility", "flagKey"), 
                           c("Porcentaje", "Porcentaje.annotation", "Porcentaje.annotationText", 
                             "Porcentaje.certainty"))
                  auxX[, categoria := Porcentaje.annotation]
                  auxX <- subset(auxX, !categoria %in% carNR)
                 return(auxX)})
  
  # # Construccion del grafico con googleVis
  varYtomar <- c("Porcentaje", "Porcentaje.certainty", 
                "Porcentaje.annotation", "Porcentaje.annotationText",
                "Habilidad Promedio")
  varXtomar <- "categoria"
  chartID   <- paste0("grafGPREP", gsub("-|\\s", "_", idPrueba))
  exaCombo <- gvisComboChart(itObs[[1]], xvar = varXtomar, chartid = chartID,
                          yvar = varYtomar,
                          options=list(width = 470, height = 310, 
                                       annotations = "{alwaysOutside: true}",
                                       vAxes="[{title:'Porcentaje', format:'#,###%', 
                                              titleTextStyle: {color: 'blue'},
                                              textStyle:{color: 'blue'}, textPosition: 'out'},
                                              {title:'Habilidad',
                                              titleTextStyle: {color: 'grey'},
                                              textStyle:{color: 'grey'}, textPosition: 'out'}]",
                                       vAxis="{gridlines: {color: 'transparent'}}",
                                       legend ='{position: "bottom"}',
                                       series="[{type:'bars', targetAxisIndex:0}, 
                                                {type:'line', targetAxisIndex:1,
                                                color:'grey'}]"))
  # # Formato de % en los toltips
  auxForma <- c('data.addRows(datajson);\n',
                "var formatter = new google.visualization.NumberFormat(\n",
                '{negativeColor: "red", negativeParens:  true,pattern:"#,###.#%"});\n',
                "formatter.format(data, 1);\n")
  exaCombo$html$chart["jsData"] <- gsub('data.addRows\\(datajson\\);',
                                    paste(auxForma, collapse = ""),
                                    exaCombo$html$chart["jsData"])

  # # Agregando parametros que permiten cambiar datos por item
  exaCombo$html$chart["jsData"] <- paste0("var current = 4", exaCombo$html$chart["jsData"])
  exaCombo$html$chart["jsData"] <- gsub('data.addRows\\(datajson\\);',
                                        'data.addRows(datajson[current]);', 
                                        exaCombo$html$chart["jsData"])
  formatDatos <- function(z) {
    z <- data.frame(z)[, gsub(" ", ".", c(varXtomar, varYtomar))]
    gsub("\\],\\[", "],\\\n[", gsub("\\\n", "", gvisFormat(z)$json))
  }
  datChart <- sapply(itObs, function(xx) return(formatDatos(xx)))
  datChart <- paste0("var datajson = [", paste(datChart, collapse = ",\n"), "];")
  exaCombo$html$chart["jsData"] <- gsub("var datajson .+(data\\.addColumn\\('string','categoria'\\))", 
                                        paste0(datChart, "\\1"), exaCombo$html$chart["jsData"])
  # # Final codigo java Script grafico de opciones
  jsonGPREP <- exaCombo$html$chart[c("jsData", "jsDrawChart", "jsDisplayChart")] 
  pathJson  <- file.path(dirBase, "../../Doc/Js")
  fileJson  <- file.path(pathJson, paste0(chartID, ".jsp"))
  dir.create(pathJson, recursive = TRUE, showWarnings = FALSE)

  cat(jsonGPREP, file = fileJson)
  cat("<script src=\"Js/", paste0(chartID, ".jsp"), "\"></script>\n", sep = "")
  cat("<script type=\"text/javascript\" src=\"https://www.google.com/jsapi?callback=displayChart", chartID,"\"></script>\n", sep = "")  # # Numero total de alertas

  # # Numero total de alertas
  x[, nAlertas := sum(FLAGA, FLAGB, FLAGBISE, FLAGCORR, FLAGCHI2,
                      FLAGKEY1, FLAGKEY2, FLAGKEY3, FLAGMEAN, FLAGDIFDIS,
                      FLAGDIF, FLAGAZAR, na.rm = TRUE), by = "item"]

  # # Redondeando 3 decimales
  cols <- c("dif_NEW", "eedif", "disc", "eedisc", "azar", "eeazar", "maxINFO")
  x[,  (cols) := round(.SD, 3), .SDcols = cols]

  removeCol <- NULL
  
  if (x[, unique(codMOD)] %in% c("05", "06")) {
    removeCol  <- 7
    auxMensaje <- "'<td colspan=\"1\" align=\"center\"><font size=\"43\" color=\"red\"> ITEM SIN ESTIMACION IRT </font></td>'"
  }
  
  if (x[, unique(codMOD)] == "07") {
    auxMensaje <- "'<td colspan=\"1\" align=\"center\"><font size=\"43\" color=\"red\"> ITEM SIN ESTIMACION IRT </font></td>'"
  }

  if (x[, unique(codMOD)] == "00") {
    removeCol  <- c(5, 7:9)
    auxMensaje <- "'<td colspan=\"1\" align=\"center\"> </td>'"
  }

  # # Ordenando columnas 
  x <- x[, list(item_blq, item, nAlertas, disc, dif_NEW, azar, maxINFO, Ancla, item_blq,                          # 2  - 10
                item, SUBBLOQUE, COMPONENTE, COMPETENCIA, keyItem,                                     # 11  - 15
                "", TRIED, RIGHT, PCT, "", BISERIAL,                                                   # 16 - 21
                'disc' = ifelse(is.na(disc), "NA", paste0(disc, " (", eedisc, ") ")),                                             # 22
                'dif'  = ifelse(is.na(dif_NEW), "NA", ifelse(eedif_NEW != "NA", paste0(dif_NEW, " (", eedif_NEW, ") "), dif_NEW)), dir_OP, dir_ICC,                   # 23 - 25
                'Mult' = paste0("M: ", round(M_prop * 100, 2), "% (",  round(M_mAbility, 3), ")"),     # 26
                'Omis' = paste0("O: ", round(O_prop * 100, 2), "% (",  round(O_mAbility, 3), ")"),     # 27
                'Chis' = ifelse(is.na(chi2), "NA", paste0(chi2, "(pval = ", p_val_chi2, ") - gl = ", gl_chi2)),                   # 28
                FLAGA, FLAGB, FLAGBISE, FLAGCORR, FLAGINFIT, FLAGKEY1, FLAGKEY2, FLAGKEY3, FLAGMEAN,   # 29 - 37
                FLAGOUTFIT, FLAGPROP, FLAGDIFDIS, FLAGAZAR, FLAGCHI2,                                  # 38 - 42
                'azar' = ifelse(is.na(azar), "NA", paste0(azar, " (", eeazar, ") ")),                                             # 43
                'posReporte' = match(x$item, names(itObs)) - 1, FLAGINFO, FLAGDIF)]                                     # 44 - 46
                #'diffRescal' = ifelse(is.na(diffRescal), "NA", paste0(diffRescal, " (", eediffRescal, ") ")))]  # 47
  x[, canName := 8 - (is.na(TRIED) + is.na(RIGHT) + is.na(PCT) + is.na(BISERIAL) + 
                 is.na(disc) + (dif == "NA") + is.na(azar) + (Chis == "NA"))]  # 47
  # # Renombrando primeras columnas
  x <- data.frame(x)
  names(x)[1:8] <- c("Nombre", "C&oacute;digo", "Alertas", 
                     "Discriminaci&oacute;n", "Dificultad", 
                     "Azar", "Informaci&oacute;n", "Ancla")
  x[is.na(x)] <- ""
  
  initCol   <- 10 # Defecto es 3PL (Disc, Azar, Diff)
  removeCol <- c(removeCol, initCol:(ncol(x) + 1))
  
  # # Tablas en Html de los items
  htmlTab1 <- datatable(#filter = 'bottom',
    cbind(' ' = '', x), escape = FALSE,
    options = list(
      columnDefs = list(
        list(visible = FALSE, targets = c(0, removeCol)),
        list(orderable = FALSE, className = 'details-control', targets = 1)
      ),
    initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}")
    ),  
    callback = JS(
    paste0("table.column(1).nodes().to$().css({cursor: 'pointer'});    
    var imprimirImagenes = function(imgOP, imgICC){
      if(imgICC != '' && imgICC != 'null'){
        return '<td colspan=\"3\" align=\"center\"> <a href=\"javascript:void(0)\" onclick=\"popup(\\''+ imgOP.replace('..', '') +'\\')\"><img align=\"middle\" style=\"width:472px;height:300px;\" src=\"'+ imgOP +'\"></a></td>' +
               '<td colspan=\"4\" align=\"center\"> <a href=\"javascript:void(0)\" onclick=\"popup(\\''+ imgICC.replace('..', '') +'\\')\"><img align=\"middle\" style=\"width:472px;height:300px;\" src=\"'+ imgICC +'\"></a></td>'

      } else if (imgOP != '' && imgOP != 'null') {
        return '<td colspan=\"6\" align=\"center\"> <a href=\"javascript:void(0)\" onclick=\"popup(\\''+ imgOP.replace('..', '') +'\\')\"><img align=\"middle\" style=\"width:472px;height:300px;\" src=\"'+ imgOP +'\"></a></td>' +", 
        auxMensaje,
      "} else {
        return '<td colspan=\"7\" align=\"center\"><font size=\"43\" color=\"red\"> ITEM SIN ESTIMACION IRT </font></td>'
      }
    };
    var mensajeInfo = function(flag){
        if (flag == 1) {
          return '<font size=\"3\" color=\"green\"> ITEM CON MAXIMA INFORMACI&Oacute;N </font>'
        } else {
          return ''
        }
    };

    var imprimirDato = function(x, label, flag, flagB){
      
      if ( x != '' && x != 'null' & x != 'NA') {
        if (flagB == 1){
          auxB  = '<b>';
          auxBF = '</b>';
        } else {
          auxB  = '';
          auxBF = '';
        }
        if (flag == 1) {
          return auxB + '<tr>' +
               '<td colspan=\"2\"> <font color=\"red\">'+ label +'</font></td>'+
               '<td colspan=\"1\"> <font color=\"red\">'+ x +'</font></td>' +
               '</tr>' + auxBF
        } else {
          return '<tr>' +
               '<td colspan=\"2\">'+ label +'</td>' +
               '<td colspan=\"1\">'+ x +'</td>' +
               '</tr>'
        }
      } else {
        return ' '
      }
    };
    var format = function(d) {
      current    = d[44];
      FLAGA      = d[29];
      FLAGB      = d[30];
      FLAGBISE   = d[31];
      FLAGCORR   = d[32];
      FLAGINFIT  = d[33];
      FLAGKEY1   = d[34];
      FLAGKEY2   = d[35];
      FLAGKEY3   = d[36];
      FLAGMEAN   = d[37];
      FLAGOUTFIT = d[38];
      FLAGPROP   = d[39];
      FLAGDIFDIS = d[40];
      FLAGAZAR   = d[41];
      FLAGCHI2   = d[42];
      FLAGINFO   = d[45];
      FLAGDIF     = d[46];
      titulos = '<table border=\"0\" cellpadding=\"5\" cellspacing=\"0\" style=\"padding-left:95px;\" width=\"100%\" >'+\n",
              "'<tr>'+
                '<th>Nombre</th>'+
                '<th>C&oacute;digo</th>'+
                '<th>Bloque</th>'+ 
                '<th>Componente</th>'+
                '<th>Competencia</th>'+
                '<th>Clave</th>'+
                '<th>&Delta;&alpha;</th>'+
              '</tr>'+
              '<tr>'+
                '<td>'+ d[10] +'</td>'+
                '<td>'+ d[11] +'</td>'+
                '<td>'+ d[12] +'</td>'+
                '<td>'+ d[13] +'</td>'+
                '<td>'+ d[14] +'</td>'+
                '<td>'+ d[15] +'</td>'+
                '<td>'+ d[16] +'</td>'+
              '</tr>'+
              '<tr>'+
                '<td colspan=\"7\">&nbsp;</td>'+
              '</tr>'+
              '<tr>'+
                '<th colspan=\"2\">Estadisticos del &iacute;tem</th>'+
                '<th colspan=\"1\">Valor</th>'+
                '<th colspan=\"4\">Distribuci&oacute;n opciones de respuestas</th>'+
              '</tr>';
              
    return titulos  + 
    '<tr>'+
      '<td colspan=\"2\">  N </td>'+
      '<td colspan=\"1\">'+ d[17] +'</td>'+
      '<td colspan=\"4\" rowspan=\"'+ d[47] +'\"> <div id=\"", chartID, "\" style=\"margin-left: 200;\"> </div>' +
      '</td>'+
    '</tr>'+
           imprimirDato(d[18], 'Correctas', 0) +
           imprimirDato(d[19] + '%', 'Porcentaje de aciertos', FLAGMEAN, 0) +           
           imprimirDato(d[20], 'Correlaci&oacute;n biserial excluyendo &iacute;tem', FLAGCORR, 0) +           
           imprimirDato(d[21], 'Correlaci&oacute;n biserial', FLAGBISE, 0) +               
           imprimirDato(d[22], 'Discriminaci&oacute;n', FLAGA, 0) +               
           imprimirDato(d[23], 'Dificultad', FLAGDIF, FLAGB) +
           imprimirDato(d[43], 'Azar', FLAGAZAR, 0) +   
           imprimirDato(d[28], 'estad&iacute;stico &Chi;<sup>2</sup>', FLAGCHI2, 0) +   
    '<tr>'+
      '<td colspan=\"3\">'+ mensajeInfo(FLAGINFO) + '</td>'+
      '<td colspan=\"2\">'+ d[26] +'</td>'+
      '<td colspan=\"2\">'+ d[27] +'</td>'+
    '</tr>'+
    '<tr>'+
     imprimirImagenes(d[24], d[25]) +
    '</tr>'+", "\n'</table>';
    };
    table.on('click', 'td.details-control', function() {
    var tr = $(this).closest('tr');
        var row = table.row( tr );
 
        if ( row.child.isShown() ) {
            
            // This row is already open - close it
            row.child.hide();
            tr.removeClass('shown');
        }
        else {
            
            // Open this row
            row.child( format(row.data()) ).show();
            tr.addClass('shown');
            drawChart", chartID, "();
        }
    } );"
  )))
  return(htmlTab1)
}