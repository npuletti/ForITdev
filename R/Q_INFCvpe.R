Q_INFCvpe <-
function(TreeList) {
  print(head(TreeList))
  # Preparing/check data:
  print("Controlla i tuoi dati:")
  # >> i dati sono aggregati per gruppi di piante con stessa specie-diametro-altezza? y/n
  #    se "yes" indicare la colonna delle frequenze, 
  Q_aggr <- readline("i dati sono aggregati per gruppi di piante con stessa specie-diametro-altezza? [Y/N] ")
  if(Q_aggr=="Y") {
    col_aggr_name <- readline("indica il nome esatto della colonna delle frequenze: ")
  } else {col_aggr_name <- NA} # "i dati non sono aggregati per gruppi di piante con stessa specie-diametro-altezza"}
  
  # >> esiste una colonna che riporta la vitalita' degli alberi ? y/n
  #    se "yes": indica la colonna degli alberi vivi
  #    se "yes": indica il/i valore/i o il/i codice/i riferito/i ad "alberi vivi"
  Q_vita <- readline("esiste una colonna che riporta la vitalita' degli alberi? [Y/N] ")
  if(Q_vita=="Y") {
    col_vita_name <- readline("indica il nome esatto della colonna degli alberi vivi: ")
    vita_value <- readline("indica il valore esatto riferito agli alberi vivi: ")
  } else {col_vita_name <- NA} # "non esiste una colonna che riporta la vitalita' degli alberi"}
  
  # >> indica la colonna dei diametri
  #    sono espressi in cm? y/n [se no indica il coefficiente di passaggio a cm]
  col_dbh_name <- readline("indica il nome esatto della colonna dei diametri: ")
  
  Q_dbh <- readline("diametri espressi in cm? [Y/N] ")
  if(Q_dbh=="Y") {dbh_coeff <- 1} else {dbh_coeff <- "indica il coefficiente di passaggio a cm: "}
  
  # >> indica la colonna delle altezze
  #    sono espresse in m? y/n [se no indica il coefficiente di passaggio a m]
  col_H_name <- readline("indica il nome esatto della colonna della altezza: ")
  
  Q_H <- readline("altezza espressa in m? [Y/N] ")
  if(Q_H=="Y") {H_coeff <- 1} else {H_coeff <- "indica il coefficiente di passaggio a m: "}
  
  # >> indica la colonna della specie ... >> e poi:
  #    estrarre dalla colonna le occorrenze e chiedere il matching con le specie presenti nel DB NFI
  col_spp_name <- readline("indica il nome esatto della colonna che contiene il codice della specie: ")
  
  col_list <- c(col_aggr_name,col_vita_name,col_dbh_name,col_H_name,col_spp_name)
  col_list <- col_list[!is.na(col_list)]
  
  if(Q_vita=="N"){
    eData <- TreeList[,col_list]
  } else {eData <- TreeList[TreeList[,col_vita_name]==vita_value,col_list]}
  
  colnames(eData)[which(colnames(eData)==col_spp_name)] <- "species_old"
  colnames(eData)[which(colnames(eData)==col_dbh_name)] <- "d130"
  colnames(eData)[which(colnames(eData)==col_H_name)] <- "Htot"
  
  
  # creare una procedura per il matching delle specie del DB inserito rispetto alla lista INFC
  spp_codici_INFC <- sqldf("SELECT INFCstats.specie as species_new, INFCstats.spg FROM INFCstats GROUP BY specie, spg")
  spp_list_INFC <- data.frame(spp_codici_INFC, id_spp=1:44)
  
  spp_old_list <- as.character(unique(eData[,"species_old"]))
  
  spp_new_list <- NULL
  for(i in 1:length(spp_old_list)){
    print(spp_list_INFC)
    spp_new <- readline(paste("assegna il numero specie al tuo codice ", spp_old_list[i], ": ", sep=''))
    spp_new_list <- c(spp_new_list, as.numeric(unlist(strsplit(spp_new, ":"))) )
  }
  
  out_spp <- data.frame(species_old=spp_old_list,
                        species_new=spp_list_INFC[spp_new_list,1],
                        codice=spp_list_INFC[spp_new_list,2])
  print(out_spp)
  decisione <- readline("Va bene questa lista di specie? [Y/N] ")
  
  if(decisione=="N"){
    spp_new_list <- NULL
    for(i in 1:length(spp_old_list)){
      print(spp_list_INFC)
      spp_new <- readline(paste("assegna il numero specie al tuo codice ", spp_old_list[i], ": ", sep=''))
      spp_new_list <- c(spp_new_list, as.numeric(unlist(strsplit(spp_new, ":"))) )
    }
    out_spp <- data.frame(species_old=spp_old_list,
                          species_new=spp_list_INFC[spp_new_list,1],
                          codice=spp_list_INFC[spp_new_list,2])
    print(out_spp)
  }
  
  eData2 <- sqldf("SELECT A.*, B.species_new FROM eData A JOIN out_spp B USING(species_old)")
  
  eData3 <- sqldf("SELECT A.*, B.spg FROM eData2 A JOIN spp_codici_INFC B USING(species_new)")
  eData4 <- eData3[,-which(colnames(eData3)=="species_old")]
  
  ####
  return(eData4)
}
