

preBLUP <- function(){
  

  # Rodar lme4 para Blup e Herdabilidade para cada local
  BLUP <- local_blup(dfLocal,duasT)

  # Classificando como alta - media - baixa pressao
  classifica <- classLocations(BLUP, checksCS)
  classifica$Local <- as.character(classifica$Local)
  resumoLoc$local <- as.character(resumoLoc$local)
  # Juntando com os dados de Herdabilidade
  novaTabela <- left_join(classifica,resumoLoc,by=c("Local"="local"))

  # Indice de qualidade/Acuracia
  finalTabela <- finalClass(novaTabela, qControl, limiteAlta, limiteBaixa)
  finalTabela2 <- finalTabela %>% dplyr::select(Local,classe,Controles,Desvio, Acuracia)
  resumoLoc <<- resumoLoc
  return(finalTabela2)
}


classLocations <- function(phenoAll, checks){

  # phenoAll <- BLUP
  # checks <- checksCS

  phenoAll <- phenoAll%>%mutate(Esperado = 0)
  phenoAll <- phenoAll%>%mutate(Check = 0)
  phenoAll <- phenoAll%>%mutate(Tipo="Obs")

  for(i in 1:nrow(phenoAll)){
    for(j in 1:nrow(checks)){
      if(phenoAll$genotipo[i] == checks$genotipo[j]){
        phenoAll$Esperado[i] <- checks$Esperado[j]
        phenoAll$Tipo[i] <- checks$Tipo[j]
        phenoAll$Check[i] <- 1
        j=nrow(checks)+1
      }else{
        j=j+1
      }
    }
  }


  phenoAll$blup <- as.numeric(phenoAll$blup)
  phenoAll$Esperado <- as.numeric(phenoAll$Esperado)



  # Limpando locais que não tem checks
  semChecks <- as.data.frame(tapply(phenoAll$Check,phenoAll$local,sum))
  colnames(semChecks)<-c("blups")
  semChecks <- semChecks %>% filter(semChecks$blups != 0)

  # Vetor com os nomes dos locais que possuem checks
  nomesLocais <- rownames(semChecks)

  # Criando um dataframe para armazenar o resultado
  classFinal <- data.frame("Local"=as.character(nomesLocais),
                           "classe"= as.character("classe"),
                           "Probabilidade"= as.numeric(0),
                           "Controles"=as.character("controles"),
                           "Desvio"=as.numeric(0))

  # Classificando os locais
  for(i in 1:length(nomesLocais)){
    # Filtrando por local
    selectedLocal <- phenoAll %>% filter(local == nomesLocais[i])
    classFinal[i,1] <- nomesLocais[i]

    #Verificando se possui checks para as 4 classes
    tipos <- as.data.frame(tapply(checks$Esperado,checks$Tipo,mean))
    tipos$tipo <- rownames(tipos)
    colnames(tipos) <- c("notas","tipos")
    ansTipos <- c("NA","NA","NA","NA")
    ansTipos[1] <- "T"  %in% tipos$tipos
    ansTipos[2] <- "MT" %in% tipos$tipos
    ansTipos[3] <- "MS" %in% tipos$tipos
    ansTipos[4] <- "S"  %in% tipos$tipos

    # Vetor com os checks
    checks <- selectedLocal %>% filter(Check == 1)
    chiDF <- data.frame(tapply(checks$blup,checks$genotipo,mean)) %>% data.frame(tapply(checks$Esperado,checks$genotipo,mean))
    colnames(chiDF) <- c("Obs","Esp")

    # Calculando chi quadrado
    chiDF$X2 <- (chiDF$Obs - chiDF$Esp)^2/chiDF$Esp
    classFinal[i,3] <- pchisq(sum(chiDF$X2),nrow(chiDF)-1)

    # Soma das diferenças para ajudar na classificação
    chiDF$Diff <- chiDF$Obs - chiDF$Esp
    classFinal[i,5] <- sum(chiDF$Diff) # para auxiliar na classificação

    # Armazenando os tipos de checks do local
    classCheck = ""
    for(z in 1:length(tipos$tipos)){
      classCheck1 <- tipos$tipos[z]
      classCheck <- paste0(classCheck1," ",classCheck)
    }

    classCheck <- na.omit(classCheck)
    classFinal[i,4] <- classCheck



}

 return(classFinal)
}


finalClass<- function(enterTable, qualC, limA, limB){
  # enterTable = novaTabela
  # qualC=qControl
  # limA=limiteAlta
  # limB=limiteBaixa

  enterTable <- na.omit(enterTable)
  outTabela <- enterTable

  for(i in 1:nrow(outTabela)){

    somaDiff <- outTabela$Desvio[i]
    qc <- outTabela$Acuracia[i]

    if( somaDiff > limA && qc > qualC){

      outTabela[i,2] <- "Alta"

    }else if(somaDiff > limB && somaDiff < limA){

      outTabela[i,2] <- "Media"

    }else if (somaDiff < limB) {

      outTabela[i,2] <- "Baixa"

    }else if(somaDiff > limA && qc < qualC){

      outTabela[i,2] <- "Low QC"

    }
  }
  outTabela$Acuracia <- round(outTabela$Acuracia,2)
  outTabela$Desvio <- round(outTabela$Desvio,2)

 return(outTabela)

}






