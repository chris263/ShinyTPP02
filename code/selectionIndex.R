

cleaning_data <- function(inFile, crit){
  # inFile <- altaP_DF

  if(crit=="yes"){
    listaZero <- as.data.frame(tapply(inFile$yield, inFile$genotipo, mean))
    colnames(listaZero) <- "yield"
    listaZero <- listaZero %>% filter(yield != 0)
    listaZero$Nomes <- rownames(listaZero)

    # Vetor com os genotipos que permanecem
    vZero <- listaZero$Nomes

    # Filtrando apenas para os nomes que devem permanecer
    preOut <- inFile[which(inFile$genotipo%in%vZero),]
    # Removendo dados errados, baixa nota e baixíssimo Yield
    # Lista de plots a serem removidos
    preOut2 <- preOut %>% filter(Nota <= 3, yield < 6)
    plotRM <- preOut2$barcode
    outFile <<- preOut[-which(preOut$barcode%in%plotRM),]
  }else{
    preOut2 <- inFile %>% filter(Nota <= 3, yield < 6)
    plotRM <- preOut2$barcode
    outFile <<- inFile[-which(inFile$barcode%in%plotRM),]
  }


}

local_blup <- function(inLocal, crt4){
  # inLocal = dfLocal
  # crt4=duasT
  if(crt4 == "no"){resumoLoc[,-3]}
  library(lme4)
  BLUPN <- c()
  for(i in 1:length(inLocal)){
    selectedLoc <- myDFF%>%filter(local==inLocal[i])
    modeloLocalN <- lmer(Nota~Rep+(1|genotipo), data=selectedLoc)

    # Capturando as variancias para calcular H2
    variance = as.data.frame(VarCorr(modeloLocalN))
    gvar = variance [1,"vcov"]
    resvar = variance [2, "vcov"]
    H2 = gvar/ (gvar + resvar)

    if(crt4 == "yes"){
      selectedLoc$yield <- as.numeric(selectedLoc$yield)
      selectedY <- selectedLoc %>% filter(yield>0)

      testeY <- sum(selectedY$yield)
      if(testeY>0){
        modeloLocalY <- lmer(yield~Rep+(1|genotipo), data=selectedLoc)
        varianceY = as.data.frame(VarCorr(modeloLocalY))
        gvarY = varianceY [1,"vcov"]
        resvarY = varianceY [2, "vcov"]
        H2Y = gvarY/ (gvarY + resvarY)
      }else{
        H2Y = H2
      }
      # Armazenando os dados de H2
      resumoLoc$local[i] <- inLocal[i]
      resumoLoc$H2N[i] <- H2
      resumoLoc$H2Y[i] <- H2Y
      # resumoLoc$Acuracia[i] <- (H2+H2Y)/2
    }else{
      resumoLoc$local[i] <- inLocal[i]
      resumoLoc$H2N[i] <- H2
      # resumoLoc$Acuracia[i] <- H2
    }


    # Extraindo os blups para Nota
    adj = coef(modeloLocalN)$genotipo
    adj = adj[1]
    adj =cbind(adj,inLocal[i],rownames(adj))

    colnames(adj)<-c("blup","local","genotipo")

    locaisNome <- c(1:nrow(adj))
    BLUPN <- rbind(BLUPN,adj)

  }

  resumoLoc$Acuracia <- (2*resumoLoc$H2N+resumoLoc$H2Y)/3
  resumoLoc <<- resumoLoc
  return(BLUPN)
}

calc_blups <- function(inModel,cr){

  # inModel <- modeloConjunto
  # cr <- duasT

  inBlupNota <- as.data.frame(inModel$U$`u:genotipo`$cNota)
  inBlupNota$genotipo <- rownames(inBlupNota)
  colnames(inBlupNota) <- c("fNota","genotipo")
  inBlupNota <- inBlupNota %>% arrange(genotipo)

  if(cr == "yes"){
    inBlupYield <- as.data.frame(inModel$U$`u:genotipo`$cYield)
    inBlupYield$genotipo <- rownames(inBlupYield)
    colnames(inBlupYield) <- c("fYield","genotipo")
    inBlupYield <- inBlupYield %>% arrange(genotipo)
    # inBlupYield <- inBlupYield %>% filter(fYield>0)
    fBLUP <- left_join(inBlupYield,inBlupNota,by=c("genotipo"="genotipo"))
    fBlup <<- fBLUP %>% relocate(genotipo, .before = fYield)
  }else{
    fBLUP <<- inBlupNota
  }





}

efeitoNY <- function(effDF){
  # aa <- as.numeric(effDF$Nota)
  # bb <- as.numeric(effDF$yield)
  lineaR <- lm(yield~Nota, data=effDF)

  # y = bx+a
  a <- coef(lineaR)[1]
  b <- coef(lineaR)[2]

  nnt <- unique(effDF$Nota)

  lineaR <- lm(yield~Nota, data=effDF)

    # y = bx+a
  a <- coef(lineaR)[1]
  b <- coef(lineaR)[2]

  pYield1 <- b*3+a
  pYield2 <- b*4+a
  resDiff<- round(pYield2 - pYield1,2)
  names(resDiff)<-c("yield")
  return(resDiff)
}



selectionIndex <- function(inModelo, inWgt){
  # inModelo <- modeloConjunto
  # inWgt <- pesos

  # Specify the additive variance and correlation: 1 on the diagonal
  conjuntaV <- summary(inModelo)$varcomp
  addVar <- c(Yield=conjuntaV$VarComp[1],CS=conjuntaV$VarComp[3])
  calcCor <- conjuntaV$VarComp[2]/(sqrt(conjuntaV$VarComp[1])*sqrt(conjuntaV$VarComp[3]))
  addCor <-  matrix(c(1, calcCor, calcCor, 1), nrow=2)
  addSD <- diag(sqrt(addVar)) # Convert cor to SD
  addCov <- addSD %*% addCor %*% addSD

  # Specify the error correlation and calculate error covariance
  errVar <- c(Yield=conjuntaV$VarCompSE[1],CS=conjuntaV$VarCompSE[3])
  calcErr <- conjuntaV$VarCompSE[2]/(sqrt(conjuntaV$VarCompSE[1])*sqrt(conjuntaV$VarCompSE[3]))
  errCor <- matrix(c(1, calcErr, calcErr, 1), nrow=2)
  errSD <- diag(sqrt(errVar))
  errCov <- errSD %*% errCor %*% errSD

  phenCov <- addCov + errCov

  indexSE <- solve(phenCov)%*%addCov%*%inWgt

  rownames(indexSE) <- c("Yield","CS") # Ordem dos pesos
  indexSE[2] <- (-1)*indexSE[2] # Invertido por queremos a menor nota
  return(indexSE)

}


ranking_data <- function(inBlup,inDFF,cr2){

  # Organizando o Blup para Nota
  modeloAllNota <- lmer(Nota ~ local + Rep + (1| genotipo), data = inDFF)
  BlupNota <- coef(modeloAllNota)$genotipo
  BlupNota <- BlupNota[1]
  finalNota <- cbind(rownames(BlupNota),BlupNota$`(Intercept)`)
  colnames(finalNota) <- c("genotipo","blupNota")
  finalNota <- as.data.frame(finalNota)

  if(cr2=="yes"){ # Se for analisar Yield em conjunto
    matrixBlup <- data.frame(yield=as.numeric(inBlup$fYield),nota=as.numeric(inBlup$fNota) )
    matrixBlup <- as.matrix(matrixBlup)
    yieldCS <-  matrixBlup %*% indexSel
    inBlup$Indice <- yieldCS
    modeloAllYield <- lmer(yield ~ local + Rep + (1| genotipo), data = inDFF)
    BlupYield <- coef(modeloAllYield)$genotipo
    BlupYield <- BlupYield[1]
    finalYield <- cbind(rownames(BlupYield),BlupYield$`(Intercept)`)
    colnames(finalYield) <- c("genotipo","blupYield")
    finalYield <- as.data.frame(finalYield)
    finalData <- left_join(finalNota,finalYield, by=c("genotipo"="genotipo"))
    preResult <- left_join(finalData,inBlup,by=c("genotipo"="genotipo"))
    preResult <- preResult[,-c(4:5)] # que é isso?
    preResult <- na.omit(preResult)
    preResult <- preResult %>% arrange(desc(Indice))
  }else{
    finalData <- finalNota
    preResult <- left_join(finalData,inBlup,by=c("genotipo"="genotipo"))
    # preResult <- preResult[,-c(4:5)] # que é isso?
    # preResult <- na.omit(preResult)
    preResult <- preResult %>% arrange(blupNota)
  }

  return(preResult)
}

qualityGeno <- function(entrada, retd, cr3,crt6){
  # entrada <- dadosGenoLoc
  # retd <- resultados
  # cr3 <- duasT

  f10 <- entrada[1,] # 1 linha adicionada apenas para criar o df.
  f10$nLocais <- as.numeric(0)
  f10$genoQC <- as.numeric(0)

  f0 <- unique(entrada$genotipo)
  for(i in 1:length(f0)){

    nomeGenotipo <- f0[i] #i
    f1 <- entrada %>% filter(genotipo == nomeGenotipo, Rep == 1)
    nLinhas <- nrow(f1)

    f2 <- f1[1,]
    f2$nLocais <- as.numeric(nLinhas)
    f2$genoQC <- as.numeric(0)



    if(nLinhas > crt6){

      f2$genoQC[1] <- round(100*(sum(as.numeric(f1$Acuracia))/nLinhas),2)

    }else if (nLinhas > 0 && nLinhas <= crt6){

      f2$genoQC[1] <- round(100*(sum(as.numeric(f1$Acuracia))/crt6),2)

    }else if (entrada$nLocais[i] == 0){
      f2$genoQC[1] <- "NA"
    }

    f10 <- rbind(f10,f2)

  }

  f10 <- f10[-1,] # Retira a primeira linha que adicionei apenas para criar o df.

  preFINAL <- left_join(f10,retd,by=c("genotipo"="genotipo"))

  if(cr3 == "yes"){
    resFINAL <- data.frame(preFINAL$genotipo, as.numeric(preFINAL$blupNota), as.numeric(preFINAL$blupYield),
                           preFINAL$Indice,preFINAL$nLocais, preFINAL$genoQC)
    colnames(resFINAL) <- c("genotipo","blupNota","blupYield","Indice","nLocais","Acuracia(%)")
    resFINAL$blupNota <- round(resFINAL$blupNota,0)
    resFINAL$blupYield <- round(resFINAL$blupYield,2)
    resFINAL <- resFINAL %>% arrange(desc(Indice))
  }else{
    resFINAL <- data.frame(preFINAL$genotipo, as.numeric(preFINAL$blupNota),
                           preFINAL$nLocais, preFINAL$genoQC)
    colnames(resFINAL) <- c("genotipo","blupNota","nLocais","Acuracia(%)")
    resFINAL$blupNota <- round(resFINAL$blupNota,0)
    resFINAL <- resFINAL %>% arrange(blupNota)
  }

  resFINAL <<- na.omit(resFINAL)
}

