# Preparing files
prepro <- function(){
  library(dplyr)
  library(ggplot2)
  rawPheno <<- readxl::read_xlsx("data/analysis-23-10.xlsx",sheet = "Analysis")
  checksCS <<- readxl::read_xlsx("data/analysis-23-10.xlsx", sheet="Checks")
  sinN <<- read.table("data/sinonimos.txt", header = T, sep="\t")

  # Criando uma coluna para ter nomes unicos para os ensaios
  rawPheno$TrialYear <- paste0(rawPheno$TrialID,rawPheno$Year)

  myDF <- data.frame(as.character(rawPheno$barcode),as.character(rawPheno$TPP),
                     as.character(rawPheno$TrialYear),as.character(rawPheno$genotipo),
                     as.character(rawPheno$Year),as.numeric(rawPheno$Rep),
                     as.numeric(rawPheno$Nota),as.numeric(rawPheno$yield), 
                     as.numeric(rawPheno$Stg),as.character(rawPheno$Year))

  colnames(myDF)<- c("barcode","TPP","local","genotipo","Ano","Rep","Nota","yield","Stage","Year")
  # Padronizando nomes
  # Removendo espaços
  myDF$genotipo <- gsub(" ", "", myDF$genotipo, fixed = TRUE)
  myDF$local <- gsub(" ", "", myDF$local, fixed = TRUE)

  # Padronizando para letras maiusculas
  myDF$genotipo <- toupper(myDF$genotipo)
  myDF$local <- toupper(myDF$local)



  # Definindo parametros

  duasT <<- "yes" # Analisar yield e CS (yes = analisa os dois. or no=analisa apenas CS) 
  Stage <<- 6 # Filtrando para stage
  pesos <<- c(Yield=60, CS=40) # Pesos para o index. Essa ordem é importante
  rmYC <<- "no" # remover os plots que não possuem dados de yield (yes or no)

  qControl <<- 0.60 # Controle de qualidade para alta pressão
  limiteAlta <<- 3 # Significancia do desvio para ser alta P
  limiteBaixa <<- (-2) # Significancia do desvio para ser baixa P
  years<<-unique(myDF$Year) # Anos Disponíveis
  filterYear <<- "yes" # Capturar dados de 2 anos?
  minLoc <<- 4 # Numero minimo de locais sem penalizacao na acuracia

  myDFF <<- filter_locais(myDF, Stage, years, filterYear, sinN) # remove locais sem dados de CS

}

curation <- function(){
  entradasLoc <- as.data.frame(tapply(myDFF$genotipo, myDFF$local,length))
  entradasLoc <- entradasLoc %>% mutate(local = rownames(entradasLoc))

  colnames(entradasLoc)[1] <- "Entradas"
  entradasLoc <- entradasLoc %>% arrange(Entradas)
  entradasLoc <<- na.omit(entradasLoc)
}

getLoc <- function(){
  dfLocal <<- entradasLoc$local
  resumoLoc <<- data.frame(local = c(1:length(dfLocal)),H2N=c(1:length(dfLocal)),
                        H2Y=c(1:length(dfLocal)),Acuracia=c(1:length(dfLocal)))
}

#filtrando para locais que possuem dados de corn stunt
filter_locais <- function(inLoc, stg, yrs, crt5, sinon){
  # inLoc <- myDF
  # stg <- Stage
  # yrs <- years
  # crt5 <- filterYear
  # sinon <- sinN

  # Alterando os sinonimos
  sin1<- sinon$genotipo
  for(i in 1:length(sin1)){
    inLoc$genotipo <- gsub(sinon$genotipo[i],sinon$Novo[1], inLoc$genotipo)
  }

  # Remover se o trial tem menos de 80 entradas
  entLoc <- as.data.frame(tapply(inLoc$genotipo, inLoc$local,length))
  entLoc <- entLoc %>% mutate(local = rownames(entLoc))

  colnames(entLoc)[1] <- "Entradas"
  entLoc <- entLoc %>% filter(Entradas > 5) %>% arrange(Entradas)

  vLoc <- entLoc$local

  if(crt5 == "yes"){
    y1 <- yrs[1]
    y2 <- yrs[2]
    stg2 <- stg-1
    inLoc$Stage <- as.numeric(inLoc$Stage)
    outLoc1 <- inLoc[which(inLoc$local%in%vLoc),] %>% filter(Stage == stg) %>% filter(Year== y1)
    outLoc2 <- inLoc[which(inLoc$local%in%vLoc),] %>% filter(Stage == stg2) %>% filter(Year== y2)

    outLoc <- rbind(outLoc1, outLoc2)
  }else{
    outLoc <- inLoc[which(inLoc$local%in%vLoc),] %>% filter(Stage == stg)
  }

  return(outLoc)
}



parsing_data <- function(stageData, nomes_entrada){
  # Criando um vetor para guardar o numero da coluna
  index_nomes <- c(1:length(nomes_selecionados))

  for (i in 1:length(nomes_selecionados)){
     index_nomes[i] <- which( colnames(stageData)==nomes_entrada[i] )
     colnames(stageData)[index_nomes[i]] <- novos_nomes[i]
   }

  stageData$genotipo <- gsub(" ", "", stageData$genotipo, fixed = TRUE)
  stageData$local <- gsub(" ", "", stageData$local, fixed = TRUE)

  stageData$genotipo <- toupper(stageData$genotipo)
  stageData$local <- toupper(stageData$local)

  stageOut <- stageData %>% select(year, local, plotName, Rep, genotipo, Nota, yield)
  filter_locais(stageOut)
}
