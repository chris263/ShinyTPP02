
#removing warnings
options(warn = -1)

# Script para gerar os graficos
require(grid)
require(hrbrthemes)
require(viridis)



makeG0 <- function(inGraph0, item, tpp){
  if(tpp != "Brazil"){
    inGraph00 <- inGraph0 %>% filter(TPP == tpp)
  }else{
    inGraph00 <- inGraph0
  }
  inGraph00 %>%
    ggplot( aes(x=local, y=get(item), fill=local)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
    # theme_ipsum() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    ggtitle("") +
    xlab("") +
    ylab(item)

}

makeH0 <- function(inGrapH0, termo, tpp){
  if(tpp != "Brazil"){
    inGrapH00 <- inGrapH0 %>% filter(TPP == tpp)
  }else{
    inGrapH00 <- inGrapH0
  }
  H0 <- inGrapH00 %>%
    ggplot( aes(x=get(termo))) +
    geom_histogram( binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
    # ggtitle("") +
    theme_ipsum() +
    theme(
      plot.title = element_text(size=15)
    ) + xlab(termo)

  return(H0)
}



makeG1 <- function(inGraph1){
  g.mid<-ggplot(inGraph1,aes(x=1,y=genotipo))+geom_text(aes(label=genotipo))+
    geom_segment(aes(x=0.94,xend=0.96,yend=genotipo))+
    geom_segment(aes(x=1.04,xend=1.065,yend=genotipo))+
    ggtitle("")+
    ylab(NULL)+
    scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065))+
    theme(axis.title=element_blank(),
          panel.grid=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          axis.text.x=element_text(color=NA),
          axis.ticks.x=element_line(color=NA),
          plot.margin = unit(c(1,-1,1,-1), "mm"))

  g1 <- ggplot(data = inGraph1, aes(x = genotipo, y = blupNota)) +
    geom_bar(stat = "identity") + ggtitle("Nota Enfezamento") +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = unit(c(1,-1,1,0), "mm")) +
    scale_y_reverse() + coord_flip()

  g2 <- ggplot(data = inGraph1, aes(x = genotipo, y = blupYield)) +xlab(NULL)+
    geom_bar(stat = "identity") + ggtitle("Yield") +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          plot.margin = unit(c(1,0,1,-1), "mm")) +
    coord_flip()
  library(gridExtra)
  gg1 <- ggplot_gtable(ggplot_build(g1))
  gg2 <- ggplot_gtable(ggplot_build(g2))
  gg.mid <- ggplot_gtable(ggplot_build(g.mid))

  grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(4/9,2/9,4/9))
}

makeG2 <- function(inGraph2){
  ggplot(inGraph2, aes(x = genotipo)) +
    geom_col(aes( y = Indice, fill="redfill")) +
    geom_text(aes(y = Indice, label = round(Indice,2)), fontface = "bold", vjust = 1.4, color = "black", size = 3) +
    geom_line(aes(y = blupNota * 1.5, group = 1, color = 'blackline')) +
    geom_text(aes(y = blupNota * 1.5, label = round(blupNota, 0)), vjust = 1.4, color = "black", size = 3) +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ . / 1.5)) +
    scale_fill_manual('', labels = 'Indice', values = "darkblue") +
    scale_color_manual('', labels = 'Nota', values = 'black') +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

}

makeG3 <- function(inGraph3){

  inGraph3$Tnota <- 10-inGraph3$blupNota

  bp <-  reshape2::melt(inGraph3, id.vars=c("genotipo"))

  bpf <- bp %>% filter(variable != "Acuracia(%)",
                       variable != "nLocais",
                       variable != "Indice",
                       variable != "blupNota")

  ggplot(bpf, aes(x=reorder(genotipo,-value, sum),y=value, fill = variable)) +
    geom_bar(stat = "identity")+
    scale_fill_viridis(discrete = T) +
    ggtitle("Agrupando Yield e Inverso da Nota CS") +
    xlab("Genotipos")+
    # theme_ipsum()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

makeG4 <- function(inGraph4){
  ggplot(inGraph4, aes(x=reorder(genotipo,blupNota, sum),y=blupNota, fill = genotipo)) +
    geom_bar(stat = "identity")+
    scale_fill_viridis(discrete = T) +
    # ggtitle("Agrupando Yield e Inverso da Nota CS") +
    xlab("Genotipos")+
    # theme_ipsum()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

makeG5 <- function(ctl, hb1, inGraph5){

  ctlHB <- append(ctl,hb1)
  inGraph5$genotipo <- as.character(inGraph5$genotipo)

  cd <- inGraph5[1,]
  for( i in 1: length(ctlHB)){
    cddf <-filter(resultadoFINAL,genotipo == ctlHB[i])
    cd <- rbind(cd,cddf)
  }
  cd <- cd[-1,]

  cdf <-  reshape2::melt(cd, id.vars=c("genotipo"))

  cdf <- cdf %>% filter(variable == "blupNota")

  ggplot(cdf %>% arrange(variable, desc(value)) %>%
           mutate(genotipo=factor(genotipo, levels=genotipo)),
         aes(x=genotipo,y=value, fill = variable)) +
    geom_bar(stat="identity", show.legend=FALSE) +
    geom_text(aes(label=round(value,2), y=0.5*value), colour="white", size=3) +
    facet_grid(. ~ variable, scales="free_x", space="free_x") +
    scale_y_continuous(limits=c(-0.005, 1.05*max(cdf$value)), expand=c(0,0)) +
    theme_classic() +
    theme(panel.spacing=unit(0,"pt"),
          panel.border=element_rect(colour="grey50", fill=NA))

}

makeG6 <- function(inGraph6){
  ggplot(inGraph6, aes(x = genotipo)) +
    geom_col(aes( y = blupYield, fill="redfill")) +
    geom_text(aes(y =blupYield, label = blupYield), fontface = "bold", vjust = 1.4, color = "black", size = 3) +
    geom_line(aes(y = blupNota * 1.5, group = 1, color = 'blackline')) +
    geom_text(aes(y = blupNota * 1.5, label = round(blupNota, 0)), vjust = 1.4, color = "black", size = 3) +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ . / 1.5)) +
    scale_fill_manual('', labels = 'Yield', values = "darkgreen") +
    scale_color_manual('', labels = 'Nota', values = 'black') +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

}

makeG7 <- function(inGraph7){
  ggplot(inGraph7,aes(x=Nota,y=yield)) +
    geom_point() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
    geom_smooth(method="lm")

}


makeG8 <- function(){
  cT <- card1$Indice[1]
  cMT <- card1$Indice[2]
  cMS <- card1$Indice[4]
  fixH <- (-1) * card1$Indice[5]
  cH <- card1$Indice[3]

  cS <- 0
  cMS <- cMS + fixH
  cMT <- cMT + fixH
  cT <- 100
  cH <- cH +fixH



  nomesG <- card1$genotipo
  nomeH <- nomesG[3]
  nomesG <- nomesG[-3]
  # Padrao
  gg.gauge <- function(pos,breaks=c(cS,cMS,cMT,cT)) {
    require(ggplot2)
    get.poly <- function(a,b,r1=0.5,r2=1.0) {
      th.start <- pi*(1-a/100)
      th.end   <- pi*(1-b/100)
      th       <- seq(th.start,th.end,length=100)
      x        <- c(r1*cos(th),rev(r2*cos(th)))
      y        <- c(r1*sin(th),rev(r2*sin(th)))
      return(data.frame(x,y))
    }
    ggplot()+
      geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="red")+
      geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="gold")+
      geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="forestgreen")+
      geom_polygon(data=get.poly(pos-1,pos+1,0.2),aes(x,y))+
      geom_text(data=as.data.frame(breaks), size=2, fontface="bold", vjust=0,
                aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=rev(nomesG)))+
      annotate("text",x=0,y=0,label=nomeH,vjust=0,size=4,fontface="bold")+
      coord_fixed()+
      theme_bw()+
      theme(axis.text=element_blank(),
            axis.title=element_blank(),
            axis.ticks=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
  }
  gg.gauge(cH,breaks=c(cS,cMS,cMT,cT))

}

