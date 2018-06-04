source("R/pkgTest.R")
pkgTest("tidyverse")
pkgTest("cowplot")
#theme_set(theme_cowplot(font_size=9))
pkgTest("RColorBrewer")

genePlot <- function(df, valName=RQ, nCol=1){
  valName <- deparse(substitute(valName))
  valName.mean <- paste(valName, ".mean",sep="")
  valName.sd <- paste(valName, ".sd",sep="")
  valName.ymax <- paste(valName.mean,"+",valName.sd,sep="")
  valName.ymin <- paste(valName.mean,"-",valName.sd,sep="")
  ngroup <- length(unique(df$Biogroup))
  
  df %>%
    ggplot(aes_string(x="Biogroup", y=valName.mean)) + 
    geom_col(aes(fill=Biogroup), colour="black", width=.8) + 
    geom_errorbar(aes_string(ymin=valName.ymin, ymax=valName.ymax), width=0.4) + 
    facet_wrap(~ Target, ncol=nCol, scales = "free_y") + 
    theme(axis.text.x=element_text(angle = 90, hjust = 1)) + 
    background_grid(major="y", minor="y") + 
    scale_fill_manual(values = colorRampPalette(brewer.pal(11, "Spectral"))(ngroup))
}

makeGenePlot <- function(df, gene, experiment, bAR=1.7, bH=6) {
  plotName <- str_c("output/", experiment, "/", experiment, "_Results","_", gene, ".png", sep="")
  plot <- df %>% filter(Target == gene) %>% genePlot()
  save_plot(plotName, plot, base_aspect_ratio = bAR, base_height = bH)
  print(plotName)
  return(plot)
}

makeAllGenePlots <- function(df, experiment, bAR=1.7, bH=6) {
  targetList <- df %>% pull(Target) %>% unique()
  plotList <- targetList %>% map(function(x) makeGenePlot(df,x,experiment,bAR,bH))
  return(df)
}

makeMultiGenePlot <- function(df,experiment,targetList = NA, antiTargetList = NA, nCol=3, bAR=NA, bH=NA, plotName=NA) {
  if(all(is.na(targetList))) {
    targetList <- df %>% pull(Target) %>% unique()
  }
  
  if(!is.na(antiTargetList)) {
    targetList <- targetList[!targetList %in% antiTargetList]
  }
  
  # print(targetList)
  
  if(is.na(bAR)) {bAR = nCol/2.0}
  if(is.na(bH)) {bH <- ((length(targetList) + (nCol-1)) %/% nCol)*2}
  
  multiplot <- df %>% filter(Target %in% targetList) %>% genePlot(nCol=nCol)
  multiplotName <- str_c("output/", experiment, "/", experiment, "_Results.png", sep="")
  if(!is.na(plotName)) {
    multiplotName <- str_c("output/", experiment, "/", experiment, "_Results-",plotName, ".png", sep="")
    }
  save_plot(multiplotName, multiplot, base_aspect_ratio = bAR, base_height = bH)
  multiplot
  print(multiplotName)
  
  return(df)
}



