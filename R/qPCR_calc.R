source("R/pkgTest.R")
# pkgTest("tidyverse")
# pkgTest("rlang")
pacman::p_load(tidyverse, rlang)

deltaCt <- function(df, controlTarget) {
  control.df <- df %>% 
    filter(Target == controlTarget) %>% 
    mutate(Ct.control = Ct)
  
  df %>% 
    filter(Target != controlTarget) %>% 
    inner_join(select(control.df, Replicate, Biogroup, Sample, Ct.control),  by = c("Biogroup","Sample","Replicate")) %>%
    mutate(deltaCt = Ct - Ct.control)
}

deltaDeltaCt <- function(df, controlBiogroup) {
  normFactor <- df %>% group_by(Target) %>% 
    filter(Biogroup == controlBiogroup) %>% 
    summarize(deltaCt.normFactor = mean(deltaCt)) 
  
  df %>% 
    inner_join(normFactor,  by = "Target") %>%
    mutate(deltaDeltaCt = deltaCt - deltaCt.normFactor)
}

calcRQ <- function(df) df %>% mutate(RQ = 2^-deltaDeltaCt)

summarizeData <- function(df, col.name) {
  col.name = enquo(col.name)
  mean.name = paste0(quo_name(col.name),".mean")
  sd.name = paste0(quo_name(col.name),".sd")
  
  df %>% group_by(Target, Biogroup) %>% 
    summarise(!! mean.name := mean(!!col.name), !!sd.name := sd(!!col.name))
}

normalizeData <- function(df, col.name, controlBiogroup) {
  col.name = enquo(col.name)
  normFactor.name = sym(paste0(quo_name(col.name),".normFactor"))
  normName = sym(paste0(quo_name(col.name),".normalized"))
  
  normFactor <- df %>% group_by(Target) %>%
    filter(Biogroup == controlBiogroup) %>%
    summarize(!!normFactor.name := mean(!!col.name))
  
  df %>%
    inner_join(normFactor,  by = "Target")  %>%
    mutate(!!normName := !!col.name/!!normFactor.name)
}

missingVal <- function(df, colName=Ct, newVal=40) {
  colName_enquo <- enquo(colName)
  colName <- deparse(substitute(colName))
  df %>% mutate(!!colName := if_else(is.na(!!colName_enquo), newVal, !!colName_enquo))
}