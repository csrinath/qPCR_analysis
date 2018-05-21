source("R/pkgTest.R")
pkgTest("tidyverse")
pkgTest("rlang")

deltaCt <- function(df, controlTarget) {
  control.df <- df %>% filter(Target == controlTarget) %>% mutate(Ct.control = Ct)
  
  df %>% 
    inner_join(select(control.df, Biogroup, Sample, Ct.control),  by = c("Biogroup","Sample")) %>%
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

summarizeData <- function(df, colName) {
  col.name = enquo(colName)
  mean.name = paste0(quo_name(col.name),".mean")
  sd.name = paste0(quo_name(col.name),".sd")
  
  df %>% group_by(Target, Biogroup) %>% 
    summarise(!! mean.name := mean(!!col.name), !!sd.name := sd(!!col.name))
}