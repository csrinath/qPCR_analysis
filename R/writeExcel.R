source("R/pkgTest.R")
pacman::p_load(tidyverse, readxl, openxlsx)

# Write Functions
xls.sheet.rq <- function(df) {
  df <- df %>% 
    select(Target,Replicate, Biogroup, RQ) %>% 
    spread(Biogroup,RQ) %>% 
    select(-Replicate)
  
  df[is.na(df)] <- 0
  
  return(df)
}

xls.sheet.deltaCt <- function(df) {
  df %>% 
    select(Target,Replicate, Biogroup, deltaCt) %>% 
    spread(Biogroup,deltaCt) %>% 
    select(-Replicate) 
}

xls.sheet.Ct <- function(df) {
  df %>% 
    select(Target,Replicate, Biogroup, Ct) %>% 
    spread(Biogroup,Ct) %>% 
    select(-Replicate) 
}

qPCR.xls.write <- function(df,experiment) {
  filename <- str_c("output/", experiment, "/", experiment, "_Results.xlsx", sep="")
  df.mean <- summarizeData(df,RQ)
  list("Results"=xls.sheet.rq(df),
       "Results (avg)"=df.mean,
       "Ct"=xls.sheet.Ct(df),
       "Delta Ct" = xls.sheet.deltaCt(df),
       "Raw"=df) %>%
    write.xlsx(filename, as.table=c(TRUE,TRUE), borders="columns")
  print(filename)
  return(df)
}
