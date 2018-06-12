source("R/pkgTest.R")
# pkgTest("tidyverse")
# pkgTest("readxl")
# pkgTest("openxlsx")
pacman::p_load(tidyverse, readxl, openxlsx)

# Makes the filepath "directory/filename". Default directory is data/
makeFilePath <- function(fileName,directory="data") {
  filePath <- str_c(directory, fileName, sep="/")
  return(filePath)
}

# Reads Sample Info from the "Sample Setup" sheet of data.xls
qPCR.sample.reader <- function(df, xlsSheet="Sample Setup", skipRows=42) {
  df %>%
    read_excel(sheet = xlsSheet, skip = skipRows) %>%
    select(Sample = "Sample Name", Biogroup = "Biogroup Name", Target = "Target Name") %>%
    drop_na() %>%
    unique()
}

# Reads Sample Data from the "Technical Analysis Result" or the "Results" sheet of data.xls
qPCR.data.reader <- function(df, skipRows=42) {
  
  # Returns "Technical Analysis Result" or "Results" or "NA" depending on what sheet exists.
  excelSheet.checker <- function(df){
    allSheets = excel_sheets(df)
    xlsSheet = case_when("Technical Analysis Result" %in% allSheets ~ "Technical Analysis Result",
                         "Results" %in% allSheets ~ "Results", 
                         TRUE ~ "NA")
    return(xlsSheet)
  }
  
  # Reads in data from "Technical Analysis Result"
  dataReader.TAR <- function(df) {
    df  %>%
      read_excel(sheet = "Technical Analysis Result", skip = skipRows)
  }
  
  # Reads in data from "Results"
  dataReader.Results <- function(df) {
    df  %>%
      read_excel(sheet = "Results", skip = skipRows)
  }
  
  # Parses read-in data and selects appropriate Ct and/or RQ columns
  dataParser <- function(data) {
    
    # Functions to select Ct and/or RQ, apart from Target and Sample info
    select.Ct.RQ <- function(df) {
      df %>%
        select(Sample= "Sample Name", Target="Target Name", Ct = "Ct Mean", RQ.flex = "RQ")
    }
    select.Ct <- function(df) {
      df %>%
        select(Sample= "Sample Name", Target="Target Name", Ct = "Ct Mean")
    }
    select.RQ <- function(df) {
      df %>%
        select(Sample= "Sample Name", Target="Target Name", RQ.flex = "RQ")
    }
    select.all <- function(df) {
      df %>%
        rename(Sample= "Sample Name", Target="Target Name")
    }
    
    # Depending on what columns exist, appropriate data is selected
    if(all(c("Ct Mean","RQ") %in% colnames(data)))  {
      return(select.Ct.RQ(data) %>% filter(startsWith(Sample,"Sample")))
    }
    if("Ct Mean" %in% colnames(data)) {
      return(select.Ct(data) %>% filter(startsWith(Sample,"Sample")))
    }
    if("RQ" %in% colnames(data)) {
      return(select.RQ(data) %>% filter(startsWith(Sample,"Sample")))
    }
    else {
      return(select.all(data) %>% filter(startsWith(Sample,"Sample")))
    }
  }
  
  xlsSheet <- excelSheet.checker(df)
  if(xlsSheet == "Technical Analysis Result") return(df %>% dataReader.TAR() %>% dataParser())
  if(xlsSheet == "Results") return(df %>% dataReader.Results() %>% dataParser())
  else stop("XLS file does not contain a 'Technical Analysis Result' or 'Results' tab")

}

# Reads Sample Info and Data using the above functions, merges the info, and numbers the replicates
qPCR.file.reader <- function(df) {
  samples <- qPCR.sample.reader(df)
  data <- qPCR.data.reader(df)
  
  fullData <- data %>% 
    inner_join(unique(select(samples, Sample, Target, Biogroup)), by = c("Sample", "Target")) %>%
    unique() %>%
    group_by(Biogroup, Target) %>% 
    mutate(Replicate = row_number()) %>%    # Number the replicates
    ungroup() %>%
    arrange(Target, Biogroup)
  return(fullData)
}

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
