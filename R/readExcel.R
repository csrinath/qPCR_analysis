source("R/pkgTest.R")
pkgTest("tidyverse")
pkgTest("readxl")

qPCR.sample.reader <- function(df, xlsSheet="Sample Setup", skipRows=42) {
  df %>%
    read_excel(sheet = xlsSheet, skip = skipRows) %>%
    select(Sample = "Sample Name", Biogroup = "Biogroup Name", Target = "Target Name") %>%
    drop_na() %>%
    unique()
}

qPCR.data.reader <- function(df, skipRows=42) {
  allSheets = excel_sheets(df)
  xlsSheet = case_when("Technical Analysis Result" %in% allSheets ~ "Technical Analysis Result",
                       "Results" %in% allSheets ~ "Results", 
                       TRUE ~ stop('The file must have a sheet labeled "Technical Analysis Result" or "Results"'))
  
  df  %>%
    read_excel(sheet = xlsSheet, skip = skipRows) %>%
    select(Sample= "Sample Name", Target="Target Name", Ct = "Ct Mean", DeltaCt.flex="Delta Ct Mean", RQ.flex = "RQ")  %>%
    filter(startsWith(Sample,"Sample"))
}
  
#   %>%
#     inner_join(unique(select(qPCR.sample.reader(df), Sample, Biogroup)),  by = "Sample") %>%
#     mutate(Replicate = as.integer(str_sub(Sample, start = 8))%%numRep + 1) %>%
#     arrange(Target, Biogroup, Sample) %>%
#     filter(Target != "GAPDH")
# }