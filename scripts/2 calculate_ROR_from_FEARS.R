### Calculation of reporting odds ratio's for potential COVID-19 drugs from FEARS data ###
### Sonja Boman and Laura Zwep                                                         ###
### ---------------------------------------------------------------------------------

#required packages
packages <- c("tidyverse")

for (package in packages) {
  #install missing packages
  if (!package %in% installed.packages()[, "Package"]) {
    install.packages(package)
  }
  #load packages
  library(package, character.only = TRUE)
}


#functions
ChangeToNumeric <- function(x) {
  as.numeric(gsub(",", "", x, fixed = T))
}
AersmineToLongData <- function(ADR_data_frame, drugs, ADR_ontology = NULL, adrs = "ADR") {
  #remove first row (with unique patients)
  ADR_data_frame <- ADR_data_frame[-1, ]
  
  #extract counts columns and ADR_IDs
  ind_counts <- c(which(colnames(ADR_data_frame) %in% c(adrs, "Total_Adverse Events_Reports")),
                  grep("Absolute Counts", colnames(ADR_data_frame)))
  ADR_wide <- ADR_data_frame %>% select(ind_counts) %>% 
    dplyr::rename(ADR = adrs[1], ADR_count_total = `Total_Adverse Events_Reports`) %>% 
    mutate(ADR_count_total = ChangeToNumeric(ADR_count_total))
  
  #if ontology is provided: replace ADR_count_total with count from ontology scraped values
  if (! is.null(ADR_ontology)) {
    for (i in 1:nrow(ADR_wide)) {
      adr <- ADR_wide$ADR[i]
      ind_ontology <- which(ADR_ontology$ADR == adr)
      if (length(ind_ontology) > 0) {
        ADR_wide$ADR_count_total[i] <- ADR_ontology[ind_ontology, "count"]
      }
    }
  }
  
  #creating long format
  ADR_long <- ADR_wide %>% 
    pivot_longer(-c(ADR, ADR_count_total, adrs[-1]), names_to = "drug", values_to = "count") %>% 
    filter(!is.na(count)) %>% 
    filter(ADR_count_total > 10) %>% 
    mutate(drug = unlist(lapply(strsplit(drug, "_"), function(x) x[2])),
           count = ChangeToNumeric(count)) %>% 
    as.data.frame() %>% 
    unique()
  
  return(ADR_long)
}
CalculateROR <- function(ADR_row, total_reports, drugs) {
  A_total <- ADR_row$ADR_count_total
  D_total <- drugs$ADR_count[drugs$drug == ADR_row$drug]
  
  a <- ADR_row$count
  b <- D_total - a
  c <- A_total - a
  d <- total_reports - (a + b + c)
  
  ROR <- (a/b)/(c/d)
  SE <- sqrt(1/a + 1/b + 1/c + 1/d)
  
  return(data.frame(ROR, SE))
}
RetrieveROR <- function(ADR_data_frame, total_reports, drugs) {
  ADR_data_frame$ROR <- ADR_data_frame$SE <- 0
  
  for (i in 1:nrow(ADR_data_frame)) {
    cat("\rProgression of ROR calculation: ", paste(round(i/nrow(ADR_data_frame)*100, 3), "%"))
    ADR_data_frame[i, c("ROR", "SE")] <- CalculateROR(ADR_data_frame[i, ], total_reports = total_reports, drugs = drugs)
  }
  cat("\rProgression of ROR calculation: Done!    ")
  
  ADR_data_frame$CI_low  <- exp(log(ADR_data_frame$ROR) - 1.96*ADR_data_frame$SE)
  ADR_data_frame$CI_up  <- exp(log(ADR_data_frame$ROR) + 1.96*ADR_data_frame$SE)
  
  return(ADR_data_frame)
}

####Load data####
#AERSMine data
ADR <- read.delim("data/AERSMine/AERS_COVID_drugs.tsv", stringsAsFactors = F, skip = 7, 
                  header = T, check.names = F, colClasses = "character",
                  na.strings = "")
#Organ class level data
ADR_top <- read_delim("data/AERSMine/AERS_COVID_drugs_organclass.tsv", "\t", escape_double = FALSE, locale = locale(), 
                      na = "", trim_ws = TRUE, skip = 5)

#ADR ontology counts
ADR_ontology <- read.delim("data/clean/ADR_ontology.txt", quote = "\"", header = T, sep = "\t", 
                           stringsAsFactors = F)

####Data cleaning####
#create data frame with drug counts
drugs <- data.frame(drug = as.character(names(ADR)[3:23]),
                    ADR_count = ChangeToNumeric(ADR[1, 26:46]), stringsAsFactors = F)

#create data frame with number of reports in long data format
ADR_low <- AersmineToLongData(ADR, drugs, ADR_ontology, adrs = "Adverse Events")

####Data analysis####
ADR_low_results <- RetrieveROR(ADR_low, total_reports = 12774185, drugs)


#export results to file (equivalent to ADR_pivot_merged in "orig_ADR_pivot_merged.csv")
write.table(ADR_low_results, file = "results/ROR_results.txt", sep = "\t", 
            row.names = F)
write.table(drugs, file = "data/clean/drugs_counts.txt", sep = "\t", row.names = F)

### ---------------------------------------------------------------------------------
##Top level ROR calculations

####data cleaning######
adrs <- data.frame(ADR_class = as.character(names(ADR_top)[3:29]),
                   ADR_count_total = ChangeToNumeric(ADR_top[1, 32:58]), stringsAsFactors = F)

ind_counts <- c(which(colnames(ADR_top) == "Drugs"), grep("Absolute Counts", colnames(ADR_top)))
ADR_top_wide <- ADR_top %>% 
  filter(Drugs %in% drugs$drug) %>% 
  select(ind_counts)

ADR_top_long <- ADR_top_wide %>% 
  pivot_longer(cols = -Drugs, names_to = "ADR_class", values_to = "count") %>% 
  mutate(ADR_class = unlist(lapply(strsplit(ADR_class, "_"), function(x) x[2]))) %>% 
  as.data.frame() %>% 
  unique() %>% 
  left_join(adrs) %>% 
  rename(drug = Drugs)

#ROR calculation
ADR_top_results <- RetrieveROR(ADR_data_frame = ADR_top_long, total_reports = 12774185, drugs = drugs)
ADR_top_results$reject_null <- ADR_top_results$CI_low > 1

write.table(ADR_top_results, file = "results/ROR_results_organclass_ADE.txt", sep = "\t", 
            row.names = F)

### ---------------------------------------------------------------------------------
#High level ADRs
ADR_class <- read.delim("data/AERSMine/AERS_COVID_drugs_class.tsv", stringsAsFactors = F, skip = 7, 
                  header = T, check.names = F, colClasses = "character",
                  na.strings = "")


ADR_high <- AersmineToLongData(ADR_class, drugs, ADR_ontology, adrs = c("Adverse Events", "Adverse Events Class"))
ADR_high <- rename(ADR_high, "ADR_class" = `Adverse Events Class`)

ADR_high_class <- ADR_high %>% group_by(ADR_class, drug) %>% 
  summarize(count_sum = sum(count), count_max = max(count)) %>% 
  ungroup() %>% 
  mutate(ADR_count_total = ChangeToNumeric(gsub("^.* \\[|\\]", "", ADR_class)),
         ADR = str_split(ADR_class, " \\[", simplify = T)[, 1])

ADR_high_class_result <- RetrieveROR(ADR_data_frame = ADR_high_class %>% rename(count = count_max), 
                                     total_reports = 12774185, drugs = drugs)

ADR_high_class_result$reject_null <- ADR_high_class_result$CI_low > 1

write.table(ADR_high_class_result, file = "results/ROR_results_highclass_ADE.txt", sep = "\t", 
            row.names = F)

### ---------------------------------------------------------------------------------