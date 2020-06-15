### Retrieve ADR information from ontology (JSON file)                                  ###
### Laura Zwep and Patrick de Koning                                                    ###
### ---------------------------------------------------------------------------------

#required packages
packages <- c("tidyverse", "rjson", "plyr")

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

#import output from webscrape
ADR_ontology_raw <- rjson::fromJSON(file = "data/AERSMine/out.json")

#convert to data.frame
ADR_ontology <- plyr::ldply(ADR_ontology_raw, data.frame, stringsAsFactors = F)

#remove duplicate enties (from multiple parents)
ADR_ontology_clean <- unique(ADR_ontology %>% select(-parentCodes)) %>% 
  mutate(count = ChangeToNumeric(count), countDirect = ChangeToNumeric(countDirect), ADR = name) %>%
  select(-name)

rm(ADR_ontology_raw, ADR_ontology)

#export data in to file
write.table(ADR_ontology_clean, file = "data/clean/ADR_ontology.txt", row.names = F, quote = T, sep = "\t")
