### Curate results based on literature                                                 ###
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


#read ROR results data
ADR_low_results <- read.delim("results/ROR_results.txt", sep = "\t", stringsAsFactors = F)
ADR_top_results <- read.delim("results/ROR_results_organclass_ADE.txt", sep = "\t", stringsAsFactors = F)
ADR_high_class_result <-  read.delim("results/ROR_results_highclass_ADE.txt", sep = "\t", stringsAsFactors = F)
drugs <- read.delim("data/clean/drugs_counts.txt", stringsAsFactors = F)

#curation at ADE level
#literature results nonsensical ADRs
nonsense_ADRs_lowlevel <- c('invasive lobular breast carcinoma',"steroid diabetes",'central nervous system lymphoma','tuberculosis of central nervous system','cutaneous tuberculosis','influenza a virus test positive',"burkitt's lymphoma","breast cellulitis","histology abnormal","spinal decompression",'varicella zoster virus infection',"aspiration joint","bladder distension","cytology abnormal",'body height below normal',"contraindicated product administered",'ankle arthroplasty',"rheumatoid arthritis", 'therapeutic product effect decreased','breast cancer stage iii','shoulder arthroplasty','treatment failure','post procedural contusion','x-ray abnormal','hepatitis c rna positive','viral load decreased','viral load','viral load decreased','hepatitis c','chronic hepatitis c','remission not achieved','chronic hepatitis c','genital herpes simplex','pregnancy of partner','viral load increased','exposure via body fluid','transmission of drug via semen','paternal exposure timing unspecified','parvovirus b19 serology positive','hepatitis b reactivation','hepatitis e','hepatitis c virus test positive','neutralising antibodies positive','exposure via direct contact','complications of transplanted liver','liver transplant','white blood cell count','parvovirus b19 test positive','viral mutation identified','haemoglobin','ammonia abnormal','therapy responder','exposure via partner','liver transplant rejection','capsule physical issue','haematology test abnormal','paternal drugs affecting foetus',"malaria", "factor v leiden mutation",'genotype drug resistance test positive','hiv-associated neurocognitive disorder','aids encephalopathy','rheumatoid factor positive','elbow deformity', 'ergot poisoning')


#known ADRs and symptoms
hc_known_ADRs <- c("deafness transitory",'external auditory canal atresia','hair follicle tumour benign','transaminases abnormal')
hc_related_ADR <- c("presbyacusis","granuloma skin","blood iron abnormal",'retinal toxicity',"ocular toxicity",'retinal deposits','retinitis','blood pressure systolic abnormal','blood pressure diastolic abnormal','c-reactive protein decreased','restrictive cardiomyopathy','panniculitis')
hc_known_symptom <- c("human antichimeric antibody positive","red blood cell sedimentation rate abnormal","rheumatoid factor positive","joint laxity","synovitis","elbow deformity",'sle arthritis',"c-reactive protein abnormal", 'pemphigus','anti-cyclic citrullinated peptide antibody positive','rheumatoid nodule','hand deformity','bone erosion','wrist deformity','musculoskeletal deformity','blood parathyrid hormone decreased','c-reactive protein','loose body in joint',"felty's syndrome", 'neck deformity','blood parathyroid hormone decreased','arthrodesis','spinal stenosis','ankle deformity','seronegative arthritis','lupus nephritis','hla marker study positive','fine motor skill dysfunction','systemic lupus erythematosus','type 2 lepra reaction','red blood cell sedimentation rate increased','bursitis infective','osteoporosis postmenopausal','joint arthroplasty','nodal osteoarthritis','dislocation of vertebra','chondromalacia',"still's disease",'malocclusion','extremity contracture','tenosynovitis stenosans','systemic lupus erythematosus rash','overlap syndrome','disease susceptibility','urticarial vasculitis',"sjogren's syndrome")

literature_results <- rbind(data.frame(ADR = hc_known_ADRs, literature = "Known ADR", drug = "hydroxychloroquine"),
                            data.frame(ADR = hc_related_ADR, literature = "Related ADR", drug = "hydroxychloroquine"),
                            data.frame(ADR = hc_known_symptom, literature = "Known Symptom", drug = "hydroxychloroquine"))

c_known_ADRs <- c("restrictive cardiomyopathy","maculopathy","bacterial diarrhoea","ocular toxicity","myopathy toxic")
c_related_ADR <- c("carditis","amaurosis","retinogram abnormal","retinal pigment epitheliopathy","mitochondrial myopathy","conjunctival pallor","aortic dilatation")
c_known_symptom <- c("complement factor c3 decreased","myelocytosis","blood brain barrier defect","glomerulonephritis chronic",'malaria','thromboangiitis obliterans','bacterial pyelonephritis',"blood immunoglobulin m decreased","still's disease adult onset","rheumatic fever","amyotrophy","infective tenosynovitis","nephritic syndrome","telangiectasia")

literature_results <- rbind(literature_results,
                            data.frame(ADR = c_known_ADRs, literature = "Known ADR", drug = "chloroquine"),
                            data.frame(ADR = c_related_ADR, literature = "Related ADR", drug = "chloroquine"),
                            data.frame(ADR = c_known_symptom, literature = "Known Symptom", drug = "chloroquine"))

r_known_ADRs <- c('vogt-koyanagi-harada syndrome','hyperuricaemia','cutaneous sarcoidosis','anaemia','hyperbilirubinaemia','alopecia universalis','neutrophil count decreased', "mitochondrial toxicity",'injection site dermatitis', 'platelet count decreased')
r_related_ADR <- c('retinal exudates','retinopathy', 'oesophageal varices haemorrhage','gastric varices haemorrhage','hydrocholecystis','red blood cell count decreased','anti-erythropoietin antibody positive','hypotrichosis','neuropsychiatric syndrome','liver transplant rejection','haemolytic anaemia','dissociative identity disorder','anorectal discomfort','anal pruritus','painful defaecation','anorectal discomfort', 'anorectal disorder')
r_known_symptom <- c('hepatocellular carcinoma','hepatic cancer recurrent','hepatitis c rna increased','proctalgia','cryoglobulinaemia','angiotensin converting enzyme increased','central pontine myelinolysis','hepatic encephalopathy','choroiditis','white blood cell count decreased','virologic failure','hepatic cirrhosis','portal hypertensive gastropathy','hepatic neoplasm malignant recurrent','hepatic pain','blood uric acid increased','alpha 1 foetoprotein increased','porphyria non-acute','membranoproliferative glomerulonephritis','glomerulonephritis membranoproliferative','oral lichen planus','hepatorenal syndrome','ammonia increased')

literature_results <- rbind(literature_results,
                            data.frame(ADR = r_known_ADRs, literature = "Known ADR", drug = "ribavirin"),
                            data.frame(ADR = r_related_ADR, literature = "Related ADR", drug = "ribavirin"),
                            data.frame(ADR = r_known_symptom, literature = "Known Symptom", drug = "ribavirin"))

lr_related_ADR <- c("hyperferritinaemia",'fanconi syndrome acquired','lipodystrophy acquired', "progressive external ophthalmoplegia")
lr_known_symptom <- c('aids encephalopathy','hiv-associated neurocognitive disorder','virologic failure','cerebral toxoplasmosis','portal fibrosis')
lr_known_ADRs <- c("mitochondrial toxicity")

literature_results <- rbind(literature_results,
                            data.frame(ADR = lr_related_ADR, literature = "Related ADR", drug = "lopinavir and ritonavir"),
                            data.frame(ADR = lr_known_symptom, literature = "Known Symptom", drug = "lopinavir and ritonavir"),
                            data.frame(ADR = lr_known_ADRs, literature = "Known ADR", drug = "lopinavir and ritonavir"))


#filter and add known drugs information to results, remove all ADRs with 100 or less observations
ADR_results_curated <-  ADR_low_results %>% 
  filter(ADR_count_total > 100) %>% 
  filter(!ADR %in% nonsense_ADRs_lowlevel) %>% 
  left_join(literature_results) %>% 
  mutate(literature = as.character(literature))

ADR_results_curated$literature[is.na(ADR_results_curated$literature)] <- "Unknown ADR"

#drug ordering in factor
drug_order <- unique(c("chloroquine", "hydroxychloroquine", "lopinavir", "lopinavir and ritonavir",
                drugs$drug[grep("vir", drugs$drug)],
                drugs$drug[grep("interferon", drugs$drug)], "tocilizumab",
                "methylprednisolone", "ruxolitinib", "thalidomide", "emtricitabine", "nitazoxanide"))

ADR_results_curated <- ADR_results_curated %>% 
  mutate(drug = factor(drug, levels = drug_order))


#export curated data to use for figures in paper
write.table(ADR_results_curated, file = "results/ADR_results_low_level_curated.txt", row.names = F,
            sep = "\t")
#save as Rdata to preserve drug ordering
save(ADR_results_curated, file = "results/ADR_results_low_level_curated.Rdata")


#drug ordering in factor on ADR class top level
nonsense_ADRs_toplevel <- c("social circumstances", "investigations", "general disorders and administration site conditions", "surgical and medical procedures")
ADR_top_results_curated <- ADR_top_results %>% 
  mutate(drug = factor(drug, levels = drug_order)) %>% 
  filter(!ADR_class %in% nonsense_ADRs_toplevel)

save(ADR_top_results_curated, file = "results/ADR_results_top_level_curated.Rdata")

#drug ordering in factor on ADR class top level
nonsense_ADRs_highlevel <- c("age related factors", "chlamydial infectious disorders", "bacterial infectious disorders","economic and housing issues","endocrine investigations (incl sex hormones)","gastrointestinal infections","gender related factors","haematological and lymphoid tissue therapeutic procedures","head and neck therapeutic procedures","skin and subcutaneous tissue therapeutic procedures","respiratory tract therapeutic procedures","protozoal infectious disorders","protein and chemistry analyses nec","obstetric and gynaecological therapeutic procedures","mycobacterial infectious disorders","musculoskeletal and soft tissue investigations (excl enzyme tests)","microbiology and serology investigations","foetal and neonatal investigations","gastrointestinal investigations","haematology investigations (incl blood groups)","hepatobiliary investigations","immunology and allergy investigations","respiratory and pulmonary investigations (excl blood gases)",'lipid analyses','leukaemias','helminthic disorders','endocrine gland therapeutic procedures','enzyme investigations nec',"fungal infectious disorders","lymphomas hodgkin's disease","lymphomas nec","lymphomas non-hodgkin's b-cell","lymphomas non-hodgkin's unspecified histology","")
congenital_ADRs_highlevel <- c("blood and lymphatic system disorders congenital","cardiac and vascular disorders congenital","congenital and hereditary disorders nec","congenital and peripartum neurological conditions","congenital cardiac disorders","congenital ear disorders (excl deafness)","congenital eye disorders (excl glaucoma)","congenital reproductive tract and breast disorders","congenital respiratory tract disorders","cytoplasmic disorders congenital","ear and labyrinthine disorders congenital","endocrine disorders congenital","eye disorders congenital","gastrointestinal tract disorders congenital","hepatobiliary disorders congenital","immune system disorders congenital",'infections and infestations congenital',"metabolic and nutritional disorders congenital","musculoskeletal and connective tissue disorders congenital","neurological disorders congenital","renal and urinary tract disorders congenital","reproductive tract and breast disorders congenital","respiratory disorders congenital","skin and subcutaneous tissue disorders congenital","inborn errors of metabolism","chromosomal abnormalities and abnormal gene carriers","developmental disorders nec","neonatal respiratory disorders",'placental, amniotic and cavity disorders','pregnancy, labour, delivery and postpartum conditions','neonatal and perinatal conditions','maternal complications of pregnancy','maternal complications of labour and delivery','foetal complications','abortions and stillbirth',"placental, amniotic and cavity disorders (excl haemorrhages)")

ADR_high_results_curated <- ADR_high_class_result %>% 
  mutate(drug = factor(drug, levels = drug_order),
         ADR_type = ifelse(ADR %in% nonsense_ADRs_highlevel, "nonsense", "useful"))
ADR_high_results_curated$ADR_type[ADR_high_results_curated$ADR %in% congenital_ADRs_highlevel] <- "congenital"


save(ADR_high_results_curated, file = "results/ADR_high_results_curated.Rdata")
