library (tidyverse)
library(janitor)

AMR_clean <- read.csv("AMR_clean.csv")

AMR_clean %>%
  count(no_isolates_susceptible)%>%
  View()

colnames(AMR_clean)


  dplyr::select(doi,country, iso_3, y_coordinate,x_coordinate,sampling_start_year,
                sampling_end_year, species, pathogen,sal_prevalence, no_sample,
                antimicrobial,antimicrobial_compound,antibiotic_class,
                who_classification, no_isolate,no_isolates_resistant, 
                no_isolates_intermediate,no_isolates_susceptible,mdr_percentage)                                      
                                  
                            
  AMR_clean<- AMR_clean %>%
    dplyr::mutate(no_isolates_susceptible=no_isolate-no_isolates_resistant)
  
  AMR_clean<- AMR_clean %>% 
    dplyr::select(-no_isolates_intermediate)
  
  
  