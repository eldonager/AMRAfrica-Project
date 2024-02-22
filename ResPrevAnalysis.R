library (tidyverse)
library(janitor)
library(ggpubr)

AMR_clean <- read.csv("AMR_clean.csv")

AMR_clean %>%
  count(no_isolates_susceptible)%>%
  View()

colnames(AMR_clean)


AMR_clean<- AMR_clean %>%
  dplyr::select(doi,country,region, iso_3, y_coordinate,x_coordinate,sampling_start_year,
                sampling_end_year, species, pathogen,sal_prevalence, no_sample,
                antimicrobial,antimicrobial_compound,antibiotic_class,
                who_classification, no_isolate,no_isolates_resistant, 
                no_isolates_intermediate,no_isolates_susceptible,mdr_percentage)                                      
                                  
                            
  AMR_clean<- AMR_clean %>%
    dplyr::mutate(no_isolates_susceptible=no_isolate-no_isolates_resistant)
  
  AMR_clean<- AMR_clean %>% 
    dplyr::select(-no_isolates_intermediate)
  
 
  ##Pooled prevalence for salmonella by antibiotic compounds- bargraphs
  #aggregate all resistant isolates and all NIsolates by speciesag, compound and region
  SalRes = aggregate(no_isolates_resistant ~ antimicrobial_compound + species + region, data = AMR_clean, FUN = sum)
  SalAll = aggregate(no_isolate ~ antimicrobial_compound + species + region, data = AMR_clean, FUN = sum)
  
  #and divide ResIso/NIsolates to return true mean
  SalMean = round((SalRes$no_isolates_resistant /SalAll$no_isolate)*100, digits = 0)
  SalMeandf = as.data.frame(cbind(SalRes$antimicrobial_compound, SalRes$species, SalRes$region, SalMean, SalAll$no_isolate))
  colnames(SalMeandf) = c("Compound","Species","region","Mean","NIsolates")
  SalMeandf$Mean = as.numeric(as.character(SalMeandf$Mean))
  SalMeandf$NIsolates = as.numeric(as.character(SalMeandf$NIsolates))
  
  #restrict analysis to bacteria-drug pairings where NIsolates > =10
  SalMeandf = SalMeandf[which(SalMeandf$NIsolates >= 10),]
  
  # Compute the 95% CI of proportion where x = p_hat and y = n two tailed z = 1.96
  CI.function <- function(x,y) {
    x + c(-1.96,1.96)*sqrt(x*(1-x)/y)}
  
  #95% CI
  CIsal = as.data.frame(t(mapply(CI.function, SalMeandf$Mean/100,  SalMeandf$NIsolates)))
  SalMeandf  = cbind(SalMeandf, round(CIsal*100, digits = 0))
  colnames(SalMeandf) = c("Compound","Species","Region","Mean","NIsolates","CILow","CIHigh")
  SalMeandf$CILow[SalMeandf$CILow < 0] = 0
  SalMeandf$CIHigh[SalMeandf$CIHigh >100] = 100
  
  col_index <- 4  # Assuming you want to check the second column
  
  # Select rows where the specified column has a value other than zero
  SalMeandf <- SalMeandf[SalMeandf[, col_index] != 0, ]
  
  #East African Bar plots
  sal.ea<-SalMeandf %>%
    dplyr::filter(Region == "Eastern Africa")
  
  sal.ea$Compound <- reorder(sal.ea$Compound, -sal.ea$Mean)
  
  sal.ea.plot <- ggbarplot(sal.ea, "Compound", "Mean", 
                                 fill = "Species", position = position_dodge(0.7),
                                 subtitle = "Eastern Africa",
                                 xlab = FALSE, ylab = FALSE,
                                 legend.title = "Species",
                                 font.subtitle = c(19))+
    rotate_x_text(90)+
    geom_linerange(aes(group = Species, ymax = CIHigh, ymin = CILow),
                   position = position_dodge(width = 0.7))+
    font("xy.text", size = 19)+
    font("legend.title",size = 19)+
    font("legend.text", size = 19)
  
  
  sal.ea.plot <- ggpar(sal.ea.plot, ylim = c(0, 100))
  
  