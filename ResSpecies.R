
library (tidyverse)
library(janitor)
library(ggpubr)
library(wesanderson)
library(RColorBrewer)
library("ggsci")

AMR_clean <- read.csv("AMR_clean.csv")



colnames(AMR_clean)


AMR_clean<- AMR_clean %>%
  dplyr::select(doi,country,region, iso_3, y_coordinate,x_coordinate,sampling_start_year,
                sampling_end_year, species, pathogen,sal_prevalence,
                antimicrobial_compound,antibiotic_class,
                who_classification, no_isolate,no_isolates_resistant, 
                no_isolates_intermediate,no_isolates_susceptible,mdr_percentage)                                      


AMR_clean<- AMR_clean %>%
  dplyr::mutate(no_isolates_susceptible=no_isolate-no_isolates_resistant)

AMR_clean<- AMR_clean %>% 
  dplyr::select(-no_isolates_intermediate)


##Pooled prevalence for salmonella by antibiotic compounds- bargraphs
#aggregate all resistant isolates and all NIsolates by speciesag, compound and region
SalRes = aggregate(no_isolates_resistant ~  antimicrobial_compound+species+country , data = AMR_clean, FUN = sum)
SalAll = aggregate(no_isolate ~  antimicrobial_compound+species+country , data = AMR_clean, FUN = sum)

#and divide ResIso/NIsolates to return true mean
SalMean = round((SalRes$no_isolates_resistant /SalAll$no_isolate)*100, digits = 0)
SalMeandf = as.data.frame(cbind(SalRes$antimicrobial_compound,  SalRes$species, SalMean, SalAll$no_isolate, SalRes$country))
colnames(SalMeandf) = c("antimicrobial_compound", "Species","Mean","NIsolates", "country")
SalMeandf$Mean = as.numeric(as.character(SalMeandf$Mean))
SalMeandf$NIsolates = as.numeric(as.character(SalMeandf$NIsolates))




SalMeandf$Mean <- as.numeric(as.character(SalMeandf$Mean))





df1 <- SalMeandf %>%
  group_by(Species) %>%
  select(
         "country",
         "antimicrobial_compound",
         "Mean",
         "NIsolates")







df2 <-aggregate(Mean~Species+antimicrobial_compound, data = df1, FUN=mean)



#changing percentage resistance into numeric class and rounding off  
df2$Mean <- as.numeric(as.character
                                      (df2$Mean))

df2$Mean <- round(df2$Mean, digits = 0)

df2<- df2[which(df2$Mean>= 1),]





species_plot<-ggplot(df2, aes(x = Species, y = Mean, fill = Species))+ 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA
  ) +
  
  scale_fill_brewer(palette = "Paired") +
  theme(legend.position = "top")+
  coord_cartesian(xlim = c(1.2, NA), clip = "off")

pp1<- species_plot+
  theme_pubr(
    base_size = 17,
    base_family = "",
    border = FALSE,
    margin = TRUE,
    legend = c("top"),
    x.text.angle = 0
  )

pp2 <- pp1 + labs(x = "Host", fill = "Host") +
  scale_color_discrete(name = "Host") +
  scale_x_discrete(name = "Host", 
                   limits = c("Cattle", "Chicken", "Goats", "Pigs", "Sheep", "Turkey", "Environment")) +
  labs(x = "Host", y = "Percent resistance") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  theme(legend.position = "none")
pp2

