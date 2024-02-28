
main_data <- read_csv("mean.data.csv")

#changing percentage resistance into numeric class and rounding off  
main_data$percent_resistant <- as.numeric(as.character
                                          (main_data$percent_resistant))

main_data$percent_resistant <- round(main_data$percent_resistant, digits = 0)


data2 <- main_data %>%
  group_by(host) %>%
  select("doi",
         "farm_type",
         "host",
         "percent_resistant")







data3 <-aggregate(percent_resistant~doi+farm_type+host, data2, mean)



#changing percentage resistance into numeric class and rounding off  
data3$percent_resistant <- as.numeric(as.character
                                      (data3$percent_resistant))

data3$percent_resistant <- round(data3$percent_resistant, digits = 0)

data3<- data3[which(data3$percent_resistant>= 1),]

#subseting organic farm data
Organic.data <- data3 %>%
  filter(farm_type == "Organic")






org.plot<-ggplot(Organic.data, aes(x = host, y = percent_resistant, fill = host))+ 
  geom_boxplot(
    width = .15, 
    outlier.shape = NA
  ) +
  
  scale_fill_manual(values = wes_palette( "Darjeeling1", n=5))+
  coord_cartesian(xlim = c(1.2, NA), clip = "off")

org.plot<- org.plot+
  theme_pubr(
    base_size = 17,
    base_family = "",
    border = FALSE,
    margin = TRUE,
    legend = c("top"),
    x.text.angle = 0
  )

org.plot<- org.plot+labs(x = "Host", fill = "Host")
org.plot<- org.plot+scale_color_discrete(name = "Host")
##Changing x axis label order
org.plot <- org.plot + scale_x_discrete(name ="Host", 
                                        limits=c("Cattle","Chicken","Pigs", "Turkey", "Environment"))

org.1 <-org.plot+labs(x = "Host", y = "Percent resistance", 
                      title = "Organic farms")+
  theme(legend.position = "none")

org.1 <-ggpar(org.1,ylim = c(0, 100))
