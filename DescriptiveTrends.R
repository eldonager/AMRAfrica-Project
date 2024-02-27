###########
#Descriptive
###########
library(dplyr)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(kimisc)
library(AICcmodavg)
library(Metrics)
library(detectseparation)
library(brglm2)
library(car)
library(boot)

#Mapping AST Methods
AMR_clean<-read.csv("AMR_clean.csv")



AMR_clean<- AMR_clean %>%
  dplyr::mutate(no_isolates_susceptible=no_isolate-no_isolates_resistant)

AMR_clean<- AMR_clean %>% 
  dplyr::select(-no_isolates_intermediate)


AMRdf<- AMR_clean%>%
  dplyr::select(-sampling_start_year)%>%
  dplyr::mutate(Year=sampling_end_year)%>%
dplyr::select(-sampling_end_year)%>%
  dplyr::select(doi,country, region, Year, species,species_2,strain,no_isolates_resistant,
                no_isolates_susceptible,no_isolate,
                antimicrobial, who_classification, ast_method)
  



data1<- AMRdf%>%
 dplyr::group_by(doi,country, region, Year, species,species_2,strain,no_isolates_resistant,
                 no_isolates_susceptible,no_isolate,
                antimicrobial, who_classification, ast_method)%>%
  dplyr::mutate(MeanRes=sum(no_isolates_resistant)/sum(no_isolate))
  

data1%>%
  count(MeanRes)%>%
  View()


data1 <- data1[!is.na(data1$Year), ]

data1%>%
  na.omit()
na.omit(data1)

data1 <- data1[data1$MeanRes != 0, ]
# Assuming data1 is your data frame


data1$Year <- as.factor(data1$Year)
data1$doi <- factor(data1$doi)
data1$Year <- factor(data1$Year)

data1$country <- factor(data1$country)
data1$region <- factor(data1$region)
data1$species <- factor(data1$species)
data1$species_2 <- factor(data1$species_2)
data1$strain <- factor(data1$strain)
data1$antimicrobial <- factor(data1$antimicrobial)
data1$who_classification <- factor(data1$who_classification)
data1$ast_method <- factor(data1$ast_method)

data1$no_isolates_resistant<- as.numeric(data1$no_isolates_resistant)
data1$no_isolates_susceptible<-as.numeric(data1$no_isolates_susceptible)


model1<-glm(cbind(no_isolates_resistant,no_isolates_susceptible) ~ Year+doi+country+region+species+
           who_classification+ast_method,
         family=binomial, weights=no_isolate, data = data1)
summary(model1)

model2<-glm(cbind(no_isolates_resistant,no_isolates_susceptible) ~ Year+doi+country+region+species+
              who_classification,
            family=binomial, weights=no_isolate, data = data1)
summary(model2)


model3<-glm(cbind(no_isolates_resistant,no_isolates_susceptible) ~ Year+doi+country+region+species,
            family=binomial, weights=no_isolate, data = data1)
summary(model3)

model4<-glm(cbind(no_isolates_resistant,no_isolates_susceptible) ~ Year+doi+country+region,
            family=binomial, weights=no_isolate, data = data1)
summary(model4)

model5<-glm(cbind(no_isolates_resistant,no_isolates_susceptible) ~ Year+doi+country+region,
              
            family=binomial, weights=no_isolate, data = data1)
summary(model5)


model6<-glm(cbind(no_isolates_resistant,no_isolates_susceptible) ~ Year+doi+country,
            
            family=binomial, weights=no_isolate, data = data1)
summary(model6)

model7<-glm(cbind(no_isolates_resistant,no_isolates_susceptible) ~ Year+doi,
            
            family=binomial, weights=no_isolate, data = data1)
summary(model7)


model_list <- lst(model1, model2, model3,model4, model5,model6,model7)

# Compare models with AIC table
aic_table <- aictab(model_list)

print(aic_table)

#Model1 fits better
anova(model1)

##Diagnostics using DHArma
glm.diag.plots(model1, glmdiag = glm.diag(model1), subset = NULL,
               iden = FALSE, labels = NULL, ret = FALSE)


# Generate predicted values
predicted_values <- predict(model1, type = "response")

print(predicted_values)
# Generate true values (response variable)
true_values <- data1$no_isolates_resistant / (data1$no_isolates_resistant + data1$no_isolates_susceptible)
vars = names(data1)[14]








tt_theme = theme(text = element_text(family="Helvetica", size=9),
                 panel.border=element_blank(),
                 panel.background=element_blank(),
                 panel.grid = element_blank(),
                 axis.ticks.x = element_line(size=0.3),
                 axis.ticks.y = element_blank(),
                 axis.title.x = element_text(lineheight = 1.2),
                 axis.title.y = element_text(size = 10),
                 axis.text.x = element_text(color="black", size=10),
                 axis.text.y = element_text(color="black", size = 10),
                 legend.position="none")


# Convert 'Year' to a factor (discrete variable)

# Ensure Year is treated as a factor if it represents a categorical variable
data1$Year <- as.numeric(as.character(data1$Year))

data1$MeanRes <- as.numeric(as.character(data1$MeanRes))






p1 <- ggboxplot(data1, x = "Year", y = "MeanRes", fill = "MeanRes",
                     xlab = "Year", ylab = "MeanRes",
                     palette = c("#FD9825")) +
  geom_boxplot(fill = "#FD9825") +
  geom_jitter(alpha = 0.4) +
  rotate_x_text(60) +
  ggpubr::font("xlab", size = 17)+
  ggpubr::font("ylab", size = 17)+
  ggpubr::font("xy.text", size = 17)+
  ggpubr::font("legend.title",size = 17)+
  ggpubr::font("legend.text", size = 17)

p1

p2<-p1 + 
  geom_smooth(aes(as.numeric(Year),
                  (MeanRes)), 
              method = "glm",
              method.args = list(family = "quasibinomial"))

p2



vars = names(data1)[14] 
fits = lapply(vars, function(x) {glm(substitute(i ~ Year, list(i = as.name(x))), data = data1, family=quasibinomial, weights = data1$no_isolate)})
lapply(fits, summary)
#extract coefficient and p value
sapply(fits, function(f) summary(f)$coefficients[,c(1,4)])[c(2,4),]
#predict and calculate RMSE
myPreds_sal = lapply(fits, function(f) predict(f,data = data1, type = "response", se.fit = T))
RMSE_mean = rmse(data1$MeanRes,myPreds_sal[[1]]$fit)


p1 =  ggplot(data = data1) +
  geom_point(mapping = aes(x=Year, y = MeanRes),
             shape=21, fill="#3182BD", col="black", alpha=0.25, size=3, stroke=0.4, position = position_jitter(width = 0.2, height = 0)) +
  geom_boxplot(mapping = aes(x=Year, y = MeanRes, group = Year), outlier.shape = NA, fill="grey 70", col="black", alpha=0.2) +
  geom_ribbon(mapping = aes(x = Year,
                            ymin = myPreds_sal[[1]]$fit - 1.96*myPreds_sal[[1]]$se.fit,
                            ymax = myPreds_sal[[1]]$fit + 1.96*myPreds_sal[[1]]$se.fit),
              alpha = 0.3, fill="#FD9825") +
  geom_line(mapping = aes(x = Year, y = myPreds_sal[[1]]$fit), size=0.75, col="darkred", linetype = "dotted") +
  xlab(label = "") +
  scale_x_continuous(breaks = c(2000,2002,2004,2006,2008,2010,2012,2014,2016,2018))+
  scale_y_continuous(n.breaks = 6) +
  theme_bw() +
  tt_theme

