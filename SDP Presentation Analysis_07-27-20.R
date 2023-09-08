## SDP Presentation Analysis Script

library(tidyverse)
library(jtools)
library(dominanceanalysis)

#Read in data
FullData <- read.csv("C:/Users/antovich/Desktop/Copy of dual_enrl_dataset_updated.csv")%>%
  #For logistic regression, make the largest ethnic group ("White, Not Hispanic) the reference group
  mutate(s_raceethnicity = factor(s_raceethnicity, 
                                  levels = c("White, Not Hispanic", "African American", "Hispanic", "Asian American", "Other")),
         hs_location = factor(hs_location,
                              levels = c("City: Large", "City: Midsize", "City: Small", "Suburb: Large", 
                                         "Suburb: Midsize", "Suburb: Small", "Town: Distant", "Town: Remote", 
                                         "Rural: Fringe", "Rural: Distant", "Rural: Remote")),
         #Ensure factors are coded appropriately
         s_male = as.factor(s_male),
         hs_diploma = as.factor(hs_diploma),
         late_grad = as.factor(late_grad),
         s_frpl_ever = as.factor(s_frpl_ever),
         lep_ever = as.factor(lep_ever),
         sped_ever = as.factor(sped_ever),
         enrl_1oct_grad_yr1_2yr = as.factor(enrl_1oct_grad_yr1_2yr),
         enrl_1oct_grad_yr1_4yr = as.factor(enrl_1oct_grad_yr1_4yr),
         enrl_1oct_grad_yr1_any = as.factor(enrl_1oct_grad_yr1_any),
         enrl_1oct_grad_yr1_vtech = as.factor(enrl_1oct_grad_yr1_vtech),
         dual_enrl_taken = as.factor(dual_enrl_taken),
         remedial_taken = as.factor(remedial_taken))

#Data just for looking at categorical proportions
PropData <- read.csv("C:/Users/antovich/Desktop/Copy of dual_enrl_dataset_updated.csv")

#Filtered datasets 
GradsOnly <- FullData %>% filter(hs_diploma == 1)
PublicOnly <- FullData %>% filter(public_enrl_1oct_grad_yr1 == 1)
CollegeOnly <- FullData %>% filter(enrl_1oct_grad_yr1_any == 1)
DualEnroll <- FullData %>% filter(dual_enrl_taken == 1)
PubDualEnroll <- filter(PublicOnly, dual_enrl_taken == 1)

#Plot missingness
vis_miss(GradsOnly, warn_large_data = FALSE)
vis_miss(FullData, warn_large_data = FALSE)

#proportion of grads who go to college  
mean(PropData$enrl_1oct_grad_yr1_any, na.rm = TRUE)

#Proportion of public college students who took remedial courses
mean(filter(PropData, public_enrl_1oct_grad_yr1 == 1)$remedial_taken, na.rm = TRUE)

#Proportion of students dual enrolled
mean(PropData$dual_enrl_taken)

#Proportion of students in 4yr vs 2yr college
mean(filter(PropData, enrl_1oct_grad_yr1_any == 1)$type_enrl_1oct_grad_yr1, na.rm = TRUE)


#Description of sample

DemoVar <- "s_raceethnicity"
OverallDem <- prop.table(table(FullData[DemoVar]))

GradDem <- prop.table(table(filter(FullData, hs_diploma == 1)[DemoVar]))
NoGradDem <- prop.table(table(filter(FullData, hs_diploma == 0)[DemoVar]))

CollegeDem <- prop.table(table(filter(FullData, enrl_1oct_grad_yr1_any == 1)[DemoVar]))
NoCollegeDem <- prop.table(table(filter(FullData, enrl_1oct_grad_yr1_any == 0)[DemoVar]))

DualDem <- prop.table(table(filter(FullData, dual_enrl_taken == 1)[DemoVar]))
NoDualDem <- prop.table(table(filter(FullData, dual_enrl_taken == 0)[DemoVar]))


PredictEnrol <- glm(dual_enrl_taken ~ s_male + s_raceethnicity + s_frpl_ever + 
                      lep_ever + sped_ever + hs_location + district_name + math_8_scaled + read_8_scaled, family = "binomial", data = FullData)

summ(PredictEnrol, exp = TRUE, confint = TRUE, )
anova(PredictEnrol, PredictEnrolWithDistrict, test = "LRT")
daPredictEnrol<-dominanceAnalysis(PredictEnrol)

EnrollPredictPlot <- plot_summs(PredictEnrol, exp = TRUE, confint = TRUE, scale = TRUE)

EnrollPredictPlot+
  xlab("OR Estimates\nwith 95% CI")+
  ylab("")+
  scale_y_discrete(labels = c("8th Grade Reading", "8th Grade Math", 
                              "Western", "Southwestern", "Southeastern", "South Central", "Northwestern","Northeastern", "Eastern", 
                              "Rural: Remote", "Rural: Distant", "Rural: Fringe", "Town: Remote", "Town: Distant",
                              "Suburb: Small", "Suburb: Midsize", "Suburb: Large", "City: Small", "City: Midsize",
                              "Special Ed", "Limited English", "FRPL", 
                              "Other", "Asian American", "Hispanic","African American", 
                              "Male"))+
  coord_cartesian(xlim = c(0,5))+
  theme_bw(base_size = 14)+
  theme(axis.text.y = element_text(hjust = 0), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())


ggsave(filename = "H:/Application Materials/Presentation/DE_ORs.jpeg", dpi = 300, width = 4, height = 5, units = "in")

unique(FullData$district_name)
plot(daPredictEnrol, which.graph ="general",fit.function = "r2.m")+
  theme_bw(base_size = 14)+
  xlab("Predictor")+
  ylab("Avg. McFadden R^2")+
  ggtitle("Dual Enrollment Predicition \nDominance Analysis: General Dominance")+
  theme(plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")+
  scale_x_discrete(limits = c("s_frpl_ever", "hs_location", "district_name","s_male",  "s_raceethnicity", "lep_ever", "sped_ever"),
                   labels = c("FRPL", "Location",  "District","Gender",  "Race/Ethnicity","Limited English",   "SpecialEd"))+
  scale_fill_manual(values = c("s_frpl_ever" = rev(pal)[2], "hs_location" = rev(pal)[3], 
                               "district_name" = rev(pal)[4],"s_male" = rev(pal)[5], 
                               "s_raceethnicity" = rev(pal)[6],"lep_ever" = rev(pal)[7], "sped_ever" = rev(pal)[8]))

Enrolled <- c('Dual Enrolled' = sum(FullData$dual_enrl_taken == 1), 'HS Only'  = sum(FullData$dual_enrl_taken != 1))
waffle(Enrolled/1000, rows = 5,  colors = c("midnightblue","deeppink3"))
ggsave(filename = "H:/Application Materials/Presentation/DE_waffle.jpeg", dpi = 300, width = 5, height = 4, units = "in")


colorspace::swatchplot(rev(pal))
ggsave(filename = "H:/Application Materials/Presentation/DE_generaldominance.jpeg", dpi = 300)
plot(dapres, which.graph ="conditional",fit.function = "r2.m")

LogReg <- effect_plot(PredictCollege, pred = act_score_math, plot.points = TRUE)

summary(daPredictEnrol)

plot(daPredictEnrol, which.graph ="general",fit.function = "r2.m")+
  theme_bw(base_size = 14)+
  xlab("Predictor")+
  ylab("Avg. McFadden R^2")+
  ggtitle("College Enrollment Predicition \nDominance Analysis: General Dominance")+
  theme(plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")+
  scale_x_discrete(limits = c("s_frpl_ever", "hs_location", "district_name","s_male",  "s_raceethnicity", "lep_ever", "sped_ever"),
                   labels = c("FRPL", "Location",  "District","Gender",  "Race/Ethnicity","Limited English",   "SpecialEd"))+
  scale_fill_manual(values = c("s_frpl_ever" = rev(pal)[2], "hs_location" = rev(pal)[3], 
                               "district_name" = rev(pal)[4],"s_male" = rev(pal)[5], 
                               "s_raceethnicity" = rev(pal)[6],"lep_ever" = rev(pal)[7], "sped_ever" = rev(pal)[8]))


#note that different metrics for and ACT are available
PredictCollege <- glm(enrl_1oct_grad_yr1_any ~ s_male + s_raceethnicity + s_frpl_ever  + late_grad +
                        lep_ever + hs_location + district_name +
                        act_score_math + act_score_eng + act_score_read + act_score_sci + dual_enrl_taken,
                      family = "binomial", data = FullData)


summ(PredictCollege, exp = TRUE, confint = TRUE)
PredictCollegePlot <- plot_summs(PredictCollege, exp = TRUE, confint = TRUE, scale = TRUE)
PredictCollegePlot+
  xlab("OR Estimates\nwith 95% CI")+
  ylab("")+
  scale_y_discrete(labels = c("Dual Enrollment","ACT Sci", "ACT Read", "ACT Eng", "ACT Math", 
                              "Western", "Southwestern", "Southeastern", "South Central", "Northwestern","Northeastern", "Eastern", 
                              "Rural: Remote", "Rural: Distant", "Rural: Fringe", "Town: Remote", "Town: Distant",
                              "Suburb: Small", "Suburb: Midsize", "Suburb: Large", "City: Small", "City: Midsize",
                              "Limited English","Late Grad", "FRPL", 
                              "Other", "Asian American", "Hispanic","African American", 
                              "Male"))+
  coord_cartesian(xlim = c(0,3))+
  theme_bw(base_size = 14)+
  theme(axis.text.y = element_text(hjust = 0), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

ggsave(filename = "H:/Application Materials/Presentation/Coll_ORs.jpeg", dpi = 300, width = 4, height = 5, units = "in")

PredictCollegeInteract <- glm(enrl_1oct_grad_yr1_any ~ s_male + s_raceethnicity + s_frpl_ever  + late_grad +
                        lep_ever + hs_location + district_name +
                        act_score_math + act_score_eng + act_score_read + act_score_sci + dual_enrl_taken + dual_enrl_taken*s_frpl_ever,
                      family = "binomial", data = FullData)
summ(PredictCollegeInteract)

AlluvialData <- FullData %>%
  reshape2::dcast(enrl_1oct_grad_yr1_any + dual_enrl_taken ~ "Frequency", fun.aggregate = length) %>%
  rename(College = enrl_1oct_grad_yr1_any, Dual_Enroll = dual_enrl_taken)%>%
  filter(!is.na(College),!is.na(Dual_Enroll))%>%
  mutate(College = recode(College, "0" = "No", "1" = "Yes"), Dual_Enroll = recode(Dual_Enroll, "0" = "No", "1" = "Yes"))

library(ggalluvial)

#This version of the alluvial plot uses wide-format data
ggplot(data = AlluvialData,
       aes(axis1 = Dual_Enroll, axis2 = College, y = Frequency)) +
  scale_x_discrete(expand = c(.1, .05), limits = c("Dual_Enroll", "College"),labels = c("Dual Enrolled in HS", "Attended College")) + #expand is related to the distance of the data from the axes
  xlab("") +
  ylab("Frequency")+
  #alluvia are the connection between x-axis points
  geom_alluvium(aes(fill = Dual_Enroll), alpha = .7, width = .2) + #if straight lines are desired add: knot.pos = 0
  #strata are the x-axis points; use geom_text in place of geom_label to remove the text background
  geom_stratum(width = .2, color = "white", fill = "grey23", alpha = .25) + geom_text(stat = "stratum", label.strata = TRUE, color = "grey23") +
  theme_minimal() +
  #theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
  #ggtitle("Correspondence between TMCQ and DSM Classification")+
  scale_fill_manual(name = "Dual\nEnrolled", values = c(rev(pal)[2], rev(pal)[10]))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave(filename = "H:/Application Materials/Presentation/ColAlluvial.tiff", dpi =600, width = 5, height = 5, units = "in", pointsize =)
tiff(filename="H:/Application Materials/Presentation/ColAlluvial2.tiff", width=5, height=5,
     units="in", res=300, pointsize=12)


#Compare models with and without a predictor var using the likelihood ratio test
anova(PredictCollegeMinusRace, PredictCollege, test = "LRT")

daPredictCollege<-dominanceAnalysis(PredictCollege)

plot(daPredictCollege, which.graph ="general",fit.function = "r2.m")+
  theme_bw(base_size = 14)+
  xlab("Predictor")+
  ylab("Avg. McFadden R^2")+
  ggtitle("College Enrollment Predicition \nDominance Analysis: General Dominance")+
  theme(plot.title = element_text(hjust = .5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")+
  scale_x_discrete(limits = c("dual_enrl_taken", "s_frpl_ever", "s_male",  "s_raceethnicity", "hs_location", "district_name","late_grad","lep_ever"),
                   labels = c("Dual Enroll", "FRPL", "Gender",  "Race/Ethnicity", "High School\nLocation","District",   "Late Grad", "Limited English"))+
  scale_fill_manual(values = c("dual_enrl_taken" = rev(pal)[2], "s_frpl_ever" = rev(pal)[3], 
                               "s_male" = rev(pal)[4],"s_raceethnicity" = rev(pal)[5],"hs_location" = rev(pal)[6],"district_name" = rev(pal)[7], 
                               "late_grad" = rev(pal)[8], "lep_ever" = rev(pal)[9]))

ggsave(filename = "H:/Application Materials/Presentation/Coll_generaldominance.jpeg", dpi = 300, height = 5, width = 5, unit = "in")

#note that different metrics for and ACT are available
PredictCollegeDualHours <- glm(enrl_1oct_grad_yr1_any ~ s_male + s_raceethnicity + s_frpl_ever  + late_grad +
                                 lep_ever + hs_location + district_name + 
                                 act_score_math + act_score_eng + act_score_read + act_score_sci + dual_enrl_hours,
                               family = "binomial", data = DualEnroll)
summ(PredictCollegeDualHours)


Predict2Yr <- glm(enrl_1oct_grad_yr1_2yr ~ s_male + s_raceethnicity + s_frpl_ever  + late_grad +
                    lep_ever + hs_location + district_name +
                    act_score_math + act_score_eng + act_score_read + act_score_sci + dual_enrl_taken,
                  family = "binomial", data = CollegeOnly)
summ(Predict2Yr)

Predict4Yr <- glm(enrl_1oct_grad_yr1_4yr ~ s_male + s_raceethnicity + s_frpl_ever  + late_grad +
                    lep_ever + hs_location + district_name +
                    act_score_math + act_score_eng + act_score_read + act_score_sci + dual_enrl_taken,
                  family = "binomial", data = CollegeOnly)
summ(Predict4Yr)

Predict4vs2Yr <- glm(type_enrl_1oct_grad_yr1 ~ s_male + s_raceethnicity + s_frpl_ever  + late_grad +
                    lep_ever + hs_location + district_name +
                    act_score_math + act_score_eng + act_score_read + act_score_sci + dual_enrl_taken,
                  family = "binomial", data = CollegeOnly)

summ(Predict4vs2Yr, exp = TRUE, confint = TRUE)

PredictRemedial <- glm(remedial_taken ~ s_male + s_raceethnicity + s_frpl_ever  + late_grad +
                         lep_ever + hs_location + district_name +
                         act_score_math + act_score_eng + act_score_read + act_score_sci +  dual_enrl_taken,
                       family = "binomial", data = PublicOnly)

summ(PredictRemedial, exp = TRUE, confint = TRUE)


PredictRemedialinteract <- glm(remedial_taken ~ s_male + s_raceethnicity + s_frpl_ever  + late_grad +
                         lep_ever + hs_location + district_name +
                         act_score_math + act_score_eng + act_score_read + act_score_sci +  dual_enrl_taken + s_frpl_ever*dual_enrl_taken,
                       family = "binomial", data = PublicOnly)

summ(PredictRemedialinteract, exp = TRUE, confint = TRUE)

#predict remedial hours only for students with remedial data and dual enrollment
PredictRemedialDualHours <- glm(remedial_taken ~ s_male + s_raceethnicity + s_frpl_ever   +
                                  lep_ever + hs_location + 
                                  act_score_math + act_score_eng + act_score_read + act_score_sci +  dual_enrl_hours,
                                family = "binomial", data = filter(PublicOnly, dual_enrl_taken == 1))

summ(PredictRemedialDualHours, exp = TRUE, confint = TRUE)


PredictGrad <- glm(hs_diploma ~ s_male + s_raceethnicity + s_frpl_ever   +
                     lep_ever + hs_location + district_name + dual_enrl_taken,
                   family = "binomial", data = FullData)

summ(PredictGrad, exp = TRUE, confint = TRUE)

PredictStatusEnroll <- multinom(status_enrl_1oct_grad_yr1 ~ s_male + s_raceethnicity + s_frpl_ever  + late_grad +
                                  lep_ever + hs_location + district_name +
                                  act_score_math + act_score_eng + act_score_read + act_score_sci + dual_enrl_taken,
                                data = CollegeOnly)


PredictACT <- lm(act_score_composite ~ s_male + s_raceethnicity + s_frpl_ever  + late_grad +
                   lep_ever + hs_location + district_name + 
                   + dual_enrl_taken, data = FullData)

summary(PredictACT)



summ(PredictStatusEnroll, confint = TRUE, exp = TRUE)


summary(PredictEnrol)

plot_coefs(PredictEnrol, confint = TRUE, exp = TRUE)
plot_summs(Predict2Yr, Predict4Yr, confint = TRUE, exp = TRUE)

unique(FullData$hs_location)
effect_plot(PredictEnrol, pred = hs_location, interval = TRUE, y.label = "% Dual Enrollment", x.label = "Location")

FullData%>%
  piv

library(wesanderson)
library(RColorBrewer)
library(viridis)

pal <- brewer.pal(11, "Spectral")

#HS Location plot
DualHSLoc <- ggplot(FullData, aes(dual_enrl_taken, ..count..)) + 
  geom_bar(aes(fill = hs_location), position = "fill")+
  labs(fill = "High School \nLocation",
       x = "Dual Enrollment Taken",
       y = "Proportion")+
  scale_x_discrete(labels = c("HS Only", "Dual Enrolled"))+
  theme_bw(base_size = 14)+
  scale_fill_manual(values = rev(pal))
#scale_fill_viridis(option = "C", discrete = TRUE)

DualDist <- ggplot(FullData, aes(dual_enrl_taken, ..count..)) + 
  geom_bar(aes(fill = district_name), position = "fill")+
  labs(fill = "School District",
       x = "Dual Enrollment Taken",
       y = "Proportion")+
  scale_x_discrete(labels = c("HS Only", "Dual Enrolled"))+
  theme_bw(base_size = 14)+
  scale_fill_manual(values = rev(pal), labels = c("East Central" , "Eastern" , 
                                                  "Northeastern" , "Northwestern" , 
                                                  "South Central" , "Southeastern", "Southwestern", "Western" ))

# DualHSBUBBLE <- ggplot(FullData, aes(x = dual_enrl_taken, y = hs_location)) + 
#   geom_count(aes(size=..prop.., group = dual_enrl_taken))+
#   scale_size(range = c(0,15))+
#   labs(fill = "High School \nLocation",
#        x = "Dual Enrollment Taken",
#        y = "Proportion")+
#   scale_x_discrete(labels = c("HS Only", "Dual Enrolled"))+
#   theme_bw()+
#   scale_fill_manual(values = rev(pal))
# #scale_fill_viridis(option = "C", discrete = TRUE)

#Demographics plot
DualRace <- ggplot(FullData, aes(dual_enrl_taken, ..count..)) + 
  geom_bar(aes(fill = s_raceethnicity), position = "fill")+
  labs(fill = "Race/Ethnicity",
       x = "Dual Enrollment Taken",
       y = "Proportion")+
  scale_x_discrete(labels = c("HS Only", "Dual Enrolled"))+
  theme_bw(base_size = 14)+
  scale_fill_manual(values = c(rev(pal)[2:6]))



DualSex <- ggplot(FullData, aes(dual_enrl_taken, ..count..)) + 
  geom_bar(aes(fill = s_male), position = "fill")+
  labs(fill = "Gender",
       x = "Dual Enrollment Taken",
       y = "Proportion")+
  scale_x_discrete(labels = c("HS Only", "Dual Enrolled"))+
  theme_bw(base_size = 14)+ #base_size = 12
  scale_fill_manual(values = c(rev(pal)[2:3]))

DualFRPL <- ggplot(FullData, aes(dual_enrl_taken, ..count..)) + 
  geom_bar(aes(fill = s_frpl_ever), position = "fill")+
  labs(x = "Dual Enrollment Taken",
       y = "Proportion")+
  scale_x_discrete(labels = c("HS Only", "Dual Enrolled"))+
  theme_bw(base_size = 14)+
  scale_fill_manual(name = "FRPL\nEligibility", 
                    values = c(rev(pal)[2:3]),
                    labels = c("No", "Yes"))

mean(FullData$math_8_scaled, na.rm = TRUE)
DualMath <- ggplot(FullData, aes(dual_enrl_taken, math_8_scaled)) + 
  geom_bar(stat = "summary", fun.y = "mean")+
  labs(x = "Dual Enrollment Taken",
       y = "Proportion")+
  scale_x_discrete(labels = c("HS Only", "Dual Enrolled"))+
  theme_bw()+
  scale_fill_manual(name = "FRPL\nEligibility", 
                    values = c(rev(pal)[2:3]),
                    labels = c("No", "Yes"))

Plot1 <- ggplot(PublicOnly, aes(remedial_taken, ..count..)) + 
  geom_bar(aes(fill = dual_enrl_taken), position = "fill")+
  labs(fill = "Dual Enrollment",
       x = "Remedial Taken",
       y = "Proportion")+
  scale_x_discrete(labels = c("No", "Yes"))+
  theme_bw(base_size = 14)+
  scale_fill_manual(values = c(rev(pal)[2:6]), labels = c("No", "Yes"))+
  theme(legend.position = "none")

Plot2 <- ggplot(CollegeOnly, aes(as.factor(type_enrl_1oct_grad_yr1), ..count..)) + 
  geom_bar(aes(fill = dual_enrl_taken), position = "fill")+
  labs(fill = "Dual Enrollment",
       x = "College Type",
       y = "Proportion")+
  scale_x_discrete(labels = c("2-yr", "4-yr"))+
  theme_bw(base_size = 14)+
  scale_fill_manual(values = c(rev(pal)[2:6]), labels = c("No", "Yes"))+
  theme(legend.position = "none")

Plot3 <- ggplot(FullData, aes(as.factor(hs_diploma), ..count..)) + 
  geom_bar(aes(fill = dual_enrl_taken), position = "fill")+
  labs(fill = "Dual Enrollment",
       x = "Graduated",
       y = "Proportion")+
  scale_x_discrete(labels = c("No", "Yes"))+
  theme_bw(base_size = 14)+
  scale_fill_manual(values = c(rev(pal)[2:6]), labels = c("No", "Yes"))

AlignedFigs <- cowplot::align_plots( DualRace, DualSex, DualFRPL, align="hv", axis="tblr")

AlignedFigs2 <- cowplot::align_plots(DualDist, DualHSLoc, align="hv", axis="tblr")


outputplot <- cowplot::plot_grid(Plot1, Plot2, Plot3, nrow= 1, align="hv", axis="tblr")
cowplot::ggsave2(plot = Plot3,filename = "H:/Application Materials/Presentation/DE_threepanel3.jpeg", dpi = 300, width = 3, height = 3, units = "in")

