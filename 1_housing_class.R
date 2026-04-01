#If there have been any changes to cesdata2 run this
# remove.packages("cesdata2")
# remotes::install_github("sjkiss/cesdata2", force=T)
library(cesdata2)
#data("cesdata2")
data("ces25b")
data("ces84")
#Create survey design
names(ces25b)
ces25b$occupation_oesch_6
ces84$occupation_oesch_6



library(tidyverse)
library(srvyr)
library(haven)
library(pollster)

#Combine the two
common_vars<-c(vote, occupation_oesch_6)
ces25b %>%
  bind_rows(., ces84)->ces
ces %>%
  select(occupation_oesch_6, vote, election)->ces

#Vote patterns of oesch classs
# Create survey desig
table(ces25b$occupation_oesch, useNA = "ifany")
#Check by retirement status
with(ces25b, table(occupation_oesch, cps25_employment,useNA = "ifany"))
#Check which NOC codes are missing
ces25b %>%
  filter(cps25_employment==1) %>%
  count(occupation_code) %>%
  write.csv(file = "missing_occupation_categories.csv")
#How many valid oesches do we have right now

ces25b %>%
  filter(working==1) %>%
  select(NOC21_5, occupation_oesch) %>% filter(!is.na(!NOC21_5)&is.na(occupation_oesch))



# Recodes

ces25b$vote<-as_factor(ces25b$vote)
ces25b$vote<-factor(ces25b$vote, levels=c("Conservative", "Liberal", "NDP", "Bloc", "Green", "Other"))
library(nnet)
table(ces25b$occupation_oesch)
ces25b$occupation_oesch<-factor(ces25b$occupation_oesch, levels=c("Production workers",
                                         "Office clerks",
                                         "Service workers",
                                         "Self-employed",
                                         "Technical Professionals",
                                         "(Associate) Managers",
                                         "Socio-cultural (semi) Professionals"))

#Set Greens to be other
ces25b$vote2<-car::Recode(ces25b$vote, "'Green'=NA;
                     'Other'=NA", levels=c(  "Conservative","Liberal", "NDP", "Bloc"))
library(crosstable)
library(kableExtra)
##Analysis
# Make Survey Design
#Make survey design
ces25b %>%
  filter(!is.na(cps25_weight_kiss_module)) %>%
  as_survey_design(., weights=cps25_weight_kiss_module)->ces25_des
#What proportion of the sample is working
# ces25_des %>%
#   group_by(working) %>%
#   summarise(pct=survey_prop())
#What is the vote
# ces25b %>%
#  filter(!is.na(cps25_weight_kiss_module)) %>%
#   filter(!is.na(vote2)) %>%
#   topline(., variable=vote2, weight=cps25_weight_kiss_module)

#Unweighted working
ces25b %>%
  crosstable(., cols=working)
#Unweighted vote intention
ces25b %>%
 # /filter(!is.na(cps25_weight_kiss_module)) %>%
  filter(!is.na(vote2)) %>%
  crosstable(., cols=vote2)
# What are the dates of our responses
#PLot of date of start dates
ces25b %>%
  ggplot(., aes(x=cps25_StartDate_DMY))+geom_histogram()


#What is the oesch by vote
#Weighted crosstab of occupation and vote2
# ces25_des %>%
#   filter(!is.na(vote2)) %>%
#   filter(!is.na(occupation_oesch)) %>%
#   group_by(occupation_oesch, vote2) %>%
#   summarise(pct=survey_prop()) %>% view()

#Install.
#
mod1<-multinom(vote2~occupation_oesch,
               data=subset(ces25b, !is.na(cps25_weight_kiss_module)))
library(modelsummary)
modelsummary(mod1, shape=model+term~response,
             coef_omit=c("(Intercept)"), fmt=2, stars=T,
             output="model1.html")


# Check the Quebec situation
ces25b$que

# Do not own homes
ces25b %>%
  mutate(homeowner=case_when(
    cps25_property_1==-99~"Renter",
    TRUE~"Owner"
  ))->ces25b
ces25b$renter<-factor(ces25b$homeowner, levels=c("Owner", "Renter"))
# Does renting reduce subjective class membership

ces25b$subjective_class<-as_factor(ces25b$kiss_module_Q2)
library(car)
ces25b$subjective_class<-Recode(ces25b$subjective_class, "'Don\\'t know/ Prefer not to answer'=NA")

levels(ces25b$subjective_class)<-c("Lower Class", "Working Class", "Middle Class","Upper-Middle Class", "Upper Class")
ces25b$occupation3<-as_factor(ces25b$occupation3)
table(ces25b$occupation3)
ces25b$occupation3<-factor(ces25b$occupation3, levels=c("Unskilled", "Skilled", "Routine_Nonmanual", "Professional", "Managers", "Self_employed"))

ces25b$occupation4<-Recode(ces25b$occupation3, "'Skilled'='Working Class' ; 'Unskilled'='Working Class'; 'Routine_Nonmanual'='Routine Non-manual' ;
       'Managers'='Managers' ; 'Professional'='Professional'; 'Self_employed'='Self-employed'", levels=c("Working Class", 'Routine Non-manual', 'Self-employed','Professional', 'Managers'))
library(crosstable)
crosstable(ces25b, cols=occupation4, by=subjective_class) %>% view()

ces25b %>%
  filter(occupation3=="Skilled"|occupation3=="Unskilled") %>%
  select(occupation3,occupation_category) %>% view()
table(ces25b$occupation3, useNA = "ifany")

library(survey)
library(nnet)
library(marginaleffects)
mod1<-multinom(subjective_class~occupation4*renter, data=ces25b)
predictions(mod1)
plot_predictions(mod1,condition=c("renter", "group", "occupation4"), type="probs")

# AS factor
ces25b$occupation3
ces25_des %>%
  group_by(as_factor(occupation3), as_factor(kiss_module_Q1)) %>%
summarise(survey_prop(), N=survey_total())


ces25_des %>%
  group_by(as_factor(occupation3), as_factor(kiss_module_Q2)) %>%
  summarise(survey_prop(), N=survey_total())


ces25_des %>%
  group_by(occupation3, renter,subjective_class) %>%
  summarize(survey_prop()) %>% as_factor() %>%
  arrange(occupation3, subjective_class,renter)


