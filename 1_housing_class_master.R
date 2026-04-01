#If there have been any changes to cesdata2 run this
# remove.packages("cesdata2")
# devtools::install_github("sjkiss/cesdata2", force=T)

#Load libraries
library(cesdata2)

library(here)
library(tidyverse)
library(srvyr)
library(haven)
library(crosstable)
library(labelled)
library(modelsummary)
library(nnet)
library(car)
library(marginaleffects)
#Load the two datasets
data("ces25b")
data("ces84")
#Checks that the essential variables are available
names(ces25b)
table(ces25b$occupation_oesch)
ces25b$occupation_oesch_5
ces25b %>%
  filter(NOC21_5==14101) %>% select(occupation_oesch)
ces84$occupation_oesch_5
ces25b$housing

ces84$weight<-ces84$WTFACTOR
ces25b$weight<-ces25b$cps25_weight_kiss_module
#Make a vector of common variables
common_vars<-c("vote", "age","occupation_oesch", "occupation_oesch_5", "own_rent", "sub_class", "election", "prov", "quebec",
               "degree", "income_tertile", "male", "weight", "ideology", "cps25_employment",
               "education")

#bind the two together
ces25b %>%
  bind_rows(., ces84)->ces
#Select only the common variables
ces %>%
  select(any_of(common_vars))->ces

#Conduct recodes
ces$vote<-factor(as_factor(ces$vote), levels=c("Conservative", "Liberal", "NDP", "Bloc", "Green", "Other"))
ces$vote2<-factor(ces$vote, levels=c("Conservative", "Liberal", "NDP", "Bloc"))
ces$age<-as.numeric(ces$age)
ces$sub_class2<-car::Recode(ces$sub_class, "'Lower Class'='Working Class';
       'Upper-Middle Class'='Upper Class'", levels=c("Middle Class", "Working Class",
                                                     "Upper Class"))
ces$own_rent<-Recode(ces$own_rent, "'Other'='Rent'", levels=c("Rent", "Own"))


#Concordant and Discordant
# We need an objective cl

levels(ces$occupation_oesch)
levels(ces$sub_class2)
ces25b$education
ces84$education
ces$education
ces%>%
  mutate(obj_class2=case_when(
    occupation_oesch==5~ "Upper Class",
    occupation_oesch==6~"Middle Class",
    occupation_oesch==7~"Working Class",
    occupation_oesch==8~"Working Class",
    occupation_oesch==9~"Upper Class",
    occupation_oesch==10~"Middle Class",
    occupation_oesch==11~"Middle Class",
    occupation_oesch==12~"Working Class",
    occupation_oesch==13~"Upper Class",
    occupation_oesch==14~"Middle Class",
    occupation_oesch==15~"Middle Class",
    occupation_oesch==16~"Working Class",
    occupation_oesch==4&education<3~"Working Class",
    occupation_oesch==4&(education>2&education<5)~"Middle Class",
    occupation_oesch==4&(education==5)~"Upper Class"
  ))->ces
table(ces$obj_class2, ces$election)
ces$obj_class2<-factor(ces$obj_class2, levels=c("Middle Class", "Working Class", "Upper Class"))
#Factor occupation_oesch_5

ces$occupation_oesch<-as_factor(ces$occupation_oesch)
table(ces$occupation_oesch)
table(ces$occupation_oesch)
ces$occupation_oesch_5<-as_factor(ces$occupation_oesch_5)
ces$occupation_oesch_5<-factor(ces$occupation_oesch_5, levels=c("Unskilled workers", "Skilled workers", "Self-employed", "Lower-grade service", "Higher-grade service"))

ces$occupation_oesch<-factor(ces$occupation_oesch,
                             levels=c("Self-employed","Low-skilled manual", "Unskilled clerks", "Low-skilled service", "Skilled manual","Skilled clerks", "Skilled service",
                                      "Technicians", "Lower-grade managers", "Socio-cultural (semi-professionals)",
                                      "Technical experts", "Higher-grade managers", "Socio-cultural professionals"))

# Concordance
# ces %>%
#   mutate(concordance=case_when(
#   str_detect(occupation_oesch_5, "workers") & sub_class2=="Working Class" ~ "Concordant",
#   str_detect(occupation_oesch_5, "Higher-grade")& str_detect(sub_class2, "Upper") ~ "Concordant",
#   str_detect(occupation_oesch_5, "Lower-")&str_detect(sub_class2, "Middle Class")~ "Concordant",
#   # Deflators
#   str_detect(occupation_oesch_5, "Higher-grade")& str_detect(sub_class2, "Working Class|Middle Class") ~ "Deflator",
#   str_detect(occupation_oesch_5, "Lower-grade")&str_detect(sub_class2, "Working Class") ~ "Deflator",
#   # Inflators
#  str_detect(occupation_oesch_5, "workers")&str_detect(sub_class2, "Middle|Upper") ~ "Inflator",
#  str_detect(occupation_oesch_5, "Lower-grade")&str_detect(sub_class2, "Upper") ~ "Inflator"))->ces

ces %>% mutate(concordance=case_when(
  obj_class2=="Middle Class"&sub_class2=="Middle Class"~"Concordant",
  obj_class2=="Middle Class"&sub_class2=="Upper Class"~"Inflator",
  obj_class2=="Working Class"&sub_class2=="Working Class"~"Concordant",
  obj_class2=="Upper Class"&sub_class2=="Upper Class"~"Concordant",
obj_class2=="Upper Class"&sub_class2=="Middle Class"~ "Deflator",
obj_class2=="Upper Class"&sub_class2=="Working Class"~ "Deflator",
obj_class2=="Middle Class"&sub_class2=="Working Class"~ "Deflator",
obj_class2=="Working Class"&sub_class2=="Middle Class"~ "Inflator",
obj_class2=="Working Class"&sub_class2=="Upper Class"~ "Inflator"
)
)->ces
ces$concordance<-factor(ces$concordance, levels=c("Concordant", "Deflator", "Inflator"))
# Here I'm just creating a second Oesch variable
# collapsing the two working classes into one
# It makes things more comparable with our previous schema
table(as_factor(ces$occupation_oesch))
ces %>%
  mutate(occupation_oesch_5_2=case_when(
    occupation_oesch_5=="Unskilled workers"~"Working Class",
    occupation_oesch_5=="Skilled workers"~"Working Class",
    TRUE~occupation_oesch_5
  ))->ces
table(ces$occupation_oesch_5_2)
ces$occupation_oesch_5_2<-factor(ces$occupation_oesch_5_2, levels=c("Working class", "Lower-grade service",  "Self-employed","Higher-grade service"))
#ces$occupation_oesch_6_2<-factor(ces$occupation_oesch_6_2, levels=c("Working Class", "Semi-Professionals Associate Managers", "Self-employed", "Professionals", "Managers"))

#### Distribution of Subjective Class by election
ces %>%
  group_by(election, sub_class2) %>%
  summarize(n=n()) %>%
  filter(!is.na(sub_class2)) %>%
  mutate(pct=n/sum(n)) %>%
  arrange(sub_class2) %>%
  ggplot(., aes(y=fct_relevel(sub_class2, "Working Class"), fill=factor(election, levels=c("1984", "2025")), x=pct))+
  geom_col(position=position_dodge2(reverse = TRUE))+
  theme_minimal()+
  scale_fill_manual(values=c("black", "lightgrey"))+
  labs(fill="Election", x="Percent")
ggsave("plots/figure_1_change_sub_class.png")

# Check chisq
chisq.test(table(ces$election, ces$sub_class))


ces$election<-as.factor(ces$election)
write.csv(ces,"housing_ces.csv")
write_dta(ces, "housing_ces.dta")

#Make two different survey designs

ces84_des<-as_survey_design(subset(ces, election==1984), weights=weight)
ces25_des<-as_survey_design(subset(ces, election==2025&!is.na(weight)), weights=weight)
