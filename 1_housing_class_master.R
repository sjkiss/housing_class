#If there have been any changes to cesdata2 run this
#remove.packages("cesdata2")
#remotes::install_github("sjkiss/cesdata2", force=T)
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
ces25b$occupation_oesch_6
ces84$occupation_oesch_6
ces25b$housing
ces84$weight<-ces84$WTFACTOR
lookfor(ces25b, "weight")
ces25b$weight<-ces25b$cps25_weight_kiss_module
#Make a vector of common variables
common_vars<-c("vote", "age", "occupation_oesch_6", "own_rent", "sub_class", "election", "prov", "quebec",
               "degree", "income_tertile", "male", "weight")

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
ces$own_rent
ces$own_rent<-Recode(ces$own_rent, "'Other'='Rent'", levels=c("Rent", "Own"))
ces$election<-as.factor(ces$election)
lookfor(ces84, "belongs")




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


write.csv(ces,"housing_ces.csv")
write_dta(ces, "housing_ces.dta")

#Make two different survey designs

ces84_des<-as_survey_design(subset(ces, election==1984), weights=weight)
ces25_des<-as_survey_design(subset(ces, election==2025&!is.na(weight)), weights=weight)
