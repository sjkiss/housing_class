#If there have been any changes to cesdata2 run this
#remove.packages("cesdata2")
# remotes::install_github("sjkiss/cesdata2", force=T)
library(cesdata2)
#data("cesdata2")
data("ces25b")
data("ces84")
#Checks that the essential variables are available
names(ces25b)
ces25b$occupation_oesch_6
ces84$occupation_oesch_6

#Load other libraries
library(tidyverse)
library(srvyr)
library(haven)
library(crosstable)
library(labelled)
library(modelsummary)
library(nnet)
library(car)
#Make a vector of common variables
common_vars<-c("vote", "age", "occupation_oesch_6", "own_rent", "sub_class", "election", "prov", "quebec")
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


#### Model Vote by occupation class by election
#Nest the two elections
ces %>%
  nest(-election) %>%
  #run the mulinomial model on each election and store the resulting models in models
  mutate(model=map(data,
                   ~multinom(vote2~occupation_oesch_6, data=.)))->models

#Print 2025
modelsummary(models$model[[1]], shape=model+term~response,
             coef_omit=c("(Intercept)"), fmt=2, stars=T,
             output="model1_2025.html", title="2025")

#Print 1984
modelsummary(models$model[[2]], shape=model+term~response,
             coef_omit=c("(Intercept)"), fmt=2, stars=T,
             output="model1_1984.html", title="1984")


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

#### Model subjective class on time
mod_time<-multinom(sub_class~as.factor(election), data=ces)
modelsummary(mod_time, stars=T,
             shape=model+term~response,
             output="model_time_subjective_class.html", fmt=2)

#### Check Class by subjective social class

ces %>%
  group_by(election) %>%
  count(occupation_oesch_6, sub_class) %>%
  group_by(election, occupation_oesch_6) %>%
  filter(!is.na(sub_class)) %>%
  filter(!is.na(occupation_oesch_6)) %>%
  mutate(pct=n/sum(n)) %>%
  arrange(occupation_oesch_6,sub_class, election) %>%
  ggplot(., aes(y=occupation_oesch_6, fill=factor(election, levels=c("1984", "2025")), x=pct))+
  geom_col(position=position_dodge2(reverse = TRUE))+
  theme_minimal()+
  scale_fill_manual(values=c("black", "lightgrey"))+
  labs(fill="Election", x="Percent")+facet_grid(~sub_class)+
  theme(legend.position="bottom")
  ggsave(filename="plots/figure_2_effect_of_class_on_subjective_social_class.png", width=10, height=5)

#### Model subjective class on objective class for each election
# Use three category subjective class for simplicity
mod_occupation<-multinom(sub_class2~occupation_oesch_6*as.factor(election), data=ces)
summary(mod_occupation)
modelsummary(mod_occupation, shape=model+term~response, stars=T,
             coef_rename=c("occupation_oesch_61"="Skilled versus all below",
                           "occupation_oesch_62"="Semi-Professionals Associates versus all below",
                           "occupation_oesch_63"="Self-Employed versus all below",
                           "occupation_oesch_64"="Professionals versus all below",
                           "occupation_oesch_65"="Managers versus all below",
                           "as.factor(election)2025"="2025"),
             output="subjective_class_on_oesch_by_election.html")

library(marginaleffects)

avg_predictions(mod_occupation, variables="election", by=c("occupation_oesch_6", "election")) %>%
  ggplot(., aes(y=occupation_oesch_6, x=estimate, col=election))+
  geom_pointrange(aes(xmin=conf.low, xmax=conf.high), position=position_dodge(width=0.25))+
  facet_grid(~fct_relevel(group, "Working Class"))
ggsave(here("plots/figure3_effect_class_time.png"), width=8, height=4)

#### Integrate Home Ownership
# Probability of owning a home by age
# Fit a model checking the effet of age (life cycle) on ownership
# We use a direct term for age and a quadratic that permits the model to have a u-shaped vurve
# And we interact each with time.
mod_own<-glm(own_rent~age*as.factor(election)+I(age^2)*as.factor(election), data=ces, family="binomial")
modelsummary(mod_own, stars=T)
# visualize the effects
avg_predictions(mod_own,
                newdata=datagrid(age=seq(20,80, by=2),
                                 election=factor(c("1984", "2025"))),
                by=c( "age", "election")) %>%
  ggplot(., aes(x=age, y=estimate, col=election))+
  geom_smooth(method="loess")+geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=election), alpha=0.2, linewidth=0)+theme_minimal()+
  theme(legend.position="bottom")+guides(fill='none')+labs(y="Probability", x="Age",
                                       fill="", col="Election",
                                       title="Probability of Reporting Owning A Home\n1984 and 2025 Canada Election Studies")
library(here)
ggsave(filename=here("plots/figure_4_p_homeownership_by_age.png"), width=8, height=6)

# Now check to see if home owership changes the
mod_occupation_own_1984<-multinom(sub_class2~own_rent*age*I(age^2)*occupation_oesch_6, data=subset(ces, election==1984))
mod_occupation_own_2025<-multinom(sub_class2~own_rent*age*I(age^2)*occupation_oesch_6, data=subset(ces, election==2025))
summary(mod_occupation_own_1984)

# Generate effect of ownership on subjective class without regard to age


comparisons(mod_occupation_own_1984, variables=c("own_rent"),
            newdata=datagrid(occupation_oesch_6=levels(ces$occupation_oesch_6), age=seq(25,55,5))) ->effects_1984

effects_1984$Election<-rep(1984, nrow(effects_1984))
comparisons(mod_occupation_own_2025, variables=c("own_rent"),
            newdata=datagrid(occupation_oesch_6=levels(ces$occupation_oesch_6), age=seq(25,55,5))) ->effects_2025
effects_2025$Election<-rep(2025, nrow(effects_2025))
effects_1984 %>%
  bind_rows(., effects_2025) %>%
  ggplot(., aes(x=age,y=estimate, col=as.factor(Election)))+geom_smooth(method="loess", se=F)+
  facet_grid(fct_relevel(group, "Working Class")~occupation_oesch_6)+geom_hline(yintercept=0)+theme(legend.position="bottom")

write.csv(ces,"housing_ces.csv")
write_dta(ces, "housing_ces.dta")
