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
#Make a vector of common variables
common_vars<-c("vote", "occupation_oesch_6", "own_rent", "sub_class", "election", "prov", "quebec")
#bind the two together
ces25b %>%
  bind_rows(., ces84)->ces
#Select only the common variables
ces %>%
  select(any_of(common_vars))->ces

#Conduct recodes
ces$vote<-factor(as_factor(ces$vote), levels=c("Conservative", "Liberal", "NDP", "Bloc", "Green", "Other"))
ces$vote2<-factor(ces$vote, levels=c("Conservative", "Liberal", "NDP", "Bloc"))
lookfor(ces84, "belongs")


#Model Vote by occupation class by election

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


#Distribution of Subjective Class by election
ces %>%
  group_by(election, sub_class) %>%
  summarize(n=n()) %>%
  filter(!is.na(sub_class)) %>%
  mutate(pct=n/sum(n)) %>%
  arrange(sub_class) %>%
  ggplot(., aes(y=sub_class, fill=factor(election, levels=c("1984", "2025")), x=pct))+
  geom_col(position=position_dodge2(reverse = TRUE))+
  theme_minimal()+
  scale_fill_manual(values=c("black", "lightgrey"))+
  labs(fill="Election", x="Percent")
ggsave("plots/figure_1_change_sub_class.png")

chisq.test(table(ces$election, ces$sub_class))


# Check Class by subjective social class

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

#I think we need to change the contrasts of these categorical variables
# To make this interpretable.
# The standard contrast is to copmpare each categorical level with a reference
  # Category
  # e.g. to compare the effect of being a skilled worker with that of being an unskilled worker
  # That makes sense, but does it make the same sense to compare the effect of being a professional
  # on picking a subjective social class compared to being an unskilled worker?

# To me, because these are effectively ordered categorical variables (low class to high class)
  # it makes more sense to contrast each level with the average of all levels below it.
  # these are known as helmert contrasts.
  # So the idea would be to compare the effect of being a "skilled worker" with being an "unskilled worker"
  # but then to compare the effect of being a semi-professional associate manager with the average of the two lower categories, e.g. Skilled and unskilled
  # and then to keep the chain going up to managers.
# I implement this gbelow.
  #Set contrasts for both categorical variables
  # but I am *really* stretching here!

contrasts(ces$occupation_oesch_6)<-contr.helmert(6)
contrasts(ces$sub_class)<-contr.helmert(5)
#Fit model
mod_occupation<-multinom(sub_class~occupation_oesch_6*as.factor(election), data=ces)
modelsummary(mod_occupation, shape=model+term~response, stars=T,
             coef_rename=c("occupation_oesch_61"="Skilled versus all below",
                           "occupation_oesch_62"="Semi-Professionals Associates versus all below",
                           "occupation_oesch_63"="Self-Employed versus all below",
                           "occupation_oesch_64"="Professionals versus all below",
                           "occupation_oesch_65"="Managers versus all below",
                           "as.factor(election)2025"="2025"),
             output="subjective_class_on_oesch_by_election.html")

avg_predictions(mod_occupation,
                variables=c("occupation_oesch_6", "election")) %>% data.frame() %>%
  ggplot(., aes(y=occupation_oesch_6, x=estimate, col=election))+geom_point()+facet_grid(~group)
