source("1_housing_class_master.R")

####Set up code ###
# Set theme for plots
theme_set(theme_minimal(base_size=20)+theme(legend.position="bottom"))

#Make vector for gof_omit
# we always omit these GOF from model print-outs
gof_omit_vector<-c("AIC|BIC|Log.Lik.|F")

#### Model Vote by occupation class by election ####

#Note, using that variable does give a hint of working class voting for the NDP, consistent with our previous work
# Good to keep in our back pocket.
# But here I'm using the stricter Oesch variable
model1_84_roc<-multinom(vote2~age+I(age^2)+as_factor(degree)+income_tertile+as_factor(degree)+own_rent,
                 data=subset(ces,quebec!=1&election==1984))
model1_84_qc<-multinom(vote2~age+I(age^2)+as_factor(degree)+income_tertile+as_factor(degree)+own_rent,
                 data=subset(ces,quebec==1&election==1984))
#add Ownership and subjective social class
model2_84_roc<-update(model1_84_roc, .~.-own_rent+sub_class2)
model2_84_qc<-update(model1_84_qc, .~-.own_rent+sub_class2)
# Add Ownership and objective class occupation_oesch_5
model3_84_roc<-update(model1_84_roc, .~.-own_rent+occupation_oesch_5)
model3_84_qc<-update(model1_84_qc, .~.-own_rent+occupation_oesch_5)
#Interaction between subjective social class and home ownership
model4_84_qc<-update(model1_84_qc, .~.+own_rent*sub_class2+occupation_oesch_5)
model4_84_roc<-update(model1_84_roc, .~.+own_rent*sub_class2+occupation_oesch_5)

#repeat with 2025
model1_25_roc<-multinom(vote2~age+I(age^2)+as_factor(degree)+income_tertile+as_factor(degree)+own_rent,
                        data=subset(ces,quebec!=1&election==2025))
model1_25_qc<-multinom(vote2~age+I(age^2)+as_factor(degree)+income_tertile+as_factor(degree)+own_rent,
                       data=subset(ces,quebec==1&election==2025))
model2_25_roc<-update(model1_25_roc, .~.-own_rent+sub_class2)
model2_25_qc<-update(model1_25_qc, .~.-own_rent+sub_class2)
model3_25_roc<-update(model1_25_roc, .~.-own_rent+occupation_oesch_5)
model3_25_qc<-update(model1_25_qc, .~.-own_rent+occupation_oesch_5)
model4_25_qc<-update(model1_25_qc, .~.+own_rent*sub_class2+occupation_oesch_5)
model4_25_roc<-update(model1_25_roc, .~.+own_rent*sub_class2+occupation_oesch_5)
#List the 1984 models in succession
roc_list_84<-list(model1_84_roc, model2_84_roc,  model3_84_roc, model4_84_roc)
#List the 2025 models in succession
roc_list_25<-list(model1_25_roc, model2_25_roc,  model3_25_roc, model4_25_roc)
#Print
modelsummary(roc_list_84,
             stars=T,
             fmt=2,
             shape=term~model+response, title="1984")

modelsummary(roc_list_25,
             stars=T,
             fmt=2,
             shape=term~model+response, title="2025")

#### Effect of ownership and subjective social class on vote choice  ####
#Here I combine the 1984 full models that interact ownership with subjective social class
model4_list<-list("1984"=model4_84_roc, "2025"=model4_25_roc)
modelsummary(model4_list,shape=term~model+response, stars=T, fmt=2)
model4_list %>%
  map(., avg_comparisons, variables=c("own_rent"), by=c("sub_class2")) %>%
  list_rbind(., names_to=c('Election')) %>%
  #filter(group!="NDP") %>%
  ggplot(., aes(y=group, x=estimate, col=group, alpha=Election))+
  geom_pointrange(aes(xmin=conf.low, xmax=conf.high))+
  facet_grid(~fct_relevel(sub_class2, "Working Class"))+
  scale_color_manual(values=c("darkblue",  "darkred", "orange"))+
  geom_vline(xintercept=0, linetype=2)


#### Concordance ####
# Basic concordance plot
# Note: This plot can be modified by replacing Percent
# with a count of respondents.
#That will show the collapse of working class respondents in the 2025 sample
# Do do that you need to just replace the x with n, rather than Percent
ces %>%
  #Count respondents by objective class and their concordance status
  count(election, obj_class2,concordance) %>%
  #Form groups of election and objective class to calcluate hwat percentage
  # of objective classes are concordant, deflators or inflators
  group_by(election, obj_class2) %>%
#Filter out missing concordants
  #These will be people who do not have an objective social class
  # mostly because of poor occupation coding
    filter(!is.na(concordance)) %>%
  #Calculate percent
  mutate(Percent=n/sum(n)) %>%
    ggplot(., aes(x=Percent, y=fct_relevel(concordance, "Deflator", "Concordant"), fill=election))+
  geom_col( position=position_dodge(reverse=T))+
  labs(x="Percent", y="Objective Social Class", fill="Election", title="Cocnordance by Objective Social Class, 1984 and 2025")+
  facet_grid(~fct_relevel(obj_class2,"Working Class"), scales="free_y")+
  scale_fill_grey()+scale_x_continuous(labels = scales::label_percent())
#Save plot
ggsave(filename=here("plots/concordance_objective_class.png"), width=10, height=8)

# Is Concordance Affected By Home ownership?
# #Report Class concordance by material class

# This is a bit tricky because we have this problem where
# working class people cannot be deflators and
# upper class people cannot be inflators
# That will leave unused factor levels in the different data sets
# Multinom() does not like that
# Set up working class data frame

ces_working_class <-filter(ces, obj_class2=="Working Class") %>%
  droplevels()
ces_upper_class <-filter(ces, obj_class2=="Upper Class") %>%
  droplevels()
# we don't need the droplevels() here because middle class can be
# inflators or deflators
ces_middle_class<-filter(ces, obj_class2=="Middle Class")

controls<-c("income_tertile", "degree")
controls
mod_own_working_84<-glm(concordance~own_rent, data=subset(ces_working_class, election=="1984"), family="binomial")
mod_own_working_84_controls<-update(mod_own_working_84,reformulate(c("own_rent+income_tertile", controls)))
mod_own_working_25<-glm(concordance~own_rent, data=subset(ces_working_class, election=="2025"), family="binomial")
mod_own_working_25_controls<-update(mod_own_working_25,reformulate(c("own_rent+income_tertile", controls)))
mod_own_upper_84<-glm(concordance~own_rent, data=subset(ces_upper_class, election=="1984"), family="binomial")
mod_own_upper_84_controls<-update(mod_own_upper_84,reformulate(c("own_rent+income_tertile", controls)))
mod_own_upper_25<-glm(concordance~own_rent, data=subset(ces_upper_class, election=="2025"), family="binomial")
mod_own_upper_25_controls<-update(mod_own_upper_25,reformulate(c("own_rent+income_tertile", controls)))
mod_own_middle_84<-multinom(concordance~own_rent, data=subset(ces_middle_class,election=="1984"))
mod_own_middle_84_controls<-update(mod_own_middle_84, reformulate(c("own_rent+income_tertile", controls)))
mod_own_middle_25<-multinom(concordance~own_rent, data=subset(ces_middle_class,election=="2025"))
mod_own_middle_25_controls<-update(mod_own_middle_25, reformulate(c("own_rent+income_tertile", controls)))

mod_own_list<-list("Working Classs\n1984 No controls"=mod_own_working_84,
                   "Working Classs\n1984 Controls"=mod_own_working_84_controls,
                   "Working Classs\n2025 No Controls"=mod_own_working_25,
                   "Working Classs\n2025 Controls"=mod_own_working_25_controls,
                   "Upper Classs\n1984 No Controls"=mod_own_upper_84,
                   "Upper Classs\n1984 Controls"=mod_own_upper_84_controls,
                   "Upper Class\n2025 No Controls"=mod_own_upper_25,
                   "Upper Class\n2025 Controls"=mod_own_upper_25_controls
                   )
modelsummary(mod_own_list, stars=T, coef_rename = c("own_rentOwn"="Ownership"), gof_omit=gof_omit_vector, title="Effect of Ownership on Class Concordance, 1984 and 2025, Working and Upper Class", fmt=2)
# Make the list of of middle class ownership models
mod_own_middle_list<-list("Middle Class 1984 No Controls"=mod_own_middle_84,
                          "Middle Class 1984 Controls"=mod_own_middle_84_controls,
                          "Middle Class 2025 No Controls"=mod_own_middle_25,
                          "Middle Class 2025 Controls"=mod_own_middle_25_controls
                          )
#Print
modelsummary(mod_own_middle_list, stars=T, shape=term~model+response,
             coef_rename=c("own_rentOwn"="Ownership"), gof_omit=gof_omit_vector,
             title="Effect of Ownership on Class Concordance 1984 and 2025, Middle Class", fmt=2)

# Generate an effect plot for middle class only
#
library(marginaleffects)
summary(mod_own_middle_84)
#Let's show the effect plot with the controls
# You have to go back up to the list of models for middle class
# And find the ones that have the controls
# If you wanted to show the ones without the controls
# You would switch these numbers
mod_own_middle_list[c(2,4)]%>%
  map(., avg_comparisons, variables=c("own_rent"), type="probs") %>%
  list_rbind() %>%
  mutate(Year=c(rep(1984, 3), rep(2025, 3))) %>%
  ggplot(., aes(x=fct_relevel(group, "Deflator"), y=estimate))+
  geom_pointrange(aes(ymin=conf.low, ymax=conf.high))+
    facet_grid(~Year)+
  labs(x="Concordance", y="Delta Probability",
       title="Effect of home ownership on concordance, controlling for income and education\n1984 and 2025")

#### Can we link this to vote at all?
# What is the research question?
# Is there a relationship between concordance and vote?
mod1_vote_concordance_working_qc_84<-multinom(vote2~concordance, data=subset(ces_working_class,election==1984&quebec!=1))
mod1_vote_concordance_working_roc_84<-multinom(vote2~concordance, data=subset(ces_working_class,election==1984&quebec==1))
mod1_vote_concordance_working_qc_25<-multinom(vote2~concordance, data=subset(ces_working_class,election==2025&quebec!=1))
mod1_vote_concordance_working_roc_25<-multinom(vote2~concordance, data=subset(ces_working_class,election==2025&quebec==1))

mod1_vote_concordance_upper_qc_84<-multinom(vote2~concordance, data=subset(ces_upper_class,election==1984&quebec!=1))
mod1_vote_concordance_upper_roc_84<-multinom(vote2~concordance, data=subset(ces_upper_class,election==1984&quebec==1))
mod1_vote_concordance_upper_qc_25<-multinom(vote2~concordance, data=subset(ces_upper_class,election==2025&quebec!=1))
mod1_vote_concordance_upper_roc_25<-multinom(vote2~concordance, data=subset(ces_upper_class,election==2025&quebec==1))

mod1_vote_concordance_middle_qc_84<-multinom(vote2~concordance, data=subset(ces_middle_class,election==1984&quebec!=1))
mod1_vote_concordance_middle_roc_84<-multinom(vote2~concordance, data=subset(ces_middle_class,election==1984&quebec==1))
mod1_vote_concordance_middle_qc_25<-multinom(vote2~concordance, data=subset(ces_middle_class,election==2025&quebec!=1))
mod1_vote_concordance_middle_roc_25<-multinom(vote2~concordance, data=subset(ces_middle_class,election==2025&quebec==1))

# model1_concordance_vote_25_roc<-multinom(vote2~concordance, data=subset(ces,election==2025&quebec!=1))
# model1_concordance_vote_25_qc<-multinom(vote2~concordance, data=subset(ces,election==2025&quebec==1))
vote_concordance_working_list<-list("QC 1984"=mod1_vote_concordance_working_qc_84,
                            "QC 2025"=mod1_vote_concordance_working_qc_25,
                            "ROC 1984"=mod1_vote_concordance_working_roc_84,
                            "ROC 2025"=mod1_vote_concordance_working_roc_25
                            )
names(vote_concordance_working_list)
vote_concordance_middle_list<-list("QC 1984"=mod1_vote_concordance_middle_qc_84,
                                    "QC 2025"=mod1_vote_concordance_middle_qc_25,
                                    "ROC 1984"=mod1_vote_concordance_middle_roc_84,
                                    "ROC 2025"=mod1_vote_concordance_middle_roc_25
)
vote_concordance_upper_list<-list("QC 1984"=mod1_vote_concordance_upper_qc_84,
                                   "QC 2025"=mod1_vote_concordance_upper_qc_25,
                                   "ROC 1984"=mod1_vote_concordance_upper_roc_84,
                                   "ROC 2025"=mod1_vote_concordance_upper_roc_25
)
names(vote_concordance_middle_list)
modelsummary(vote_concordance_working_list, fmt=2, stars=T,shape=term~response+model,
             gof_omit=gof_omit_vector)
modelsummary(vote_concordance_middle_list, fmt=2, stars=T,shape=term~response+model,
             gof_omit=gof_omit_vector)
modelsummary(vote_concordance_middle_list, fmt=2, stars=T,shape=term~response+model,
             gof_omit=gof_omit_vector)
#these models break out the material classes to see how class concordance interacts with vote ces$occupation_oesch_5
model1a_concordance_vote_84_roc<-update(model1_concordance_vote_84_roc, .~., data=subset(ces,election==1984&quebec!=1&occupation_oesch_5=="Unskilled workers"))
model1b_concordance_vote_84_roc<-update(model1_concordance_vote_84_roc, .~., data=subset(ces,election==1984&quebec!=1&occupation_oesch_5=="Skilled workers"))
model1c_concordance_vote_84_roc<-update(model1_concordance_vote_84_roc, .~., data=subset(ces,election==1984&quebec!=1&occupation_oesch_5=="Lower-grade service"))
model1d_concordance_vote_84_roc<-update(model1_concordance_vote_84_roc, .~., data=subset(ces,election==1984&quebec!=1&occupation_oesch_5=="Higher-grade service"))
#these models break out the material classes to see how class concordance interacts with vote ces$occupation_oesch_5
model1a_concordance_vote_25_roc<-update(model1_concordance_vote_25_roc, .~., data=subset(ces,election==2025&quebec!=1&occupation_oesch_5=="Unskilled workers"))
model1b_concordance_vote_25_roc<-update(model1_concordance_vote_25_roc, .~., data=subset(ces,election==2025&quebec!=1&occupation_oesch_5=="Skilled workers"))
model1c_concordance_vote_25_roc<-update(model1_concordance_vote_25_roc, .~., data=subset(ces,election==2025&quebec!=1&occupation_oesch_5=="Lower-grade service"))
model1d_concordance_vote_25_roc<-update(model1_concordance_vote_25_roc, .~., data=subset(ces,election==2025&quebec!=1&occupation_oesch_5=="Higher-grade service"))
#model1c_concordance_vote_25_roc<-update(model1_concordance_vote_25_roc, .~., data=subset(ces,election==2025&quebec!=1&occupation_oesch_5=="Self-employed"))
# model1d_concordance_vote_25_roc<-update(model1_concordance_vote_25_roc, .~., data=subset(ces,election==2025&quebec!=1&occupation_oesch_5=="Professionals"))
# model1e_concordance_vote_25_roc<-update(model1_concordance_vote_25_roc, .~., data=subset(ces,election==2025&quebec!=1&occupation_oesch_5=="Managers"))

#These models add home ownership to the mdoels
#Take out concordance and add own_rent
#1984
model2_concordance_vote_84_roc<-update(model1_concordance_vote_84_roc, .~-concordance+own_rent)
#re-add concordance
model3_concordance_vote_84_roc<-update(model1_concordance_vote_84_roc, .~concordance+own_rent)
#add interaction term
model4_concordance_vote_84_roc<-update(model1_concordance_vote_84_roc, .~concordance*own_rent)
#Take out concordance and add own_rent
#2025
model2_concordance_vote_25_roc<-update(model1_concordance_vote_25_roc, .~-concordance+own_rent)
#re-add concordance
model3_concordance_vote_25_roc<-update(model1_concordance_vote_25_roc, .~concordance+own_rent)
#add interaction term
model4_concordance_vote_25_roc<-update(model1_concordance_vote_25_roc, .~concordance*own_rent)
#summary(model4_concordance_vote_25_roc)
#Report basic lass concordance over time 1984 and 2025
concordance_vote_list<-list("1984"=model1_concordance_vote_84_roc, "2025"=model1_concordance_vote_25_roc)
modelsummary(concordance_vote_list, stars=T,
             fmt=2, shape=term~model+response, gof_omit=gof_omit_vector)
concordance_vote_own_list<-list("1984"=model3_concordance_vote_84_roc, "2025"=model3_concordance_vote_25_roc)
modelsummary(concordance_vote_own_list, stars=T,
             fmt=2, shape=term~model+response, gof_omit=gof_omit_vector)
# Show concordance and homeownership interaction in 84 and 2025
concordance_own_rent_list<-list("1984"=model4_concordance_vote_84_roc, "2025"=model4_concordance_vote_25_roc)
modelsummary(
  concordance_own_rent_list,
  fmt=2,
  stars=T, gof_omit=gof_omit_vector,
  shape=term~model+response
)
ces$concordance

ces_working_class <-filter(ces, obj_class2=="Working Class") %>%
  droplevels()
ces_upper_class <-filter(ces, obj_class2=="Upper Class") %>%
  droplevels()
model_concordance_vote_working_class<-multinom(vote2~concordance*own_rent,
data=subset(ces_working_class, election==1984)

model_concordance_vote_middle_class<-update(model_concordance_vote_working_class, data=subset(ces, election==1984&obj_class2=="Middle Class"))
model_concordance_vote_upper_class<-update(model_concordance_vote_working_class, data=subset(ces, election==1984&obj_class2=="Upper Class"))
model_concordance_vote_list<-list(model_concordance_vote_working_class,
                                  model_concordance_vote_middle_class,
                                  model_concordance_vote_upper_class)
summary(model_concordance_vote_working_class)
modelsummary(model_concordance_vote_list,
             fmt=2, stars=T, gof_omit=gof_omit_vector)
concordance_vote_list_material_84<-list(model1a_concordance_vote_84_roc, model1b_concordance_vote_84_roc, model1c_concordance_vote_84_roc, model1d_concordance_vote_84_roc)
modelsummary(concordance_vote_list_material_84,
             stars=T,
             fmt=2, shape=term~model+response)
concordance_vote_list_material<-list(model1a_concordance_vote_25_roc, model1b_concordance_vote_25_roc, model1c_concordance_vote_25_roc, model1d_concordance_vote_25_roc)
modelsummary(concordance_vote_list_material,
             stars=T,
             fmt=2, shape=term~model+response)
#Report Class concordance and home ownership
concordance_vote_list_ownership<-list(model1_concordance_vote_25_roc, model2_concordance_vote_25_roc, model3_concordance_vote_25_roc, model4_concordance_vote_25_roc)
modelsummary(concordance_vote_list_ownership, stars=T,
             fmt=2, shape=term~model+response)

ces25b %>%
  select(cps25_employment, NOC21_5, occupation_oesch) %>%
  as_factor() %>%
  group_by(cps25_employment) %>%
  count(valid_noc=is.na(NOC21_5), valid_oesch=is.na(occupation_oesch)) %>%
  filter(valid_noc==T)
