source("1_housing_class_master.R")

#### Model Vote by occupation class by election
#Note, using that variable does give a hint of working class voting for the NDP, consistent with our previous work
# Good to keep in our back pocket.
# But here I'm using the stricter Oesch variable
model1_84_roc<-multinom(vote2~age+I(age^2)+as_factor(degree)+income_tertile+as_factor(degree)+own_rent,
                 data=subset(ces,quebec!=1&election==1984))
model1_84_qc<-multinom(vote2~age+I(age^2)+as_factor(degree)+income_tertile+as_factor(degree)+own_rent,
                 data=subset(ces,quebec==1&election==1984))
model2_84_roc<-update(model1_84_roc, .~.-own_rent+sub_class2)
model2_84_qc<-update(model1_84_qc, .~-.own_rent+sub_class2)
model3_84_roc<-update(model1_84_roc, .~.-own_rent+occupation_oesch_5)
model3_84_qc<-update(model1_84_qc, .~.-own_rent+occupation_oesch_5)
model4_84_qc<-update(model1_84_qc, .~.+own_rent*sub_class2+occupation_oesch_5)
model4_84_roc<-update(model1_84_roc, .~.+own_rent*sub_class2+occupation_oesch_5)

summary(model2_84_roc)
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
roc_list_84<-list(model1_84_roc, model2_84_roc,  model3_84_roc, model4_84_roc)
roc_list_25<-list(model1_25_roc, model2_25_roc,  model3_25_roc, model4_25_roc)
modelsummary(roc_list_84,
             stars=T,
             fmt=2,
             shape=term~model+response, title="1984")

modelsummary(roc_list_25,
             stars=T,
             fmt=2,
             shape=term~model+response, title="2025")

#Here I combine the 1984 full models that interact ownership with subjective social class
model4_list<-list("1984"=model4_84_roc, "2025"=model4_25_roc)
modelsummary(model4_list,shape=term~model+response, stars=T, fmt=2)
model4_list %>%
  map(., avg_comparisons, variables=c("own_rent"), by=c("sub_class2")) %>%
  list_rbind(., names_to=c('Election')) %>%
  #filter(group!="NDP") %>%
  ggplot(., aes(y=group, x=estimate, col=group, alpha=Election))+geom_pointrange(aes(xmin=conf.low, xmax=conf.high))+facet_grid(~fct_relevel(sub_class2, "Working Class"))+
  scale_color_manual(values=c("darkblue",  "darkred", "orange"))+geom_vline(xintercept=0, linetype=2)


#### Concordance
levels(ces$occupation_oesch_5)
model1_concordance_vote_84_roc<-multinom(vote2~concordance, data=subset(ces,election==1984&quebec!=1))
model1_concordance_vote_84_qc<-multinom(vote2~concordance, data=subset(ces,election==1984&quebec==1))
model1_concordance_vote_25_roc<-multinom(vote2~concordance, data=subset(ces,election==2025&quebec!=1))
model1_concordance_vote_25_qc<-multinom(vote2~concordance, data=subset(ces,election==2025&quebec==1))
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
model2_concordance_vote_25_roc<-update(model1_concordance_vote_25_roc, .~-concordance+own_rent)
#replace
model3_concordance_vote_25_roc<-update(model1_concordance_vote_25_roc, .~concordance+own_rent)
#add interaction term
model4_concordance_vote_25_roc<-update(model1_concordance_vote_25_roc, .~concordance*own_rent)
#summary(model4_concordance_vote_25_roc)
#Report Class concordance over time 1984 and 2025
concordance_vote_list<-list("1984"=model1_concordance_vote_84_roc, "2025"=model1_concordance_vote_25_roc)
modelsummary(concordance_vote_list, stars=T,
             fmt=2, shape=term~model+response)
#Report Class concordance by material class
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
