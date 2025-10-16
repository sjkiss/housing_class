source("1_housing_class_master.R")

#### Model Vote by occupation class by election
# Here I'm just creating a second Oesch variable
# collapsing the two working classes into one
# It makes things more comparable with our previous schema

ces %>%
  mutate(occupation_oesch_6_2=case_when(
    occupation_oesch_6=="Unskilled Workers"~"Working Class",
    occupation_oesch_6=="Skilled Workers"~"Working Class",
TRUE~occupation_oesch_6
  ))->ces
table(ces$occupation_oesch_6_2)
ces$occupation_oesch_6_2<-factor(ces$occupation_oesch_6_2, levels=c("Working Class", "Semi-Professionals Associate Managers", "Self-employed", "Professionals", "Managers"))
#Note, using that variable does give a hint of working class voting for the NDP, consistent with our previous work
# Good to keep in our back pocket.
# But here I'm using the stricter Oesch variable
model1_84_roc<-multinom(vote2~age+I(age^2)+as_factor(degree)+income_tertile+as_factor(degree)+own_rent,
                 data=subset(ces,quebec!=1&election==1984))
model1_84_qc<-multinom(vote2~age+I(age^2)+as_factor(degree)+income_tertile+as_factor(degree)+own_rent,
                 data=subset(ces,quebec==1&election==1984))
model2_84_roc<-update(model1_84_roc, .~.-own_rent+sub_class2)
model2_84_qc<-update(model1_84_qc, .~-.own_rent+sub_class2)
model3_84_roc<-update(model1_84_roc, .~.-own_rent+occupation_oesch_6_2)
model3_84_qc<-update(model1_84_qc, .~.-own_rent+occupation_oesch_6_2)
model4_84_qc<-update(model1_84_qc, .~.+own_rent*sub_class2+occupation_oesch_6_2)
model4_84_roc<-update(model1_84_roc, .~.+own_rent*sub_class2+occupation_oesch_6_2)

summary(model2_84_roc)
model1_25_roc<-multinom(vote2~age+I(age^2)+as_factor(degree)+income_tertile+as_factor(degree)+own_rent,
                        data=subset(ces,quebec!=1&election==2025))
model1_25_qc<-multinom(vote2~age+I(age^2)+as_factor(degree)+income_tertile+as_factor(degree)+own_rent,
                       data=subset(ces,quebec==1&election==2025))
model2_25_roc<-update(model1_25_roc, .~.-own_rent+sub_class2)
model2_25_qc<-update(model1_25_qc, .~.-own_rent+sub_class2)
model3_25_roc<-update(model1_25_roc, .~.-own_rent+occupation_oesch_6_2)
model3_25_qc<-update(model1_25_qc, .~.-own_rent+occupation_oesch_6_2)
model4_25_qc<-update(model1_25_qc, .~.+own_rent*sub_class2+occupation_oesch_6_2)
model4_25_roc<-update(model1_25_roc, .~.+own_rent*sub_class2+occupation_oesch_6_2)
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

model4_list %>%
  map(., avg_comparisons, variables=c("own_rent"), by=c("sub_class2")) %>%
  list_rbind(., names_to=c('Election')) %>%
  #filter(group!="NDP") %>%
  ggplot(., aes(y=group, x=estimate, col=group, alpha=Election))+geom_pointrange(aes(xmin=conf.low, xmax=conf.high))+facet_grid(~fct_relevel(sub_class2, "Working Class"))+
  scale_color_manual(values=c("darkblue",  "darkred", "orange"))+geom_vline(xintercept=0, linetype=2)
