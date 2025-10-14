source("1_housing_class_master.R")

#### Model Vote by occupation class by election
#Nest the two elections
model1<-multinom(vote2~degree+income_tertile+own_rent*election,
                 data=subset(ces,quebec!=1))
ces$degree
modelsummary(model1, shape=model+term~response+,
             coef_omit=c("(Intercept)"), fmt=2, stars=T)
summary(model1)
ces$election
ces %>%
  nest(-election) %>%
  #run the mulinomial model on each election and store the resulting models in models
  mutate(model=map(data,
                   ~multinom(vote2~occupation_oesch_6, data=.)),
         model2=map(data, ~multinom))->models

#Print 2025
modelsummary(models$model[[1]], shape=model+term~response,
             coef_omit=c("(Intercept)"), fmt=2, stars=T,
             output="model1_2025.html", title="2025")

#Print 1984
modelsummary(models$model[[2]], shape=model+term~response,
             coef_omit=c("(Intercept)"), fmt=2, stars=T,
             output="model1_1984.html", title="1984")
