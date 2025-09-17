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
common_vars<-c("vote", "occupation_oesch_6", "election", "prov", "quebec")
ces25b %>%
  bind_rows(., ces84)->ces
ces %>%
  select(any_of(common_vars))->ces

#Recodes
ces84$vote
ces$vote<-factor(as_factor(ces$vote), levels=c("Conservative", "Liberal", "NDP", "Bloc", "Green", "Other"))
ces$vote2<-factor(ces$vote, levels=c("Conservative", "Liberal", "NDP", "Bloc"))
# Vote by occupation class by election

#Nest the two elections
ces %>%
  nest(-election) %>%
  #run the mulinomial model on each election and store the resulting models in models
  mutate(model=map(data, ~multinom(vote2~occupation_oesch_6, data=.)))->models

#Print 2025
modelsummary(models$model[[1]], shape=model+term~response,
             coef_omit=c("(Intercept)"), fmt=2, stars=T,
             output="model1_2025.html", title="2025")

#Print 1984
modelsummary(models$model[[2]], shape=model+term~response,
             coef_omit=c("(Intercept)"), fmt=2, stars=T,
             output="model1_1984.html", title="1984")
