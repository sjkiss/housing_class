# #checking missing values 2025
source("1_housing_class_master.R")

ces %>%
  filter(election==2025) %>%
  select(occupation_oesch_5,obj_class2, sub_class2) %>%
  summary()

lookfor(ces25, "occupation")
lookfor(ces25, "retired")
ces25b %>%
  filter(is.na(occupation_oesch)) %>%
  filter(cps25_employment==4) %>%
  select(pes25_occ_select, occupation_oesch, pes25_occ_select_2, NOC21_5)

# How many are retired
