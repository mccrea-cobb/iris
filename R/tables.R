### PRIMR summary tables


# ---- table1
## Table of surveys by refuge

library(DT)

foo <- survey_data %>%
  select(stationName, type.name, id) %>%
  group_by(stationName, type.name) %>%
  summarise(count = length(id)) %>%
  spread(type.name, count)

datatable(foo)


survey_data %>%
  select(stationName, type.name, id) %>%
  group_by(stationName, type.name) %>%
  summarise(count = length(id)) %>%
  spread(type.name, count) %>%
  DT::datatable()
