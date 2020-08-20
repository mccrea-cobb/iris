

## ---- figure1

require(tidyverse)
survey_data %>%
  mutate(stationName = fct_reorder(stationName, id, .fun = 'length')) %>%  # Order refuges by number of surveys
  ggplot(aes(stationName, fill = status.name)) +
  geom_bar() +  #bar plot
  coord_flip() +  # flip the axises
  theme(axis.title.y = element_blank(),
        legend.position = c(0.85, 0.2)) +  # Remove to x axis label
  labs(y = "Number of current surveys",
       fill = "Survey status") # custom y axis label


## ---- table1

require(tidyverse)
survey_data %>%
  select(stationName, type.name, id) %>%
  group_by(stationName, type.name) %>%
  summarise(count = length(id)) %>%
  spread(type.name, count) %>%
  DT::datatable()