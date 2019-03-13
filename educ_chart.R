
library(haven)
library(labelled)

cces16 <- read_dta("D://cces/data/cces16.dta")
source("D://measuring_evangelicals/reltrad16.R")





graph <- cces16 %>% 
  filter(religpew <= 11) %>% 
  group_by(religpew) %>% 
  mean_ci(educ) %>% 
  mutate(relig = to_factor(religpew)) 

graph2 <- cces16 %>% 
  filter(evangelical ==1) %>% 
  mean_ci(educ) %>%
  mutate(relig = "Evangelicals")

graph3 <- cces16 %>% 
  filter(mainline ==1) %>% 
  mean_ci(educ) %>%
  mutate(relig = "Mainline")

ff <- bind_df("graph")

ff <- ff %>% 
  mutate(color = case_when(relig == "Atheist" | relig == "Agnostic" |  relig == "Nothing in particular" ~ 1, 
                           TRUE ~ 2))

ff %>% 
  filter(relig != "Protestant") %>% 
  ggplot(., aes(x= reorder(relig, mean), y = mean, fill = factor(color))) +
  geom_col(color = "black")+
  scale_fill_manual(values = c("firebrick3", "azure4")) +
  coord_flip() +
  scale_y_continuous(breaks = c(1,2,3,4,5,6), labels = c("Less\nThan HS", "HS Grad", "Some\nCollege", "2-year", "4-year", "Post-Grad")) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9))  +
  theme_gg("Source Sans Pro") +
  labs(y=  "Mean Level of Education", x = "", title = "Nothing in Particulars Have the Lowest Level of Education", caption = "Data: CCES 2016") +
  ggsave("D://nips/reltrad_educ.png", width = 9)