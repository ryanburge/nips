## Age Beeswarms ####

library(ggbeeswarm)

cces16 %>% 
  mutate(religpew = frcode(religpew == 1 ~ "Protestant", 
                           religpew == 2 ~ "Roman\nCatholic",
                           religpew == 3 ~ "Mormon", 
                           religpew == 4 ~ "Orthodox", 
                           religpew == 5 ~ "Jewish", 
                           religpew == 6 ~ "Muslim",
                           religpew == 7 ~ "Buddhist", 
                           religpew == 8 ~ "Hindu",
                           religpew == 9 ~ "Atheist", 
                           religpew == 10 ~ "Agnostic",
                           religpew == 11 ~ "Nothing\nin Particular", 
                           TRUE ~ "REMOVE")) %>% 
  mutate(age = 2016 - birthyr) %>% 
  filter(religpew != "REMOVE") %>% 
  ggplot(., aes(x= religpew, y = age, color = religpew)) +
  geom_quasirandom() +
  theme_gg("Source Sans Pro") +
  scale_color_paletteer_d(dutchmasters, milkmaid) +
  annotate("text", x=1, y = 100, label = "51.7", size = 3, family = "font") +
  annotate("text", x=2, y = 100, label = "49.0", size = 3, family = "font") +
  annotate("text", x=3, y = 100, label = "44.7", size = 3, family = "font") +
  annotate("text", x=4, y = 100, label = "44.4", size = 3, family = "font") +
  annotate("text", x=5, y = 100, label = "52.9", size = 3, family = "font") +
  annotate("text", x=6, y = 100, label = "34.6", size = 3, family = "font") +
  annotate("text", x=7, y = 100, label = "43.1", size = 3, family = "font") +
  annotate("text", x=8, y = 100, label = "39.0", size = 3, family = "font") +
  annotate("text", x=9, y = 100, label = "42.7", size = 3, family = "font") +
  annotate("text", x=10, y = 100, label = "43.7", size = 3, family = "font") +
  annotate("text", x=11, y = 100, label = "43.8", size = 3, family = "font") +
  labs(x = "", y = "Age", title = "Age Distribution of Religious Groups") +
  ggsave("D://nips/age_beeswarm.png", width = 10)
  
  
## Racial Groups ####

graph <- cces16 %>% 
  mutate(religpew = frcode(religpew == 1 ~ "Protestant", 
                           religpew == 2 ~ "Roman\nCatholic",
                           religpew == 3 ~ "Mormon", 
                           religpew == 4 ~ "Orthodox", 
                           religpew == 5 ~ "Jewish", 
                           religpew == 6 ~ "Muslim",
                           religpew == 7 ~ "Buddhist", 
                           religpew == 8 ~ "Hindu",
                           religpew == 9 ~ "Atheist", 
                           religpew == 10 ~ "Agnostic",
                           religpew == 11 ~ "Nothing\nin Particular", 
                           TRUE ~ "REMOVE")) %>% 
  filter(religpew != "REMOVE") %>% 
  mutate(race = car::recode(race, "5:8 = 5")) %>% 
  group_by(religpew) %>% 
  ct(race, wt = commonweight_vv) %>% 
  mutate(race = frcode(race == 1 ~ " White ",
                       race == 2 ~ " Black ", 
                       race == 3 ~ " Hispanic ",
                       race == 4 ~ " Asian ", 
                       race == 5 ~ " All Other "))

graph %>% 
  ggplot(., aes(x = religpew, y = pct, fill = fct_rev(race))) +
  geom_col(color = "black") +
  coord_flip() +
  scale_fill_paletteer_d(awtools, a_palette) +
  theme_gg("Source Sans Pro") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(reverse=T)) +
  scale_y_continuous(labels = percent) +
  labs(x = "", y = "", title = "Racial Breakdown of Religious Traditions", caption = "Data: CCES 2016") + 
  ggsave("D://nips/race_stacked.png")
