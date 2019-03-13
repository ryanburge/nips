www <- cces16 %>% 
  mutate(male = car::recode(gender, "1=1; else =0")) %>% 
  # filter(religpew == 9 | religpew == 10 | religpew == 11) %>% 
  group_by(religpew) %>% 
  mean_ci(male, wt = commonweight_vv) %>% 
  ungroup(religpew) %>% 
  mutate(religpew = to_factor(religpew)) %>% 
  mutate(gender = "  Male  ")
  
www1 <- cces16 %>% 
  mutate(male = car::recode(gender, "2=1; else =0")) %>% 
  # filter(religpew == 9 | religpew == 10 | religpew == 11) %>% 
  group_by(religpew) %>% 
  mean_ci(male, wt = commonweight_vv) %>% 
  ungroup(religpew) %>% 
  mutate(religpew = to_factor(religpew)) %>% 
  mutate(gender = "  Female")

graph <- bind_df("www")

graph %>% 
  filter(religpew != "Skipped") %>% 
  filter(religpew != "Something else") %>% 
  ggplot(., aes(x = fct_rev(religpew), y = mean, fill = gender)) +
  theme_gg("Source Sans Pro") +
  geom_col(color = "black") +
  coord_flip()  +
  scale_fill_manual(values = c("#e4002b", "#b5b7b4", "#51626f")) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "bottom") +
  geom_text(aes(label = paste0(mean*100, '%')), position = position_stack(vjust = 0.5), size = 6, family = "font") +
  labs(x = "", y = "", title = "Gender Breakdown of Religious Groups", caption = "Data: CCES 2016") +
  guides(fill = guide_legend(reverse=T)) +
  ggsave("D://nips/gender.png")
