graph <- cces16 %>% 
  filter(religpew == 9 | religpew == 10 | religpew ==11) %>% 
  filter(pid7 <= 7) %>% 
  mutate(religpew = to_factor(religpew)) %>% 
  group_by(religpew) %>% 
  ct(pid7, wt = commonweight_vv) %>% 
  mutate(pid7 = frcode(pid7 == 1 ~ "Strong Democrat", 
                       pid7 == 2 ~ "Not very\nstrong Democrat",
                       pid7 == 3 ~ "Lean Democrat",
                       pid7 == 4 ~ "Independent", 
                       pid7 == 5 ~ "Lean Republican",
                       pid7 == 6 ~ "Not very\nstrong Republican",
                       pid7 == 7 ~ "Strong Republican"))
  
graph %>% 
  ggplot(., aes(x= pid7, y = pct, fill = religpew)) +
  geom_col(color = "black", position = "dodge") +
  theme_gg("Source Sans Pro") +
  scale_fill_manual(values = c("#e4002b", "#b5b7b4", "#51626f")) +
  scale_y_continuous(labels = percent) +
  theme(legend.position = c(.75, .75)) +
  labs(x = "", y = "", title = "Party Identification of the 'Nones'", caption = "Data: CCES 2016") +
  geom_text(aes(y = pct + .01, label = paste0(pct*100, '%')), position = position_dodge(width = .9), size = 2.5, family = "font") +
  ggsave("D://nips/pid_bars.png", width = 8)