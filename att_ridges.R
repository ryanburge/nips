library(ggridges)

cces16 %>% 
  mutate(att = car::recode(pew_churatd, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; else = NA")) %>% 
  mutate(religpew = to_factor(religpew)) %>% 
  filter(religpew != "Skipped") %>% 
  filter(religpew != "Something else") %>% 
  ggplot(., aes(x = att, y = religpew)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale =3, size = .03) +
  scale_fill_gradientn(colours = c("#8360c3", "gray", "#2ebf91")) +
  theme_gg("Source Sans Pro") +
  scale_x_continuous(limits = c(-.5,8), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  labs(x = "", y ="", title = "Distribution of Church Attendance", caption = "Data: CCES 2016", subtitle = "") +
  ggsave("D://nips/ridges.png", width = 8)
  