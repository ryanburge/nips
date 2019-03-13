
reg <- cces16 %>% 
  mutate(app1 = car::recode(CC16_320a, "5=1; else =0")) %>% 
  mutate(app2 = car::recode(CC16_320b, "5=1; else =0")) %>% 
  mutate(app3 = car::recode(CC16_320c, "5=1; else =0")) %>% 
  mutate(app4 = car::recode(CC16_320d, "5=1; else =0")) %>% 
  mutate(app5 = car::recode(CC16_320e, "5=1; else =0")) %>% 
  mutate(id = car::recode(ideo5, "6=1; else =0")) %>% 
  mutate(pid = car::recode(pid7, "8=1; else =0")) %>% 
  mutate(notsure = app1 + app2 + app3 + app4 + app5 + id + pid) %>% 
  mutate(nones = car::recode(religpew, "9=1; 10=2; 11=3")) %>% 
  mutate(income = car::recode(faminc, "31:99 = NA")) %>% 
  mutate(male = car::recode(gender, "1=1; else =0")) %>% 
  mutate(white = car::recode(race, "1=1; else =0"))


gg <- lm(notsure ~ educ*nones + white + male + income, data = reg)

gg2 <- interact_plot(gg, pred = educ, modx = nones, int.width = .76, interval = TRUE, modx.labels = c("Atheists", "Agnostics", "Nothing in Particular"))

gg2 +
  labs(x = "Education", y = "Number of Not Sures", title = "Interaction of Education and Religious Tradition on 'Not Sure' Responses") +
  theme_gg("Source Sans Pro") +
  scale_color_manual(values = c("#e4002b", "#b5b7b4", "#51626f")) +
  scale_fill_manual(values = c("#e4002b", "#b5b7b4", "#51626f")) +
  theme(legend.position = c(.75,.45)) +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = c("No HS", "HS Grad", "Some College", "2 Yr College", "4 Yr College", "Post-Grad")) +
  theme(plot.title = element_text(size = 12)) +
  theme(plot.subtitle = element_text(size = 10)) +
  ggsave("D://nips/not_sure.png", width = 12, height = 7)