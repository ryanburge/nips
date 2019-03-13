library(jtools)


## Political Knowledge ####

cces16 <- cces16 %>% 
  mutate(right_fed_house = recode(CC16_321a, "1=1; 2:4= 0; else =NA")) %>% 
  mutate(right_fed_sen = recode(CC16_321b, "1=1; 2:4= 0; else =NA"))

cces16 <- cces16 %>% 
  mutate(state = to_factor(inputstate))


sen_rep <- cces16 %>% 
  filter(state == "Alabama" | state == "Alaska" | state == "Arizona" | state == "Arkansas" | state == "Colorado" | state == "Florida" | state == "Georgia" | state == "Idaho" | state == "Indiana" | state == "Kansas" | state == "Kentucky" | state == "Louisiana" | state == "Maine"| state == "Michigan" | state == "Mississippi" |  state == "Missouri" | state == "Montana" | state == "Nevada" |  state == "New Hampshire" | state == "New York" | state == "North Carolina" | state == "North Dakota" | state == "Ohio" | state == "Oklahoma" | state == "Pennsylvania" | state == "South Carolina" | state == "South Dakota" | state == "Tennessee" | state == "Texas" | state == "Utah" | state == "Virginia" | state == "Washington" | state == "West Virginia" | state == "Wisconsin" | state == "Wyoming")


sen_dem <- cces16 %>% 
  filter(state != "Nebraska" & state != "Alabama" & state != "Alaska" & state != "Arkansas" &state != "Arizona" & state != "Colorado" & state != "Florida" & state != "Georgia" & state != "Idaho" & state != "Indiana" & state != "Kansas" & state != "Kentucky" & state != "Louisiana" & state != "Maine"& state != "Michigan" & state != "Mississippi" &  state != "Missouri" & state != "Montana" & state != "Nevada" &  state != "New Hampshire" & state != "New York" & state != "North Carolina" & state != "North Dakota" & state != "Ohio" & state != "Oklahoma" & state != "Pennsylvania" & state != "South Carolina" & state != "South Dakota" & state != "Tennessee" & state != "Texas" & state != "Utah" & state != "Virginia" & state != "Washington" & state != "West Virginia" & state != "Wisconsin" & state != "Wyoming")


sen_rep <- sen_rep %>% 
  mutate(right_sen = recode(CC16_321c, "1=1; 2:4= 0; else = NA"))

sen_dem <- sen_dem %>% 
  mutate(right_sen = recode(CC16_321c, "1=0; 2=1; 3:4= 0; else = NA"))


house_rep <- cces16 %>% 
  filter(state == "Alabama" | state == "Alaska" | state == "Arizona" | state == "Arkansas" | state == "Florida" | state == "Georgia" | state == "Idaho" | state == "Indiana" | state == "Iowa" | state == "Kansas" | state == "Kentucky" | state == "Louisiana" | state == "Maine"| state == "Michigan" | state == "Minnesota" | state == "Mississippi" |  state == "Missouri" | state == "Montana" | state == "Nevada" |  state == "New Hampshire" |  state == "New Mexico" | state == "New York" | state == "North Carolina" | state == "North Dakota" | state == "Ohio" | state == "Oklahoma" | state == "Pennsylvania" | state == "South Carolina" | state == "South Dakota" | state == "Tennessee" | state == "Texas" | state == "Utah" | state == "Virginia" | state == "Washington" | state == "West Virginia" | state == "Wisconsin" | state == "Wyoming")

house_dem <- cces16 %>% 
  filter(state != "Nebraska" & state != "Alabama" & state != "Alaska" & state != "Arkansas" &state != "Arizona" &  state != "Florida" & state != "Georgia" & state != "Idaho" & state != "Indiana" & state != "Iowa" & state != "Kansas" & state != "Kentucky" & state != "Louisiana" & state != "Maine"& state != "Michigan" & state != "Minnesota" & state != "Mississippi" &  state != "Missouri" & state != "Montana" & state != "Nevada" &  state != "New Hampshire" &  state != "New Mexico" & state != "New York" & state != "North Carolina" & state != "North Dakota" & state != "Ohio" & state != "Oklahoma" & state != "Pennsylvania" & state != "South Carolina" & state != "South Dakota" & state != "Tennessee" & state != "Texas" & state != "Utah" & state != "Virginia" & state != "Washington" & state != "West Virginia" & state != "Wisconsin" & state != "Wyoming")


house_rep <- house_rep %>% 
  mutate(right_house = recode(CC16_321d, "1=1; 2:4= 0; else = NA"))

house_dem <- house_dem %>% 
  mutate(right_house = recode(CC16_321d, "1=0; 2=1; 3:4= 0; else = NA"))


gov_rep <- cces16 %>% 
  filter(state == "Alabama" | state == "Arizona" | state == "Arkansas" | state == "Florida" |state == "Georgia" | state == "Idaho" | state == "Illinois" |state == "Indiana" | state == "Iowa" | state == "Kansas" | state == "Kentucky" | state == "Louisiana" | state == "Maine"| state == "Maryland" | state == "Massachusetts" |state == "Michigan" |  state == "Mississippi" |  state == "Nevada" |  state == "Nebraska" | state == "New Mexico" |state == "New Jersey" | state == "North Carolina" | state == "North Dakota" | state == "Ohio" | state == "Oklahoma" | state == "Pennsylvania" | state == "South Carolina" | state == "South Dakota" | state == "Tennessee" | state == "Texas" | state == "Utah" | state == "Vermont" |  state == "Wisconsin" | state == "Wyoming")


gov_dem <- cces16 %>% 
  filter(state != "Alabama" & state != "Alaska" & state != "Arizona" & state != "Arkansas" & state != "Florida" &state != "Georgia" & state != "Idaho" & state != "Illinois" &state != "Indiana" & state != "Iowa" & state != "Kansas" & state != "Kentucky" & state != "Louisiana" & state != "Maine"& state != "Maryland" & state != "Massachusetts" &state != "Michigan" &  state != "Mississippi" &  state != "Nevada" &  state != "Nebraska" & state != "New Mexico" &state != "New Jersey" & state != "North Carolina" & state != "North Dakota" & state != "Ohio" & state != "Oklahoma" & state != "Pennsylvania" & state != "South Carolina" & state != "South Dakota" & state != "Tennessee" & state != "Texas" & state != "Utah" & state != "Vermont" &  state != "Wisconsin" & state != "Wyoming")


gov_rep <- gov_rep %>% 
  mutate(right_gov = recode(CC16_322a, "2=1; 1=0; 3:4= 0; else = NA"))

gov_dem <- gov_dem %>% 
  mutate(right_gov = recode(CC16_322a, "3=1; 1:2=0; 4:5= 0; else = NA"))


sen_rep <- sen_rep %>% select(V101, right_sen)
sen_dem <- sen_dem %>% select(V101, right_sen)

sen <- bind_rows(sen_rep, sen_dem)

house_rep <- house_rep %>% select(V101, right_house)
house_dem <- house_dem %>% select(V101, right_house)

house <- bind_rows(house_rep, house_dem)

gov_rep <- gov_rep %>% select(V101, right_gov)
gov_dem <- gov_dem %>% select(V101, right_gov)

gov <- bind_rows(gov_rep, gov_dem)


join <- left_join(sen, house) %>% 
  left_join(gov)

join <- join %>% mutate(V101 = as.numeric(V101))
cces16 <- cces16 %>% mutate(V101 = as.numeric(V101))


cces16 <- left_join(cces16, join)

cces16 <- cces16 %>% 
  mutate(know = right_fed_house + right_fed_sen + right_house + right_sen + right_gov)


graph <- cces16 %>% 
  filter(religpew <= 11) %>% 
  group_by(religpew) %>% 
  mean_ci(know) %>% 
  mutate(relig = to_factor(religpew)) 

reg <- cces16 %>% 
  mutate(nones = car::recode(religpew, "9=1; 10=2; 11=3")) %>% 
  mutate(income = car::recode(faminc, "31:99 = NA")) %>% 
  mutate(male = car::recode(gender, "1=1; else =0")) %>% 
  mutate(white = car::recode(race, "1=1; else =0"))

gg <- lm(know ~ educ*nones*white + male + income, data = reg)

gg2 <- interact_plot(gg, pred = educ, modx = nones, mod2 = white, int.width = .76, interval = TRUE, modx.labels = c("Atheists", "Agnostics", "Nothing in Particular"), mod2.labels = c("Non-White", "White"))

gg2 +
  labs(x = "Education", y = "Political Knowledge", title = "Interaction of Education and Religious Tradition on Political Knowledge") +
  theme_gg("Source Sans Pro") +
  scale_color_manual(values = c("#e4002b", "#b5b7b4", "#51626f")) +
  scale_fill_manual(values = c("#e4002b", "#b5b7b4", "#51626f")) +
  theme(legend.position = c(.75,.15)) +
  scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = c("No HS", "HS Grad", "Some College", "2 Yr College", "4 Yr College", "Post-Grad")) +
  theme(plot.title = element_text(size = 12)) +
  theme(plot.subtitle = element_text(size = 10)) +
  ggsave("D://nips/nones_interact_know.png", width = 12, height = 7)



cces16 <- cces16 %>% 
  mutate(nones = car::recode(religpew, "9=1; 10=2; 11=3; else = NA")) %>% 
  mutate(college = car::recode(educ, "1:4=0; 5:6=1; else = NA")) %>% 
  mutate(meet = recode(CC16_417a_1, "1=1; else =0")) %>% 
  mutate(sign = recode(CC16_417a_2, "1=1; else =0")) %>% 
  mutate(vol = recode(CC16_417a_3, "1=1; else =0")) %>% 
  mutate(money = recode(CC16_417a_4, "1=1; else =0")) %>% 
  mutate(blood = recode(CC16_417a_5, "1=1; else =0")) %>% 
  mutate(activity = meet + sign + vol + money + blood)

gg <- lm(activity ~ know*college*nones, data = cces16)
gg2 <- interact_plot(gg, pred= know, modx = college, mod2 = nones, int.width = .76, interval = TRUE, mod2.values = c(1,2,3), modx.labels = c("No College Degree", "College Degree"), mod2.labels = c("Atheist", "Agnositc", "Nothing in Particular")) 

## Predicting Political Activity ####

gg2 +
  labs(x = "Political Knowledge", y = "Community Activity", title = "Interaction of Education and Political Knowledge on Community/Political Activity", subtitle = "Among 'Nones'") +
  theme_gg("Overpass") +
  scale_color_manual(values = c("#e4002b", "#b5b7b4", "#51626f")) +
  scale_fill_manual(values = c("#e4002b", "#b5b7b4", "#51626f")) +
  theme(legend.position = c(.75,.65)) +
  # scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  theme(plot.title = element_text(size = 12)) +
  theme(plot.subtitle = element_text(size = 10)) +
  ggsave("D://nips/jtools_nones_polact.png", width = 12, height = 7)


## Predicting Blood Donations ####



gg <- glm(blood ~ know*college*nones, family = "binomial", data = cces16)
gg2 <- interact_plot(gg, pred= know, modx = college, mod2 = nones, int.width = .76, interval = TRUE, mod2.values = c(1,2,3), modx.labels = c("No College Degree", "College Degree"), mod2.labels = c("Atheist", "Agnositc", "Nothing in Particular")) 


gg2 +
  labs(x = "Political Knowledge", y = "Donating Blood", title = "Interaction of Education and Political Knowledge on Giving Blood", subtitle = "Among 'Nones'") +
  theme_gg("Overpass") +
  scale_color_manual(values = c("#e4002b", "#b5b7b4", "#51626f")) +
  scale_fill_manual(values = c("#e4002b", "#b5b7b4", "#51626f")) +
  theme(legend.position = c(.75,.75)) +
  # scale_x_continuous(limits = c(1,6), breaks = c(1,2,3,4,5,6), labels = c("Never", "Seldom", "Yearly", "Monthly", "Weekly", "Weekly+")) +
  theme(plot.title = element_text(size = 12)) +
  theme(plot.subtitle = element_text(size = 10)) +
  ggsave("D://nips/jtools_nones_blood.png", width = 12, height = 7)



