
## Writing a fucntion to calculate means and CIs for each group in each year ####
count_fun <- function(df, var, weight, year){
  
  var <- enquo(var)
  weight <- enquo(weight)
  
  df1 <- df %>% 
    mutate(relig = car::recode(!! var, "9=1; else =0")) %>% 
    mean_ci(relig, wt = !! weight) %>% 
    mutate(group = "  Atheists") %>% 
    mutate(year = year)
  
  df2 <- df %>% 
    mutate(relig = car::recode(!! var, "10=1; else =0")) %>% 
    mean_ci(relig, wt = !! weight) %>% 
    mutate(group = "  Agnostics") %>% 
    mutate(year = year)
  
  df3 <- df %>% 
    mutate(relig = car::recode(!! var, "11=1; else =0")) %>% 
    mean_ci(relig, wt = !! weight) %>% 
    mutate(group = "  Nothing in Particular") %>% 
    mutate(year = year) 
  
  df4 <- df %>% 
    mean_ci(evangelical, wt = !! weight) %>% 
    mutate(group = "  Evangelicals") %>% 
    mutate(year = year)
  
  df5 <- df %>% 
    mean_ci(catholic, wt = !! weight) %>% 
    mutate(group = "  Catholics") %>% 
    mutate(year = year)
  
  bind_rows(df1, df2, df3, df4, df5)
}
### Calcuating means with the function ####

fff1 <- cces08 %>% count_fun(V219, V201, "2008")
fff2 <- cces10 %>% count_fun(religpew, V101, "2010")
fff3 <- cces12 %>% count_fun(religpew, V103, "2012")
fff4 <- cces14 %>% count_fun(religpew, weight, "2014")
fff5 <- cces16 %>% count_fun(religpew, commonweight_vv, "2016")
fff6 <- cces17 %>% count_fun(religpew, weights_common, "2017")

graph <- bind_df("fff") %>% 
  as.tibble()

## For some reason 2010 has zero atheists, I just deleted that row ####
graph <- graph[-c(6), ]

## Now to graph ####
graph %>% 
  ggplot(., aes(x = year, y = mean, color = group, group = group)) +
  geom_point(show.legend = FALSE) + 
  geom_ribbon(aes(ymin=lower, ymax=upper, color = group, fill = group), alpha = .65, show.legend = TRUE) +
  scale_y_continuous(limits = c(0, .275), labels = percent) +
  scale_color_paletteer_d(awtools, mpalette) +
  scale_fill_paletteer_d(awtools, mpalette) +
  theme_gg("Source Sans Pro") +
  theme(legend.position = c(.75, .45)) +
  labs(x= "", y = "Share of the Population (Weighted)", title = "Religious Groups Share of the Population", caption = "Data: CCES 2008-2017") +
  ggsave("D://nips/long_fig.png")