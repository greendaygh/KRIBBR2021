airmean <- airquality %>% 
  filter(complete.cases(.)) %>% 
  select(-Day) %>% 
  group_by(Month) %>% 
  summarise(across(everything(), mean)) %>% 
  pivot_longer(-Month, values_to = "mean")

airsd <- airquality %>% 
  filter(complete.cases(.)) %>% 
  select(-Day) %>% 
  group_by(Month) %>% 
  summarise(across(everything(), sd)) %>% 
  pivot_longer(-Month, values_to = "sd")


airdata <- left_join(airmean, airsd, by=c("Month", "name"))

q <- ggplot(airdata, aes(x=Month, y=mean, fill=name)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), position=position_dodge(width=0.9), width=0.4)