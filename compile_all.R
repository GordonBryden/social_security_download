#compile data


all_data <-uc_data2 %>%
  bind_rows(attendance_data2) %>%
  bind_rows(bereavement_data2) %>%
  bind_rows(carers_data2) %>%
  bind_rows(housing_data2) %>%
  bind_rows(pension_data2) %>%
  bind_rows(pip_data2) %>%
  bind_rows(state_pension_data2) %>%
  bind_rows(income_support_data2) %>%
  bind_rows(employment_support_data2) %>%
  bind_rows(child_benefit3 ) %>%
  bind_rows(job_seekers_data) %>%
  bind_rows(incapacity_data) %>%
  bind_rows(attendance_expenditure2) %>%
  bind_rows(bereavement_expenditure2) %>%
  bind_rows(carers_expenditure2) %>%
  bind_rows(housing_expenditure2) %>%
  bind_rows(pension_expenditure2) %>%
  bind_rows(state_pension_expenditure2) %>%
  bind_rows(income_support_expenditure2) %>%
  bind_rows(employment_support_expenditure2) %>%
  bind_rows(jsa_benefit) %>%
  bind_rows(uc_data2) %>%
  bind_rows(uc_expenditure2) %>%
  bind_rows(disability_data2) %>%
  bind_rows(disability_expenditure2) %>%
  bind_rows(incapacity_data2) %>%
  bind_rows(incapacity_expenditure2)

  #mutate(new_date = floor_date(date, "quarter"))

write_csv(all_data,"all_benefit_data.csv")

quarterised <- 
  pip_mobility2 %>%
  #all_data %>%
  #filter(!source == "universal_credit") %>%
  #filter(date >= "2000-01-01") %>%
  mutate(new_date = floor_date(date, "quarter")) %>%
  group_by(source, new_date) %>%
  mutate(last_date = last(date)) %>%
  filter(date == last_date) %>%
  ungroup() #%>%
  #group_by(new_date) #%>%
  #summarise(total_non_uc = sum(value)) %>%
  #filter(new_date <= "2018-01-01") %>%
  #ggplot(aes(x=new_date, y = total_non_uc, group = 1, colour = 1)) +
  #geom_line() +
  #theme_classic()   + expand_limits(y = 0)+

  #scale_y_continuous(expand = c(0, 0))

  write_csv(quarterised,"quarterised_benefit_data.csv")
  
all_data %>%
  ggplot(aes(x=new_date, y = value, group = source, colour = source)) +
  geom_line() +
  theme_classic() + scale_y_continuous(expand = c(0, 0))

for_export <- incapacity_data2 %>%
  left_join(incapacity_expenditure2)

 pip_for_export <- pip_living2 %>%
   left_join(pip_mobility2, by=c("date"))
