library(nomisr)
x <- nomis_data_info()

job_seekers_raw <- nomis_get_data("NM_1_1", geography = "2092957701",
                                  select = c("DATE",
                                             "GEOGRAPHY_NAME",
                                             "SEX_NAME",
                                             "ITEM_NAME",
                                             "MEASURES_NAME",
                                             "OBS_VALUE")
                                  )

job_seekers_data<-job_seekers_raw %>%
  select(DATE,
         GEOGRAPHY_NAME,
         SEX_NAME,
         ITEM_NAME,
         MEASURES_NAME,
         OBS_VALUE) %>%
  filter(MEASURES_NAME == "Persons claiming JSA",
         SEX_NAME == "Total",
         ITEM_NAME == "Total claimants") %>%
  mutate(date = parse_date_time2(DATE,orders="%Y-%m"),
         region = "Scotland",
         source = "job_seekers",
         type = "numbers") %>%
  rename(value = OBS_VALUE) %>%
  select(date,region,source,value, type)

job_seekers_data %>%
  ggplot(aes(x=date,y=value, group=1)) +
  geom_line()

incapacity_data<-nomis_get_data("NM_115_1",
                     #time=c("previous","latest"),
                     geography = "2092957701",
                     sex="7",
                     measures = 20100,
                     DURATION = 0,
                     CONDITION = 0,
                     AGE = 0,
                     ITEM = 2,
                      select = c("DATE",
                                 "GEOGRAPHY",
                                 "SEX_NAME",
                                 "CONDITION_NAME",
                                 "AGE_NAME",
                                 "DURATION_NAME",
                                 "ITEM_NAME",
                                 "MEASURES_NAME",
                                 "OBS_VALUE")
                     ) %>%
  mutate(date = parse_date_time2(DATE,orders="%Y-%m"),
         region = "Scotland",
         source = "incapacity",
         type = "numbers") %>%
  rename(value = OBS_VALUE) %>%
  select(date,region,source,value,type)

incapacity_data %>%
  ggplot(aes(x=date,y=value, group = 1)) +
  geom_line()

