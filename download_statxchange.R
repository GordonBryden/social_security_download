library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(statxplorer)
library(stringr)

key<-"65794a30655841694f694a4b563151694c434a68624763694f694a49557a49314e694a392e65794a7063334d694f694a7a644849756333526c6247786863694973496e4e3159694936496d6476636d527662693569636e6c6b5a5735415a3239324c6e4e6a623351694c434a70595851694f6a45314e544d324d5449774d445973496d46315a434936496e4e30636935765a47456966512e32516d732d4b6f302d6655565a387a6d344a5246352d623647797a534c6230316e6235634a43456f706b73"

statxplorer::set_api_key(key)

download_from_json <- function(file_name) {

new_names<- c("region","date","value")
  
query <- read_file(file_name)
results <- fetch_table(query)
data<-results$dfs[[1]]
names(data) <- new_names

return(data)

}



uc_data <- download_from_json("./json_queries/uc.json")
uc_expenditure <- download_from_json("./json_queries/uc_mean_income.json")

attendance_data <- download_from_json("./json_queries/attendance_allowance.json")
attendance_data_new <- download_from_json("./json_queries/attendance_allowance_new.json")
attendance_expenditure <- download_from_json("./json_queries/attendance_expenditure.json")
attendance_expenditure_new <- download_from_json("./json_queries/attendance_expenditure_new.json")

bereavement_data <- download_from_json("./json_queries/bereavement.json")
bereavement_data_new <- download_from_json("./json_queries/bereavement_new.json")
bereavement_expenditure <- download_from_json("./json_queries/bereavement_expenditure.json")
bereavement_expenditure_new <- download_from_json("./json_queries/bereavement_expenditure_new.json")

carers_data <- download_from_json("./json_queries/carers_allowance.json")
carers_data_new <- download_from_json("./json_queries/carers_allowance_new.json")
carers_expenditure <- download_from_json("./json_queries/carers_expenditure.json")
carers_expenditure_new <- download_from_json("./json_queries/carers_expenditure.json")

disability_data <- download_from_json("./json_queries/disability_living_allowance.json")
disability_data_new <- download_from_json("./json_queries/disability_living_allowance_new.json")
disability_expenditure <- download_from_json("./json_queries/disability_living_allowance_expenditure.json")
disability_expenditure_new <- download_from_json("./json_queries/disability_living_allowance_expenditure_new.json")

housing_data <- download_from_json("./json_queries/housing_benefit.json")
housing_data_new <- download_from_json("./json_queries/housing_benefit_new.json")
housing_expenditure <- download_from_json("./json_queries/housing_expenditure.json")
housing_expenditure_new <- download_from_json("./json_queries/housing_expenditure_new.json")


pension_data <- download_from_json("./json_queries/pension_credit.json")
pension_data_new <- download_from_json("./json_queries/pension_credit_new.json")
pension_expenditure <- download_from_json("./json_queries/pension_credit_expenditure.json")
pension_expenditure_new <- download_from_json("./json_queries/pension_credit_expenditure_new.json")

#pip_data<- download_from_json("./json_queries/pip_payments.json")
query <- read_file("./json_queries/pip_payments.json")
results <- fetch_table(query)
pip_data_daily_living<-results$dfs[[1]]
query <- read_file("./json_queries/pip_payments_mobility.json")
results <- fetch_table(query)
pip_data_mobility<-results$dfs[[1]]
#pip_expenditure<- download_from_json("./json_queries/pip_payments_expenditure.json")

state_pension_data <- download_from_json("./json_queries/state_pension.json")
state_pension_data_new <- download_from_json("./json_queries/state_pension_new.json")
state_pension_expenditure<- download_from_json("./json_queries/average_state_pension.json")
state_pension_expenditure_new<- download_from_json("./json_queries/average_state_pension_new.json")

income_support_data <- download_from_json("./json_queries/income_support.json")
income_support_expenditure <- download_from_json("./json_queries/income_support_expenditure.json")

incapacity_allowance_data <- download_from_json("./json_queries/incapacity_allowance.json")
incapacity_allowance_expenditure <- download_from_json("./json_queries/incapacity_allowance_expenditure.json")


employment_support_data <- download_from_json("./json_queries/employment_and_support.json")
employment_support_data_new <- download_from_json("./json_queries/employment_and_support_new.json")
employment_support_expenditure <- download_from_json("./json_queries/employment_and_support_expenditure.json")
employment_support_expenditure_new <- download_from_json("./json_queries/employment_and_support_expenditure_new.json")


uc_data2 <- uc_data %>%
  mutate(date = lubridate::parse_date_time2(date, orders = c("%m %Y")),
         source = "universal_credit",
         type = "numbers")
uc_expenditure2 <- uc_expenditure %>%
  mutate(date = lubridate::parse_date_time2(date, orders = c("%m %Y")),
         source = "universal_credit",
         type = "expenditure")


attendance_data2 <- attendance_data %>%
  rbind(attendance_data_new) %>%
  mutate(date = parse_date_time2(date,orders= "%m-%y"),
         source = "attendance_allowance",
         type = "numbers")
attendance_expenditure2 <- attendance_expenditure %>%
  rbind(attendance_data_new) %>%
  mutate(date = parse_date_time2(date,orders= "%m-%y"),
         source = "attendance_allowance",
         type = "expenditure")

bereavement_data2 <- bereavement_data %>%
  rbind(bereavement_data_new) %>%
  mutate(date = parse_date_time2(date,orders = "%m-%y"),
         source = "bereavement_income",
         type = "numbers")
bereavement_expenditure2 <- bereavement_expenditure %>%
  rbind(bereavement_expenditure_new) %>%
  mutate(date = parse_date_time2(date,orders = "%m-%y"),
         source = "bereavement_income",
         type = "expenditure")

carers_data2 <- carers_data %>%
  rbind(carers_data_new) %>%
  mutate(date = parse_date_time2(date,orders = "%m-%y"),
         source = "carers_allowance",
         type = "numbers")
carers_expenditure2 <- carers_expenditure %>%
  rbind(carers_expenditure_new) %>%
  mutate(date = parse_date_time2(date,orders = "%m-%y"),
         source = "carers_allowance",
         type = "expenditure")

disability_data2 <- disability_data %>%
  rbind(disability_data_new) %>%
  mutate(date = parse_date_time2(date,orders = "%m-%y"),
         source = "disability_living_allowance",
         type = "numbers")

disability_expenditure2 <- disability_expenditure %>%
  rbind(disability_expenditure_new) %>%
  mutate(date = parse_date_time2(date,orders = "%m-%y"),
         source = "disability_living_allowance",
         type = "expenditure")

housing_data2 <- housing_data %>%
  rbind(housing_data_new) %>%
  mutate(date = parse_date_time2(substr(date,1,6),orders="%Y%m"),
         source = "housing_benefit",
         type = "numbers")
housing_expenditure2 <- housing_expenditure %>%
  rbind(housing_expenditure_new) %>%
  mutate(date = parse_date_time2(substr(date,1,6),orders="%Y%m"),
         source = "housing_benefit",
         type = "expenditure")

pension_data2 <- pension_data %>%
  rbind(pension_data_new) %>%
  mutate(date = parse_date_time2(date, orders = "%m-%y"),
         source = "pension_allowance",
         type = "numbers")
pension_expenditure2 <- pension_expenditure %>%
  rbind(pension_expenditure_new) %>%
  mutate(date = parse_date_time2(date, orders = "%m-%y"),
         source = "pension_allowance",
         type = "expenditure")

pip_living2 <- pip_data_daily_living %>%
  #filter(region == "Total") %>%
  rename(region = `National - Regional - LA - OAs`,
         date = Month) %>%
  mutate(region = "Scotland",
         date = parse_date_time2(substr(date,1,6),orders = "%Y%m"),
         source = "personal_independance_payment",
         type = "numbers")

pip_mobility2 <- pip_data_mobility %>%
  #filter(region == "Total") %>%
  rename(region = `National - Regional - LA - OAs`,
         date = Month) %>%
  mutate(region = "Scotland",
         date = parse_date_time2(substr(date,1,6),orders = "%Y%m"),
         source = "personal_independance_payment",
         type = "numbers")

state_pension_data2 <- state_pension_data %>%
  rbind(state_pension_data_new) %>%
  mutate(date = parse_date_time2(date, orders = "%m-%y"),
         source=  "state_pension",
         type = "numbers")
state_pension_expenditure2 <- state_pension_expenditure %>%
  rbind(state_pension_expenditure_new) %>%
  mutate(date = parse_date_time2(date, orders = "%m-%y"),
         source=  "state_pension",
         type = "expenditure")

income_support_data2 <- income_support_data %>%
  mutate(date = parse_date_time2(date, orders = "%m-%y"),
         source = "income_support",
         type = "numbers")
income_support_expenditure2 <- income_support_expenditure %>%
  mutate(date = parse_date_time2(date, orders = "%m-%y"),
         source = "income_support",
         type = "expenditure")

incapacity_data2 <- incapacity_allowance_data %>%
  mutate(date = parse_date_time2(date, orders = "%m-%y"),
         source = "incapacity_allowance",
         type = "numbers")
incapacity_expenditure2 <- incapacity_allowance_expenditure %>%
  mutate(date = parse_date_time2(date, orders = "%m-%y"),
         source = "incapacity_allowance",
         type = "expenditure")

employment_support_data2 <- employment_support_data %>%
  rbind(employment_support_data_new) %>%
  mutate(date = parse_date_time2(date, orders = "%m-%y"),
         source = "employment_support",
         type = "numbers")
employment_support_expenditure2 <- employment_support_expenditure %>%
  rbind(employment_support_expenditure_new) %>%
  mutate(date = parse_date_time2(date, orders = "%m-%y"),
         source = "employment_support",
         type = "expenditure")


# xchange_data <- uc_data2 %>%
#   bind_rows(attendance_data2) %>%
#   bind_rows(bereavement_data2) %>%
#   bind_rows(carers_data2) %>%
#   bind_rows(housing_data2) %>%
#   bind_rows(pension_data2) %>%
#   bind_rows(pip_data2) %>%
#   bind_rows(state_pension_data2) %>%
#   bind_rows(income_support_data2) %>%
#   bind_rows(employment_support_data2)
# 
# 
# xchange_data %>%
#   ggplot(aes(x=date, y = value, group = source, colour = source)) +
#   geom_line()
