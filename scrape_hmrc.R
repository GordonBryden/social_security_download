library(rvest)
library(readxl)
library(dplyr)
library(readr)
library(readODS)
library(lubridate)

#hmrc_url <- "https://www.gov.uk/government/statistics/child-benefit-small-area-statistics-august-2018"

hmrc_url2 <- "https://www.gov.uk/government/statistics/child-benefit-statistics-geographical-analysis-august-2018"

dwp_url <- "https://www.gov.uk/government/statistics/dwp-benefits-statistics-february-2019"

#hmrc_webpage <- read_html(hmrc_url)
hmrc_webpage2 <- read_html(hmrc_url2)
dwp_webpage <- read_html(dwp_url)

#links <-hmrc_webpage %>% html_nodes("a") %>% html_attr("href")
links2 <- hmrc_webpage2 %>% html_nodes("a") %>% html_attr("href")
dwp_links <- dwp_webpage %>% html_nodes("a") %>% html_attr("href")

#download_link <- tibble(links = links) %>% filter( grepl('Scotland', links)) %>% unique()
download_link2 <- tibble(links = links2) %>% filter( grepl('Main_.xlsx', links)) %>% unique()  
dwp_download_link <- tibble(links = dwp_links) %>% filter( grepl('jsa-average-weekly', links)) %>% unique()  
#my_data <- read_xlsx("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/782015/2018_-_Scotland__DZONE_.xlsx")
  
download.file(destfile = "child_benefit.xlsx", download_link2[[1]])
download.file(destfile = "jsa.ods", dwp_download_link[[1]])

child_benefit <- read_xlsx("child_benefit.xlsx", sheet = "Table 1", skip = 3)
child_benefit2 <- child_benefit %>% tail(-5) %>% select(`Time Series`, Scotland)
first_na <- min(which(is.na(child_benefit2$Scotland)))

child_benefit3 <- head(child_benefit2,first_na - 1) %>%
  rename(date = `Time Series`) %>%
  mutate(region = "Scotland",
         date = parse_date_time2(date, orders = "%m %Y"),
         source = "child_benefit",
         value = as.numeric(Scotland),
         type = "numbers") %>%
  select(-Scotland)

child_benefit3 %>%
  ggplot(aes(x=date,y=value,group=1)) +
  geom_line()

jsa_benefit_raw <- read_ods("jsa.ods", sheet = 3, skip = 7)


names(jsa_benefit_raw)[2] <- "date"

jsa_benefit<-jsa_benefit_raw %>% 
  select(date, Scotland) %>%
  tail(-2) %>%
  head(-7) %>%
  mutate(date = parse_date_time2(date, orders = "%m-%y"),
         source = "job_seekers_allowance") %>%
  mutate(new_date = floor_date(date, "quarter"),
         type = "expenditure",
         value = Scotland,
         region = "Scotland")%>%
  select(-Scotland,
         -date) %>%
  mutate(date = new_date)

#new_names<- c("region","date","value"


