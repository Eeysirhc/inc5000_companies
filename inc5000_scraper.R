
library(tidyverse)
library(rvest)

########## Get URLs from CSV ##########

# Unable to find elegant RESCUE solution so for now will have manual exclusion
inc5000 <- read_csv("inc5000_fastest_growing_companies.csv") %>% 
  rename(urls = `Inc.com URL`,
         website = URLs) %>% 
  filter(urls != 'https://www.inc.com/profile/servicetitan', # DATA FORMATTING
         urls != 'https://www.inc.com/profile/bevara-building-services', # DATA FORMATTING
         urls != 'https://www.inc.com/profile/nationwide-transport-services', # NO EMPLOYEE DIV
         urls != 'https://www.inc.com/profile/knewsales-group', # NO EMPLOYEE DIV
         urls != 'https://www.inc.com/profile/proximity-learning', # NO EMPLOYEE DIV
         urls != 'https://www.inc.com/profile/brandywine-homes', # NO EMPLOYEE DIV
         urls != 'https://www.inc.com/profile/turbonomic', # NO EMPLOYEE DIV
         urls != 'https://www.inc.com/profile/real-people-realty', # NO EMPLOYEE DIV
         urls != 'https://www.inc.com/profile/bng-team', # NO EMPLOYEE DIV
         urls != 'https://www.inc.com/profile/the-roman-guy', # 404 NOT FOUND
         urls != 'https://www.inc.com/profile/us-aluminum-services', # 404 NOT FOUND
         urls != 'https://www.inc.com/profile/mobile-text-alerts', # 404 NOT FOUND
         urls != 'https://www.inc.com/profile/100%-chiropractic', # 404 NOT FOUND
         urls != 'https://www.inc.com/profile/aps-marketing-group', # 404 NOT FOUND
         urls != 'https://www.inc.com/profile/atlantic-petroleum', # 404 NOT FOUND
         urls != 'https://www.inc.com/profile/angeion-group', # 404 NOT FOUND
         urls != 'https://www.inc.com/profile/cloudticity', # 50X SERVER ERROR
         urls != 'https://www.inc.com/profile/access-tca', # 50X SERVER ERROR
         urls != 'https://www.inc.com/profile/fe-international') %>% # CURRENCY IN POUNDS
  mutate(id = row_number())

company_urls <- inc5000$urls

########## function to crawl and extract data ##########

# INITIALIZE DATA FRAME
company_raw <- data.frame()

for (page_url in company_urls){
  print(page_url)
  
  # RETRIEVE INC5000 PROFILE PAGE
  page <- read_html(page_url)
  
  # PARSE REVENUE
  revenue_millions <- page %>% 
    html_nodes(xpath = '//*/section[1]/div[3]/dl[1]/dd') %>% 
    html_text() %>% 
    str_replace(" Million", "") %>% 
    str_replace("\\$", "")
  
  # PARSE INDUSTRY
  industry <- page %>% 
    html_nodes(xpath = '//*/section[1]/div[3]/dl[3]/dd') %>% 
    html_text()
  
  # PARSE YEAR FOUND
  year_founded <- page %>% 
    html_nodes(xpath = '//*/section[1]/div[3]/dl[5]/dd') %>% 
    html_text()
  
  # PARSE EMPLOYEE COUNT
  employees <- page %>% 
    html_nodes(xpath = '//*/section[1]/div[3]/dl[6]/dd') %>% 
    html_text()
  
  # TEMP TO STORE LOOP DATA
  temp_df <- data.frame(page_url, revenue_millions, industry, year_founded, employees)
  
  # COMBINE TEMP WITH ALL DATA
  company_raw <- rbind(company_raw, temp_df) 
}

########## clean data ##########

company_data <- company_raw %>% 
  as_tibble() 

billions <- company_data %>% 
  filter(grepl(" Billion", revenue_millions)) %>% 
  mutate(revenue_millions = str_replace(revenue_millions, " Billion", ""),
         revenue_millions = as.numeric(as.character(revenue_millions)),
         revenue_millions = revenue_millions*1000)

company_data <- company_data %>% 
  filter(!grepl(" Billion", revenue_millions)) %>% 
  mutate(revenue_millions = as.numeric(as.character(revenue_millions)))

company_data <- rbind(company_data, billions)

company_parsed <- company_data %>% 
  group_by(industry) %>% 
  summarize(revenue_millions = sum(revenue_millions),
            count = n()) %>% 
  ungroup() %>% 
  filter(!is.na(revenue_millions)) %>% 
  mutate(pct_count = count / sum(count),
         pct_revenue = revenue_millions / sum(revenue_millions))

########## join final with original data ##########

company_data %>% 
  inner_join(inc5000, by = c("page_url" = "urls")) %>%
  write_csv("companies_data_final.csv")
