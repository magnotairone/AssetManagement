library(tidyverse)
library(lubridate)

# processar performance diaria -------------------------------------------------

tibble(file = list.files("data/hist/"),
       file_path = list.files("data/hist/", full.names = TRUE)) %>% 
  filter(str_detect(file, "csv")) %>% 
  mutate(data = map(file_path, read.csv)) %>% 
  unnest(data) %>% 
  janitor::clean_names() %>% 
  mutate(
    asset = str_trim(str_extract(file, "^[^-]+")),  # extrai a parte inicial até o primeiro "-"
    empresa = str_extract(file, "(?<=- ).+?(?= - Histórico)"),  # extrai o nome da empresa entre os traços
    .after = file
  ) %>% 
  mutate(across(c('abertura', 'fechamento', 'variacao', 'minimo', 'maximo'), 
                ~str_replace(.x, pattern = ",", replacement = ".")),
         across(c('abertura', 'fechamento', 'variacao', 'minimo', 'maximo'), 
                as.numeric),
         data = dmy(data)) %>% 
  select(asset, empresa, data, abertura, fechamento) %>% 
  write_csv("data/daily.csv")

# VALE3
# PETR4


# noticias ----------------------------------------------------------------
read_csv2("data/news_raw.csv", locale = locale(encoding = "latin1")) %>%
  janitor::clean_names() %>% 
  mutate(data = dmy(data)) %>% 
  write_csv("data/news.csv")

