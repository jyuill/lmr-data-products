# test out different table aggregation / filtering options

data <- lmr_data 
# table for sales by year
data_yr <- data %>% group_by(cyr) %>% summarize(netsales = sum(netsales))
# filter yr
yr <- 2024
data_f <- data %>% filter(cyr == yr)
data_yr_f <- data_yr %>% filter(cyr == yr)

q_test <- lmr_data %>% group_by(cyr, cqtr, end_qtr_dt) %>% 
  summarize(netsales = sum(netsales)) %>% ungroup() %>%
  mutate(qoq = (netsales - lag(netsales))/lag(netsales))

yr_test_cat <- lmr_data %>% group_by(cyr, cat_type) %>% 
  summarize(netsales = sum(netsales)) %>% ungroup() %>%
  mutate(yoy = (netsales - lag(netsales, n=4))/lag(netsales, n=4))
