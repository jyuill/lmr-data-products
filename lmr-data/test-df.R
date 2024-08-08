# test out different table aggregation / filtering options

data <- lmr_data 
# table for sales by year
data_yr <- data %>% group_by(cyr) %>% summarize(netsales = sum(netsales))
# filter yr
yr <- 2024
data_f <- data %>% filter(cyr == yr)
data_yr_f <- data_yr %>% filter(cyr == yr)
