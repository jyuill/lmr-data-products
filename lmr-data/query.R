
library(tidyverse) 
library(RMariaDB) ## best way to access MySQL from R
library(dotenv)

# create a .env file in the root directory of the project
# and add the following lines
# ENDPT="...rds.amazonaws.com"
# APWD="A...KOCX"
# APORT=3..6  

## Load the .env file
#readRenviron("../.env")
#dotenv::load_dot_env("../.env")


## Load the environment variables
endpt <- Sys.getenv("ENDPT")
apwd <- Sys.getenv("APWD")
aport <- as.numeric(Sys.getenv("APORT"))

con_aws <- dbConnect(RMariaDB::MariaDB(),
                     host=endpt,
                     user='admin',
                     password=apwd,
                     port=aport)
# main query - all the data
lmr_data <- dbGetQuery(con_aws, "SELECT * FROM bcbg.tblLDB_lmr lmr
                           LEFT JOIN bcbg.tblLDB_quarter qtr ON lmr.fy_qtr=qtr.fy_qtr;")
# convert from integer64 to numeric
lmr_data$netsales <- as.numeric(lmr_data$netsales)
lmr_data$litres <- as.numeric(lmr_data$litres)
dbDisconnect(con_aws)