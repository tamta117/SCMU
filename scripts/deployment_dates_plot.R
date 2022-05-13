## Deployment dates plot
## A DuVall ajduvall@uw.edu

## load libraries
library(here)
library(tidyverse)
library(lubridate)
library(ggplot2)

## load deployment data
deploy <- read.csv(here("data", "deployment_data.csv"), sep = ";")

deploy_plot <- deploy %>%
  filter(unit_type == "Reconyx" | unit_type == "SM4") %>%
  dplyr::select(site, unit_type, unit_id, date_deployed, date_retrieved) %>%
  group_by(site, unit_type, unit_id) %>%
  pivot_wider()
#calculate how many days in this time interval
>n_days <- interval(start_date,end_date)/days(1)
>start_date + days(0:n_days)
[1]"2011-12-30" "2011-12-31" "2012-01-01" "2012-01-02" "2012-01-03" "2012-01-04"
