## Deployment dates plot
## A DuVall ajduvall@uw.edu

## load libraries
library(here)
library(tidyverse)
library(lubridate)
library(ggplot2)

## load deployment data
deploy <- read.csv(here("data", "deployment_data.csv"), sep = ";") 

deploy2 <- deploy %>%
  filter(unit_type == "Reconyx" | unit_type == "SM4") %>%
  dplyr::select(site, unit_type, unit_id, date_deployed, date_retrieved) %>%
  group_by(site, unit_type, unit_id) %>% 
  pivot_longer(cols = c(date_deployed, date_retrieved), names_to = "type", values_to = "date") %>%
  mutate(date = as_date(date, format = "%m/%d/%Y"))

## lava1 cam
lava1 <- deploy2 %>%
  filter(site == "Lava1" & unit_id == "cam_01") 
lava1[1,5]
start_date <- ymd("2021-02-10")
lava1[nrow(lava1),5]
end_date <- ymd("2021-05-23")
n_days <- interval(start_date,end_date)/days(1)
int <- start_date + days(0:n_days)
site <- rep("Lava1", length(int))
cam <- rep("cam_01", length(int))
df1 <- bind_cols(site, int, cam)
colnames(df1) <- c("site", "date", "unit_id")

## lava 1 SM4
lava1 <- deploy2 %>%
  filter(site == "Lava1" & unit_id == "SM4") 
lava1[1,5]
start_date <- ymd("2021-02-10")
lava1[nrow(lava1),5]
end_date <- ymd("2021-05-23")
n_days <- interval(start_date,end_date)/days(1)
int <- start_date + days(0:n_days)
site <- rep("Lava1", length(int))
cam <- rep("Sm4", length(int))
df1 <- bind_cols(site, int, cam)
colnames(df1) <- c("site", "date", "unit_id")

## lava2
lava2 <- deploy2 %>%
  filter(site == "Lava2" & unit_id == "cam_02") 
lava2[1,5]
start_date <- ymd("2021-02-10")
lava2[nrow(lava2),5]
end_date <- ymd("2021-05-22")
n_days <- interval(start_date,end_date)/days(1)
int <- start_date + days(0:n_days)
site <- rep("Lava2", length(int))
cam <- rep("cam_02", length(int))
df2 <- bind_cols(site, int, cam)
colnames(df2) <- c("site", "date", "unit_id")

## moss first cam
moss1 <- deploy2 %>%
  filter(site == "Moss" & unit_id == "cam_03") 
moss1[1,5]
start_date <- ymd("2021-02-10")
moss1[nrow(moss1),5]
end_date <- ymd("2021-07-07")
n_days <- interval(start_date,end_date)/days(1)
int <- start_date + days(0:n_days)
site <- rep("Moss", length(int))
cam <- rep("cam_03", length(int))
df3 <- bind_cols(site, int, cam)
colnames(df3) <- c("site", "date", "unit_id")

## moss second cam
moss2 <- deploy2 %>%
  filter(site == "Moss" & unit_id == "cam_04") 
moss2[1,5]
start_date <- ymd("2021-02-10")
moss2[nrow(moss2),5]
end_date <- ymd("2021-07-07")
n_days <- interval(start_date,end_date)/days(1)
int <- start_date + days(0:n_days)
site <- rep("Moss", length(int))
cam <- rep("cam_04", length(int))
df4 <- bind_cols(site, int, cam)
colnames(df4) <- c("site", "date", "unit_id")

## pinn first cam
pinn1 <- deploy2 %>%
  filter(site == "Pinnacle" & unit_id == "cam_05") 
pinn1[1,5]
start_date <- ymd("2021-02-10")
pinn1[nrow(pinn1),5]
end_date <- ymd("2021-07-07")
n_days <- interval(start_date,end_date)/days(1)
int <- start_date + days(0:n_days)
site <- rep("Pinnacle", length(int))
cam <- rep("cam_05", length(int))
df5 <- bind_cols(site, int, cam)
colnames(df5) <- c("site", "date", "unit_id")

## pinn second cam
pinn2 <- deploy2 %>%
  filter(site == "Pinnacle" & unit_id == "cam_06") 
pinn2[1,5]
start_date <- ymd("2021-02-10")
pinn2[nrow(pinn2),5]
end_date <- ymd("2021-07-07")
n_days <- interval(start_date,end_date)/days(1)
int <- start_date + days(0:n_days)
site <- rep("Pinnacle", length(int))
cam <- rep("cam_06", length(int))
df6 <- bind_cols(site, int, cam)
colnames(df6) <- c("site", "date", "unit_id")

## refuge
refuge <- deploy2 %>%
  filter(site == "Refuge" & unit_id == "cam_07") 
refuge[1,5]
start_date <- ymd("2021-02-10")
refuge[nrow(refuge),5]
end_date <- ymd("2021-07-07")
n_days <- interval(start_date,end_date)/days(1)
int <- start_date + days(0:n_days)
site <- rep("Refuge", length(int))
cam <- rep("cam_07", length(int))
df7 <- bind_cols(site, int, cam)
colnames(df7) <- c("site", "date", "unit_id")

### final camera df
plotdf <- bind_rows(df1, df2, df3, df4, df5, df6, df7) %>%
  mutate(jul = as.numeric(format(date, "%j")))

### plotting
ggplot(plotdf) +
  geom_line(aes(x = jul, y = unit_id, color = site)) +
  xlab("Day of Year") + ylab("") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

ggplot(camdf) +
  geom_point(aes(x = jul2, y = as.factor(year)), size = 0.5) +
  geom_point(aes(x = jul2, y = as.factor(year), color = island, size = Total), alpha = 0.5) +
  theme_minimal() + xlab("Survey Date") + ylab("Year") +
  #scale_x_continuous(position = "top") +
  scale_x_date(labels = function(x) format(x, "%d-%b"), date_breaks = "1 month") +
  scale_size_binned(breaks = c(1, 10, 20, 30, 40, 50, 60),
                    labels = function(x) as.character(round(x,0))) +
  theme(axis.text.x = element_text(angle = 90, size = 10),
        legend.position = "bottom") +
  guides(size = guide_bins(axis = FALSE))

