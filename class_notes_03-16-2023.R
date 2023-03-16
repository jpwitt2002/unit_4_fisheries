# 2023-03-16
# JPW
# join some fishery data, how cool is that

library(tidyverse)
load("data/RAMLDB v4.495/R Data/DBdata[asmt][v4.495].RData")

glimpse(stock)
glimpse(timeseries_values_views)
glimpse(taxonomy)

fish = timeseries_values_views %>%
  left_join(stock) %>%
  left_join(taxonomy) %>%
  select(stockid, stocklong, year, TCbest, tsn, scientificname, commonname, 
         region, FisheryType, taxGroup)

glimpse(tsmetrics)
tsmetrics %>% filter(tsshort == "TCbest")


fish = fish %>%
  filter(stockid!= "ACADREDGOMGB")

ggplot() + 
  geom_line(aes(x = year, y = TCbest, color = stockid), data = fish) + 
  theme(legend.position = "none")

fish %>% filter(TCbest > 6000000)

fish %>%
  filter(scientificname == "Gadus morhua") %>%
  distinct(region)

codcan = fish %>% 
  filter(scientificname == "Gadus morhua", 
         region == "Canada East Coast", 
         !is.na(TCbest))
head(codcan)
ggplot(data=codcan) + 
  geom_line(aes(x=year, y=TCbest, color = stockid))

codcan_total = codcan %>%
  group_by(year) %>%
  summarize(total_catch = sum(TCbest))

head(codcan_total)

ggplot(data=codcan_total) + 
  geom_line(aes(x=year, y=total_catch))
# good picture of fishery collapse
# is catch less than 10% of historical max catch? if so, fishery has collapsed

# intermission for cum function lol
dat = c(1,3,6,2,3,9,-1)
dat_max = cummax(dat)
# gives max up to that point
dat_sum = cumsum(dat)
# gives sum up to that point
cbind(dat, dat_max, dat_sum)

# bach to your regularly scheduled programming
cod_collapse = codcan_total %>%
  mutate(historical_max_catch = cummax(total_catch), 
         collapse = total_catch <= 0.1*historical_max_catch)
head(cod_collapse)
tail(cod_collapse)

cod_collapse_year = cod_collapse %>%
  filter(collapse==T) %>%
  summarize(year=min(year)) %>%
  pull(year)
# pull pops year out of the tibble

ggplot() +
  geom_line(aes(x = year, y = total_catch, color=collapse), data=cod_collapse) +
  geom_vline(xintercept=cod_collapse_year)


# global stock collapse
collapse = fish %>%
  filter(!is.na(TCbest)) %>%
  group_by(stockid) %>%
  mutate(historical_max_catch = cummax(TCbest), 
         current_collapse = TCbest<= 0.1*historical_max_catch, 
         collapse_yet = cumsum(current_collapse)>0) %>%
  ungroup()
glimpse(collapse)  

collapse_yr = collapse %>%
  group_by(stockid) %>%
  filter(collapse_yet == T)  %>%
  summarize(first_collapse_year = min(year))
  
  
  


