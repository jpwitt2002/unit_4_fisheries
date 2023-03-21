# 2023-03-21
# JPW

source("build_collapse_table.R")
head(fish)
head(collapse)

# logistic and poisson regressions 
# can we please be done with linear models please im begging

# logistic regressions
# predicted Y is between 0 and 1 - used for probabilities or any binary variables 
# doesnt look linear but it does if you rearrange it 
# "doesnt have to be stressful", does in fact look stressful

# what can give us the ability to predict if a stock will collapse?
model_data = collapse %>%
  group_by(stockid) %>%
  summarize(ever_collapsed = any(current_collapse)) %>%
  ungroup() %>%
  left_join(stock) %>%
  left_join(metadata) %>%
  mutate(FisheryType = as.factor(FisheryType)) %>%
  filter(!is.na(FisheryType))

summary(model_data)


# build logistic regression
model_l = glm(ever_collapsed ~ FisheryType, data=model_data, family="binomial")
summary(model_l)

model_data %>% distinct(FisheryType) %>% arrange(FisheryType)


FisheryType = model_data %>% 
  distinct(FisheryType)

model_l_predict = predict(model_l, newdata=FisheryType, se.fit=T, type="response")
head(model_l_predict)
collapse_fishery_type_predictions = cbind(FisheryType, model_l_predict)

ggplot(data=collapse_fishery_type_predictions, 
       aes(x=FisheryType, y=fit, fill=FisheryType)) + 
  geom_bar(stat="identity", show.legend=F) + 
  geom_errorbar(aes(ymin=fit-se.fit, ymax=fit+se.fit), width=0.2) +
  coord_flip()


# poisson model
# "junk, junk, junk, junk, junk" - erin

u_summary = timeseries_values_views %>%
  filter(!is.na(UdivUmsypref), 
         !is.na(BdivBmsypref)) %>%
  group_by(stockid) %>%
  summarize(yrs_data=n(), 
            ratio_yrs_overfished = sum(UdivUmsypref > 1)/yrs_data, 
            ratio_yrs_low_stock = sum(BdivBmsypref < 1)/yrs_data) %>%
  select(-yrs_data)

collapse_summary = collapse %>%
  group_by(stockid) %>%
  summarize(yrs_data=n(), 
            yrs_collapsed = sum(current_collapse)) %>%
  inner_join(u_summary, by="stockid")

head(collapse_summary)  
hist(collapse_summary$yrs_collapsed)
table(collapse_summary$yrs_collapsed)
# toooooo many zeros, get rid of em 

collapse_summary_zero_trunc = collapse_summary %>%
  filter(yrs_collapsed>0)
table(collapse_summary_zero_trunc$yrs_collapsed)  

# build model

model_p = glm(yrs_collapsed ~ ratio_yrs_overfished + ratio_yrs_low_stock, 
              offset(log(yrs_data)), 
              data=collapse_summary_zero_trunc, 
              family="poisson")
summary(model_p)
library(AER)
AER::dispersiontest(model_p)
# our model is overdispersed 

# make a fake poisson bc theyre less sensitive
model_qp = glm(yrs_collapsed ~ ratio_yrs_overfished + ratio_yrs_low_stock, 
              offset(log(yrs_data)), 
              data=collapse_summary_zero_trunc, 
              family="quasipoisson")
summary(model_qp)

