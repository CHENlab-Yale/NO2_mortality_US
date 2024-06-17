######################################################################################
### Title: Disparities in NO2-related health burden in the United States
### Dr. Lingzhi Chu, Yale School of Public Health
### June 16, 2024
######################################################################################
require(fixest); require(splines); library(dplyr)


### 1. main model for a specific cause of death
model.data <- all.data %>% 
  filter(income="low") # stratified by county's income level: low, middle, high
model = feglm(death.count ~ no2.ma*race.ethnicity + pm25.ma*race.ethnicity + o3.ma*race.ethnicity + ns(temp.ma, df=5)*race.ethnicity | 
                county^race.ethnicity + state^year^race.ethnicity + year^month^race.ethnicity, 
              data = model.data, family="quasipoisson", offset = ~log(pop), cluster = "county")
# death.count was age-adjusted
# .ma indicated a moving average.
model.summary <- summary(model)

point.estimates <- model.summary$coeftable
point.estimates <- as.data.frame(point.estimates)
colnames(point.estimates) <- c("Est","SE","Z-value","p-value")
point.estimates$variable <- rownames(point.estimates)
var.estimates <- model.summary$cov.scaled


### 2. ratio of relative risk (per 1 unit increase in NO2)
logrrr.black <- point.estimates %>% 
  filter(variable == "no2.ma:race.ethnicityblack_non-hispanic")
logrrr.hispanic <- point.estimates %>% 
  filter(variable == "no2.ma:race.ethnicityany_hispanic")



### 3. relative risk (per 1 unit increase in NO2)
## (1) variance for RR
logrr.black.var <- var.estimates["no2.ma", "no2.ma"] + var.estimates["no2.ma:race.ethnicityblack_non-hispanic", "no2.ma:race.ethnicityblack_non-hispanic"] + 2 * var.estimates["no2.ma", "no2.ma:race.ethnicityblack_non-hispanic"]
logrr.hispanic.var <- var.estimates["no2.ma", "no2.ma"] + var.estimates["no2.ma:race.ethnicityany_hispanic", "no2.ma:race.ethnicityany_hispanic"] + 2 * var.estimates["no2.ma", "no2.ma:race.ethnicityany_hispanic"]

## (2) point estimates
logrr.white <- point.estimates %>% 
  filter(variable == "no2.ma") # including information of variance
logrr.black.est <- logrr.white$Est + logrrr.black$Est
logrr.hispanic.est <- logrr.white$Est + logrrr.hispanic$Est