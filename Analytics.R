library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)
library(scales)
data <- read_excel("clikthru.xls")

#To Understand the dataset
View(data)
summary(data)
str(data)

######## Question 1 (The Economics of Targeting)

# 1A.	(2.5 points) What is the Post-Acquisition Value (PAV) of a customer before 
# subtracting acquisition cost?

Diff=100
i=0.02
r=1-0.02
PAV = (Diff) * ((1+i)/(1-r+i))
PAV

# 1B. (4 points) Imagine that the targeting model is not available, so 
# prospects have to be picked at random for solicitation. What is the long-term 
# value in $ of this customer acquisition campaign to the firm? 
# [Hint: Do not assume that the data in the file are sorted randomly. 
# Instead, use the theoretical idea of “picking at random.” 
# That is, when picked at random, %Hit is equal to %Exposed in the Gains Chart.] 

campaign_budget = 1000
converstion_cost = 100
prospects = campaign_budget/converstion_cost
expectation_random_model = (12/30)
num_paying_customer = expectation_random_model*prospects

PAV_random_campaign = PAV*num_paying_customer
LTV_random_campaign =  PAV_random_campaign - campaign_budget
LTV_random_campaign

# 1C. C.	(4 points) Suppose the targeting model is available, and the budget 
# available remains $1,000 to acquire customers. What is the long-term value in 
# $ of this campaign to the firm? [Predicted scores from the targeting model are
# in Column C of tab “Gains Chart”.]

campaign_budget = 1000
converstion_cost = 100
prospects = campaign_budget/converstion_cost

num_paying_customer_after_targeting = 8 
#After sorting by propensity to convert, 8 out of 10 are actual paying customer

PAV_target_campaign = PAV*num_paying_customer_after_targeting
LTV_target_campaign =  PAV_target_campaign - campaign_budget
LTV_target_campaign

# 1D.	(2 points) What is the maximum amount that the sales manager should be willing 
# to pay to acquire data and develop this scoring model?

Maximum_model_value = LTV_target_campaign - LTV_random_campaign
Maximum_model_value

######## Question 2 Banner Ad Targeting

# 2A. (1.5 point) Compute and report the click-through rate in the Estimation sample.  
data_est <- read_excel(path = "clikthru.xls", sheet = "Estimation")
data1 <- data_est %>%
  select(everything()) %>%
  mutate(
    num_visits = exp(`log(num visits)`),
  )

total_clicks=sum(data1$`CLICK (1=yes, 0=no)`)
total_exposures= sum(data1$num_visits)
click_thourgh_rate = total_clicks/(total_exposures)
click_thourgh_rate

# 2B. (4 points) Estimate a Linear (not semi-log or any other form) regression
# model on the Estimation sample. Show the regression output and highlight the R2 of the model.
data_est <- read_excel(path = "clikthru.xls", sheet = "Estimation")
model_est = lm(data_est$`CLICK (1=yes, 0=no)`~., data = data_est)
summary(model_est)
summary(model_est)$r.squared

# 2C.	(2 points) Estimate the same regression model that you applied in
# Question B, but now use Validation sample data. 
# Show the regression output and highlight the R2 of the model. 
data_val <- read_excel(path = "clikthru.xls", sheet = "Validation")
model_val = lm(data_val$`CLICK (1=yes, 0=no)`~., data = data_val)
summary(model_val)
summary(model_val)$r.squared

# 2D.	(3 points) Use the regression results from the Estimation sample (i.e., 
# model developed in part B) to predict the score for each visitor in the 
# Validation sample.  Plot a “Gains Chart” that illustrates how well the model 
# does in targeting.  In the same chart also plot the performance of a model 
# that randomly picks individuals to target.  When 20% of the sample is exposed, 
# what is the approximate lift due to the model? 
data_val$score_e = predict(model_est, newdata = data_val)
gainchart_e.df = data_val %>%
  select(`CLICK (1=yes, 0=no)`, score_e) %>%
  arrange(desc(score_e)) %>%
  mutate(perc_exposed = (1:nrow(.))/nrow(.),
         num_hit = cumsum(`CLICK (1=yes, 0=no)`),
         perc_hit = num_hit/sum(`CLICK (1=yes, 0=no)`),
         perc_randhit = perc_exposed)
ggplot(data=gainchart_e.df)+
  geom_line(aes(perc_exposed, perc_hit, color='%Hit'))+
  geom_line(aes(perc_exposed, perc_randhit, color='%Hit at Random'))

id = which.min(abs(gainchart_e.df$perc_exposed-0.2))
lift_2D = gainchart_e.df$perc_hit[id] - gainchart_e.df$perc_randhit[id]
lift_2D


# 2E.(2 points) Repeat the analysis in Question D (i.e., predict the score for
# each visitor in the Validation sample and plot a Gains Chart), but now use the
# regression estimates from the Validation sample (i.e., model developed in part C).  
# Does the Gains chart look better or worse than what you obtained in part D?  
# Why (answer in no more than two sentences)?
data_val$score_v = predict(model_val, newdata = data_val)
gainchart_v.df = data_val %>%
  select(`CLICK (1=yes, 0=no)`, score_v) %>%
  arrange(desc(score_v)) %>%
  mutate(perc_exposed = (1:nrow(.))/nrow(.),
         num_hit = cumsum(`CLICK (1=yes, 0=no)`),
         perc_hit = num_hit/sum(`CLICK (1=yes, 0=no)`),
         perc_randhit = perc_exposed)
ggplot(data=gainchart_v.df)+
  geom_line(aes(perc_exposed, perc_hit, color='%Hit'))+
  geom_line(aes(perc_exposed, perc_randhit, color='%Hit at Random'))

id = which.min(abs(gainchart_v.df$perc_exposed-0.2))
lift_2E = gainchart_v.df$perc_hit[id] - gainchart_v.df$perc_randhit[id]
lift_2E

# Explanation:
# The gains chart looks slightly better than the one obtained before because 
# predictions are made within the same sample in which the model is estimated. 
# In this case (i.e. part E) the model estimates have a chance to be affected by 
# the noise characteristics of the particular sample (a phenomenon known as
# overfitting), and the predictions are therefore superior.
