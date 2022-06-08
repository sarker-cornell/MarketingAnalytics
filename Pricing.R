library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)
library(scales)
data <- read_excel("Assignment_Pricing_Data.xls")

#To Understand the dataset
View(data)
summary(data)
str(data)

# 1. (2 points) Compute summary statistics to understand the data:
# a) Plot the sales data for the two brands separately, over time. 
# b) Report the (simple) average retail price per unit of each brand. Compute the $ price 
# gap between the brands at the average prices.
# c) Report the average weekly % retail margin of each brand.  
# d) Report the average weekly unit sales of each brand. 
# e) Report the average weekly market share of the two brands (within this retail account) 
# based on unit sales. Assume the category consists of these two brands only.
# f) Report the average weekly profits (in $) of the retailer and the manufacturer. 
# Remember that the retailer gets profits from selling brand 1 and the private label.

data1 <- data %>%
  select(Week,private_label_price, private_label_sales, private_label_cost, brand1_price,brand1_sales, brand1_cost, brand1_manufacturing_cost) %>%
  mutate(
    private_label_sales_units=private_label_sales*24,
    brand1_sales_units=brand1_sales*24,
    category_actual_sales=private_label_sales_units+brand1_sales_units,
    price_gap = brand1_price - private_label_price,
    retail_margin_private_label = (private_label_price - (private_label_cost/24))/private_label_price,
    retail_margin_brand1 = (brand1_price -(brand1_cost/24))/brand1_price,
    log_sales_brand1 = log(brand1_sales*24),
    price_private_label=2.39,
    price_brand1=2.69,
    log_sales_private_label=log(private_label_sales*24),
  )
View(data1)

# Part 1a. Plot of Sales data separately over time
plot_private_label <- ggplot(data=data1, aes(x=Week, y=private_label_sales_units, group=1)) +
  geom_line()+
  geom_point()+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  xlab("Week") + 
  ylab("Private Label Sales over Time")

plot_brand1 <- ggplot(data=data1, aes(x=Week, y=brand1_sales_units, group=1)) +
  geom_line()+
  geom_point()+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  xlab("Week") + 
  ylab("Brand1 Sales over Time")

plot_grid(plot_private_label,plot_brand1)

# Part 1b
Average_retail_price_private_label = mean(data1$private_label_price)
Average_retail_price_brand1 = mean(data1$brand1_price)
Price_gap = mean(data1$price_gap)
print(paste0("Average retail price of private label per unit is: $",format(round(Average_retail_price_private_label, 2), nsmall = 2))) 
print(paste0("Average retail price of brand 1 per unit is: $",format(round(Average_retail_price_brand1, 2), nsmall = 2))) 
print(paste0("Price Gap is: $",format(round(Price_gap, 2), nsmall = 2))) 

# Part 1c
Average_weekly_retail_margin_private_label = mean(data1$retail_margin_private_label)
Average_weekly_retail_margin_brand1 = mean(data1$retail_margin_brand1)
print(paste0("Average weekly retail margin of private label is: ",percent(Average_weekly_retail_margin_private_label, accuracy = 0.01))) 
print(paste0("Average weekly retail margin of brand 1 is: ",percent(Average_weekly_retail_margin_brand1, accuracy = 0.01))) 


# Part1d
Average_weekly_unit_sales_private_label = mean(data1$private_label_sales_units)
Average_weekly_unit_sales_brand1 = mean(data1$brand1_sales_units)
print(paste0("Average weekly unit sales of private label is: ",format(round(Average_weekly_unit_sales_private_label, 0), nsmall = 0)," units")) 
print(paste0("Average weekly unit sales of brand 1 is: ",format(round(Average_weekly_unit_sales_brand1, 0), nsmall = 0)," units")) 


#Part 1e
Market_share_private_label = Average_weekly_unit_sales_private_label/(Average_weekly_unit_sales_private_label+Average_weekly_unit_sales_brand1)
Market_share_brand1 = Average_weekly_unit_sales_brand1/(Average_weekly_unit_sales_private_label+Average_weekly_unit_sales_brand1)
print(paste0("Market share of private label is: ",percent(Market_share_private_label, accuracy = 0.01))) 
print(paste0("Market share of brand 1 is: ",percent(Market_share_brand1, accuracy = 0.01))) 



#Part 1f
Retailer_profit_private_label=(mean(data1$private_label_price)-mean(data1$private_label_cost)/24)*Average_weekly_unit_sales_private_label
Retailer_profit_brand1=(mean(data1$brand1_price)-mean(data1$brand1_cost)/24)*Average_weekly_unit_sales_brand1
Total_Retailer_profit=Retailer_profit_private_label+Retailer_profit_brand1

Manufacturer_cost_brand1 = mean(data1$brand1_manufacturing_cost)/24 + 10/24
Manufacturer_selling_price_brand1 = mean(data1$brand1_cost)/24
Total_Manufacturer_profit =  (Manufacturer_selling_price_brand1-Manufacturer_cost_brand1)*Average_weekly_unit_sales_brand1


print(paste0("Weekly Retailer Profit is: $",format(round(Total_Retailer_profit, 2), nsmall = 2)))
print(paste0("Weekly Manufacturer Profit is: $",format(round(Total_Manufacturer_profit, 2), nsmall = 2)))


# 2. (3 points) Determine the variables that should be included in the marketing mix models 
# (one model each for brand 1 and the private label).  Estimate the models. 
# Use the semi-log (or exponential) form of the model. Show the regression outputs. 
# Note: DO NOT include a trend variable in the model. DO NOT include or control for seasonality in the model

reg1 <- lm(log(private_label_sales_units) ~ private_label_price + brand1_price, data = data1)
summary(reg1)

reg2 <- lm(log(brand1_sales_units) ~ private_label_price + brand1_price, data = data1)
summary(reg2)


# 3. (4 points) Assess the validity of the models based on (i) face validity of the estimated 
# coefficients, (ii) statistical significance of the overall models, and (iii) statistical significance 
# of estimated coefficients.  Plot the predicted versus actual sales (within the estimation 
# sample) for each brand and for the category. You should have three graphs in total, one each 
# for brand 1, private label, and the category. Horizontal axis is time, vertical axis is sales.   
# Category sales is sum of sales of the two brands.

data2 <- data1 %>%
  select(everything()) %>%
  mutate(
    log_private_label_sales_units=log(private_label_sales_units),
    log_brand1_sales_units=log(brand1_sales_units),
    private_label_predicted_sales=exp(reg1$coefficients[1]+reg1$coefficients["private_label_price"]*private_label_price+reg1$coefficients["brand1_price"]*brand1_price)*exp(1/2*anova(reg1)['Residuals', 'Mean Sq']),
    brand1_predicted_sales=exp(reg2$coefficients[1]+reg2$coefficients["private_label_price"]*private_label_price+reg2$coefficients["brand1_price"]*brand1_price)*exp(1/2*anova(reg2)['Residuals', 'Mean Sq']),
    category_predicted_sales=private_label_predicted_sales+brand1_predicted_sales,
  )
View(data2)

ggplot(data=data2, aes(x=Week)) + 
  geom_line(aes(y = private_label_sales_units, colour = "Private Label Actual Sales (in units)")) + 
  geom_line(aes(y = private_label_predicted_sales, colour = "Private Label Predicted Sales"))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  xlab("Week") + 
  ylab("Private Label Sales")

ggplot(data=data2, aes(x=Week)) + 
  geom_line(aes(y = brand1_sales_units, colour = "Brand1 Actual Sales (in units)")) + 
  geom_line(aes(y = brand1_predicted_sales, colour = "Brand1 Predicted Sales"))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  xlab("Week") + 
  ylab("Brand1 Sales")

ggplot(data=data2, aes(x=Week)) + 
  geom_line(aes(y = category_actual_sales, colour = "category_actual_sales")) + 
  geom_line(aes(y = category_predicted_sales, colour = "category_predicted_sales"))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  xlab("Week") + 
  ylab("Category Sales")


# 4. (7 points) In this question you will determine the optimal selling price of the manufacturer 
# using Excel or calculus for an average week. Make reasonable assumptions about 
# manufacturer costs and state them. Remember that the retailer sets the retail price.  Assume 
# that the retailer has margin goals that serve as norms. Please state your assumptions about 
# the retailer clearly. With regard to the competitor’s price, consider the following two 
# scenarios (these reflect our assumptions about how the retailer will price the private label) 
# and provide the optimal selling price of the manufacturer under each scenario separately:
# a) Assume that the private label’s retail price is fixed at the average level in the data.  
# b) Assume that the private label’s retail price is always 35 cents below the retail price 
# of brand 1.
# Please make sure that the steps you followed in your analysis are clear, otherwise we cannot 
# give partial credit.

#Key Parameters
Average_retail_margin_brand1 <- 0.17
Brand1_product_cost_per_case <- 28.43
Brand1_selling_cost_per_case <- 10
Brand1_cost_per_case = Brand1_product_cost_per_case+ Brand1_selling_cost_per_case
Brand1_cost_per_unit = Brand1_cost_per_case/24

Private_label_cost_per_case <- 39.79
Private_label_cost_per_unit = Private_label_cost_per_case/24


# Part 4a: Private Label retail price fixed at current level
fn <- function(v) {
  price_to_retailer = v
  manufacturer_markup = price_to_retailer - Brand1_cost_per_case/24
  case_price_retailer = price_to_retailer*24
  consumer_price=price_to_retailer/(1-Average_retail_margin_brand1)
  
  private_label_price = consumer_price-0.35
  
  brand1_sales = exp(reg2$coefficients[1]+reg2$coefficients["private_label_price"]*private_label_price+reg2$coefficients["brand1_price"]*consumer_price)*exp(1/2*anova(reg2)['Residuals', 'Mean Sq'])
  manufacturer_profit = manufacturer_markup*brand1_sales
  
  return <- (manufacturer_profit)
}
out_4a<-optim(par = c(1), fn = fn,control=list(fnscale=-1))
# Need to add "control=list(fnscale=-1)" to maximize the manufacturer profit in the optim function 

out_4a
print(paste0("Optimized Manufacutrer price (4a) is: $",format(round(out_4a$par, 2), nsmall = 2))) 


# Part 4b: Private Label retail price fixed at Brand1 price less 35 cents
fn <- function(v) {
  price_to_retailer = v
  manufacturer_markup = price_to_retailer - Brand1_cost_per_case/24
  case_price_retailer = price_to_retailer*24
  consumer_price=price_to_retailer/(1-Average_retail_margin_brand1)
  
  private_label_price =2.15
  
  brand1_sales = exp(reg2$coefficients[1]+reg2$coefficients["private_label_price"]*private_label_price+reg2$coefficients["brand1_price"]*consumer_price)*exp(1/2*anova(reg2)['Residuals', 'Mean Sq'])
  manufacturer_profit = manufacturer_markup*brand1_sales
  
  return <- (manufacturer_profit)
}
out_4b<-optim(par = c(1), fn = fn,control=list(fnscale=-1))
out_4b
print(paste0("Optimized Manufacutrer price (4b) is: $",format(round(out_4b$par, 2), nsmall = 2))) 


# 5. (4 points) In this question you will compute the “implied” values of different variables at the 
# optimal prices in each of scenarios 4(a) and 4(b) and compare them with the current 
# situation.  Fill in the table below for a typical week. Take the retailer’s cost of the private 
# label to be the average cost in the historical data. The last column can be filled in by 
# reproducing your answers to Q1 (d), (e), and (f).

#5a
price_to_retailer = out_4a$par
manufacturer_markup = price_to_retailer - Brand1_cost_per_case/24
case_price_retailer = price_to_retailer*24
consumer_price=price_to_retailer/(1-Average_retail_margin_brand1)

private_label_price = consumer_price-0.35


brand1_sales = exp(reg2$coefficients[1]+reg2$coefficients["private_label_price"]*private_label_price+reg2$coefficients["brand1_price"]*consumer_price)*exp(1/2*anova(reg2)['Residuals', 'Mean Sq'])
private_label_sales = exp(reg1$coefficients[1]+reg1$coefficients["private_label_price"]*private_label_price+reg1$coefficients["brand1_price"]*consumer_price)*exp(1/2*anova(reg1)['Residuals', 'Mean Sq'])

manufacturer_profit = manufacturer_markup*brand1_sales
Retailer_profit_private_label=(private_label_price-Private_label_cost_per_unit)*private_label_sales
Retailer_profit_brand1= (consumer_price-price_to_retailer)*brand1_sales
Retailer_total_profit=Retailer_profit_private_label+Retailer_profit_brand1

Market_share_brand1 = brand1_sales/(brand1_sales+private_label_sales)
Market_share_private_label = private_label_sales/(brand1_sales+private_label_sales)

print("####################")
print("Table 5: Scenario 4a")
print(paste0("Brand1 sales is: $",format(round(brand1_sales, 2), nsmall = 2))) 
print(paste0("Private label sales is: $",format(round(private_label_sales, 2), nsmall = 2))) 

print(paste0("Market share of brand 1 is: ",percent(Market_share_brand1, accuracy = 0.01))) 
print(paste0("Market share of private label is: ",percent(Market_share_private_label, accuracy = 0.01))) 

print(paste0("Retailer Weekly profit is: $",format(round(Retailer_total_profit, 2), nsmall = 2))) 
print(paste0("Manufacturer weekly profit is: $",format(round(manufacturer_profit, 2), nsmall = 2))) 

#5b
price_to_retailer = out_4b$par
manufacturer_markup = price_to_retailer - Brand1_cost_per_case/24
case_price_retailer = price_to_retailer*24
consumer_price=price_to_retailer/(1-Average_retail_margin_brand1)

private_label_price =2.15

brand1_sales_4b = exp(reg2$coefficients[1]+reg2$coefficients["private_label_price"]*private_label_price+reg2$coefficients["brand1_price"]*consumer_price)*exp(1/2*anova(reg2)['Residuals', 'Mean Sq'])
private_label_sales_4b = exp(reg1$coefficients[1]+reg1$coefficients["private_label_price"]*private_label_price+reg1$coefficients["brand1_price"]*consumer_price)*exp(1/2*anova(reg1)['Residuals', 'Mean Sq'])

manufacturer_profit_4b = manufacturer_markup*brand1_sales_4b
Retailer_profit_private_label_4b=(private_label_price-Private_label_cost_per_unit)*private_label_sales_4b
Retailer_profit_brand1_4b= (consumer_price-price_to_retailer)*brand1_sales_4b
Retailer_total_profit_4b=Retailer_profit_private_label_4b+Retailer_profit_brand1_4b

Market_share_brand1_4b = brand1_sales_4b/(brand1_sales_4b+private_label_sales_4b)
Market_share_private_label_4b = private_label_sales_4b/(brand1_sales_4b+private_label_sales_4b)

print("####################")
print("Table 5: Scenario 4b")
print(paste0("Brand1 sales is: $",format(round(brand1_sales_4b, 2), nsmall = 2))) 
print(paste0("Private label sales is: $",format(round(private_label_sales_4b, 2), nsmall = 2))) 

print(paste0("Market share of brand 1 is: ",percent(Market_share_brand1_4b, accuracy = 0.01))) 
print(paste0("Market share of private label is: ",percent(Market_share_private_label_4b, accuracy = 0.01))) 

print(paste0("Retailer Weekly profit is: $",format(round(Retailer_total_profit_4b, 2), nsmall = 2))) 
print(paste0("Manufacturer weekly profit is: $",format(round(manufacturer_profit_4b, 2), nsmall = 2))) 

