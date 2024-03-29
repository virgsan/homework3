#Part1
install.packages("tidyverse")
library(tidyverse)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, modelsummary)
install.packages("fixest")
library(fixest)

final.data <- final.data %>% group_by(state) %>% arrange(state, Year) %>%
  mutate(tax_change = tax_state - lag(tax_state),
         tax_change_d = ifelse(tax_change == 0,0,1),
         price_cpi_2012 = cost_per_pack*(229.5939/index),
         total_tax_cpi_2012=tax_dollar*(229.5939/index),
         ln_sales=log(sales_per_capita),
         ln_price_2012=log(price_cpi_2012))

tax.change.plot <- final.data %>% group_by(Year) %>% filter(Year<1986, Year> 1970) %>%
  summarize(mean_change=mean(tax_change_d)) %>% 
  ggplot(aes(x=as.factor(Year), y=mean_change)) +
  geom_bar(stat= "identity") +
  labs(
    x="Year",
    y = "Share of States"
  ) + ylim(0,1) +
  theme_bw()

tax.change.plot
save.image("Hwk3_workspace.Rdata")

#2 

#ian's code
tax.price.data <- final.data %>% select(Year, state, total_tax_cpi_2012, price_cpi_2012)  %>%
  group_by(Year)  %>% summarize(mean_tax=mean(total_tax_cpi_2012, na.rm = TRUE),
                                mean_price =mean(price_cpi_2012, na.rm = TRUE))  %>%
  pivot_longer(cols=c("mean_tax", "mean_price"),
               names_to="var", values_to = "dollars")

tax.price.plot <- tax.price.data  %>%
  ggplot(aes(x=Year, y=dollars, color=var)) +
  geom_line() +
  labs(
    x="Year",
    y= "Price per Pack in 2012 Dollars"
  ) + ylim(0,10) +
  geom_text(data = tax.price.data %>% filter(Year == 2018),
            aes(label = c("Mean Price", "Mean Tax"),
                x= Year,
                Y= dollars-.3)) +
  scale_color_manual(values=c("black", "black")) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(1970, 2020, 5))
tax.price.plot
save.image("Hwk3_workspace.Rdata")


#og code
final.data <- filter(final.data, Year >= 1970 & Year <= 2018)

average_tax <- final.data %>% group_by(Year) %>% summarize(avg_tax = mean(tax_dollar)) 
average_price <- final.data %>% group_by(Year) %>% summarize(avg_price = mean(cost_per_pack)) 

graph_2 <- ggplot() +
  geom_line(data = average_tax, aes(x = Year, y = avg_tax, color = "orange")) +
  geom_line(data = average_price, aes(x = Year, y = avg_price, color = "purple")) +
  labs(x = "Year", y = "Average Tax/Price", 
       title = "Average Tax and Price of Cigarettes (1970-2018)",
       color = "Legend") +
  scale_color_manual(values = c("orange", "purple"), labels = c("Price", "Tax")) +
  theme_minimal()

graph_2
save.image("Hwk3_workspace.Rdata")

#3 
cig.data.change <- final.data %>% ungroup() %>%
  filter(Year==1970)%>% select(state, price_1970= price_cpi_2012)%>%
  left_join(final.data%>% filter(Year== 2018)%>% select(state, price_2018 = price_cpi_2012),
            by=c("state"))%>%
  mutate(price_change = price_2018-price_1970)


high.change <- cig.data.change%>% slice_max(price_change, n=5)%>% mutate(change_group = "high")
low.change <- cig.data.change%>% slice_min(price_change, n=5) %>% mutate(change_group = "low")
change.group<- rbind(high.change, low.change)

top.bottom.price <- final.data %>% ungroup() %>%
  inner_join(change.group %>% select(state, change_group),
             by=c("state"))

high.price.plot <- top.bottom.price %>% filter(change_group=="high") %>%
  ggplot(aes(x=Year, y=sales_per_capita, color= state)) +
  stat_summary(fun= "mean", geom="line") +
  labs(
    x="Year",
    y="Packs per Capita",
    color= "State"
  ) + theme_bw() +
  scale_x_continuous(breaks=seq(1970,2020,5))

high.price.plot
save.image("Hwk3_workspace.Rdata")

#question4

low.price.plot <- top.bottom.price %>% filter(change_group=="low") %>%
  ggplot(aes(x=Year, y=sales_per_capita, color= state)) +
  stat_summary(fun= "mean", geom="line") +
  labs(
    x="Year",
    y="Packs per Capita",
    color= "State"
  ) + theme_bw() +
  scale_x_continuous(breaks=seq(1970,2019,5))

low.price.plot
save.image("Hwk3_workspace.Rdata")


#question5

difference <- final.data %>%
  group_by(state) %>%
  summarize(price_change = price_cpi_2012[Year == 2018] - price_cpi_2012[Year == 1970])

sort_data <- difference %>%
  arrange(desc(price_change))

top_5 <- sort_data %>%
  head(5)

final.data.five <- final.data %>%
  filter(state %in% top_5$state) %>%
  group_by(Year) %>%
  summarize(sales_per_capita_five = mean (sales_per_capita)) 

difference <- final.data %>%
  group_by(state) %>%
  summarize(price_change = price_cpi_2012[Year == 2018] - price_cpi_2012[Year == 1970])

sort_data <- difference %>%
  arrange(desc(price_change))

bottom_5 <- sort_data %>%
  tail(5)

final.data.bfive <- final.data %>%
  filter(state %in% bottom_5$state) %>%
  group_by(Year) %>%
  summarize(sales_per_capita_fiveb = mean (sales_per_capita)) 

graph_5 <- ggplot() +
  geom_line(data = final.data.five, aes(x = Year, y = sales_per_capita_five, color = "orange")) +
  geom_line(data = final.data.bfive, aes(x = Year, y = sales_per_capita_fiveb , color = "purple")) +
  labs(x = "Year", y = "Average Cigarette Packs Sold Per Capita", 
       title = "States with Top 5 Highest and Top 5 Lowest Price Increases",
       color = "Legend") +
  scale_color_manual(values = c("orange", "purple"), labels = c("Low","High")) +
  theme_minimal()

graph_5
save.image("Hwk3_workspace.Rdata")

#Part 2
#question1

ln_sales <- log(final.data$sales_per_capita)
ln_price_2012 <-log(final.data$price_cpi_2012)

ols <- feols(ln_sales~ln_price_2012, data = final.data %>% filter(Year<1991))
summary(ols)


#question2
iv <- feols(ln_sales ~1 | ln_price_2012 ~ total_tax_cpi_2012, data = final.data %>% filter(Year<1991))
summary(iv)

#question3
first.stage <- feols(ln_price_2012~total_tax_cpi_2012, data= final.data %>% filter(Year<1991))
summary(first.stage)

reduced.form <- feols(ln_sales~total_tax_cpi_2012, data= final.data %>% filter(Year<1991))
summary(reduced.form)

#question4
#a
ols2 <- feols(ln_sales~ln_price_2012, data = final.data %>% filter(Year<=2015 & Year>=1991))
summary(ols2)

#b
iv2 <- feols(ln_sales ~1 | ln_price_2012 ~ total_tax_cpi_2012, data = final.data %>% filter(Year>=1991 & Year<= 2015))
summary(iv2)

#c
first.stage2 <- feols(ln_price_2012~total_tax_cpi_2012,data= final.data %>% filter(Year>= 1991 & Year <= 2015))
summary(first.stage2)

reduced.form2 <- feols(ln_sales~total_tax_cpi_2012, data= final.data %>% filter(Year>= 1991 & Year <= 2015))
summary(reduced.form2)

#formatting 
modelsummary(list("OLS"=ols, "IV"=iv, "OLS"=ols2, "IV"= iv2),
             title="Point Estimates", 
             coef_map=c('ln_price_2012'="Log Price",
                        'fit_ln_price_2012' = "Log Price"),
             gof_map=c("N"="nobs", "r.squared"))
