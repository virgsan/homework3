#Summarize the data

#question1

states <- unique(tax_state) %>% filter(!is.na(state))

states_list <- list()
# Loop over each state and calculate the tax change
for (i in states) {
  # Subset the data for the current state
  state_data <- filter(taxburden, state == i)
  # Calculate the tax change for the current state
  state_data2 <- state_data %>% mutate(tax_change = c(0,ifelse(diff(tax_state) != 0, 1, 0)))
  states_list[[i]] <- state_data2
}

taxburden2 <- do.call(rbind, states_list)

table_1 <- taxburden2%>% filter(Year %in% c(1970:1985)) %>% 
  group_by(Year, state)%>% summarize(tax_change = tax_change)%>% 
  group_by(Year) %>% 
  summarize(prop = sum(tax_change == TRUE)/n())

taxburden2%>% filter(Year %in% c(1970:1985)) %>% 
  group_by(Year)%>% 
  summarize(prop = sum(tax_change == 1)/n())




#question2
q2 <- ggplot(cig.data, aes(x=Year,y=total_tax_cpi)) + 
  stat_summary(fun="mean",geom="line") +
  labs(
    x="Year",
    y="Tax per Pack ($)",
    title="Cigarette Taxes in 2012 Real Dollars"
  ) + theme_bw() +
  scale_x_continuous(breaks=seq(1970, 2018, 5))


q2


library(ggplot2)
library(dplyr)

