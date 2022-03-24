
## GROUP B 

library(ggplot2)
library(data.table)
library(dplyr)

# Set the folder (path) that contains this R file as the working directory
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

data<-read.csv("madrid_transactions.csv",header=TRUE,sep=",",dec=".")
head(data)
summary(data)

transactions <- as.data.table(data)

daytime_order <- c('Dawn','Morning','Mid morning','Afternoon','Evening','Night','Midnight') #this vector might be useful for other plots/analyses

##Add country data to our DF ##

country_codes<- read.csv("country_codes.csv",header=TRUE,sep=",",dec=".")  ## country codes

#rename to make the join cleaner
country_codes<-country_codes%>% 
  rename(
    customer_country=Code,
    country=Name ##using region to match the maps 
  )


## Add country to our DF 
transactions<-left_join(transactions,country_codes,by='customer_country')
#View(transactions)

country <- transactions[, .(total = sum(amount), average = mean(amount)), by=customer_country][total > 15000]

######. - General Overview

Avg_Spend_CAT <- transactions %>%
  group_by(category) %>%
  summarise(n = n(),total_spend=sum(amount),Avg_Spend = mean(amount))%>%top_n(n=10, wt=total_spend)

a <- ggplot(Avg_Spend_CAT, aes(x = total_spend, y = Avg_Spend, color=category)) + 
  geom_point() + geom_text(aes(label=category),hjust=0, vjust=0)

a + labs(title= "Total and Average Spending per Category", x="Total Spending",y="Average Spending", col = "Customer Country") + 
  scale_x_continuous(breaks = seq(0, 500000, by = 50000))

###### -	Deep Dive Categories

#########################################################################
#  	Fashion & Shoes

#Top 10 Average spend/vs total by nationality for fashion

Avg_Sp_fash <- transactions %>%
  filter(category == "Fashion & Shoes")%>%
  group_by(country) %>%
  summarise(n_transactions = n(),total_spend=sum(amount),Avg_Spend = mean(amount))%>%top_n(n=10, wt=total_spend)


ggplot(Avg_Sp_fash, aes(x = total_spend, y = Avg_Spend, color=country, size=n_transactions)) + 
  geom_point() + geom_text(aes(label=country),hjust=0, vjust=0)+
  labs(title ="Total vs. Avg Spend by Country in Fashion & Shoes", x="Total Spend",y="Average Spend", size = "Transactions", color = "Customer Country")

##Top 4 spenders##
Avg_Sp_fash_Hour_top_4 <- transactions %>%
  filter(category == "Fashion & Shoes")%>%
  group_by(daytime,country) %>%
  summarise(n = n(),total_spend=sum(amount),Avg_Spend = mean(amount))%>%top_n(n=4, wt=total_spend)

ggplot(Avg_Sp_fash_Hour_top_4, aes(x = factor(daytime, level=daytime_order), y = total_spend, size=Avg_Spend)) + 
  geom_point()+geom_text(aes(label=country),hjust=0, vjust=0)+
  labs(title ="Top 4 spenders by time of the day", x="  ",y="Total Spend", size = "Average Spend")


#########################################################################
#  	Bars & Restaurants

#Top 10 Average spend/vs total by nationality for Bars & Restaurants

Avg_Sp_bars <- transactions %>%
  filter(category == "Bars & restaurants")%>%
  group_by(country) %>%
  summarise(n_transactions = n(),total_spend=sum(amount),Avg_Spend = mean(amount))%>%top_n(n=10, wt=total_spend)


ggplot(Avg_Sp_bars, aes(x = total_spend, y = Avg_Spend, color=country, size=n_transactions)) + 
  geom_point() + geom_text(aes(label=country),hjust=0, vjust=0)+
  labs(title ="Total spend vs avg spend by country in Bars & restaurants", x="Total Spend",y="Average Spend",size = "Transactions", color = "Customer Country")

##Top 4 spenders##
Avg_Sp_BR_Hour_top_4 <- transactions %>%
  filter(category == "Bars & restaurants")%>%
  group_by(daytime,country) %>%
  summarise(n = n(),total_spend=sum(amount),Avg_Spend = mean(amount))%>%top_n(n=4, wt=total_spend)

ggplot(Avg_Sp_BR_Hour_top_4, aes(x = factor(daytime, level=daytime_order), y = total_spend, size=Avg_Spend)) + 
  geom_point()+geom_text(aes(label=country),hjust=0, vjust=0)+
  labs(title ="Top 4 spenders by time of the day", x="  ",y="Total Spend", size = "Average Spend")

#########################################################################
#   Accommodation

#Accomodation by top 10 countries total spending

Accomodation <- transactions %>%
  filter(category %in% c("Accommodation")) %>% 
  group_by(country) %>%
  summarise(n = n(),total_spend=sum(amount),avg_spend=mean(amount), transactions = n) %>%
  top_n(n=10, wt=total_spend) 

a <- ggplot(Accomodation, aes(x=transactions, y=total_spend)) + 
  geom_point(aes(col=country, size=avg_spend)) +
  geom_text(aes(label=country),hjust=0, vjust=0)

a + labs(title= "Top 10 nationalities Spending on Accomodation", x="Transactions",y="Total Amount Spend", size = "Average Spend", col = "Customer Country")

#compare spending over time and day - in
theme_set(theme_classic())

data_day <- data %>% 
  filter(category %in% c("Accommodation"))

a <- ggplot(data_day, aes(x=hour, fill=weekday))+geom_density(alpha=0.7)

a + labs(title= "Compare Spending over Time and Weekday", x="Hour",y="Density", fill = "Weekday")

#conclusion: higher peak on thursdays around 8am, explore if promotions would trigger more transactions during the same time on friday for example

###########################################################################
#   Upper left quadrant (high average spending / low total spending categories)

#what nationalities do shop in these categories?
Country_Spending <- transactions %>%
  filter(category %in% c("Culture & Leisure", "Transportation", "Books & Music")) %>% 
  filter(amount != 0) %>%
  group_by(country) %>%
  summarise(n = n(),avg_spend=mean(amount),total_spend=sum(amount), transactions = n) %>% 
  top_n(n=5, wt=avg_spend)

a <- ggplot(Country_Spending, aes(x = transactions, y = avg_spend, color = total_spend)) + geom_point() + geom_text(aes(label=country),hjust=0, vjust=0)

a + labs(title= "Compare Spending of Customers for Culture & Leisure, Transportation and Books & Music", x="Transactions",y="Average Spend", color = "Total Spend")
