library(tidyverse)
library(plotly)
orders=read.csv('./Orders.csv', stringsAsFactors = FALSE)
returns=read.csv('./Returns.csv', stringsAsFactors = FALSE)
summary(orders)
summary(returns)
orders$Profit=gsub("[^0-9.]", "", orders$Profit)
orders$Sales=gsub("[^0-9.]", "", orders$Sales)
orders$Profit=as.numeric(orders$Profit)
orders$Sales<-as.numeric(orders$Sales)

orders=orders%>%mutate(Order.Date=as.POSIXct(Order.Date, format='%m/%d/%y'))
orders=orders%>%mutate(Ship.Date=as.POSIXct(Ship.Date, format='%m/%d/%y'))
orders$month=months(orders$Order.Date)


p=ggplot(data=orders, aes(x=orders$month, y=orders$Sales, fill=orders$Category)) +
  geom_line(shape=1)+
  geom_bar(stat = "identity", position = "stack")
ggplotly(p)




