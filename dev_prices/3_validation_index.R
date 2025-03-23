# validate the indices

library(readr)
prices <- read_csv("dev_prices/datasource_2025/US.CommodityPriceIndices_A_20250309_144518.csv")
prices
plot(prices$`All groups_Index_Base_2015_Value`, type = 'b')

prices$Commodity_Label |> table()
d <- filter(prices, Commodity_Label == 'All groups') 
points(d$Index_Base_2015_Value, type = 'l', col = 'red')
