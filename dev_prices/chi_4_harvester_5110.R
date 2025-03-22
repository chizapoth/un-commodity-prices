# understand in detail what 5110 does


library(openxlsx)

# commodity index
CommodityKeyWorldBank <- data.frame(
  CommodityProduct = c('020100.01','030613.01','080300.01','090200.01','100600.01','120100.01','120810.01','140400.03','150710.01','150810.01','151110.01','151211.01','151311.01','151321.01','230120.01','251000.01','270900.01','270900.02','271100.01','400100.02','440300.01','440700.01','441200.01'), 
  WorldBankProduct = c("BEEF", "SHRIMP_MEX", "BANANA_US", "TEA_MOMBASA", "RICE_05", "SOYBEANS", "SOYBEAN_MEAL", "COTTON_A_INDX", "SOYBEAN_OIL", "GRNUT_OIL", "PALM_OIL", "SUNFLOWER_OIL", "COCONUT_OIL", "PLMKRNL_OIL", "FISH_MEAL", "PHOSROCK", "CRUDE_BRENT", "CRUDE_DUBAI", 'iNATGAS', "RUBBER1_MYSG", "LOGS_CMR", "SAWNWD_MYS","PLYWOOD"), 
  WorldBankCols = c(42, 45, 40, 18, 33, 25, 27, 55, 26, 22, 23, 29, 19, 24, 21, 58, 3, 4, 11, 57, 50, 53, 54)) 
CommodityKeyWorldBank |> head()


CommodityKeyWorldBank
View(CommodityKeyWorldBank)
# metal:
# gold: 70
# copper 65
# aluminium 63
# silver 72
# nickel 68
# zinc 69
# platinum 71
# lead 66
# tin 67
# manganese ore NA


# source data
data <- openxlsx::read.xlsx("https://thedocs.worldbank.org/en/doc/5d903e848db1d1b83e0ec8f744e55570-0350012021/related/CMO-Historical-Data-Monthly.xlsx", 
                            sheet = "Monthly Prices", startRow = 7)


data <- openxlsx::read.xlsx("~/Documents/Data/unctad/CMO-Historical-Data-Monthly.xlsx", 
                            sheet = "Monthly Prices", startRow = 5)

head(data)
str(data)


# add a checker for column names, to make sure it is extracting the correct ones
col_names <- colnames(data)
col_id <- 1:ncol(data)
commodity_ref <- data.frame(col_id, col_names)
commodity_ref

# data.frame()

# placeholder
# df0 <- data.frame(CommodityProduct="",
#                   Year=0,
#                   Period="",
#                   Value=0)
# 
# df0 <- df0[NULL]
# 
# 
# 
# for (i in 1:nrow(CommodityKeyWorldBank)){
#   
#   # i <- 1
#   df1 <- data.frame(CommodityProduct = CommodityKeyWorldBank$CommodityProduct[i],
#                     Year = substr(data[, 1], 1, 4),
#                     Period = substr(data[, 1], 5, 7),
#                     Value = as.numeric(data[, CommodityKeyWorldBank$WorldBankCols[i]]))
#   
#   # want long format
#   df0 <- rbind(df0, df1)
#   
# }

# df0$Year <- as.integer(df0$Year)
# df0$Value <- as.numeric(df0$Value)
# 
# df0$Value <- ifelse(df0$CommodityProduct=="400100.02",
#                     df0$Value*100,
#                     ifelse(df0$CommodityProduct=="140400.03",
#                            df0$Value*100*0.453592,
#                            df0$Value))
# 
# df0 <- df0[df0$Year>=1995,]
# 
# df0$DataSource <- "5110"

# write.csv(df0, file="DataNew/Collected_5110.csv", row.names=FALSE)



# ____ METAL ____ ----
# metal from wb -----

# load metadata with commodity code
library(data.table)
library(tidyr)
library(dplyr)
library(zoo)

meta <- read.xlsx('./dev/metadata/commodity_metadata.xlsx')
meta <- data.table(meta)
unique(meta$datasource_code)
meta[datasource_code == 6801]



# these are the column id from worldbank
# metal:
# gold: 70
# copper 65
# aluminium 63
# silver 72
# nickel 68
# zinc 69
# platinum 71
# lead 66
# tin 67
# manganese NA

# in the order of code
# manganese <- list('260200.01', 'manganese_44', NA)
copper <- list('260300.01', 'Copper', 65)
nickel <- list('260400.01', 'Nickel', 68)
aluminum <- list('260600.01', 'Aluminum', 63)
lead <- list('260700.02', 'Lead', 66)
zinc <- list('260800.01', 'Zinc', 69)
tin <- list('260900.01', 'Tin', 67)
silver <- list('261600.01', 'Silver', 72)
gold <- list('710800.01', 'Gold', 70)
platinum <- list('711000.01', 'Platinum', 71)

##### add iron ore: 64


wbref_metal <- data.frame(rbind(
  # manganese, 
  copper, 
  nickel,
  aluminum, 
  lead,
  zinc,
  tin,
  silver,
  gold,
  platinum))

# rename columns, fix type
colnames(wbref_metal) <- c('price_series', 'name_short', 'wb_col_id')
wbref_metal$price_series <- as.character(wbref_metal$price_series)
wbref_metal$name_short <- as.character(wbref_metal$name_short)
wbref_metal$wb_col_id <- as.integer(wbref_metal$wb_col_id)
wbref_metal

# now extract from original data, 
# compare code id
data <- openxlsx::read.xlsx("~/Documents/Data/unctad/CMO-Historical-Data-Monthly.xlsx", 
                            sheet = "Monthly Prices", startRow = 5)

# data <- data.table(data)
# head(data)
# str(data)

# only take those above 1995
data$year <- as.numeric(substr(data$X1, 1, 4))
d <- filter(data, year >=1995)

# also attach the time (X1)
d <- d[, c(1, wbref_metal$wb_col_id)]

# make long format
d_long <- pivot_longer(data = d, !X1, names_to = 'name_short', values_to = 'value')
d_long

# attach back the commodity ID
d_long <- full_join(d_long, wbref_metal[,c(1,2)])

# create time and months
d_long$year <- substr(d_long$X1, 1, 4)
d_long$period <- substr(d_long$X1, 5, 7)

# add data source
d_long$data_source <- '5110'
head(d_long)

# if order by time
# d2[order(d2$year, d2$period),]
# if order by commodity
d_long[order(d_long$price_series, d_long$year, d_long$period),]

metal_wb <- arrange(d_long, price_series, year, period)
metal_wb


# metal from mb -----

# when loading the data, be careful with the decimal
# make sure to keep the .01, .02 after the main code

metal_mb <- read.csv("~/Documents/Data/unctad/6801_MetalBulletin_patched.csv", dec=",")
head(metal_mb)

# how many different ones
metal_mb$CommodityProduct |> unique() |> length() # 16
metal_mb$CommodityProduct |> table() # some code have more than others

wbref_metal # 9

meta |> head()

unique_metals <- unique(metal_mb$CommodityProduct)
meta[price_series %in% unique_metals]


# 261100.01-02: not in wb
# check quality 

copper_mb <- filter(metal_mb, CommodityProduct == '260300.01')
copper_wb <- filter(metal_wb, price_series == '260300.01')

head(copper_mb)
nrow(copper_mb)
head(copper_wb)

# should keep the time (year, month) together



#as.yearmon('1995M01', "%YM%m")
#as.Date(as.yearmon('1995M01', "%YM%m"))

copper_mb$time <- paste0(copper_mb$Year, copper_mb$Period)
copper_mb$dtime <- as.Date(as.yearmon(copper_mb$time, "%YM%m"))

# set value as numeric value
copper_mb$Value <- as.numeric(copper_mb$Value)
str(copper_mb)

p <- ggplot(copper_mb, aes(x = dtime, y = Value))
p <- p + geom_line()
p

# plot the values for wb

copper_wb$time <- paste0(copper_wb$year, copper_wb$period)
copper_wb$dtime <- as.Date(as.yearmon(copper_wb$time, "%YM%m"))
# set value
copper_wb$value <- as.numeric(copper_wb$value)


p <- ggplot(copper_wb, aes(x = dtime, y = value))
p <- p + geom_line()
p


# plot these two together ---- 
head(copper_mb)

mb <- select(copper_mb, 
             id = CommodityProduct, 
             source = DataSource,
             # source_label = DataSourceLabel,
             dtime = dtime,
             value = Value)
head(mb)


wb <- select(copper_wb, 
             id = price_series, 
             source = data_source,
             # source_label = "WorldBank",
             dtime = dtime,
             value = value)
head(mb)

copper_merge <- rbind(
  mb, wb  
)


# plot together
p <- ggplot(copper_merge, aes(x = dtime, y = value, colour = source))
p <- p + geom_line()
p

# wb$value / mb$value |>  plot()
# potentially difference in exchange rate?


# compare wb mb ----
metal_mb

# metal bulletin
metal_mb$time <- paste0(metal_mb$Year, metal_mb$Period)
metal_mb$dtime <- as.Date(as.yearmon(metal_mb$time, "%YM%m"))
metal_mb$Value <- as.numeric(metal_mb$Value)


# world bank
metal_wb$time <- paste0(metal_wb$year, metal_wb$period)
metal_wb$dtime <- as.Date(as.yearmon(metal_wb$time, "%YM%m"))
metal_wb$value <- as.numeric(metal_wb$value)



# ID 

filter(metal_wb, price_series == 3)
metal_mb


get_commodity_pair <- function(data_mb, 
                                data_wb, 
                                commodity_id){
  
  # commodity_id <- '260300.01'
  # data_mb <- metal_mb
  # data_wb <- metal_wb
  # check if ID exists in the data
  # if not, return error 
  if(!commodity_id %in% unique(data_mb$CommodityProduct)){
    stop('Commodity not available in MetalBulletin')
  }
  if(!commodity_id %in% unique(data_wb$price_series)){
    stop('Commodity not available in WorldBank')
  }
  

  mb <- filter(data_mb, CommodityProduct == commodity_id)
  wb <- filter(data_wb, price_series == commodity_id)
  
  # select subset of data
  mb <- select(mb, 
               id = CommodityProduct, 
               source = DataSource,
               # source_label = DataSourceLabel,
               dtime = dtime,
               value = Value)
  # head(mb)
  wb <- select(wb, 
               id = price_series, 
               source = data_source,
               # source_label = "WorldBank",
               dtime = dtime,
               value = value)
  # head(mb)
  
  dmerge <- rbind(
    mb, wb  
  )
  
  return(dmerge)
}

wbref_metal

# copper 260300.01
# nickel 260400.01
# platium 711000.01 # 3 sources

# silver 261600.01
# gold 710800.01
# plat 711000.01


# for copper:: 
# unit 
# currency
# changes in copper market


test <- get_commodity_pair(data_mb = metal_mb,
                           data_wb = metal_wb,
                           commodity_id = '260300.01')

test |> head()

# plot 
p <- ggplot(# filter(test, source == 5110), 
  test, 
            aes(x = dtime, 
                      y = value, 
                      colour = source))
p <- p + geom_line()
p



# 8401


# iron ----
# data: thomson reuter 
# it is also in MB
# double check!
# code: 260100.02, 03
# verdict: very similar to both, can use 02 directly after 2010.9

iron <- list('', '', 64)

tr <- read_excel("~/Documents/Data/unctad/7901_Thomson-Reuters.xlsx")
tr$CommodityProduct |> table()

# iron is a combination of 2
# can split and check
iron_tr02 <- filter(tr, CommodityProduct %in% c( '260100.02'))
iron_tr03 <- filter(tr, CommodityProduct %in% c( '260100.03'))


# check the difference 
tail(iron_tr02) # contains 2025
View(iron_tr02)
View(iron_tr03)
plot(iron_tr02$Value)
plot(iron_tr03$Value)
# slight difference, but both starting from 2010.9


# wb
iron_wb <- d[, c(1, 64)]
iron_wb$iron <- as.numeric(iron_wb$`Iron.ore,.cfr.spot`)

View(iron_wb)
# 2010.9 corresponds to row 189 in iron_wb

# plot

plot(iron_tr02$Value, type = 'l')
lines(iron_wb$iron[189:360], col = 'red')

plot(iron_tr03$Value, type = 'l')
lines(iron_wb$iron[189:360], col = 'red')

# very trivial difference, can use 02 directly



# ____ COAL ____ ----
# 7901
# code: 270100.02
# data source is thomson reuter
# it is the australian price: col 6

tr <- read_excel("~/Documents/Data/unctad/7901_Thomson-Reuters.xlsx")
# tr$CommodityProduct |> table()


coal_tr <- filter(tr, CommodityProduct == '270100.02')
View(coal_tr)

# two sources 
coal_au <- list('', '', 6)
coal_sa <- list('', '', 7)

coal_wbau <- d[, c(1, 6)]
coal_wbsa <- d[, c(1, 7)]

coal_wbau$coal <- as.numeric(coal_wbau$`Coal,.Australian`)
coal_wbsa$coal <- as.numeric(coal_wbsa$`Coal,.South.African.**`)

plot(coal_tr$Value, type = 'l')
lines(coal_wbau$coal, col = 'red')
lines(coal_wbsa$coal, col = 'green')




# ____ FOODS ____ ----
meta

# load wb data
data <- openxlsx::read.xlsx("~/Documents/Data/unctad/CMO-Historical-Data-Monthly.xlsx", 
                            sheet = "Monthly Prices", startRow = 5)

# only take those above 1995
data$year <- as.numeric(substr(data$X1, 1, 4))
d <- filter(data, year >=1995)

# ISO 2401: sugar
# ICO 1901: coffee
# IGC 6901: maize, wheat
# ICoO 1801: cocoa
# IRSG: rubber
# (these are the ones WB has)
# check which one is the corresponding series 

# sugar ----
# code: 170100.01
# sugar_world
# differ by 45 times


sugar_eu <- list('', 'Sugar_eu', 46)
sugar_us <- list('', 'Sugar_us', 47)
sugar_world <- list('', 'Sugar_world', 48) # this is the one

sugar_iso <- read_excel("~/Documents/Data/unctad/2401_ISO_2024.xlsx")
sugar_iso$CommodityProductLabel[1]

sugar_iso |> head()
sugar_iso$CommodityProduct |> table() 


# only after 1995
sugar_wb <- d[, c(1, 46, 47, 48)]
sugar_wb |> head()

# some processing, make it easy to plot
# iso
sugar_iso$time <- paste0(sugar_iso$Year, sugar_iso$Period)
sugar_iso$dtime <- as.Date(as.yearmon(sugar_iso$time, "%YM%m"))
sugar_iso$Value <- as.numeric(sugar_iso$Value)


# world bank
sugar_wb$dtime <- as.Date(as.yearmon(sugar_wb$X1, "%YM%m"))
sugar_wb$value_eu <- as.numeric(sugar_wb$`Sugar,.EU`)
sugar_wb$value_us <- as.numeric(sugar_wb$`Sugar,.US`)
sugar_wb$value_world <- as.numeric(sugar_wb$`Sugar,.world`)


plot(sugar_iso$Value)
plot(sugar_wb$value_eu)
plot(sugar_wb$value_us)
plot(sugar_wb$value_world)

sugar_iso$Value / sugar_wb$value_world  # differ by 45
ratio <- sugar_iso$Value / sugar_wb$value_world 
plot(ratio)



# cocoa ----
# ICCO 1801: cocoa

# code: 180100.01

cocoa <- list('180100.01', 'Cocoa', 12)

cocoa_icco <- read_excel("~/Documents/Data/unctad/1801_ICCO.xlsx")
cocoa_icco |> head()
cocoa_icco$CommodityProduct |> table() # 372 values, as it has 2025


# only after 1995
cocoa_wb <- d[, c(1, 12)]
cocoa_wb |> head()

# some processing, make it easy to plot
# iso
cocoa_icco$time <- paste0(cocoa_icco$Year, sugar_iso$Period)
cocoa_icco$dtime <- as.Date(as.yearmon(cocoa_icco$time, "%YM%m"))
cocoa_icco$Value <- as.numeric(cocoa_icco$Value)

# world bank
cocoa_wb$dtime <- as.Date(as.yearmon(cocoa_wb$X1, "%YM%m"))
cocoa_wb$cocoa <- as.numeric(cocoa_wb$Cocoa)

plot(cocoa_icco$Value)
plot(cocoa_wb$cocoa)

ratio <- cocoa_icco$Value[1:360] / cocoa_wb$cocoa 
plot(ratio)
# slight difference, but mostly around 45



# maize, wheat ----

# IGC 6901: maize, wheat
# maize:
# wheat: 

d_igc <- read_excel("~/Documents/Data/unctad/6901_IGC.xls", 
                        sheet = "ready for SAM")


d_igc$CommodityProduct |> table()

# 4 types
# only one in wb

maize <- list('', 'Maize', 31)
wheat_srw <- list('', 'Wheat_srw', 37)
wheat_hrw <- list('', 'Wheat_hrw', 38)




# only after 1995
maize_wb <- d[, c(1, 31)]
maize_wb |> head()

wheat_wb <- d[, c(1, 37, 38)]
wheat_wb |> head()

# igc 

g1 <- filter(d_igc, CommodityProduct == '100500.01') # maize
g2 <- filter(d_igc, CommodityProduct == '100500.02') # maize this is closer
g3 <- filter(d_igc, CommodityProduct == '100100.01') # wheat
g4 <- filter(d_igc, CommodityProduct == '100100.01') # wheat

grain <- g4
grain$time <- paste0(grain$Year, grain$Period)
grain$dtime <- as.Date(as.yearmon(grain$time, "%YM%m"))
grain$Value <- as.numeric(grain$Value)

grain |> head()

# world bank maize
maize_wb$dtime <- as.Date(as.yearmon(maize_wb$X1, "%YM%m"))
maize_wb$maize <- as.numeric(maize_wb$Maize)


plot(grain$Value)

plot(maize_wb$maize)
plot(grain$Value, maize_wb$maize)

# world bank wheat

wheat_wb$dtime <- as.Date(as.yearmon(wheat_wb$X1, "%YM%m"))
wheat_wb$ws <- as.numeric(wheat_wb$`Wheat,.US.SRW`)
wheat_wb$wh <- as.numeric(wheat_wb$`Wheat,.US.HRW`)

plot(grain$Value)
plot(wheat_wb$ws)
plot(wheat_wb$wh)

# both close enough
plot(wheat_wb$ws, wheat_wb$wh) # not really fundamentally different



# coffee ----
# ICO 1901: coffee
# world bank price is $/kg

coffee_ico <- read.csv("~/Documents/Data/unctad/1901_ICO_using_R_script.csv")
coffee_ico |> head()
str(coffee_ico)
coffee_ico$CommodityProduct |> table()


# 1,2,3,4,5,6 all available

c1 <- filter(coffee_ico, CommodityProduct == 90100.01) # NO
c2 <- filter(coffee_ico, CommodityProduct == 90100.02) # NO
c3 <- filter(coffee_ico, CommodityProduct == 90100.03) # close enough
c4 <- filter(coffee_ico, CommodityProduct == 90100.04) # should be the one
c5 <- filter(coffee_ico, CommodityProduct == 90100.05) # 
c6 <- filter(coffee_ico, CommodityProduct == 90100.06) # should be the one


coffee_arabica <- list('090100.04', '', 13)
coffee_robust <- list('090100.06', '', 14)


# only after 1995
coffee_wb <- d[, c(1, 13, 14)]
coffee_wb |> head()
coffee_wb$coffee_ara <- as.numeric(coffee_wb$`Coffee,.Arabica`)
coffee_wb$coffee_rob <- as.numeric(coffee_wb$`Coffee,.Robusta`)


# find which one is arabica (1-4)
coffee <- c4
coffee$Value <- as.numeric(coffee$Value)
# should be between 3 and 4, both close enough
plot(c3$Value, c4$Value)
c3$Value / c4$Value

plot(coffee_wb$coffee_ara, c(coffee$Value, 0))



# robusta

c5$Value <- as.numeric(c5$Value)
c6$Value <- as.numeric(c6$Value)

plot(c5$Value, c6$Value)


plot(coffee_wb$coffee_rob, c(c5$Value, 0))
plot(coffee_wb$coffee_rob, c(c6$Value, 0))



# banana ----
# 1603, 7901, wb
# actually banana is only in 7901, not 1603 (only jute and sisal)
# fao <- read_excel("~/Documents/Data/unctad/1603_FAO_Jute-Sisal.xlsx", sheet = "ready for SAM")
# fao$CommodityProduct |> table() 

# 7901: thomson reuters
# 1603: fao

tr <- read_excel("~/Documents/Data/unctad/7901_Thomson-Reuters.xlsx")
tr$CommodityProduct |> table()

banana_tr <- filter(tr, CommodityProduct == '080300.03')

# wb 
# banana europe only available from 1996
# banana us is complete

banana_eu <- list('', '', 39)
banana_us <- list('', '', 40)

# only after 1995
banana_wb <- d[, c(1, 39, 40)]

banana_wb$banana_eu <- as.numeric(banana_wb$`Banana,.Europe`)
banana_wb$banana_us <- as.numeric(banana_wb$`Banana,.US`)


# should be just us
plot(tr$Value)
plot(banana_wb$banana_us)
plot(banana_wb$banana_eu)




# ____ AGRI ____ ----

# rubber ----
# IRSG 8301: rubber

irsg <- read_excel("~/Documents/Data/unctad/8301_IRSG.xlsx", sheet = "ready for SAM")
irsg$CommodityProduct |> table() # just one, 400100.01

irsg |> head()

# wb
rubber_tsr20 <- list('400100.01', '', 56)
rubber_rss3 <- list('400100.02', '', 57)

rubber_wb <- d[, c(1, 56, 57)]
rubber_wb |> head()

# rubber tsr20 does not have before 1998, only from 1999
# rubber rss3 available at all times
# data from irsg is only for tsr20

plot(irsg$Value)

rubber_wb$value_tsr20 <- as.numeric(rubber_wb$`Rubber,.TSR20.**`)
rubber_wb$value_rss3 <- as.numeric(rubber_wb$`Rubber,.RSS3`)

plot(rubber_wb$value_tsr20)
plot(rubber_wb$value_rss3)

plot(irsg$Value, rubber_wb$value_rss3) # very similar

sum(is.na(rubber_wb$value_tsr20))

plot(irsg$Value, rubber_wb$value_tsr20) # very similar
plot(rubber_wb$value_rss3, rubber_wb$value_tsr20) # very similar


# jute -----
# FAO historical

fao <- read_excel("~/Documents/Data/unctad/1603_FAO_Jute-Sisal.xlsx", 
                  sheet = "ready for SAM")
fao$CommodityProduct |> table() 
fao$CommodityProductLabel |> table()
fao

"530300.01"

jute <- filter(fao, CommodityProduct == "530300.01")

plot(jute$Value, type = 'l')


# FAO FPMA automatic sources
# only available from 2004.1 to 2024.4

# library(tidyverse)
library(jsonlite)
library(httr)

#note that FPMA is a slightly different data set than FAO normal data so the API access is different
#return all international price series from FPMA
FPMA_int <- GET("https://fpma.fao.org/giews/v4/price_module/api/v1/FpmaSerieInternational/")
FPMA_int <- FPMA_int$content %>% 
  rawToChar() %>% 
  fromJSON() %>% 
  .$results

FPMA_int |> head()

jute <-FPMA_int %>% #filters anything with dairy in. Includes cheese and the two milk powders
  filter(grepl('Jute',commodity_name))
jute$uuid
jute$commodity_name

jute <- GET(paste0("https://fpma.fao.org/giews/v4/price_module/api/v1/FpmaSeriePrice/",jute$uuid,"/"))
jute <- fromJSON(rawToChar(jute$content))

jute$datapoints |> head()
head(jute$datapoints)
tail(jute$datapoints)
jute <- jute$datapoints %>% 
  select(2,4,6)
# only from 2004.1 to 2024.1



# this is how to get data for dairy
# which is not used
# dairy<-FPMA_int %>% #filters anything with dairy in. Includes cheese and the two milk powders
#   filter(grepl('Dairy',commodity_name))
# dairy<-cbind(dairy$uuid,dairy$commodity_name)
# 
# Skim_Milk<-GET(paste0("https://fpma.fao.org/giews/v4/price_module/api/v1/FpmaSeriePrice/",dairy[1,1],"/"))
# Skim_Milk<-fromJSON(rawToChar(Skim_Milk$content))
# Skim_Milk<-Skim_Milk$datapoints %>% 
#   select(2,4,6)
# head(Skim_Milk)
# Skim_Milk$S3<-"022"









