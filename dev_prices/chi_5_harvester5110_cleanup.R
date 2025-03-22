# clean up the code from chi_4_harvester_5110

# ____ master data _____ ----
# index ----
# this is the index from the manual 
library(data.table)
library(openxlsx)

meta <- read.xlsx('./dev/metadata/commodity_metadata.xlsx')
View(meta)
meta <- data.table(meta)
head(meta)
unique(meta$datasource_code)
unique(meta$price_series)


# also do necessary processing, such as datetime
# 5110 (worldbank) ----

wb <- openxlsx::read.xlsx("~/Documents/Data/unctad/CMO-Historical-Data-Monthly.xlsx", 
                            sheet = "Monthly Prices", startRow = 5)
head(wb)
colnames(wb)
# View(wb)
# checker for commodity names
col_names <- colnames(wb)
col_id <- 1:ncol(wb)
commodity_ref <- data.frame(col_id, col_names)
commodity_ref


# only take those above 1995
wb$year <- as.numeric(substr(wb$X1, 1, 4))
wb$period <- substr(wb$X1, 5, 7)
wb <- filter(wb, year >=1995)

# world bank
wb$time <- paste0(wb$year, wb$period)
wb$dtime <- as.Date(as.yearmon(wb$time, "%YM%m"))

wb |> head()

# old data to validate 
# 6801 (metalbulletin) ---- 

mb <- read.csv("~/Documents/Data/unctad/6801_MetalBulletin_patched.csv", dec=",")
head(mb)

mb$CommodityProduct |> unique() # 16 unique

mb |> head()

# transform time and values
mb$time <- paste0(mb$Year, mb$Period)
mb$dtime <- as.Date(as.yearmon(mb$time, "%YM%m"))
mb$Value <- as.numeric(mb$Value)

mb <- data.table(mb)
mb <- mb[order(mb$CommodityProduct, mb$dtime)]
# corresponds to 6801
# meta[datasource_code == 6801] # 16 unique
# meta[datasource_code == 6801, price_series] # matches the mb data source


# (thompson reuters) -----

tr <- read_excel("~/Documents/Data/unctad/7901_Thomson-Reuters.xlsx")
tr$CommodityProduct |> table()

tr$time <- paste0(tr$Year, tr$Period)
tr$dtime <- as.Date(as.yearmon(tr$time, "%YM%m"))
tr$Value <- as.numeric(tr$Value)



# iso (sugar) ----

sugar_iso <- read_excel("~/Documents/Data/unctad/2401_ISO_2024.xlsx")
sugar_iso$CommodityProductLabel[1]

# icco ----






# ________ -----


# manganese <- list('260200.01', 'manganese_44', NA)

# copper ----

# wb: $ per mt
# mt: gbp 

copper <- list('260300.01', 'Copper', 65)

d1 <- select(wb, dtime, tag = Copper)
d1$tagv <- as.numeric(d1$tag)
d2 <- filter(mb, CommodityProduct == '260300.01') |> select(dtime, Value)

plot(d1$tagv, type = 'l')
lines(d2$Value, col = 'red')

ratio <- d1$tagv / d2$Value
ratio |> plot()





# nickel ----

nickel <- list('260400.01', 'Nickel', 68)


# extract corresponding pairs

d1 <- select(wb, dtime, tag = Nickel)
d1$tagv <- as.numeric(d1$tag)
d2 <- filter(mb, CommodityProduct == '260400.01') |> select(dtime, Value)

plot(d1$tagv, type = 'l')
lines(d2$Value, col = 'red')




# aluminum ----
aluminum <- list('260600.01', 'Aluminum', 63)


d1 <- select(wb, dtime, tag = Aluminum)
d1$tagv <- as.numeric(d1$tag)
d2 <- filter(mb, CommodityProduct == '260600.01') |> select(dtime, Value) 

plot(d1$tagv, type = 'l')
lines(d2$Value, col = 'red')


# lead ----
lead <- list('260700.02', 'Lead', 66)

d1 <- select(wb, dtime, tag = Lead)
d1$tagv <- as.numeric(d1$tag)
d2 <- filter(mb, CommodityProduct == '260700.02') |> select(dtime, Value) 

plot(d1$tagv, type = 'l')
lines(d2$Value, col = 'red')



# zinc -----
zinc <- list('260800.01', 'Zinc', 69)

d1 <- select(wb, dtime, tag = Zinc)
d1$tagv <- as.numeric(d1$tag)
d2 <- filter(mb, CommodityProduct == '260800.01') |> select(dtime, Value) 

plot(d1$tagv, type = 'l')
lines(d2$Value, col = 'red')


# tin ----
tin <- list('260900.01', 'Tin', 67)


d1 <- select(wb, dtime, tag = Tin)
d1$tagv <- as.numeric(d1$tag)
d2 <- filter(mb, CommodityProduct == '260900.01') |> select(dtime, Value) 

plot(d1$tagv, type = 'l')
lines(d2$Value, col = 'red')


# silver ----

silver <- list('261600.01', 'Silver', 72)

d1 <- select(wb, dtime, tag = Silver)
d1$tagv <- as.numeric(d1$tag)
d2 <- filter(mb, CommodityProduct == '261600.01') |> select(dtime, Value) 

plot(d1$tagv, type = 'l')
lines(d2$Value/100, col = 'red')




# gold ----
gold <- list('710800.01', 'Gold', 70)

d1 <- select(wb, dtime, tag = Gold)
d1$tagv <- as.numeric(d1$tag)
d2 <- filter(mb, CommodityProduct == '710800.01') |> select(dtime, Value) 

plot(d1$tagv, type = 'l')
lines(d2$Value, col = 'red')


# platinum ----
platinum <- list('711000.01', 'Platinum', 71)


d1 <- select(wb, dtime, tag = Platinum)
d1$tagv <- as.numeric(d1$tag)
d2 <- filter(mb, CommodityProduct == '711000.01') |> select(dtime, Value) 

plot(d1$tagv, type = 'l')
lines(d2$Value, col = 'red')




# _______ -----


# sugar ----

# sugar_eu <- list('', 'Sugar_eu', 46)
# sugar_us <- list('', 'Sugar_us', 47)
sugar_world <- list('170100.01', 'Sugar_world', 48) # this is the one

sugar_iso <- read_excel("~/Documents/Data/unctad/2401_ISO_2024.xlsx")

sugar_iso |> head()
sugar_iso$CommodityProduct |> table() 


# some processing, make it easy to plot
# iso
sugar_iso$time <- paste0(sugar_iso$Year, sugar_iso$Period)
sugar_iso$dtime <- as.Date(as.yearmon(sugar_iso$time, "%YM%m"))
sugar_iso$Value <- as.numeric(sugar_iso$Value)

d1 <- select(wb, dtime, tag = `Sugar,.world`)
d1$tagv <- as.numeric(d1$tag)
d2 <- filter(sugar_iso, CommodityProduct == '170100.01') |> select(dtime, Value) 

# differ by 45 times
plot(d1$tagv, type = 'l')
lines((d2$Value /45), col = 'red')
plot(d2$Value, type = 'l')


# cocoa ----
# it is ok



# maize ----
commodity_ref
d_igc <- read_excel("~/Documents/Data/unctad/6901_IGC.xls", 
                    sheet = "ready for SAM")


d_igc$CommodityProduct |> table()

d_igc$time <- paste0(d_igc$Year, d_igc$Period)
d_igc$dtime <- as.Date(as.yearmon(d_igc$time, "%YM%m"))
d_igc$Value <- as.numeric(d_igc$Value)



maize <- list('100500.02', 'Maize', 31)

# # only after 1995
# maize_wb <- d[, c(1, 31)]
# maize_wb |> head()


d1 <- select(wb, dtime, tag = Maize)
d1$tagv <- as.numeric(d1$tag)

# igc 
g1 <- filter(d_igc, CommodityProduct == '100500.01') |> select(dtime, Value) 
g2 <- filter(d_igc, CommodityProduct == '100500.02') |> select(dtime, Value) 


plot(d1$tagv, type = 'l')
lines(g1$Value, col = 'red')
lines(g2$Value, col = 'blue')

# 100500.02 is closer to wb source



# wheat ----
# use 100100.01
commodity_ref
# 4 types
# only one in wb

wheat_srw <- list('', 'Wheat_srw', 37)
# wheat_hrw <- list('', 'Wheat_hrw', 38)

# using the US.SRW
d1 <- select(wb, dtime, tag = `Wheat,.US.HRW`)
d1$tagv <- as.numeric(d1$tag)

# igc 
g1 <- filter(d_igc, CommodityProduct == '100100.01') |> select(dtime, Value) 
g2 <- filter(d_igc, CommodityProduct == '100100.02') |> select(dtime, Value) 


plot(d1$tagv, type = 'l')
lines(g1$Value, col = 'red')
lines(g2$Value, col = 'blue')



# tobacco ----
commodity_ref
usda <- read.csv("~/Documents/Data/unctad/4901_USDA.csv")

usda$CommodityProduct |> table() # 240100.01

usda$time <- paste0(usda$Year, usda$Period)
usda$dtime <- as.Date(as.yearmon(usda$time, "%YM%m"))
usda$Value <- as.numeric(usda$Value)

tobacco <- list('240100.01', 'Tobacco', 49)

d1 <- select(wb, dtime, tag = `Tobacco,.US.import.u.v.`)
d1$tagv <- as.numeric(d1$tag)
d2 <- filter(usda, CommodityProduct == '240100.01') |> select(dtime, Value) 

plot(d1$tagv, type = 'l')
lines(d2$Value, col = 'red')


# jute (FAO FPMA) ----
# only available from 2004.1 until 2024.4
# check with data here:
# https://fpma.fao.org/giews/fpmat4/#/dashboard/tool/international

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







# ______ ISSUES _____ ----
# banana ----
# actually banana is only in 7901, not 1603 (only jute and sisal)


# banana_tr <- filter(tr, CommodityProduct == '080300.03')


# wb 
# banana europe only available from 1996
# banana us is complete

banana_eu <- list('', '', 39)
banana_us <- list('', '', 40)

# only after 1995
banana_wb <- d[, c(1, 39, 40)]

d1 <- select(wb, dtime, tag = `Banana,.US`)
d1$tagv <- as.numeric(d1$tag)
d2 <- filter(tr, CommodityProduct == '080300.03') |> select(dtime, Value) 

# eu prices from wb 
d3 <- select(wb, dtime, tag = `Banana,.Europe`)
d3$tagv <- as.numeric(d3$tag)

plot(d1$tagv, type = 'l')
lines(d2$Value, col = 'red')
lines(d3$tagv, col = 'green')

plot(d2$Value, type = 'l')





# rubber ----


irsg <- read_excel("~/Documents/Data/unctad/8301_IRSG.xlsx", sheet = "ready for SAM")
irsg$CommodityProduct |> table() # just one, 400100.01

irsg$time <- paste0(irsg$Year, irsg$Period)
irsg$dtime <- as.Date(as.yearmon(irsg$time, "%YM%m"))
irsg$Value <- as.numeric(irsg$Value)



# wb
rubber_tsr20 <- list('400100.01', '', 56)
rubber_rss3 <- list('400100.02', '', 57)

# rubber tsr20 does not have before 1998, only from 1999
# rubber rss3 available at all times
# data from irsg is only for tsr20
  
plot(irsg$Value)


tsr <- select(wb, dtime, tag = `Rubber,.TSR20.**`)
tsr$tagv <- as.numeric(tsr$tag)

# tsr, only available from 1999
plot(tsr$tagv, type = 'l')
lines(irsg$Value, col = 'red') # need to divide by a ratio

plot(irsg$Value, type = 'l')
ratio <- irsg$Value/tsr$tagv
plot(ratio, type = 'l')


# rss, available for all times
# probably rss closer to irsg 
rss <- select(wb, dtime, tag = `Rubber,.RSS3`)
rss$tagv <- as.numeric(rss$tag)


plot(rss$tagv, type = 'l')
plot(irsg$Value, col = 'red')

ratio <- irsg$Value/rss$tagv
plot(ratio, type = 'l')

# compare rss and tsr
plot(rss$tagv, type = 'l')
lines(tsr$tagv, col = 'blue')






