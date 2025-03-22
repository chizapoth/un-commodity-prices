# code for validating the new source 
library(ggplot2)
library(dplyr)
source('~/Documents/GitHub/unctad-int/dev_prices/code_chi/util.R')

# set data paths
read_path <- '~/Documents/GitHub/unctad-int/dev_prices/'
dir_metadata <- 'metadata/'
dir_datasource_2024 <- 'datasource_2024/'
dir_datasource_2025 <- 'datasource_2025/'

write_path <- '~/Documents/GitHub/unctad-int/dev_prices/results/'




# load new data
dcommodity <- read.xlsx(paste0(write_path, 'prices_2025_precheck.xlsx'))
dcommodity |> head()


# qc new source alone ----
# should provide results for proportion of missing
# period of missing






# remember some series needs to be combined!






# load comparison data ----
# combine old source data into one
mb <- read.csv(paste0(read_path, dir_datasource_2024, "6801_MetalBulletin_patched.csv"), dec = ",")
tr <- read_excel(paste0(read_path, dir_datasource_2024, "7901_Thomson-Reuters.xlsx"))
sugar_iso <- read_excel(paste0(read_path, dir_datasource_2024, "2401_ISO_2024.xlsx"))
igc <- read_excel(paste0(read_path, dir_datasource_2024, "6901_IGC.xls"), sheet = "ready for SAM")
irsg <- read_excel(paste0(read_path, dir_datasource_2024, "8301_IRSG.xlsx"), sheet = "ready for SAM")
cocoa_icco <- read_excel(paste0(read_path, dir_datasource_2024, "1801_ICCO.xlsx"))
awi <- read_excel(paste0(read_path, dir_datasource_2024, "8001_AWI.xlsx"), sheet = "ready for SAM")
# codes needs to be modified slightly
# fish
ssb <- read.csv(paste0(read_path, dir_datasource_2024, "7801_StatNorway_from_API.csv"))
ssb$CommodityProduct <- paste0('0', ssb$CommodityProduct)
# coffee
coffee_ico <- read.csv(paste0(read_path, dir_datasource_2024, "1901_ICO_using_R_script.csv"))
coffee_ico$CommodityProduct <- paste0('0', coffee_ico$CommodityProduct)



# merge into one, check colnames first
colnames(mb)
colnames(tr)
colnames(sugar_iso)
colnames(igc)
colnames(irsg)
colnames(coffee_ico)
colnames(cocoa_icco)
colnames(awi)
colnames(ssb)

# only keep consistent names
keep_var <- c('CommodityProduct', 
              'DataSource', 
              'Year', 
              'Period',
              'Value')


mb_narrow <- select(mb, all_of(keep_var))
tr_narrow <- select(tr, all_of(keep_var))
sugar_narrow <- select(sugar_iso, all_of(keep_var))
igc_narrow <- select(igc, all_of(keep_var))
irsg_narrow <- select(irsg, all_of(keep_var))
coffee_narrow <- select(coffee_ico, all_of(keep_var))
cocoa_narrow <- select(cocoa_icco, all_of(keep_var))
awi_narrow <- select(awi, all_of(keep_var))
ssb_narrow <- select(ssb, all_of(keep_var))


# coffee_narrow$CommodityProduct |> table()

# bind together
dcompare <- rbind(mb_narrow, 
                  tr_narrow, 
                  sugar_narrow, 
                  igc_narrow, 
                  irsg_narrow, 
                  coffee_narrow, 
                  cocoa_narrow, 
                  awi_narrow, 
                  ssb_narrow)

head(dcompare)
# dcompare$CommodityProduct |> table()


# transform time and values

dcompare$time <- paste0(dcompare$Year, dcompare$Period)
dcompare$dtime <- as.Date(as.yearmon(dcompare$time, "%YM%m"))
dcompare$Value <- as.numeric(dcompare$Value)

dcompare <- data.table(dcompare)
dcompare <- dcompare[order(dcompare$CommodityProduct, dcompare$dtime)]

# tail(ssb_narrow)

# tail(dcompare)
dcompare[is.na(CommodityProduct)]


# drop NA
dcompare <- dcompare[-which(is.na(dcompare$CommodityProduct))]

# write.xlsx(dcompare, file = paste0(write_path, 'prices_2024_compare.xlsx'))

write.csv(dcompare, file = paste0(write_path, 'prices_2024_compare.csv'))
saveRDS(dcompare, file = paste0(write_path, 'prices_2024_compare.rds'))

# ________ ----
# comparison ----



# select pairs 
metadata <- read.xlsx(paste0(read_path, dir_metadata, 'commodity_metadata.xlsx'), 
                      sheet = 'commodity')

View(metadata)
# select the ones we need
set_newsource <- filter(metadata, check_two_sources == 'yes')

set_newsource$label_source_2025 <- gsub(',', '.', set_newsource$label_source_2025) # substitute the commas
set_newsource$label_source_2025 <- gsub('\\*', '.', set_newsource$label_source_2025) # substitute the star (careful since it's wildcard)
# set_newsource <- gsub('%', '.', wb_labels) # substitute the commas


# irsg_narrow$CommodityProduct |> table()


checklist <- select(set_newsource, 
       index_sort, 
       series_id, 
       description_long,
       unit_2024, 
       data_source_2024_code,
       data_source_2025_code,
       label_source_2025,
       label_display)



write.xlsx(checklist, file = paste0(write_path, 'checklist.xlsx'))




# this is how to use it
info_target <- set_newsource[19,]
info_target

# dcompare |> filter(CommodityProduct == '400100.01')
# dcommodity |> coln

dout <- merge_commodity_data(data_new = dcommodity, 
                             data_old = dcompare, 
                             info_target = info_target)

dout$dnew |> head()
# dout$dold
dout$dold
dout$dnew
p1 <- plot_comparison(dobj = d)
p1
# View(set_newsource)

d$dboth |> tail()
# if needed
# p1 + facet_wrap(~data_source, nrow = 3, scale = 'free')
plot_facet(p1, target = 'data_source', free_scale = F)
plot_facet(p1, target = 'data_source', free_scale = T)

# dout$commodity_name



# ggsave(filename = 'test.png', 
#        plot = p1, 
#        width = 15,
#        height = 15,
#        units = 'cm')

# units 
# dout$dnew$value |> plot()

# iron
# manganese
# generally the ones from IMF are problematic 







# streamline ----
i <- 1

for(i in 1:nrow(set_newsource)){
  
  # i <- 1
  info_target <- set_newsource[i]
  
  dout <- merge_commodity_data(data_new = dcommodity, 
                               data_old = dcompare, 
                               info_target = info_target)
  
  # d_both <- dout$dboth
  # save data 
  # write.xlsx(dcommodity, file = '~/Documents/GitHub/unctad-int/reports/prices_new.xlsx')
  savepath <- '~/Documents/GitHub/unctad-int/reports/results/'
  
  saveRDS(dout, file = paste0(savepath, i, '_', info_target$description_short, '.rds'))
  
  p1 <- plot_comparison(dobj = dout)
  # p1
  ggsave(filename = paste0(savepath, i, '_', info_target$description_short, '.png'), 
         plot = p1, 
         width = 15,
         height = 15,
         units = 'cm')
  
  
  # if needed
  # p1 + facet_wrap(~data_source, nrow = 3, scale = 'free')
  p2 <- plot_facet(p1, target = 'data_source', r = 2, free_scale = T)
  # plot_facet(p1, target = 'data_source', r = 3, free_scale = T)
  ggsave(filename = paste0(savepath, i, '_', info_target$description_short, '_freescale.png'), 
         plot = p2, 
         width = 15,
         height = 15,
         units = 'cm')
  
  
}


# ______ ----
# trouble shoot ----
# a few series are problematic
# jute: no comparison
# wool: 510100.03 is available from 2013. can use imf directly
# hide: no comparison
# manga: need to backfill
# fish: ok


# wool ----
set_newsource[24,]
awi_narrow$CommodityProduct |> table()

info_target <- set_newsource[23,]
info_target

plot(awi_narrow$Value)
# dcommodity |> coln
wool <- merge_commodity_data(data_new = dcommodity, 
                             data_old = dcompare,
                             info_target = set_newsource[23,],
                             series_id = '510100.03')

wool$dboth$data_source |> table()
p1 <- plot_comparison(dobj = wool)
p1
plot_facet(p1, target = 'data_source', free_scale = T)






# hides ----
# does not have historical data?
set_newsource[22,]

hides <- merge_commodity_data(data_new = dcommodity, 
                             data_old = dcompare,
                             info_target = set_newsource[22,])


# 410100.01
# filter(dcompare, CommodityProduct == '410100.02')

plot_comparison(dobj = hides)





# manganese ----
# 260200.02

set_newsource[11,]

mb_narrow

# both series available
filter(mb_narrow, CommodityProduct == '260200.02')
filter(mb_narrow, CommodityProduct == '260200.01') # not this!!

test <- merge_commodity_data(data_new = dcommodity, 
                             data_old = dcompare,
                             info_target = set_newsource[11,],
                             series_id = '260200.02')

p1 <- plot_comparison(dobj = test)
p1
View(set_newsource)

plot_facet(p1, target = 'data_source', free_scale = T)



# _________ ----
# VALIDATE THE INDEX ----
# validate the indices

library(readr)
prices <- read_csv("dev_prices/datasource_2025/US.CommodityPriceIndices_A_20250309_144518.csv")
prices
plot(prices$`All groups_Index_Base_2015_Value`, type = 'b')

prices$Commodity_Label |> table()
d <- filter(prices, Commodity_Label == 'All groups') 
points(d$Index_Base_2015_Value, type = 'l', col = 'red')


