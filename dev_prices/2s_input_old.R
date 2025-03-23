# generate a unified table for input prices from old sources

library(data.table)
source('~/Documents/GitHub/un-commodity-prices/dev_prices/util.R')

# set data paths
read_path <- '~/Documents/GitHub/un-commodity-prices/data-raw/'
dir_metadata <- 'metadata/'
dir_datasource_2024 <- 'datasource_2024/'
dir_datasource_2025 <- 'datasource_2025/'

write_path <- '~/Documents/GitHub/un-commodity-prices/results/'


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

# drop NA
dcompare <- dcompare[-which(is.na(dcompare$CommodityProduct))]

# write.xlsx(dcompare, file = paste0(write_path, 'prices_2024_compare.xlsx'))
# write.csv(dcompare, file = paste0(write_path, 'prices_2024_compare.csv'))
saveRDS(dcompare, file = paste0(write_path, 'prices_2024_compare.rds'))



