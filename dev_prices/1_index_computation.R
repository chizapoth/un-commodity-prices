# harvester for new, automated sources
# validation data is in another script

# library(data.table)
library(openxlsx)
library(readxl)
library(jsonlite)
library(httr)
library(zoo)
library(dplyr)
library(naniar) # missing data vis

# source the functions needed
source('~/Documents/GitHub/un-commodity-prices/dev_prices/util.R')

# set data paths
read_path <- '~/Documents/GitHub/un-commodity-prices/data-raw/'
dir_metadata <- 'metadata/'
dir_datasource_2024 <- 'datasource_2024/'
dir_datasource_2025 <- 'datasource_2025/'

result_path <- '~/Documents/GitHub/un-commodity-prices/results/'



# metadata ----

metadata <- read.xlsx(paste0(read_path, dir_metadata, 'commodity_metadata.xlsx'), sheet = 'commodity')
source <- read.xlsx(paste0(read_path, dir_metadata, 'commodity_metadata.xlsx'), sheet = 'source')

# View(metadata)
head(metadata)



# 5110 World bank ----
# also do necessary processing, such as datetime
# this needs to be replaced with URL

wb_link <- 'https://thedocs.worldbank.org/en/doc/18675f1d1639c7a34d463f59263ba0a2-0050012025/related/CMO-Historical-Data-Monthly.xlsx'

# wb_raw <- read.xlsx(paste0(read_path, 
#                            dir_datasource_2025, 
#                            "CMO-Historical-Data-Monthly.xlsx"), 
#                           sheet = "Monthly Prices", startRow = 5)

wb_raw <- read.xlsx(wb_link, 
                    sheet = "Monthly Prices", startRow = 5)


# print the column names and location
wb_var <- get_info_wb(wb_raw)
# View(wb_var)



# process
# make the values numeric
wb <- process_data_wb(data = wb_raw)



# select relevant series, based on metadata

# this also has to be within the wb scope

# View(meta)
# View(wb_info)
wb_info <- filter(metadata, data_source_2025_code == 5110 & !is.na(label_source_2025))
# double check if this is what we need

# process the labels to remove the special characters
wb_labels <- wb_info$label_source_2025
wb_labels <- gsub(',', '.', wb_labels) # substitute the commas
wb_labels <- gsub('\\*', '.', wb_labels) # substitute the star (careful since it's wildcard)
wb_labels <- gsub('%', '.', wb_labels) # substitute the commas
wb_labels

# select based on names
wb_narrow <- select(wb, year, period, time, datetime, all_of(wb_labels))
head(wb_narrow)

# reset the colnames
colnames(wb_narrow)[5:ncol(wb_narrow)]
colnames(wb_narrow)[5:ncol(wb_narrow)] <- wb_info$description_short




# 2311 IMF ----

imf_link <- 'https://www.imf.org/-/media/Files/Research/CommodityPrices/Monthly/external-data.ashx'

# to use the link, need to first download the data
# can not direclty import the excel sheet since it's ashx format
imf_loc <- tempfile()
download.file(imf_link, imf_loc)

# to read xls requires readxl::read_excel
# imf_raw <- read_excel(paste0(read_path, dir_datasource_2025, "imf.xls"))

imf_raw <- read_excel(path = imf_loc)


# check variables
imf_var <- get_info_imf(imf_raw)
# View(imf_var)

# process data
imf <- process_data_imf(imf_raw)

# need to fill in Manganese
imf <- fill_imf_name(data = imf, 
                     keyword = 'Mang', 
                     col_to_fill = '...92', 
                     fill_name = 'PMANG')

# select 

imf_info <- filter(metadata, data_source_2025_code == 2311 & 
                     !is.na(label_source_2025) & keep == 'yes')
imf_info
imf_narrow <- select(imf, year, datetime, all_of(imf_info$label_source_2025))

imf_narrow

# reset name 
colnames(imf_narrow)[3:ncol(imf_narrow)]
colnames(imf_narrow)[3:ncol(imf_narrow)] <- imf_info$description_short



# 1603 FAO FPMA ----

# first get metadata
fpma_api <- GET("https://fpma.fao.org/giews/v4/price_module/api/v1/FpmaSerieInternational/")
fpma_raw <- fromJSON(rawToChar(fpma_api$content))
# str(fpma_raw)
fpma_data <- fpma_raw$results




# get information for jute
# do the same for other commodity if needed
jute_info <- filter(fpma_data, grepl('Jute', commodity_name))

jute_raw <- GET(paste0("https://fpma.fao.org/giews/v4/price_module/api/v1/FpmaSeriePrice/",jute_info$uuid,"/"))
jute <- fromJSON(rawToChar(jute_raw$content))

head(jute$datapoints)
tail(jute$datapoints)

# only from 2004.1 to 2024.1
# convert to consistent format
jute <- data.frame(jute$datapoints[, c('date', 'price_value_dollar')])
colnames(jute)[1] <- 'datetime'
colnames(jute)[2] <- 'jute'
jute$datetime <- as.Date(jute$datetime)



# merge -----
# join wb, imf, jute
dcommodity <- left_join(wb_narrow, imf_narrow) |> 
  left_join(jute)


colnames(dcommodity)



# basic info ----
# two aspects: last updated time + missing
# if missing a lot, also backfill with historical data


head(dcommodity)


# visualise which series are missing
commodity_only <- select(dcommodity, -c(year, period, time, datetime))
vis_miss(commodity_only) 


# more details for the missing series
check_missing_period(data = dcommodity, tag = 'shrimps_mex')
check_missing_period(data = dcommodity, tag = 'sunflower_oil')
check_missing_period(data = dcommodity, tag = 'palmkernel_oil')
check_missing_period(data = dcommodity, tag = 'rubber_tsr20')
check_missing_period(data = dcommodity, tag = 'manganese_99')
check_missing_period(data = dcommodity, tag = 'jute')



# these require backfill
dcompare <- readRDS(paste0(result_path, 'prices_2024_compare.rds'))
head(dcompare)

# find a way to match the products then fill in the missing data
d <- merge_price_new_old(data_new = dcommodity, 
                         data_old = dcompare, 
                         info_target = checklist[1,])


checklist <- openxlsx::read.xlsx(paste0(result_path, 'checklist.xlsx'))
# backfill_missing <- function(data_old)
# consideration: unit difference have to be dealt with 







# trouble shooting
# something wrong happened when merging 

# d1 <- wb_narrow[, 1:5]
# d2 <- imf_narrow[, 1:5]
# head(d1)
# head(d2)
# test <- left_join(d1, d2)
# head(test)

# save for future use
# write.xlsx(dcommodity, file = paste0(result_path, 'prices_2025_precheck.xlsx'))
# write.csv(dcommodity, file = paste0(write_path, 'prices_2025_precheck.csv'))






# _________ -----
# compute index -----


# rebase the prices, 2015 as 100
# use the average of 2015 price 
# View(metadata)
head(metadata)

# weights 
weights <- read.xlsx(paste0(read_path, dir_metadata, 'weights.xlsx'),
                     sheet = 'weight_reference')
head(weights)
tail(weights)

# shares sum up to 1
weights$s |> sum()
weights$s |> round(4)




# take necessary columns and rows
# join the metadata and weights together
colnames(metadata)
m <- filter(metadata, keep == 'yes') |> 
  select(index_sort, 
         series_id, 
         description_short, 
         within_product_weight, 
         share_scale)

m |> head()


ws <- filter(weights, available == 'yes') |> 
  select(index_sort, 
         group, 
         subgroup,
         index_description,
         s)

nrow(m) # 46
nrow(ws) # 44

# View(m)
# only 1101 (coffee), 4201 (crude oil) have more frequency
table(m$index_sort) |> sort()


M <- full_join(m, ws, by = 'index_sort')
# View(M)

# standardize based on 2015 as 100
# compute the within group 

basis_2015 <- filter(dcommodity, year == 2015) |> 
  select(-c(year, period, time, datetime)) |> 
  apply(MARGIN = 2, mean) 

basis_2015 <- data.frame(basis_2015)
basis_2015$description_short <- rownames(basis_2015)

# merge it to M
M2 <- left_join(M, basis_2015, by = 'description_short')



# original price matrix -----

dcommodity |> head()

# percentage missing 
apply(dcommodity, 2, function(x){sum(is.na(x))})


# select only relevant columns from commodity
dcwide <- select(dcommodity, -c(year, period, time, datetime))
# still want to keep track of the time information

rownames(dcwide) <- dcommodity$datetime
head(dcwide)



# grouping ----
# grouping information
# process grouping 
# level 0: all
# level 1: group
# level 2: subgroup and combinations


grouping <- read.xlsx(paste0(read_path, dir_metadata, 'weights.xlsx'), 
                               sheet = 'validation_unctad')


grouping

# select one combination
# from 1 to 14
cg <- query_commodity_group(target_group_info = grouping[14,])


index_onegroup <- compile_index(d_price = dcwide,
                      d_weight_unscaled = M2,
                      commodity_group = cg)


# do a forloop for all 14 groups

indices <- list()

for(i in 1:nrow(grouping)){
  # select group info
  cg <- query_commodity_group(target_group_info = grouping[i,])
  # compute index
  index <- compile_index(d_price = dcwide,
                         d_weight_unscaled = M2,
                         commodity_group = cg)
  indices[[i]] <- index$index
}


names(indices) <- grouping$group_name

# indices$all_excl_precious_metal_fuels
# collect together 
index_matrix <- do.call(cbind, indices)
View(index_matrix)





# __________ ----
# validate -----

price_public <- read.csv("~/Documents/GitHub/un-commodity-prices/data-raw/datasource_2025/US.CommodityPriceIndices_M_20250317_100020.csv")
View(price_public)

head(price_public)
# check names
colnames(price_public)

# process datetime 
datetime_public <- process_historic_time(time_vec = price_public$Period_Label)


grouping

# process the unctad published data for consistent names

indices_val <- list()

for(i in 1:nrow(grouping)){
  ind <- extract_historic_single(data = price_public, 
                          public_name = grouping[i,]$unctad_name,
                          new_name = grouping[i,]$group_name)
  indices_val[[i]] <- ind
}

indices_validate <- do.call(cbind, indices_val)
indices_validate <- data.frame(indices_validate)
# bigger difference: mineral_ore_metal_non_precious_metal

dim(indices_validate)

# attach datetime as datetime
indices_validate$datetime <- datetime_public

head(indices_validate)



# make long formatted for both public and ours
# ours
index_matrix <- data.frame(index_matrix)
index_matrix$datetime <- rownames(index_matrix)
index_long <- tidyr::pivot_longer(index_matrix, -datetime)
index_long$type <- 'Auto'


# public 

indices_validate_long <- tidyr::pivot_longer(indices_validate, -datetime)
indices_validate_long$type <- 'UNCTAD public'




# combine these two 
indices_both <- rbind(index_long, indices_validate_long)
indices_both$datetime <- as.Date.character(indices_both$datetime)


saveRDS(indices_both, file = paste0(result_path, 'indices_both.rds'))


# plot ----

library(ggplot2)

tag <- 'all'
title <- 'All commodities'
pd <- filter(indices_both, name == tag)

p <- ggplot(pd, aes(x = datetime, y = value, colour = type))
p <- p + geom_line()
p


# put into function
grouping
plot_index_comparison(data = indices_both, 
                      tag = 'all', 
                      title = 'All commodities')

plot_index_comparison(data = indices_both, 
                      tag = 'all_food', 
                      title = 'All food')

plot_index_comparison(data = indices_both, 
                      tag = 'food', 
                      title = 'Food')

plot_index_comparison(data = indices_both, 
                      tag = 'tropical_beverages', 
                      title = 'Tropical beverages')

plot_index_comparison(data = indices_both, 
                      tag = 'tropical_beverages_food', 
                      title = 'Tropical beverages, food')

plot_index_comparison(data = indices_both, 
                      tag = 'vegetable_oilseeds_oil', 
                      title = 'Vegetable, oilseeds, oil')

plot_index_comparison(data = indices_both, 
                      tag = 'agricultural_raw_material', 
                      title = 'Agricultural raw material')

plot_index_comparison(data = indices_both, 
                      tag = 'minerals_ore_metal', 
                      title = 'Minerals ore metal')

plot_index_comparison(data = indices_both, 
                      tag = 'minerals_ore_metal_non_precious_metal', 
                      title = 'Minerals ore metal (non precious metal)')

plot_index_comparison(data = indices_both, 
                      tag = 'precious_metal', 
                      title = 'Precious metal')

plot_index_comparison(data = indices_both, 
                      tag = 'fuels', 
                      title = 'Fuels')


plot_index_comparison(data = indices_both, 
                      tag = 'all_excl_fuels', 
                      title = 'All (excluding fuels)')

plot_index_comparison(data = indices_both, 
                      tag = 'all_excl_precious_metal', 
                      title = 'All (excluding precious metal)')

plot_index_comparison(data = indices_both, 
                      tag = 'all_excl_precious_metal_fuels', 
                      title = 'All (excluding precious metal, fuels)')









