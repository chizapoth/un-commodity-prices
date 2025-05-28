# COMPUTE AND VALIDATE THE COMMODITY PRICE INDEX

# (please use the document outline functionality in Rstudio to navigate the sections)
# this R script contains the following sections:

# 1. load raw data and process
# 2. missing data handling
# 3. compute index from merged dataset from the new source
# 4. validation and plots




# load necessary packages

library(openxlsx)
library(readxl)
library(jsonlite)
library(httr)
library(dplyr)
library(naniar) # missing data vis
library(zoo) # datetime management


# set paths ----
# please set the correct directory for your machine


project_path <- '~/Documents/GitHub/un-commodity-prices/deliverables/'

# source the functions needed
source(paste0(project_path, 'rscripts/util.R'))

# set data paths
read_path <- paste0(project_path, 'data/')
dir_metadata <- 'metadata/'
dir_datasource_2024 <- 'datasource_2024/'
dir_val <- 'validation/'










# ____________ ----
# section 1: load and process raw data
# three data sources are used here: world bank, imf and fao
# in addition, metadata need to be loaded so that the column selection is simplified



# metadata ----
# metadata contains important information


metadata <- read.xlsx(paste0(read_path, dir_metadata, 'commodity_metadata.xlsx'), 
                      sheet = 'commodity')

# View(metadata)
head(metadata)


# 5110 World bank ----


# if you have the dataset downloaded to your machine, can load it directly
# wb_raw <- read.xlsx(paste0(read_path, 
#                            dir_datasource_2025, 
#                            "CMO-Historical-Data-Monthly.xlsx"), 
#                           sheet = "Monthly Prices", startRow = 5)

# read directly from online url
wb_link <- 'https://thedocs.worldbank.org/en/doc/18675f1d1639c7a34d463f59263ba0a2-0050012025/related/CMO-Historical-Data-Monthly.xlsx'
wb_raw <- read.xlsx(wb_link, 
                    sheet = "Monthly Prices", startRow = 5)


# print the column names
wb_var <- get_info_wb(wb_raw)
# View(wb_var)

#### TO DO: add counts ----
# e.g. 72 this month, if it's still 72 next month
# check colname changes 







# process the raw data
# make the values numeric
wb <- process_data_wb(data = wb_raw)



# select relevant series
# based on metadata

# this also has to be within the wb scope

wb_info <- filter(metadata, data_source_2025_code == 5110 & !is.na(label_source_2025) & keep == 'yes')
wb_info
# double check if this is what we need



# process the labels to remove the special characters
wb_labels <- wb_info$label_source_2025
wb_labels <- gsub(',', '.', wb_labels) # substitute the commas
wb_labels <- gsub('\\*', '.', wb_labels) # substitute the star (careful since it's wildcard)
wb_labels <- gsub('%', '.', wb_labels) # substitute the commas
wb_labels



# select based on the labels along with datetime 
wb_narrow <- select(wb, year, period, time, datetime, all_of(wb_labels))
head(wb_narrow)


# reset the colnames to short descriptions
# original names have special characters
colnames(wb_narrow)[5:ncol(wb_narrow)] # original name
colnames(wb_narrow)[5:ncol(wb_narrow)] <- wb_info$description_short




# 2311 IMF ----

# imf data api has been recently updated
# the link below is working as of 2025.4

# to import the data, we can not directly read the excel like we did for worldbank above
# can not directly import the excel sheet since it's ashx format

# the data has to be downloaded first, either to a known destination or tempfile()
# to read xls requires readxl::read_excel

imf_link <- 'https://www.imf.org/-/media/Files/Research/CommodityPrices/Monthly/external-data.ashx'

# imf_raw <- read_excel(paste0('YOUR_PATH', "imf.xls"))

imf_loc <- tempfile()
download.file(imf_link, imf_loc)
# read from temporary path
imf_raw <- read_excel(path = imf_loc)


# check variables
# note that the last variables are not available
# however users can still distinguish what it is from the description

imf_var <- get_info_imf(imf_raw)
# View(imf_var)



# process data
imf <- process_data_imf(imf_raw)

# need to fill in Manganese, as we know it is in the 92th place
imf <- fill_imf_name(data = imf, 
                     keyword = 'Mang', 
                     col_to_fill = '...92', 
                     fill_name = 'PMANG')


# select column based on metadata

imf_info <- filter(metadata, data_source_2025_code == 2311 & 
                     !is.na(label_source_2025) & keep == 'yes')
imf_info

# select datetime along with variables
imf_narrow <- select(imf, year, datetime, all_of(imf_info$label_source_2025))
imf_narrow

# reset name 
colnames(imf_narrow)[3:ncol(imf_narrow)]
colnames(imf_narrow)[3:ncol(imf_narrow)] <- imf_info$description_short



# 1603 FAO FPMA ----
# this data source is only used for jute
# it is not complete, so we might have to drop it entirely


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
# this step merges three data sources

# join wb, imf, jute
dcommodity <- left_join(wb_narrow, imf_narrow) |> 
  left_join(jute)


colnames(dcommodity)
# View(dcommodity)


# if you want, save it somewhere
year_month <- format(Sys.Date(), "%Y_%m")
saveRDS(dcommodity, paste0(read_path, dir_val, 'dcommodity_', year_month, '.rds'))







# ____________ ----

# check 1 ----

# flag for notable jumps: +- 30% for latest vs previous month (apr 2025 vs mar) 
# final check for indices

# drop jute
# View(dcommodity)
dcommodity <- select(dcommodity, -c(jute))

# select commodity only
commodity_only <- select(dcommodity, -c(year, period, time, datetime))

n <- nrow(commodity_only)
last_two <- commodity_only[c(n-1, n),]

# compute the percentage change
perc_change <- apply(last_two, 2, function(x){round((x[2] - x[1])/x[1],2)})

# put into a dataframe
perc_change <- data.frame(variable = names(perc_change), 
                          value = 100* perc_change)

p <- ggplot(perc_change, aes(x = variable, y = value)) 
p <- p + geom_bar(stat = 'identity') + ylim(c(31, -31))
p <- p + geom_hline(yintercept = c(30, -30), col = 'red')
p <- p + labs(
  x = 'Product names', 
  y = 'Change', 
  title = 'Relative percent change compared to previous month',
  subtitle = 'Threshold set to +- 30%. Investigate if exceeding the threshold'
)
p <- p + coord_flip()

p



# check 2 ----

# flag for revision: +- 30% for latest vs 1m ago for all individual series  (do the ratio of 
# two published series)


library(lubridate)

this_month <- format(Sys.Date(), "%Y_%m")
dcommodity_thismonth <- readRDS(paste0(read_path, dir_val, 'dcommodity_', this_month, '.rds'))

# pseudo-time stamped data
# dcommodity |> tail()
# create a copy that does not have the last month
# this is for development
# in practice, save the data with timestamp, load directly
# dcommodity_lastmonth <- dcommodity[-nrow(dcommodity),] 
# saveRDS(dcommodity_lastmonth, paste0(read_path, dir_val, 'dcommodity_', '2025_04', '.rds'))

last_month <- format(Sys.Date() %m-% months(1), "%Y_%m")
dcommodity_lastmonth <- readRDS(paste0(read_path, dir_val, 'dcommodity_', last_month, '.rds'))


# check if they differ by 1 row

if(nrow(dcommodity_thismonth) == nrow(dcommodity_lastmonth)+1){
  print('Number of rows checked, ok')
}else{
  print('Please double check the number of rows in the new data')
}

# check if columns match 
if(all.equal(colnames(dcommodity_thismonth), colnames(dcommodity_lastmonth))){
  print('Column names match, ok')
}else{
  print('Please double check the variables')
}


# compare by point-wise division
# remove the last line in this month's data
# divide

d_until_thismonth <- dcommodity_thismonth[-nrow(dcommodity_thismonth), ]
# colnames(d_until_thismonth)

# remove the time stamps
# commodity only
d_until_thismonth_co <- select(d_until_thismonth, -c(year, period, time, datetime))
d_lastmonth_co <- select(dcommodity_lastmonth, -c(year, period, time, datetime))

# pointwise division
dimension <- dim(d_lastmonth_co)

ratio <- matrix(nrow = dimension[1], ncol = dimension[2])
for(i in 1:dimension[2]){
  ratio[,i] <- d_until_thismonth_co[, i] / d_lastmonth_co[, i]
}
colnames(ratio) <- colnames(d_lastmonth_co)

# return the column name if anything exceeds +- 30%

id_abnormal <- which((ratio > 1.3) | (ratio < 0.7), arr.ind = T)
# which(ratio == 1, arr.ind = T)
# in this example nothing should return, as we have the exact same copies
if(nrow(id_abnormal) == 0){
  print('Checked ratio, nothing exceeds +- 30%, ok')
}else{
  print('Ratio between new and old are beyond +- 30%, check data sources')
}


# ____________ ----

# section 2: handle missing data


# check missing----
# two aspects: last updated time + missing
# if missing a lot, also backfill with historical data

# dcommodity <- readRDS(paste0(read_path, dir_val, 'dcommodity.rds'))
head(dcommodity)


# visualise which series are missing
commodity_only <- select(dcommodity, -c(year, period, time, datetime))
vis_miss(commodity_only) 


# more details for the missing series
# check_missing_period(data = dcommodity, tag = 'shrimps_mex')
check_missing_period(data = dcommodity, tag = 'sunflower_oil')
check_missing_period(data = dcommodity, tag = 'palmkernel_oil')
check_missing_period(data = dcommodity, tag = 'phosphate_rock')
# check_missing_period(data = dcommodity, tag = 'rubber_tsr20')
check_missing_period(data = dcommodity, tag = 'manganese_99')
check_missing_period(data = dcommodity, tag = 'jute')




# handle missing
# create a replacement dataset
# operate on this

dcommodity_filled <- dcommodity



# backfill -----
# 6801 manganese, use existing data

# load comparison data
dcompare <- readRDS(paste0(read_path, dir_val, 'prices_2024_compare.rds'))

# head(dcompare)
# manganese, use the one for 260200.02
# missing period in the wb data
manga_mp <- check_missing_period(data = dcommodity, tag = 'manganese_99')
manga_missing_period <- manga_mp$missing_datetime

manga_compare <- filter(
  dcompare, CommodityProduct == '260200.02' & dtime %in% manga_missing_period
)|> select(manganese_99 = Value, 
           datetime = dtime)



# select the price in the original data
manga_dcommodity <- select(dcommodity_filled, 
                   manganese_99, datetime)

manga_dcommodity
plot(manga_dcommodity$manganese_99)
plot(manga_compare$manganese_99)

# patch the same period in the comparison data

manga_filled <- rows_patch(manga_dcommodity, manga_compare, by = 'datetime')
plot(manga_filled$manganese_99)


# replace the filled series in dcommodity
dcommodity_filled$manganese_99 <- manga_filled$manganese_99





# discontinued impute ----- # 

# shrimp
# imf has shrimp, but price values are different
# imf 58

# dcommodity |> head()
# shrimp <- dcommodity$shrimps_mex
# plot(shrimp, type = 'l', main = 'wb shrimp')
# 
# 
# imf_var
# imf_raw[, 59]
# # PSHRI
# 
# shrimp_imf <- imf_raw[40:nrow(imf_raw), 59]
# lines(shrimp_imf, col = 'red')
# par(mfrow = c(1,1))
# plot(shrimp_imf, type = 'l', main = 'imf shrimp')



# sunflower oil (imf 65) ----

plot(dcommodity$sunflower_oil, type = 'l', main = 'wb sunflower')

sunflower_dcommodity <- select(dcommodity_filled, 
                               sunflower_oil, datetime)

sunflower_mp <- check_missing_period(data = dcommodity, tag = 'sunflower_oil')
sunflower_missing_period <- sunflower_mp$missing_datetime

# select sunflower oil in the imf data
sunflower_imf <- select(imf, sunflower_oil = PSUNO, datetime) |> 
  filter(datetime %in% sunflower_missing_period)


# patch the same period in the comparison data

sunflower_filled <- rows_patch(sunflower_dcommodity, sunflower_imf, by = 'datetime')
plot(sunflower_filled$sunflower_oil)
# replace the filled series in dcommodity
dcommodity_filled$sunflower_oil <- sunflower_filled$sunflower_oil




# palmkernel ----
plot(dcommodity$palmkernel_oil, type = 'l', main = 'wb palm kernel')


palmkernel_dcommodity <- select(dcommodity_filled, 
                                palmkernel_oil, datetime)

palmkernel_mp <- check_missing_period(data = dcommodity, tag = 'palmkernel_oil')
palmkernel_missing_period <- palmkernel_mp$missing_datetime


# select palm oil in the imf data
palm_imf <- select(imf, palmkernel_oil = PPOIL, datetime) |> 
  filter(datetime %in% palmkernel_missing_period)


# patch the same period in the comparison data

palmkernel_filled <- rows_patch(palmkernel_dcommodity, palm_imf, by = 'datetime')
plot(palmkernel_filled$palmkernel_oil)
# replace the filled series in dcommodity
dcommodity_filled$palmkernel_oil <- palmkernel_filled$palmkernel_oil





# phosphate_rock ----

# only 1 missing point
# to use the function, need to manually specify the variable name
# use only after visual inspection

phosphate_rock_filled <- impute_with_mean(data = dcommodity_filled,
                                          varname = 'phosphate_rock')
phosphate_rock_filled$d_tofill
phosphate_rock_filled$d_filled
dcommodity_filled$phosphate_rock <- phosphate_rock_filled

#### TO DO: impute with neighbors ----
# additional imputation: 
# latest missing: carry forward 







# double check filled data
vis_miss(dcommodity_filled)

# if wanted, save for future use
saveRDS(dcommodity_filled, paste0(read_path, dir_val, 'dcommodity_filled.rds'))





# ____________ ----

# section 3: compute index
# this section contains code for computing indices in different subgroups

# call it a name that will distinguish itself
# depending whether you want to use the filled data or not, 
# select either dcommodity or dcommodity_filled

dcommodity_filled <- readRDS(paste0(read_path, dir_val, 'dcommodity_filled.rds'))
dprices <- dcommodity_filled


# process metadata, weights -----


# weights 
weights <- read.xlsx(paste0(read_path, dir_metadata, 'weights.xlsx'),
                     sheet = 'weight_reference')
weights

# shares sum up to 1
weights$s |> sum()
weights$s |> round(4)



# take necessary columns and rows 

colnames(metadata)

# from metadata:

m <- filter(metadata, keep == 'yes') |> 
  select(index_sort, 
         series_id, 
         description_short, 
         within_product_weight, 
         share_scale)

m |> head()


# from weights:

ws <- filter(weights, available == 'yes') |> 
  select(index_sort, 
         group, 
         subgroup,
         index_description,
         s)

nrow(m)
nrow(ws) 

# View(m)
# only 1101 (coffee), 4201 (crude oil) have more frequency
# this is ok
table(m$index_sort) |> sort()



# join the metadata and weights together
M <- full_join(m, ws, by = 'index_sort')
# View(M)





# rebase prices 2015 ----
# rebase the prices, 2015 as 100
# use the average of 2015 price 


# compute 2015 average for each commodity

basis_2015 <- filter(dprices, year == 2015) |> 
  select(-c(year, period, time, datetime)) |> 
  apply(MARGIN = 2, mean) 

basis_2015 <- data.frame(basis_2015)
basis_2015$description_short <- rownames(basis_2015)
basis_2015

# merge it to M
M2 <- left_join(M, basis_2015, by = 'description_short')







# grouping ----
# grouping information

# level 0: all
# level 1: group
# level 2: subgroup and combinations

# grouping information is in the validation_unctad tab

grouping <- read.xlsx(paste0(read_path, dir_metadata, 'weights.xlsx'), 
                               sheet = 'validation_unctad')


grouping

# select one combination
# from 1 to 14
cg <- query_commodity_group(target_group_info = grouping[14,])
cg



# original price matrix
# select only relevant columns from dprices
dcwide <- select(dprices, -c(year, period, time, datetime))

# keep track of the time information, but put it as rowname rather than a column
rownames(dcwide) <- dprices$datetime
head(dcwide)



# compute index ----
# this step requires 3 pieces of information:
# price series (original scale)
# weights
# grouping information

index_onegroup <- compile_index(d_price = dcwide,
                                d_weight_unscaled = M2,
                                commodity_group = cg)


head(index_onegroup)
index_onegroup




# do a forloop for all 14 groups
indices <- list()

for(i in 1:nrow(grouping)){
  # select group info
  cg <- query_commodity_group(target_group_info = grouping[i,])
  # compute index
  # cg is changing while dcwide, M2 are the same
  index <- compile_index(d_price = dcwide,
                         d_weight_unscaled = M2,
                         commodity_group = cg)
  indices[[i]] <- index$index
}


names(indices) <- grouping$group_name

# indices$all_excl_precious_metal_fuels
# collect together 
index_matrix <- do.call(cbind, indices)
head(index_matrix)

# each column is one group



# ____________ ----

# section 4: validation
# this section contains code for validating the compiled indices against published data


# process published data -----

# download the unctad published dataset
# then load it 

price_public <- read.csv(paste0(read_path, dir_val, "US.CommodityPriceIndices_M_20250317_100020.csv"))
head(price_public)

# check names
colnames(price_public)

# process datetime 
datetime_public <- process_historic_time(time_vec = price_public$Period_Label)


# use the grouping from previous section
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



# make long formatted for both published and our compiled indices (auto)
# our compiled

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


# saveRDS(indices_both, file = paste0(read_path, dir_val, 'indices_both.rds'))


# plot ----

library(ggplot2)

tag <- 'all'
title <- 'All commodities'
pd <- filter(indices_both, name == tag)

p <- ggplot(pd, aes(x = datetime, y = value, colour = type))
p <- p + geom_line()
p


# put into function, and make it nicer looking

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



