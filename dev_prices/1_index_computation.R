# harvester for new, automated sources
# validation data is in another script

# library(data.table)
library(openxlsx)
library(jsonlite)
library(httr)
library(readxl)
library(zoo)
library(dplyr)

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

wb_raw <- read.xlsx(paste0(read_path, 
                           dir_datasource_2025, 
                           "CMO-Historical-Data-Monthly.xlsx"), 
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


# TO DO: ----# 
# create identifiers without special characters
# for easy computation of index



# 2311 IMF ----

imf_raw <- read_excel(paste0(read_path, dir_datasource_2025, "imf.xls"))

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

# TO DO:
# some basic information about the results at this step


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

colnames(metadata)
m <- filter(metadata, keep == 'yes') |> 
  select(index_sort, 
         series_id, 
         description_short, 
         within_product_weight)

m |> head()


ws <- filter(weights, available == 'yes') |> 
  select(index_sort, 
         group, 
         subgroup,
         index_description,
         s)

nrow(m) # 47
nrow(ws) # 44

# View(m)
# only 1101 (coffee), 2010 (rubber) 4201 (crude oil) have more frequency
table(m$index_sort)


M <- full_join(m, ws, by = 'index_sort')
# View(M)




# price process -----

dcommodity |> head()

# percentage missing 
apply(dcommodity, 2, function(x){sum(is.na(x))})




# dclong <- tidyr::pivot_longer(dcommodity, 
#                               cols = -c('year', 'period', 'time', 'datetime'))



# standardize based on 2015 as 100
# compute the within group 


basis_2015 <- filter(dcommodity, year == 2015) |> 
  select(-c(year, period, time, datetime)) |> 
  apply(MARGIN = 2, mean) 

basis_2015 <- data.frame(basis_2015)
basis_2015$description_short <- rownames(basis_2015)

# merge it to M
M2 <- left_join(M, basis_2015, by = 'description_short')


# select only relevant columns
dcwide <- select(dcommodity, -c(year, period, time, datetime))
# still want to keep track of the time information

rownames(dcwide) <- dcommodity$datetime
head(dcwide)


# divide by 2015 values, multiply by 100
# dc_centered <- sweep(dcwide, 2, basis_2015) 
# dc_centered





dcwide

target <- d_weight[1,]
target

# test <- compute_weighted_price(d_price = dcwide,
#                                d_weight = M2,
#                                product = products[6])


# do it for all 47 series


# 1. index all -----

products <- M2$description_short
reslist <- list()

for(i in 1:length(products)){
  reslist[[i]] <- compute_weighted_price(
    d_price = dcwide,
    d_weight = M2,
    product = products[i], 
    rescale = F
  )
  cat('processing product ', i, '\n')
}

names(reslist) <- products


reslist$beef$d_weighted
# put together
list_weighted <- purrr::map(reslist, function(x){x$d_weighted})
mat_weighted <- do.call(cbind, list_weighted)


# compute index (omit missing)
index <- apply(mat_weighted, 1, function(x)(sum(x, na.rm = T)))
plot(index, type = 'l')



# 2. index level 1/2 group -----


View(M2)

M2$group |> table()


# process grouping 
# level 0: all
# level 1: group
# level 2: subgroup and combinations


validation_groups <- read.xlsx(paste0(read_path, dir_metadata, 'weights.xlsx'), 
                               sheet = 'validation_unctad')


validation_groups

cg <- query_commodity_group(target_group_info = validation_groups[4,])

# M2
cg
M2
View(M2)
test <- compile_index(d_price = dcwide,
                      d_weight_unscaled = M2,
                      commodity_group = cg)

plot(test$index, type = 'l')


lines(index, col = 'red')

# price_public$Tropical.beverages_Index_Base_2015_Value / index |> plot()



# __________ ----
# validate (all) -----

price_public <- read.csv("~/Documents/GitHub/unctad-int/dev_prices/datasource_2025/US.CommodityPriceIndices_M_20250317_100020.csv")
View(price_public)

head(price_public)
# check names
colnames(price_public)

lines(price_public$All.groups_Index_Base_2015_Value, col = 'red')
lines(price_public$All.food_Index_Base_2015_Value, col = 'red')
lines(price_public$Food_Index_Base_2015_Value, col = 'red')
lines(price_public$Tropical.beverages_Index_Base_2015_Value, col = 'red')
lines(price_public$Precious.metals_Index_Base_2015_Value, col = 'red')
lines(price_public$All.groups_Index_Base_2015_Value, col = 'red')
lines(price_public$Minerals..ores.and.non.precious.metals_Index_Base_2015_Value, col = 'red')
lines(price_public$Vegetable.oilseeds.and.oils_Index_Base_2015_Value, col = 'red')



# process the historical data 
colnames(price_public)

price_public$Period_Label

datetime_public <- process_historic_time(time_vec = price_public$Period_Label)










process_historic_time <- function(time_vec){
  # time_vec <- price_public$Period_Label
  year <- as.numeric(substr(time_vec, 6, 9))
  # month: only use the first 3 letters
  month_chr <- substr(time_vec, 1, 3)
  month_ref <- substr(month.name, 1,3) # built in
  # match the ref, convert to number
  month_num <- match(month_chr, month_ref)
  # put together
  time <- paste0(year, "M", month_num)
  datetime <- as.Date(as.yearmon(time, "%YM%m"))
  
  return(datetime)
}


extract_historic_single <- function(data, public_name, new_name){
  # data <- price_public
  # public_name <- "All.groups_Index_Base_2015_Value"
  # new_name <- 'all'
  series <- data[public_name]
  # change to a better name
  colnames(series) <- new_name
  return(series)
}




compile_index <- function(d_price, d_weight_unscaled, commodity_group){
  
  info <- commodity_group$target_group_info
  select_group_from <- commodity_group$weight_ref_column
  commodity_groups <- commodity_group$commodity_groups
  
  m <- dplyr::filter(d_weight_unscaled, 
                     .data[[select_group_from]] == commodity_groups)
  
  # rescale the m weights
  m$s_rescale <- m$s / sum(m$s)
  
  
  products <- m$description_short
  reslist <- list()
  cat('compute index for group: ', info$group_name, '\n')
  
  for(i in 1:length(products)){
    reslist[[i]] <- compute_weighted_price(
      d_price = d_price,
      d_weight = m,
      product = products[i],
      rescale = T
    )
    cat('processing product ', i, ': ',products[i], '\n')
  }
  
  names(reslist) <- products
  
  # put together
  list_weighted <- purrr::map(reslist, function(x){x$d_weighted})
  mat_weighted <- do.call(cbind, list_weighted)
  
  # compute index (omit missing)
  index <- apply(mat_weighted, 1, function(x)(sum(x, na.rm = T)))
  
  return(list(
    weight_matrix = m, 
    info = info,
    index = index
  ))
}




