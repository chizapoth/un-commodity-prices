# COMPARE INDIVIDUAL PRICE SERIES BETWEEN TWO DATA SOURCES

# (please use the document outline functionality in Rstudio to navigate the sections)
# this R script contains the following sections:

# 1. make checklist so we know which ones are changed
# 2. compile data from old sources (up to 2024.12)
# 3. validate single commodity from new and old sources
# 4. make tables
# 5. (DEV ONLY) trouble shoot



# load necessary packages

library(ggplot2)
library(dplyr)
library(data.table)
library(openxlsx)


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
# section 1: make checklist
# these are useful so that we know which data sources are updated



# make checklist ----

metadata <- read.xlsx(paste0(read_path, dir_metadata, 'commodity_metadata.xlsx'), 
                      sheet = 'commodity')

# View(metadata)

# select the ones we need: only compare two sources
set_newsource <- filter(metadata, check_two_sources == 'yes')

# might need some processing of names
# set_newsource$label_source_2025 <- gsub(',', '.', set_newsource$label_source_2025) # substitute the commas
# set_newsource$label_source_2025 <- gsub('\\*', '.', set_newsource$label_source_2025) # substitute the star (careful since it's wildcard)
# set_newsource <- gsub('%', '.', wb_labels) # substitute the commas

colnames(set_newsource)



# ____________ ----

# 2024 source ----

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


# require dplyr
# select variables from the master data
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

dcompare <- data.table::data.table(dcompare)
dcompare <- dcompare[order(dcompare$CommodityProduct, dcompare$dtime)]

# drop NA
dcompare <- dcompare[-which(is.na(dcompare$CommodityProduct))]
dcompare

# if needed:
# saveRDS(dcompare, file = paste0(read_path, dir_val, 'prices_2024_compare.rds'))


# dcompare <- readRDS(paste0(read_path, dir_val, 'prices_2024_compare.rds'))




# ____________ ----

# section 3: validate single series


# load data: new source ----

dcommodity <- readRDS(paste0(read_path, dir_val, 'dcommodity.rds'))
dcommodity |> head()

# datetime check and convert (if needed)
# dcommodity$datetime <- check_date_convert(dcommodity$datetime)




# validate one pair ----


# need to use the information from metadata

info_target <- set_newsource[10,]
info_target


# select the target product from both data sources

dout <- merge_price_new_old(data_new = dcommodity, 
                             data_old = dcompare, 
                             info_target = info_target)

dout$dnew |> head()
# dout$dold
# dout$dold
# dout$dnew

d <- dout

# make plots
p1 <- plot_comparison_price(dobj = d)
p1



# if needed

# plot for the standardized (both)
plot_comparison_price_facet(dobj = d)
plot_comparison_price_facet(dobj = d, free_scale = T)


# table individual product 
library(gt)
make_commodity_infocard(set_newsource[4,])






# ____________ ----

# additional information -----
# put it in a gt
library(gt)

# table 1: data source codes ----

source <- read.xlsx(paste0(read_path, dir_metadata, 'commodity_metadata.xlsx'), sheet = 'source')

source

tb <- gt(source)
# tab_header(tb, title = 'stuff')
cols_label(tb, 
           code = 'Code', 
           label = 'Label', 
           label_short = 'Label (short)')



# table 2: commodity to compare ----

metadata <- read.xlsx(paste0(read_path, dir_metadata, 'commodity_metadata.xlsx'), 
                      sheet = 'commodity')

View(metadata)

# select the ones we need: only compare two sources
set_newsource <- filter(metadata, check_two_sources == 'yes')

# might need some processing of names
# set_newsource$label_source_2025 <- gsub(',', '.', set_newsource$label_source_2025) # substitute the commas
# set_newsource$label_source_2025 <- gsub('\\*', '.', set_newsource$label_source_2025) # substitute the star (careful since it's wildcard)
# set_newsource <- gsub('%', '.', wb_labels) # substitute the commas

colnames(set_newsource)

# create a copy of checklist in case needed
checklist <- select(set_newsource, 
                    # take the following columns
                    index_sort, 
                    series_id, 
                    description_short,
                    description_long,
                    unit_2024, 
                    data_source_2024_code,
                    data_source_2024,
                    data_source_2025_code,
                    data_source_2025,
                    label_source_2025,
                    label_display)

# write.xlsx(checklist, file = paste0(result_path, 'checklist.xlsx'))



# narrow down further if needed
checklist_narrow <- select(checklist,
                           description_short, 
                           series_id, 
                           data_source_2024_code,
                           data_source_2025_code)


# the information below are summary information on the comparison
good_match <- c('fish_salmon',
                'nickel',
                'aluminum',
                'lead',
                'zinc',
                'tin',
                'coal',
                'gold',
                'platinum',
                'wheat', 
                'maize')

unit_difference <- c('coffee_arabica',
                     'coffee_robusta',
                     'sugar',
                     'cocoa',
                     'copper',
                     'silver',
                     'wool_fine',
                     'rubber_rss3')

period_difference <- c('manganese_99',
                       'iron_ore',
                       'wool_fine',
                       'rubber_rss3')

no_comparison <- c('banana',
                   'tobaco',
                   'hides',
                   'jute')


checklist_narrow$value_match <- 'Good'
checklist_narrow$availability_match <- 'Good'

# replace a few values based on the condition
cl <- mutate(checklist_narrow, value_match = replace(value_match, description_short %in% no_comparison, 'No comparison'))
cl < mutate(cl, value_match = replace(value_match, description_short %in% unit_difference, 'Unit difference'))

cl <- mutate(cl, availability_match = replace(availability_match, description_short %in% no_comparison, 'No comparison'))
cl <- mutate(cl, availability_match = replace(availability_match, description_short %in% period_difference, 'Period difference'))


cl

# write.xlsx(cl, file = paste0(result_path, 'checklist_report.xlsx'))


# put into table
tb2 <- gt(cl)

tb2 <- cols_label(tb2, 
                  description_short = 'Product', 
                  series_id = 'Product ID', 
                  data_source_2024_code = 'Data Source (2024)',
                  data_source_2025_code = 'Data Source (2025)',
                  value_match = 'Values matched?',
                  availability_match = 'Availability matched?')
tb2


# ____________ ----


# DEV ONLY: trouble shoot  ----


# wool ----

info_target <- filter(set_newsource, description_short == 'wool_fine')
info_target


# dcommodity |> coln
wool <- merge_price_new_old(data_new = dcommodity, 
                             data_old = dcompare,
                             info_target = info_target,
                             series_id = '510100.03')

wool$dboth$data_source |> table()
p1 <- plot_comparison(dobj = wool)
p1
plot_facet(p1, target = 'data_source', free_scale = T)




# manganese ----
# 260200.02
info_target <- filter(set_newsource, description_short == 'manganese_99')


# both series available
filter(mb_narrow, CommodityProduct == '260200.02')
filter(mb_narrow, CommodityProduct == '260200.01') # not this!!

manga <- merge_commodity_data(data_new = dcommodity, 
                             data_old = dcompare,
                             info_target = info_target,
                             series_id = '260200.02')

p1 <- plot_comparison(dobj = test)
p1

plot_facet(p1, target = 'data_source', free_scale = T)





# hides ----
# does not have historical data?
info_target <- filter(set_newsource, description_short == 'hides')


hides <- merge_commodity_data(data_new = dcommodity, 
                              data_old = dcompare,
                              info_target = info_target)


# 410100.01
# filter(dcompare, CommodityProduct == '410100.02')

plot_comparison(dobj = hides)

