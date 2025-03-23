# code for validating the new source 
library(ggplot2)
library(dplyr)
library(data.table)
source('~/Documents/GitHub/un-commodity-prices/dev_prices/util.R')

# set data paths
read_path <- '~/Documents/GitHub/un-commodity-prices/data-raw/'
dir_metadata <- 'metadata/'
dir_datasource_2024 <- 'datasource_2024/'
dir_datasource_2025 <- 'datasource_2025/'

result_path <- '~/Documents/GitHub/un-commodity-prices/results/'

# metadata ----

# select pairs 
metadata <- read.xlsx(paste0(read_path, dir_metadata, 'commodity_metadata.xlsx'), 
                      sheet = 'commodity')

View(metadata)

# select the ones we need: only compare two sources
set_newsource <- filter(metadata, check_two_sources == 'yes')

# might need some processing of names
# set_newsource$label_source_2025 <- gsub(',', '.', set_newsource$label_source_2025) # substitute the commas
# set_newsource$label_source_2025 <- gsub('\\*', '.', set_newsource$label_source_2025) # substitute the star (careful since it's wildcard)
# set_newsource <- gsub('%', '.', wb_labels) # substitute the commas



# create a copy of checklist in case needed
checklist <- select(set_newsource, 
                    # take the following columns
                    index_sort, 
                    series_id, 
                    description_short,
                    description_long,
                    unit_2024, 
                    data_source_2024_code,
                    data_source_2025_code,
                    label_source_2025,
                    label_display)

write.xlsx(checklist, file = paste0(result_path, 'checklist.xlsx'))



# further select if needed
checklist_narrow <- select(checklist,
                           description_short, 
                           series_id, 
                           data_source_2024_code,
                           data_source_2025_code)


# add some more information manually 

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

avail_difference <- c('manganese_99',
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
cl <- mutate(cl, availability_match = replace(availability_match, description_short %in% avail_difference, 'Period difference'))


write.xlsx(cl, file = paste0(result_path, 'checklist_report.xlsx'))








# load data ----
# new
dcommodity <- read.xlsx(paste0(result_path, 'prices_2025_precheck.xlsx'))
dcommodity |> head()

# datetime check and convert
dcommodity$datetime <- check_date_convert(dcommodity$datetime)


# old
dcompare <- readRDS(paste0(result_path, 'prices_2024_compare.rds'))








# validate one pair ----
# this is how to use it
info_target <- set_newsource[19,]
info_target

# dcompare |> filter(CommodityProduct == '400100.01')
# dcommodity |> coln


dout <- merge_price_new_old(data_new = dcommodity, 
                             data_old = dcompare, 
                             info_target = info_target)

dout$dnew |> head()
# dout$dold
dout$dold
dout$dnew

d <- dout
p1 <- plot_comparison(dobj = d)
p1



# if needed
# p1 + facet_wrap(~data_source, nrow = 3, scale = 'free')
plot_facet(p1, target = 'data_source', free_scale = F)
plot_facet(p1, target = 'data_source', free_scale = T)

# dout$commodity_name








# streamline if needed ----# 
# i <- 1
# 
# for(i in 1:nrow(set_newsource)){
#   
#   # i <- 1
#   info_target <- set_newsource[i]
#   
#   dout <- merge_price_new_old(data_new = dcommodity, 
# merge_price_new_olddata_old = dcompare, 
#                                info_target = info_target)
#   
#   # d_both <- dout$dboth
#   # save data 
#   # write.xlsx(dcommodity, file = '~/Documents/GitHub/unctad-int/reports/prices_new.xlsx')
#   savepath <- '~/Documents/GitHub/unctad-int/reports/results/'
#   
#   saveRDS(dout, file = paste0(savepath, i, '_', info_target$description_short, '.rds'))
#   
#   p1 <- plot_comparison(dobj = dout)
#   # p1
#   ggsave(filename = paste0(savepath, i, '_', info_target$description_short, '.png'), 
#          plot = p1, 
#          width = 15,
#          height = 15,
#          units = 'cm')
#   
#   
#   # if needed
#   # p1 + facet_wrap(~data_source, nrow = 3, scale = 'free')
#   p2 <- plot_facet(p1, target = 'data_source', r = 2, free_scale = T)
#   # plot_facet(p1, target = 'data_source', r = 3, free_scale = T)
#   ggsave(filename = paste0(savepath, i, '_', info_target$description_short, '_freescale.png'), 
#          plot = p2, 
#          width = 15,
#          height = 15,
#          units = 'cm')
#   
#   
# }


# ______ ----
# trouble shoot individual series ----

# a few series are problematic
# jute: no comparison
# wool: 510100.03 is available from 2013. can use imf directly
# hide: no comparison
# manga: need to backfill
# fish: ok




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






# ________ -----
# tables -----
# put it in a gt
library(gt)

# table 1: data source codes ----

dir_metadata <- '~/Documents/GitHub/un-commodity-prices/data-raw/metadata/'

source <- openxlsx::read.xlsx(paste0(dir_metadata, 'commodity_metadata.xlsx'), sheet = 'source')
source
tb <- gt(source)
# tab_header(tb, title = 'stuff')
cols_label(tb, 
           code = 'Code', 
           label = 'Label', 
           label_short = 'Label (short)')



# table 2: commodity to compare ----




# write.xlsx(cl, file = paste0(result_path, 'checklist.xlsx'))


# put into table
tb2 <- gt(cl)

tb2 <- cols_label(tb2, 
                  description_short = 'Product', 
                  series_id = 'Product ID', 
                  data_source_2024_code = 'Data Source (2024)',
                  data_source_2025_code = 'Data Source (2025)',
                  value_match = 'Values matched?',
                  availability_match = 'Availability matched?')

