# utilities for data processing, checking and index computation 

# process input ----

process_data_wb <- function(data){
  
  # transform into numeric
  # starting from second row
  d <- apply(data[2:nrow(data), 2:ncol(data)], 2, as.numeric)
  
  # rename X1 into time
  time <- data$X1[2:length(data$X1)]
  year <- as.numeric(substr(time, 1, 4))
  period <- substr(time, 5, 7)
  
  # assign date of the month
  # data$time <- paste0(data$year, data$period)
  datetime <- as.Date(as.yearmon(time, "%YM%m"))
  
  # bind together
  dwide <- data.frame(time = time, 
                      year = year,
                      period = period, 
                      datetime = datetime, 
                      d)
  
  res <- dplyr::filter(dwide, year >=1995)
  return(res)
}


# get_info_wb(wbraw)
get_info_wb <- function(data){
  # data <- wbraw
  # checker for commodity names
  # col_id <- 1:ncol(wb)
  res <- data.frame(commodity = colnames(data))
  return(res)
  
}


process_data_imf <- function(data, drop_m = T){
  
  # data <- imf
  # starting from row 4 in imf data
  d <- apply(data[4:nrow(data), 2:ncol(data)], 2, as.numeric)
  
  # rename X1 into time
  time <- data$Commodity[4:length(data$Commodity)]
  year <- as.numeric(substr(time, 1, 4))
  period <- substr(time, 5, 7)
  
  # assign date of the month
  # data$time <- paste0(data$year, data$period)
  datetime <- as.Date(as.yearmon(time, "%YM%m"))
  
  # bind together
  if(drop_m == F){
    # drop month and M
    # it is coded with 0 between, WB does not
    dwide <- data.frame(time = time, 
                        year = year,
                        period = period, 
                        datetime = datetime, 
                        d)
  }else{
    dwide <- data.frame(datetime = datetime, 
                        year = year,
                        d)
  }
  
  res <- dplyr::filter(dwide, year >=1995)
  
  return(res)
}



get_info_imf <- function(data){
  # data <- imf_raw
  info <- data[1, 2:ncol(data)]
  
  res <- data.frame(label = colnames(info), 
                    description = as.character(info[1,])) 
  return(res)
}


fill_imf_name <- function(data, keyword, col_to_fill, fill_name){
  
  # checks
  # manganese
  check <- grep(keyword, imf_var[which(imf_var$label == col_to_fill),]$description)
  
  if(check == 1){
    colnames(data)[which(colnames(data)== col_to_fill)] <- fill_name
  }
  
  return(data)
}



check_date_convert <- function(x){
  if(typeof(x) == 'double'){
    x <- openxlsx::convertToDate(x, origin = '1900-01-01')
  }
  return(x)
}



# compute index ----


compute_weighted_price <- function(d_price, d_weight, product, rescale = T){
  # product <- 'silver'
  # d_weight <- M3
  # d_price <- dcwide
  info <- filter(d_weight, description_short == product)
  basis_2015 <- info$basis_2015
  within_weight <- info$within_product_weight
  # scale is for computing using the weights from subgroups
  if(rescale == T){
    s <- info$s_rescale
  }else{
    s <- info$s
  }
  
  # select relevant series
  d <- select(d_price, all_of(product))
  
  # rebase 
  d_rebase <- d/basis_2015 * 100
  
  # multiply by s and within product weight
  d_weighted <- d_rebase * within_weight * s
  
  return(list(
    info = info, 
    d_rebase = d_rebase,
    d_weighted = d_weighted
  ))
  
}





# validate with old data ----


merge_price_new_old <- function(data_new, data_old, info_target, series_id = NULL){
  
  # data_new <- dcommodity
  # data_old <- dcompare
  commodity_name <- info_target$description_short
  commodity_desc <- info_target$description_long
  data_source_code_new <- info_target$data_source_2025_code
  commodity_series <- info_target$series_id
  label_display <- info_target$label_display
  
  # allow manually set
  if(!is.null(series_id)){
    commodity_series <- series_id
  }
  
  # data_source_label_new <- info_target$data_source_2025
  
  # new
  dnew <- select(data_new, 
                 datetime, 
                 value = all_of(commodity_name))
  
  # attach the new data source code
  dnew$data_source <- data_source_code_new

  # old
  dold <- filter(data_old, CommodityProduct == commodity_series) |> 
    select(datetime = dtime, 
           value = Value, 
           data_source = DataSource)
  dold$datetime <- as.character(dold$datetime)
  
  # put together
  dboth <- rbind(dnew, dold)
  dboth$data_source <- as.character(dboth$data_source)
  dboth$datetime <- as.Date(dboth$datetime)
  # str(dboth)
  
  return(list(dnew = dnew, 
              dold = dold,
              dboth = dboth, 
              commodity_name = commodity_name, 
              commodity_series = commodity_series, 
              commodity_desc = commodity_desc,
              label_display = label_display,
              data_source_code_2025 = data_source_code_new))
  
}



# validate with index ----

query_commodity_group <- function(target_group_info){
  
  # target_group_info <- validation_groups[6,]
  group_name <- target_group_info$group_name
  select_group_from <- target_group_info$select_group_from
  # define the groups
  # also make sure that we distinguish the ones from level 0,1,2
  master_list_level1 <- list(
    
    all = c('ALL FOOD', 
            'AGRICULTURAL RAW MATERIALS',
            'ALL MINERALS, ORES AND METALS',
            'FUELS'),
    
    # level 1
    all_food = c('ALL FOOD'),
    agricultural_raw_material = c('AGRICULTURAL RAW MATERIALS'),
    minerals_ore_metal = c('ALL MINERALS, ORES AND METALS'),
    fuels = c('FUELS'),
    all_excl_fuels = c('ALL FOOD', 
                       'AGRICULTURAL RAW MATERIALS',
                       'ALL MINERALS, ORES AND METALS')
    
  )
  
  # level 2 are those from subgroup
  master_list_level2 <- list(
    
    food = c('FOOD'),
    tropical_beverages = c('TROPICAL BEVERAGES'),
    vegetable_oilseeds_oil = c('VEGETABLE OILSEEDS AND OILS'),
    agriculture_raw_material = c('AGRICULTURAL RAW MATERIALS'),
    minerals_ore_metal_non_precious_metal = c('MINERALS, ORES AND METALS'),
    precious_metal = c('PRECIOUS METALS'),
    
    # combinations
    tropical_beverages_food = c('FOOD', 
                                'TROPICAL BEVERAGES'),
    
    
    all_excl_precious_metal = c('FOOD',
                                'TROPICAL BEVERAGES',
                                'VEGETABLE OILSEEDS AND OILS',
                                'AGRICULTURAL RAW MATERIALS',
                                'MINERALS, ORES AND METALS',
                                'GAS',
                                'COAL',
                                'PETROLEUM'),
    
    all_excl_precious_metal_fuels = c('FOOD',
                                      'TROPICAL BEVERAGES',
                                      'VEGETABLE OILSEEDS AND OILS',
                                      'AGRICULTURAL RAW MATERIALS',
                                      'MINERALS, ORES AND METALS')
  )
  
  
  # based on the group, select from master list 1 or 2
  if(select_group_from == 'level_1'){
    commodity_groups = master_list_level1[[group_name]]
    weight_ref_column <- 'group'
  }else{
    commodity_groups = master_list_level2[[group_name]]
    weight_ref_column <- 'subgroup'
  }
  
  return(list(target_group_info = target_group_info,
              commodity_groups = commodity_groups,
              weight_ref_column = weight_ref_column))
  
}




# plots ----



plot_comparison <- function(dobj){
  
  pd <- dobj$dboth
  p <- ggplot(pd, aes(x = datetime, 
                      y = value, 
                      colour = data_source))
  p <- p + geom_line(linewidth =1)
  # add 'today'
  p <- p + geom_vline(xintercept = Sys.Date(), 
                      col = 'blue', 
                      linetype = 'dashed', 
                      linewidth = 0.6)
  p <- p + labs(
    x = 'Datetime', 
    y = 'Price', 
    title = dobj$label_display,
    subtitle = paste0('Name in original source: ', dobj$commodity_desc)
  )
  # make white background
  p <- p + theme_bw() 
  # change text size
  p <- p + theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10), 
    plot.title = element_text(size = 12), 
    plot.subtitle = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
    legend.position = 'none' # drop legend
  )
  # adjust plotting of time
  p <- p + scale_x_date(date_breaks = "3 year", date_labels = "%Y")
  # change color
  p <- p + scale_color_brewer(palette = 'Set1')
  # p <- p + facet_wrap(~data_source, nrow = 3)
  return(p)
  
}


plot_facet <- function(pltobj, target, free_scale = F){
  
  # use .data[[colname]] to pass on customized names
  # p <- pltobj + facet_wrap( ~ .data[[target]])
  p <- pltobj + facet_grid(rows = vars(.data[[target]]))
  if(free_scale == T){
    # p <- pltobj + facet_wrap( ~ .data[[target]], nrow = r, scale = 'free')
    p <- pltobj + ggh4x::facet_grid2(rows = vars(.data[[target]]), 
                                     scales = "free_y", 
                                     independent = "y")
    
  }
  
  return(p)
}










