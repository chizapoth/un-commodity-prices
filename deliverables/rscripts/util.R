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




check_missing_period <- function(data, tag){
  # data <- dcommodity
  # tag <- 'shrimps_mex'
  
  d_select <- select(data, datetime, all_of(tag))
  missing_datetime <- d_select$datetime[which(is.na(d_select[[tag]]))]
  n_missing <- length(missing_datetime)
  perc_missing <- n_missing / nrow(d_select)
  
  return(list(
    missing_datetime = missing_datetime, 
    n_missing = n_missing,
    perc_missing = perc_missing
  ))
  
}



impute_with_mean <- function(data, varname){
  
  # data <- dcommodity_filled
  # varname <- 'phosphate_rock'
  mean_val <- mean(data[[varname]], na.rm = T)
  n_impute <- sum(is.na(data[[varname]]))
  
  d_tofill <- data[[varname]]
  d_filled <- d_tofill
  d_filled[is.na(d_filled)] <- mean_val
  
  return(list(d_tofill = d_tofill,
              d_filled = d_filled,
              n_impute = n_impute, 
              mean_val = mean_val))
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


compute_scaled_weight <- function(d_weight_unscaled){
  # d_weight_unscaled <- m
  # only select the unduplicated
  # dplyr::filter(d_weight_unscaled, share_scale != 1)
  mn <- dplyr::filter(d_weight_unscaled, share_scale == 1)
  mn$s_rescale <- mn$s / sum(mn$s)
  
  # coffee
  if('coffee_robusta' %in% d_weight_unscaled$description_short){
    coffee_arabica <- dplyr::filter(mn, description_short == 'coffee_arabica')
    coffee_robusta <- dplyr::filter(d_weight_unscaled, description_short == 'coffee_robusta')
    coffee_robusta$s_rescale <- coffee_arabica$s_rescale  
    # attach back
    mn <- rbind(mn, coffee_robusta)
  }
  
  # oil
  if('crude_oil_dubai' %in% d_weight_unscaled$description_short){
    crude_oil_brent <- dplyr::filter(mn, description_short == 'crude_oil_brent')
    crude_oil_dubai <- dplyr::filter(d_weight_unscaled, description_short == 'crude_oil_dubai')
    crude_oil_dubai$s_rescale <- crude_oil_brent$s_rescale  
    # attach back
    mn <- rbind(mn, crude_oil_dubai)
  }
  return(mn)
}





compile_index <- function(d_price, d_weight_unscaled, commodity_group){
  
  info <- commodity_group$target_group_info
  select_group_from <- commodity_group$weight_ref_column
  commodity_groups <- commodity_group$commodity_groups
  
  m <- dplyr::filter(d_weight_unscaled, 
                     .data[[select_group_from]] %in% commodity_groups)
  
  # rescale the m weights
  # m$s_rescale <- m$s / sum(m$s)
  ms <- compute_scaled_weight(d_weight_unscaled = m)
  
  products <- ms$description_short
  reslist <- list()
  cat('compute index for group: ', info$group_name, '\n')
  
  for(i in 1:length(products)){
    reslist[[i]] <- compute_weighted_price(
      d_price = d_price,
      d_weight = ms,
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




# validate with single  ----


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



# validate with compiled index ----

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
  datetime <- as.character(datetime)
  
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

# plots ----





plot_comparison_price <- function(dobj){
  
  # dobj <- d
  
  # by default plot together
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
    subtitle = paste0('Original scale')
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
    legend.position = 'bottom' # if drop legend, use 'none'
  )
  # adjust plotting of time
  p <- p + scale_x_date(date_breaks = "3 year", date_labels = "%Y")
  # change color
  p <- p + scale_color_brewer(palette = 'Set1')
  # p <- p + facet_wrap(~data_source, nrow = 3)
  
  return(p)
  
}

plot_comparison_price_facet <- function(dobj, free_scale = F){
  
  # dobj <- d
  
  if(free_scale == T){
    # use the original values
    pd <- dobj$dboth
    # plot with facet
    p <- ggplot(pd, aes(x = datetime, 
                        y = value,
                        colour = data_source))
    
    subtitle_text <- 'Adjusted (original unit, not using 2015 basis)'
    
  }else{
    # require some processing
    # use 2015 as basis: need first 12 points
    # d$dnew$datetime[1:12]
    new <- dobj$dnew
    basis_2015_new <- mean(new$value[1:12])
    new$value_2015b <- new$value/basis_2015_new * 100
    
    # old
    old <- dobj$dold
    basis_2015_old <- mean(old$value[1:12])
    old$value_2015b <- old$value/basis_2015_old * 100
    
    # put together
    pd <- rbind(new, old)
    
    # plot with facet
    p <- ggplot(pd, aes(x = datetime, 
                        y = value_2015b,  # use 2015b
                        colour = data_source))
    
    subtitle_text <- 'Using 2015 average as basis'
  }
  
  p <- p + geom_line(linewidth =1)
  # add facet 
  # p <- p + facet_grid(rows = vars(data_source))
  p <- p + ggh4x::facet_grid2(rows = vars(data_source), 
                              scales = "free_y", 
                              independent = "y")
  
  # add 'today'
  p <- p + geom_vline(xintercept = Sys.Date(), 
                      col = 'blue', 
                      linetype = 'dashed', 
                      linewidth = 0.6)
  p <- p + labs(
    x = 'Datetime', 
    y = 'Price', 
    title = dobj$label_display,
    subtitle = subtitle_text
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
  
  return(p)
}




plot_index_comparison <- function(data, tag, title){
  # tag <- 'all'
  # title <- 'All commodities'
  # data <- indices_both
  pd <- filter(data, name == tag)
  
  p <- ggplot(pd, aes(x = datetime, 
                      y = value, 
                      colour = type))
  p <- p + geom_line(linewidth =0.8)
  # add 'today'
  p <- p + geom_vline(xintercept = Sys.Date(), 
                      col = 'blue', 
                      linetype = 'dashed', 
                      linewidth = 0.6)
  p <- p + labs(
    x = 'Datetime', 
    y = 'Index', 
    title = title# ,
    # subtitle = paste0('Original scale')
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
    legend.position = 'bottom', # keep legend
    legend.title = element_blank()
  )
  # adjust plotting of time
  p <- p + scale_x_date(date_breaks = "3 year", date_labels = "%Y")
  # change color
  p <- p + scale_color_brewer(palette = 'Set2')
  # p <- p + facet_wrap(~data_source, nrow = 3)
  return(p)
  
}




# tables ----

make_commodity_infocard <- function(info_target){
  # info_target <- info_target
  
  index_sort <-info_target$index_sort
  series_id <- info_target$series_id
  description_label <- info_target$label_display
  description_short <- info_target$description_short
  description_long <- info_target$description_long
  unit_2024 <- info_target$unit_2024
  # combine a few columns
  data_source_2024 <- paste(info_target$data_source_2024_code, 
                            info_target$data_source_2024)
  data_source_2025 <- paste(info_target$data_source_2025_code, 
                            info_target$data_source_2025)
  
  # put together
  d <- rbind(description_label, 
             description_short,
             index_sort, 
             series_id,
             description_long,
             unit_2024,
             data_source_2024,
             data_source_2025)
  
  rn <- rbind('Description', 
              'Description (code)',
              'Index sort',
              'Product ID',
              'Description (as of 2024)',
              'Unit (as of 2024)',
              'Data source (2024)',
              'Data source (2025)') 
  dw <- data.frame(cbind(rn, d))
  
  # make the column name as the first row, then remove it
  colnames(dw) <- dw[1,]
  dw <- dw[-1,]
  # make gt
  
  tb <- gt(dw)
  return(tb)
}



# NOT USED ----

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
    subtitle = paste0('Original scale')
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
  p <- p + labs(subtitle = 'Adjusted units')
  return(p)
}



