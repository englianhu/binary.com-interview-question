## norm distribution generates highest ROI compare to snorm.
flsEWMA <- dir('./data', pattern = '.hybrid')

fundList <- llply(flsEWMA, function(dt) {
  fl = paste0('./data/', dt)
  if(file.size(fl) > 100) {
    cbind(Model = str_replace_all(dt, '.rds', ''), 
        readRDS(file = fl)) %>% tbl_df
  }})

#'@ names(fundList) <- sapply(fundList, function(x) xts::first(x$Model))
names(fundList) <- flsEWMA %>% str_replace_all('.rds', '')
fundList[sapply(fundList, is.null)] <- NULL

## Summary of ROI
gm.tbl <- ldply(fundList, function(x) { x %>% mutate(StartDate = xts::first(Date), LatestDate = last(Date), InitFund = xts::first(BR), LatestFund = last(Bal), Profit = sum(Profit), RR = LatestFund/InitFund) %>% dplyr::select(StartDate, LatestDate, InitFund, LatestFund, Profit, RR) %>% unique }) %>% tbl_df

gm.tbl %>% formattable(list(
    
    .id = color_tile('white', 'darkgoldenrod'), 
    
    LatestFund = formatter('span', style = x ~ formattable::style(color = ifelse(rank(-x) <= 3, 'green', 'gray')), x ~ paste0(round(x, 2), ' (rank: ', sprintf('%02d', rank(-x)), ')')), 
    
    Profit = color_tile('white', '#9B870C'), 
    
    RR = formatter('span', style = x ~ formattable::style(
	      color = ifelse(rank(-x) <= 3, 'green', 'gray')), x ~ sprintf('%1.2f%% (rank: %02d)', 100 * x, rank(-x)))))

 ## =============================================
tagList(
  tags$div(align = "center", 
           class = "bg-info", 
           tags$h3(class = "bg-primary", "Profit and Loss of Investment"), 
           tags$h5(align = "center", class = "text-muted", 
                   "Annual Stakes and Profit and Loss of Firm A at Agency A (2011-2015) ($0,000)")), 
  as.htmlwidget(gm.tbl %>% formattable(list(
    
    .id = color_tile('white', 'darkgoldenrod'), 
    
    LatestFund = formatter('span', style = x ~ formattable::style(color = ifelse(rank(-x) <= 3, 'green', 'gray')), x ~ paste0(round(x, 2), ' (rank: ', sprintf('%02d', rank(-x)), ')')), 
    
    Profit = color_tile('white', '#9B870C'), 
    
    RR = formatter('span', style = x ~ formattable::style(
	      color = ifelse(rank(-x) <= 3, 'green', 'gray')), x ~ sprintf('%1.2f%% (rank: %02d)', 100 * x, rank(-x)))))))
	
	
	