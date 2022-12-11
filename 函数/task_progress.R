task_progress <- function(scs = 60, .pattern = '^pred1', .loops = TRUE, 
                          .date = FALSE, .filter.completed.data = FALSE) {
  ## ------------- 定时查询进度 ----------------------
  ## 每分钟自动查询与更新以上模拟calC()预测汇价进度（储存文件量）。
  
  if (.loops == TRUE) {
    while(1) {
      cat('Current Tokyo Time :', as.character(now('Asia/Tokyo')), '\n')
      cat(.pattern, '\n\n')
      
      z <- ldply(mbase, function(dtm) {
        y = index(dtm)
        y = y[y >= timeID0]
        
        cr = as.character(unique(substr(names(dtm), 1, 6)))
        x = list.files(paste0('./data/fx/', cr), pattern = .pattern) %>% 
          str_extract_all('[0-9]{4}-[0-9]{2}-[0-9]{2}') %>% 
          unlist %>% as.Date %>% sort
        x = x[x >= y[1] & x <= xts::last(y)]
        
        if (.date == TRUE) {
          data.frame(.id = cr, date = xts::last(x), 
                     date_i = paste0(which(y == xts::last(x)), '/', length(y)))
        } else {
          data.frame(.id = cr, x = length(x), n = length(y)) %>% 
            mutate(progress = percent(x/n))
        }
      })# %>% tbl_df
      
      print(z)
      
      if (.date == TRUE) {
        prg = z$date_i %>% str_split_fixed('/', 2) %>% 
          tbl_df %>% mutate_all(as.numeric) %>% 
          colSums %>% t %>% tbl_df %>% mutate(pr = V1/V2) %>% 
          .[3] %>% unlist %>% percent
        
      } else {
        prg = sum(z$x)/sum(z$n)
      }
      
      cat('\n================', as.character(percent(prg)), '================\n\n')
      if (prg == 1) break #倘若进度达到100%就停止更新。
      
      Sys.sleep(scs) #以上ldply()耗时3~5秒，而休息时间60秒。
    }
  } else {
    
    cat('Current Tokyo Time :', as.character(now('Asia/Tokyo')), '\n')
    cat(.pattern, '\n\n')
    
    z <- ldply(mbase, function(dtm) {
      y = index(dtm)
      y = y[y >= timeID0]
      
      cr = as.character(unique(substr(names(dtm), 1, 6)))
      x = list.files(paste0('./data/fx/', cr), pattern = .pattern) %>% 
        str_extract_all('[0-9]{4}-[0-9]{2}-[0-9]{2}') %>% 
        unlist %>% as.Date %>% sort
      x = x[x >= y[1] & x <= xts::last(y)]
      
      if (.filter.completed.data == TRUE) {
        data.frame(.id = cr, date = x) %>% tbl_df
        
      } else {
        if (.date == TRUE) {
          data.frame(.id = cr, date = xts::last(x), 
                     date_i = paste0(which(y == xts::last(x)), '/', length(y)))
        } else {
          data.frame(.id = cr, x = length(x), n = length(y)) %>% 
            mutate(progress = percent(x/n))
        }
      }
    })# %>% tbl_df
    
    if (.filter.completed.data == TRUE) {
      return(tbl_df(z))
    } else {
      print(z)
      
      if (.date == TRUE) {
        prg = z$date_i %>% str_split_fixed('/', 2) %>% 
          tbl_df %>% mutate_all(as.numeric) %>% 
          colSums %>% t %>% tbl_df %>% mutate(pr = V1/V2) %>% 
          .[3] %>% unlist %>% percent
        
      } else {
        prg = sum(z$x)/sum(z$n)
      }
      cat('\n================', as.character(percent(prg)), '================\n\n')
    }
  }
}
