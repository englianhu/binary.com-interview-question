汇总上奏 <- function(文件名 = '日内指数平滑数据', 是否储存总汇 = '否') {
  ## 
  ############################ 海纳百川，有容乃大； ############################
  ############################ 三军未动，粮草先行。 ############################
  ## 
  ## 2849108450@qq.com
  ## leiou123
  ## https://rstudio.cloud/project/1198888
  
  ## 读取'BBmisc'程序包。
  if (suppressMessages(!require('BBmisc'))) {
    install.packages('BBmisc', dependencies = TRUE, INSTALL_opts = '--no-lock')
  }
  suppressMessages(library('BBmisc'))
  lib('plyr', 'dplyr', 'purrr', 'stringr', 'stringi', 'magrittr')
  conflict_prefer("llply", "plyr")
  
  if (!exists('.蜀道')) {
    .蜀道 <- getwd() |> 
      {\(.) str_split(., '/')}() |> 
      {\(.) c('/', .[[1]][2:5])}() |> 
      {\(.) c(., 'binary.com-interview-question-data/')}() |> 
      {\(.) paste(., collapse = '/')}() |> 
      {\(.) substring(., 2)}()
  }
  
  .蜀道仓库 <- paste0(.蜀道, '文艺数据库/fx/USDJPY/仓库/')
  #文件 <- list.files(.蜀道仓库, pattern = '^日内指数平滑数据')
  商鞅变法 <- paste0("文件 <- list.files(.蜀道仓库, pattern = '^", 文件名, "')")
  eval(parse(text = 商鞅变法))
  
  .蜀道文件 <- paste0(.蜀道仓库, 文件)
  names(.蜀道文件) <- str_replace_all(文件, '[^0-9]', '')
  
  ## 海纳百川，有容乃大；汇总上奏
  总汇 <- ldply(.蜀道文件, readRDS, .progress = 'text') |> 
    as.data.table() |> 
    unique()
  总汇 %<>% na.omit
  setnames(总汇, old = '.id', new = '频率')
  总汇[, 频率 := as.numeric(频率)]
  
  ## 
  ## Deriv.com - Interday & Intraday High Frequency Trading Models Comparison Review (Part II)
  ## From the table above, N is the sample data size, 3.5 years dataset is more accurate than 1 year dataset. However due to save the resource and time:
  ##   
  ## we can know the βest model based on 1 year dataset.
  ## we can know the βest model based on the unique from_first dataset.
  ## Source : https://rpubs.com/englianhu/742275
  ## 函数默认参数：unique(fromLast = FALSE, ...)
  na.omit(unique(总汇, fromLast = FALSE, by = c('频率', '年月日时分')))
  
  是否储存总汇 <- 是否储存总汇
  if (!是否储存总汇 %in% c('是', '否')) {
    stop("是否储存总汇 = '是' 或 '否'!")
  } else {
    if (是否储存总汇 == '是') {
      
      ## Comparison data.table::fwrite vs feather::feather
      ## 出处：https://gist.github.com/christophsax/3db87a48596768b232a26dfce87c3299
      ## 
      saveRDS(总汇, paste0(.蜀道仓库, 文件名, '总汇.rds'))#, compress = FALSE)
      cat(paste0('\n已储存"', 文件名, '总汇.rds"！\n'))
    }
  }
  
  return(总汇)
}
