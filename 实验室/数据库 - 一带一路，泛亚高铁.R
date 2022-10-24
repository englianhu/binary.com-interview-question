Sys.setlocale("LC_ALL", "en_US.UTF-8")
## 更换时间区域，保留日期时间。
Sys.setenv(TZ = 'Asia/Tapei')

## 忽略所有警讯
## https://stackoverflow.com/a/36846793/3806250
## 设置宽度
## options(knitr.table.format = 'html')将所有kableExtra图表一致设置为'html'格式，省略设置各别图表。
## options(repos = 'https://cran.rstudio.com')将仓库设置为安全网。
## options(repos = 'http://cran.rstudio.com')将仓库设置为普通网。
options(warn = -1, width = 999, knitr.table.format = 'html', 
        digits = 16, digits.secs = Inf, repos = 'https://cran.rstudio.com')

setwd('/home/englianhu/文档/GitHub/binary.com-interview-question')
## setwd('/home/englianhu/文档/GitHub/binary.com-interview-question-data')
## 数据库蜀道 <- paste0(getwd(), '/文艺数据库/fx/USDJPY/仓库/')
数据库蜀道 <- paste0('/home/englianhu/文档/GitHub/binary.com-interview-question-data/文艺数据库/fx/USDJPY/仓库/')
数据库文件夹 <- dir(数据库蜀道, pattern = '0$')

## 检验是否已设置途径。
if(!exists('.蜀道')) {
  .蜀道 <- getwd() |> 
    {\(.) str_split(., '/')}() |> 
    {\(.) c('/', .[[1]][2:5])}() |> 
    {\(.) c(., 'binary.com-interview-question-data/')}() |> 
    {\(.) paste(., collapse = '/')}() |> 
    {\(.) substring(., 2)}()
  }

## 倘若环境尚未有数据，读取文件数据。
if(!exists('样本')) {
  样本 <- readRDS(paste0(.蜀道, '文艺数据库/fx/USDJPY/样本1.rds'))
  }

######## 秦孝公秦始皇NonMuslim邓小平，商鞅变法六四焚可兰经坑巫师巫婆乩童 ########
######## 三军未动，粮草先行 ########
######## 一带一路，泛亚高铁 ########
## 
## 其实秦惠文王早有吞并蜀国之意，但那时候的蜀国地处四川盆地，还不是天府之国，属于老少边穷地区，还没有得到很好的开发，还很贫穷，加之没有火车、没有公路、没有飞机，外面要想进入蜀地非常艰难，以至一千年以后的李白还在感叹，蜀道之难难于上青天!
## 
## 【原创】灭蜀大将司马错：秦国统一的三大战役
## https://www.lishixinzhi.com/lishilunwen/178620.html
## 
## 
## 《史记·张仪列传》：蜀既属秦，秦以益彊，富厚，轻诸侯。
## 而且由于巴蜀特有的防御优势，可以不用考虑外部的威胁，完全成为了秦国的物资仓库，安安心心为秦国输送粮草物资。
## 
## 司马错拿下巴蜀，竟激活了一个胜利公式，让秦、汉、唐都受益无穷
## http://www.360doc.com/content/22/0918/22/5512889_1048445265.shtml
## 
## 新马高铁项目，谈久必崩，崩久必谈，这次是否真有望能坐高铁上北京了？
## https://i.ifeng.com/c/8J5lpeRPkv6
## 
## -----------------------------------------------------------
## 
## 中国的核弹飞到日本只需要十分钟，我们假设发射100枚核弹头，采用多弹头分射，日本拦截成功率可能在10%，也就是90枚导弹打到日本，每一颗核弹威力相当于广岛核弹小男孩爆炸的威力的500-1000倍。按最低威力计算，500*99个小男孩的威力，日本国土面积377962平方千米，广岛的面积为900平方千米，足够毁灭日本500*99*900/377962=117.87次。所以，我们有能力在30分钟之内毁灭日本。那么日本为啥不怕我们，因为我们并不想屠杀日本，放下仇恨对我们来说很艰难，但是我们不会忘记，如果有一日日本胆敢来犯，必将整个日本化为灰烬，从地球消失。
## 
## 作者：画画
## 链接：https://www.zhihu.com/question/348501093/answer/1906987881
## 来源：知乎
## 著作权归作者所有。商业转载请联系作者获得授权，非商业转载请注明出处。
## 
## 中国灭日本需要多久？局座说的两个小时之内能灭掉日本吗？
## https://www.zhihu.com/question/348501093
## 
## -----------------------------------------------------------
## 
## 如果中日开战，日本能挺多久？专家：不会超过7天
## https://page.om.qq.com/page/OsOjxqeEX31r-TrGRfH-a48g0
## 

数据库文件120 <- ldply(数据库文件夹[1], function(x) {
    明修蜀道 = paste0(数据库蜀道, x)
    粮草 = list.files(明修蜀道, '*.rds')
    readRDS(paste0(明修蜀道, '/', 粮草))
    }, .progress = 'text')

ldply(数据库文件夹[1], function(x) {
     明修蜀道 = paste0(数据库蜀道, x)
     粮草 = list.files(明修蜀道, '*.rds')[1:100]
     paste0(明修蜀道, '/', 粮草)})

microbenchmark(
    蜀道 = dir(paste0(数据库蜀道, 数据库文件夹)[1]), 
    列表 = list.files(paste0(数据库蜀道, 数据库文件夹)[1], '*.rds')
    )

system.time({蜀道 = dir(paste0(数据库蜀道, 数据库文件夹)[1])})
system.time({蜀道 = dir(paste0(数据库蜀道, 数据库文件夹)[1], '*.rds')})
system.time({列表 = list.files(paste0(数据库蜀道, 数据库文件夹)[1], '*.rds')})

## =======================================================================

整顿数据 <- function(频率) {
   ## 韩信点兵，多多益善
   ## 军队编制
   ## http://adv-r.had.co.nz/Functional-programming.html
   ## https://shixiangwang.github.io/home/cn/post/2019-11-20-meta-programming/
   ## -----------------------------------------------------------------------
   ## 一般上，使用eval(parse(text = 参数))可以执行所有任何R语言代码方程，不过每行代码都使用分号，代码排列就不整齐，类似抒写一篇只有一段而已的作文。
   ## 商鞅变法：将所有方程、参数、任何代码编写为文字、依照可兰经回教刑事法典断肢法执行、再评估成效。
   #王翦点兵 <- paste0('list(列表 = 列表', substitute(频率), ', 日内平滑指数数据', substitute(频率), 'A)')
   # 
   #return(eval(parse(text = 王翦点兵)))
   ## -----------------------------------------------------------------------
   ## 所有频率组类似军队兵种编制
   ## 将每一分钟的汇价数据整顿、组合为一个数据组
   #列表24 <- list.files(paste0(数据库蜀道, 数据库文件夹)[1], '*.rds')
   列表 <- list.files(paste0(数据库蜀道, 频率), '*.rds')
   saveRDS(列表, paste0(数据库蜀道, '列表', 频率, '.rds'))
   列表 <- readRDS(paste0(数据库蜀道, '列表', 频率, '.rds'))
   日内平滑指数数据A <- ldply(列表, function(参数) readRDS(paste0(数据库蜀道, 频率, '/', 参数))) %>% as.data.table()
   
   ## 检验并整顿数据
   王翦点兵 <- paste0(数据库蜀道, '日内平滑指数数据', 频率, '.rds')
   if(!file.exists(王翦点兵)) {
      日内平滑指数数据 <- 日内平滑指数数据A
      saveRDS(日内平滑指数数据, paste0(数据库蜀道, '日内平滑指数数据', 频率, '.rds'))
   
   } else {
      日内平滑指数数据B <- readRDS(paste0(数据库蜀道, '日内平滑指数数据', 频率, '.rds')) %>% as.data.table()
   日内平滑指数数据 <- rbind(日内平滑指数数据A, 日内平滑指数数据B)[order(年月日时分)] %>% unique
   saveRDS(日内平滑指数数据, paste0(数据库蜀道, '日内平滑指数数据', 频率, '.rds'))
   }
   
   #日内平滑指数数据 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据', 频率, '.rds'))
   rm(列表, 日内平滑指数数据A, 日内平滑指数数据B)
   #file.remove(paste0(数据库蜀道, '列表24.rds'))
   return(日内平滑指数数据)
}

整顿数据(24)

#列表30 <- list.files(paste0(数据库蜀道, 数据库文件夹)[1], '*.rds')
列表30 <- list.files(paste0(数据库蜀道, 30), '*.rds')
saveRDS(列表30, paste0(数据库蜀道, '列表30.rds'))
列表30 <- readRDS(paste0(数据库蜀道, '列表30.rds'))
日内平滑指数数据30A <- ldply(列表30, function(参数) readRDS(paste0(数据库蜀道, 30, '/', 参数))) %>% as.data.table()
日内平滑指数数据30B <- readRDS(paste0(数据库蜀道, '日内平滑指数数据30.rds')) %>% as.data.table()
日内平滑指数数据30 <- rbind(日内平滑指数数据30A, 日内平滑指数数据30B)[order(年月日时分)] %>% unique
saveRDS(日内平滑指数数据30, paste0(数据库蜀道, '日内平滑指数数据30.rds'))
日内平滑指数数据30 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据30.rds'))
rm(列表30, 日内平滑指数数据30A, 日内平滑指数数据30B)
file.remove(paste0(数据库蜀道, '列表30.rds'))

#列表40 <- list.files(paste0(数据库蜀道, 数据库文件夹)[1], '*.rds')
列表40 <- list.files(paste0(数据库蜀道, 40), '*.rds')
saveRDS(列表40, paste0(数据库蜀道, '列表40.rds'))
列表40 <- readRDS(paste0(数据库蜀道, '列表40.rds'))
日内平滑指数数据40A <- ldply(列表40, function(参数) readRDS(paste0(数据库蜀道, 40, '/', 参数))) %>% as.data.table()
日内平滑指数数据40B <- readRDS(paste0(数据库蜀道, '日内平滑指数数据40.rds')) %>% as.data.table()
日内平滑指数数据40 <- rbind(日内平滑指数数据40A, 日内平滑指数数据40B)[order(年月日时分)] %>% unique
saveRDS(日内平滑指数数据40, paste0(数据库蜀道, '日内平滑指数数据40.rds'))
日内平滑指数数据40 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据40.rds'))
rm(列表40, 日内平滑指数数据40A, 日内平滑指数数据40B)
file.remove(paste0(数据库蜀道, '列表40.rds'))

#列表50 <- list.files(paste0(数据库蜀道, 数据库文件夹)[1], '*.rds')
列表50 <- list.files(paste0(数据库蜀道, 50), '*.rds')
saveRDS(列表50, paste0(数据库蜀道, '列表50.rds'))
列表50 <- readRDS(paste0(数据库蜀道, '列表50.rds'))
日内平滑指数数据50A <- ldply(列表50, function(参数) readRDS(paste0(数据库蜀道, 50, '/', 参数))) %>% as.data.table()
日内平滑指数数据50B <- readRDS(paste0(数据库蜀道, '日内平滑指数数据50.rds')) %>% as.data.table()
日内平滑指数数据50 <- rbind(日内平滑指数数据50A, 日内平滑指数数据50B)[order(年月日时分)] %>% unique
saveRDS(日内平滑指数数据50, paste0(数据库蜀道, '日内平滑指数数据50.rds'))
日内平滑指数数据50 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据50.rds'))
rm(列表50, 日内平滑指数数据50A, 日内平滑指数数据50B)
file.remove(paste0(数据库蜀道, '列表50.rds'))

#列表60 <- list.files(paste0(数据库蜀道, 数据库文件夹)[1], '*.rds')
列表60 <- list.files(paste0(数据库蜀道, 60), '*.rds')
saveRDS(列表60, paste0(数据库蜀道, '列表60.rds'))
列表60 <- readRDS(paste0(数据库蜀道, '列表60.rds'))
日内平滑指数数据60A <- ldply(列表60, function(参数) readRDS(paste0(数据库蜀道, 60, '/', 参数))) %>% as.data.table()
日内平滑指数数据60B <- readRDS(paste0(数据库蜀道, '日内平滑指数数据60.rds')) %>% as.data.table()
日内平滑指数数据60 <- rbind(日内平滑指数数据60A, 日内平滑指数数据60B)[order(年月日时分)] %>% unique
saveRDS(日内平滑指数数据60, paste0(数据库蜀道, '日内平滑指数数据60.rds'))
日内平滑指数数据60 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据60.rds'))
rm(列表60, 日内平滑指数数据60A, 日内平滑指数数据60B)
file.remove(paste0(数据库蜀道, '列表60.rds'))

#列表80 <- list.files(paste0(数据库蜀道, 数据库文件夹)[1], '*.rds')
列表80 <- list.files(paste0(数据库蜀道, 80), '*.rds')
saveRDS(列表80, paste0(数据库蜀道, '列表80.rds'))
列表80 <- readRDS(paste0(数据库蜀道, '列表80.rds'))
#日内平滑指数数据80 <- ldply(列表80, function(参数) readRDS(paste0(数据库蜀道, 数据库文件夹[1], '/', 参数))) %>% as_tibble()
日内平滑指数数据80A <- ldply(列表80, function(参数) readRDS(paste0(数据库蜀道, 80, '/', 参数))) %>% as.data.table()
日内平滑指数数据80B <- readRDS(paste0(数据库蜀道, '日内平滑指数数据80.rds')) %>% as.data.table()
日内平滑指数数据80 <- rbind(日内平滑指数数据80A, 日内平滑指数数据80B)[order(年月日时分)] %>% unique
saveRDS(日内平滑指数数据80, paste0(数据库蜀道, '日内平滑指数数据80.rds'))
日内平滑指数数据80 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据80.rds'))
rm(列表80, 日内平滑指数数据80A, 日内平滑指数数据80B)
file.remove(paste0(数据库蜀道, '列表80.rds'))

#列表100 <- list.files(paste0(数据库蜀道, 数据库文件夹)[1], '*.rds')
列表100 <- list.files(paste0(数据库蜀道, 100), '*.rds')
saveRDS(列表100, paste0(数据库蜀道, '列表100.rds'))
列表100 <- readRDS(paste0(数据库蜀道, '列表100.rds'))
日内平滑指数数据100A <- ldply(列表100, function(参数) readRDS(paste0(数据库蜀道, 100, '/', 参数))) %>% as.data.table()
日内平滑指数数据100B <- readRDS(paste0(数据库蜀道, '日内平滑指数数据100.rds')) %>% as.data.table()
日内平滑指数数据100 <- rbind(日内平滑指数数据100A, 日内平滑指数数据100B)[order(年月日时分)] %>% unique
saveRDS(日内平滑指数数据100, paste0(数据库蜀道, '日内平滑指数数据100.rds'))
日内平滑指数数据100 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据100.rds'))
rm(列表100, 日内平滑指数数据100A, 日内平滑指数数据100B)
file.remove(paste0(数据库蜀道, '列表100.rds'))

#列表120 <- list.files(paste0(数据库蜀道, 数据库文件夹)[1], '*.rds')
列表120 <- list.files(paste0(数据库蜀道, 120), '*.rds')
saveRDS(列表120, paste0(数据库蜀道, '列表120.rds'))
列表120 <- readRDS(paste0(数据库蜀道, '列表120.rds'))
日内平滑指数数据120A <- ldply(列表120, function(参数) readRDS(paste0(数据库蜀道, 120, '/', 参数))) %>% as.data.table()
日内平滑指数数据120B <- readRDS(paste0(数据库蜀道, '日内平滑指数数据120.rds')) %>% as.data.table()
日内平滑指数数据120 <- rbind(日内平滑指数数据120A, 日内平滑指数数据120B)[order(年月日时分)] %>% unique
saveRDS(日内平滑指数数据120, paste0(数据库蜀道, '日内平滑指数数据120.rds'))
日内平滑指数数据120 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据120.rds'))
rm(列表120, 日内平滑指数数据120A, 日内平滑指数数据120B)
file.remove(paste0(数据库蜀道, '列表120.rds'))

#列表150 <- list.files(paste0(数据库蜀道, 数据库文件夹)[3], '*.rds')
列表150 <- list.files(paste0(数据库蜀道, 150), '*.rds')
saveRDS(列表150, paste0(数据库蜀道, '列表150.rds'))
列表150 <- readRDS(paste0(数据库蜀道, '列表150.rds'))
日内平滑指数数据150A <- ldply(列表150, function(参数) readRDS(paste0(数据库蜀道, 150, '/', 参数))) %>% as.data.table()
日内平滑指数数据150B <- readRDS(paste0(数据库蜀道, '日内平滑指数数据150.rds')) %>% as.data.table()
日内平滑指数数据150 <- rbind(日内平滑指数数据150A, 日内平滑指数数据150B)[order(年月日时分)] %>% unique
saveRDS(日内平滑指数数据150, paste0(数据库蜀道, '日内平滑指数数据150.rds'))
日内平滑指数数据150 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据150.rds'))
rm(列表150, 日内平滑指数数据150A, 日内平滑指数数据150B)
file.remove(paste0(数据库蜀道, '列表150.rds'))

#列表200 <- list.files(paste0(数据库蜀道, 数据库文件夹)[4], '*.rds')
列表200 <- list.files(paste0(数据库蜀道, 200), '*.rds')
saveRDS(列表200, paste0(数据库蜀道, '列表200.rds'))
列表200 <- readRDS(paste0(数据库蜀道, '列表200.rds'))
日内平滑指数数据200A <- ldply(列表200, function(参数) readRDS(paste0(数据库蜀道, 200, '/', 参数))) %>% as.data.table()
日内平滑指数数据200B <- readRDS(paste0(数据库蜀道, '日内平滑指数数据200.rds')) %>% as.data.table()
日内平滑指数数据200 <- rbind(日内平滑指数数据200A, 日内平滑指数数据200B)[order(年月日时分)] %>% unique
saveRDS(日内平滑指数数据200, paste0(数据库蜀道, '日内平滑指数数据200.rds'))
日内平滑指数数据200 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据200.rds'))
rm(列表200, 日内平滑指数数据200A, 日内平滑指数数据200B)
file.remove(paste0(数据库蜀道, '列表200.rds'))

#列表240 <- list.files(paste0(数据库蜀道, 数据库文件夹)[5], '*.rds')
列表240 <- list.files(paste0(数据库蜀道, 240), '*.rds')
saveRDS(列表240, paste0(数据库蜀道, '列表240.rds'))
列表240 <- readRDS(paste0(数据库蜀道, '列表240.rds'))
日内平滑指数数据240A <- ldply(列表240, function(参数) readRDS(paste0(数据库蜀道, 240, '/', 参数))) %>% as.data.table()
日内平滑指数数据240B <- readRDS(paste0(数据库蜀道, '日内平滑指数数据240.rds')) %>% as.data.table()
日内平滑指数数据240 <- rbind(日内平滑指数数据240A, 日内平滑指数数据240B)[order(年月日时分)] %>% unique
saveRDS(日内平滑指数数据240, paste0(数据库蜀道, '日内平滑指数数据240.rds'))
日内平滑指数数据240 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据240.rds'))
rm(列表240, 日内平滑指数数据240A, 日内平滑指数数据240B)
file.remove(paste0(数据库蜀道, '列表240.rds'))

#列表300 <- list.files(paste0(数据库蜀道, 数据库文件夹)[6], '*.rds')
列表300 <- list.files(paste0(数据库蜀道, 300), '*.rds')
saveRDS(列表300, paste0(数据库蜀道, '列表300.rds'))
列表300 <- readRDS(paste0(数据库蜀道, '列表300.rds'))
日内平滑指数数据300A <- ldply(列表300, function(参数) readRDS(paste0(数据库蜀道, 300, '/', 参数))) %>% as.data.table()
日内平滑指数数据300B <- readRDS(paste0(数据库蜀道, '日内平滑指数数据300.rds')) %>% as.data.table()
日内平滑指数数据300 <- rbind(日内平滑指数数据300A, 日内平滑指数数据300B)[order(年月日时分)] %>% unique
saveRDS(日内平滑指数数据300, paste0(数据库蜀道, '日内平滑指数数据300.rds'))
日内平滑指数数据300 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据300.rds'))
rm(列表300, 日内平滑指数数据300A, 日内平滑指数数据300B)
file.remove(paste0(数据库蜀道, '列表300.rds'))

#列表400 <- list.files(paste0(数据库蜀道, 数据库文件夹)[7], '*.rds')
列表400 <- list.files(paste0(数据库蜀道, 400), '*.rds')
saveRDS(列表400, paste0(数据库蜀道, '列表400.rds'))
列表400 <- readRDS(paste0(数据库蜀道, '列表400.rds'))
日内平滑指数数据400A <- ldply(列表400, function(参数) readRDS(paste0(数据库蜀道, 400, '/', 参数))) %>% as.data.table()
日内平滑指数数据400B <- readRDS(paste0(数据库蜀道, '日内平滑指数数据400.rds')) %>% as.data.table()
日内平滑指数数据400 <- rbind(日内平滑指数数据400A, 日内平滑指数数据400B)[order(年月日时分)] %>% unique
saveRDS(日内平滑指数数据400, paste0(数据库蜀道, '日内平滑指数数据400.rds'))
日内平滑指数数据400 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据400.rds'))
rm(列表400, 日内平滑指数数据400A, 日内平滑指数数据400B)
file.remove(paste0(数据库蜀道, '列表400.rds'))

#列表600 <- list.files(paste0(数据库蜀道, 数据库文件夹)[8], '*.rds')
列表600 <- list.files(paste0(数据库蜀道, 600), '*.rds')
saveRDS(列表600, paste0(数据库蜀道, '列表600.rds'))
列表600 <- readRDS(paste0(数据库蜀道, '列表600.rds'))
日内平滑指数数据600A <- ldply(列表600, function(参数) readRDS(paste0(数据库蜀道, 600, '/', 参数))) %>% as.data.table()
日内平滑指数数据600B <- readRDS(paste0(数据库蜀道, '日内平滑指数数据600.rds')) %>% as.data.table()
日内平滑指数数据600 <- rbind(日内平滑指数数据600A, 日内平滑指数数据600B)[order(年月日时分)] %>% unique
saveRDS(日内平滑指数数据600, paste0(数据库蜀道, '日内平滑指数数据600.rds'))
日内平滑指数数据600 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据600.rds'))
rm(列表600, 日内平滑指数数据600A, 日内平滑指数数据600B)
file.remove(paste0(数据库蜀道, '列表600.rds'))

#列表1200 <- list.files(paste0(数据库蜀道, 数据库文件夹)[i], '*.rds')
列表1200 <- list.files(paste0(数据库蜀道, 1200), '*.rds')
                      saveRDS(列表1200, paste0(数据库蜀道, '列表1200.rds'))
列表1200 <- readRDS(paste0(数据库蜀道, '列表1200.rds'))
#日内平滑指数数据1200A <- ldply(列表1200, function(参数) readRDS(paste0(数据库蜀道, 数据库文件夹[2], '/', 参数))) %>% as.data.table()
日内平滑指数数据1200A <- ldply(列表1200, function(参数) readRDS(paste0(数据库蜀道, 1200, '/', 参数))) %>% as.data.table()
日内平滑指数数据1200B <- readRDS(paste0(数据库蜀道, '日内平滑指数数据1200.rds')) %>% as.data.table()
日内平滑指数数据1200 <- rbind(日内平滑指数数据1200A, 日内平滑指数数据1200B)[order(年月日时分)] %>% unique
saveRDS(日内平滑指数数据1200, paste0(数据库蜀道, '日内平滑指数数据1200.rds'))
日内平滑指数数据1200 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据1200.rds'))
rm(列表1200, 日内平滑指数数据1200A, 日内平滑指数数据1200B)
file.remove(paste0(数据库蜀道, '列表1200.rds'))



## https://stackoverflow.com/a/34957199/3806250
library(plyr)
library(doParallel)
cl <- makeCluster(2)

if (.Platform$OS.type == "windows") {
    registerDoParallel(cl)
    opts <- list(preschedule=TRUE)
    clusterSetRNGStream(cl, 123)
    r <- llply(1:20,
                .fun = function(x) runif(10),
                .parallel = TRUE,
                .paropts = list(.options.snow=opts))
}

if (.Platform$OS.type != "windows") {
    registerDoParallel(2)
    RNGkind("L'Ecuyer-CMRG")
    set.seed(123)
    mc.reset.stream()
    r <- llply(1:20,
                .fun = function(x) runif(10),
                .parallel = TRUE)
}

#### =============================================================
#### =============================================================
#### =============================================================
options(digits = 16)
## setwd('/home/englianhu/文档/GitHub/binary.com-interview-question')
setwd('/home/englianhu/文档/GitHub/binary.com-interview-question-data')
## 数据库蜀道 <- paste0(getwd(), '/文艺数据库/fx/USDJPY/仓库/')
数据库蜀道 <- paste0('/home/englianhu/文档/GitHub/binary.com-interview-question-data/文艺数据库/fx/USDJPY/仓库/')
数据库文件夹 <- dir(数据库蜀道, pattern = '0$')

## 检验是否已设置途径。
if(!exists('.蜀道')) {
  .蜀道 <- getwd() |> 
    {\(.) str_split(., '/')}() |> 
    {\(.) c('/', .[[1]][2:5])}() |> 
    {\(.) c(., 'binary.com-interview-question-data/')}() |> 
    {\(.) paste(., collapse = '/')}() |> 
    {\(.) substring(., 2)}()
  }

## 倘若环境尚未有数据，读取文件数据。
if(!exists('样本')) {
  样本 <- readRDS(paste0(.蜀道, '文艺数据库/fx/USDJPY/样本1.rds'))
  }
                     
时间索引 <- unique(样本$日期)
# 基准 <- filter(样本, 年份 == 2016)$日期[1] #"2016-01-04" 第2年第1个交易日
基准 <- 样本[年份 == 2016]$日期[1]
时间索引 %<>% .[. >= 基准]
# 时间索引 %<>% .[. >= as_date('2016-01-04')]
迭代基准 <- 样本[日期 %chin% 时间索引]$序列
数据量 <- 1200 #筛选数据中的最后1200观测值：样本[(.N - (数据量 - 1)):.N]
预测时间单位 <- 1

.模型选项 = c('MNN')

                      
conflict_prefer('llply', 'plyr', quiet = TRUE)
conflict_prefer('filter', 'dplyr', quiet = TRUE)
conflict_prefer('select', 'dplyr', quiet = TRUE)
conflict_prefer('mutate', 'dplyr', quiet = TRUE)
conflict_prefer('rename', 'dplyr', quiet = TRUE)
conflict_prefer('collapse', 'dplyr', quiet = TRUE)
conflict_prefer('year', 'lubridate', quiet = TRUE)
conflict_prefer('first', 'data.table', quiet = TRUE)
conflict_prefer('last', 'data.table', quiet = TRUE)
conflict_prefer('transpose', 'data.table', quiet = TRUE)

#数据库蜀道 <- paste0('/home/englianhu/文档/GitHub/binary.com-interview-question-data/文艺数据库/fx/USDJPY/仓库/')
#数据库文件夹 <- dir(数据库蜀道, pattern = '0$')
# source('函数/日内高频指数平滑.R')
source('/home/englianhu/文档/GitHub/binary.com-interview-question/函数/日内高频指数平滑.R')

################ 频率 = 1200 ##############
日内平滑指数数据1200 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据1200.rds')) %>% 
                     as.data.table()
基准 <- last(日内平滑指数数据1200$年月日时分, 3)[1] %>% as_date()
时间索引 %<>% .[. >= 基准]
迭代基准 <- 样本[日期 %chin% 时间索引]$序列
频率 = 1200

日内高频指数平滑(
     时间索引 = 时间索引, 样本 = 样本, 数据量 = 数据量, 频率 = 频率, 
     预测时间单位 = 预测时间单位, .模型选项 = .模型选项)

################ 频率 = 600 ##############
日内平滑指数数据600 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据600.rds')) %>% 
                     as.data.table()
基准 <- last(日内平滑指数数据600$年月日时分, 3)[1] %>% as_date()
时间索引 %<>% .[. >= 基准]
迭代基准 <- 样本[日期 %chin% 时间索引]$序列
频率 = 600

日内高频指数平滑(
     时间索引 = 时间索引, 样本 = 样本, 数据量 = 数据量, 频率 = 频率, 
     预测时间单位 = 预测时间单位, .模型选项 = .模型选项)

################ 频率 = 400 ##############
日内平滑指数数据400 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据400.rds')) %>% 
                     as.data.table()
基准 <- last(日内平滑指数数据400$年月日时分, 3)[1] %>% as_date()
时间索引 %<>% .[. >= 基准]
迭代基准 <- 样本[日期 %chin% 时间索引]$序列
频率 = 400

日内高频指数平滑(
     时间索引 = 时间索引, 样本 = 样本, 数据量 = 数据量, 频率 = 频率, 
     预测时间单位 = 预测时间单位, .模型选项 = .模型选项)

################ 频率 = 300 ##############
日内平滑指数数据300 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据300.rds')) %>% 
                     as.data.table()
基准 <- last(日内平滑指数数据300$年月日时分, 3)[1] %>% as_date()
时间索引 %<>% .[. >= 基准]
迭代基准 <- 样本[日期 %chin% 时间索引]$序列
频率 = 300

日内高频指数平滑(
     时间索引 = 时间索引, 样本 = 样本, 数据量 = 数据量, 频率 = 频率, 
     预测时间单位 = 预测时间单位, .模型选项 = .模型选项)

################ 频率 = 240 ##############
日内平滑指数数据240 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据240.rds')) %>% 
                     as.data.table()
基准 <- last(日内平滑指数数据240$年月日时分, 3)[1] %>% as_date()
时间索引 %<>% .[. >= 基准]
迭代基准 <- 样本[日期 %chin% 时间索引]$序列
频率 = 240

日内高频指数平滑(
     时间索引 = 时间索引, 样本 = 样本, 数据量 = 数据量, 频率 = 频率, 
     预测时间单位 = 预测时间单位, .模型选项 = .模型选项)

################ 频率 = 200 ##############
日内平滑指数数据200 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据200.rds')) %>% 
                     as.data.table()
基准 <- last(日内平滑指数数据200$年月日时分, 3)[1] %>% as_date()
时间索引 %<>% .[. >= 基准]
迭代基准 <- 样本[日期 %chin% 时间索引]$序列
频率 = 200

日内高频指数平滑(
     时间索引 = 时间索引, 样本 = 样本, 数据量 = 数据量, 频率 = 频率, 
     预测时间单位 = 预测时间单位, .模型选项 = .模型选项)

################ 频率 = 150 ##############
日内平滑指数数据150 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据150.rds')) %>% 
                     as.data.table()
基准 <- last(日内平滑指数数据150$年月日时分, 3)[1] %>% as_date()
时间索引 %<>% .[. >= 基准]
迭代基准 <- 样本[日期 %chin% 时间索引]$序列
频率 = 150

日内高频指数平滑(
     时间索引 = 时间索引, 样本 = 样本, 数据量 = 数据量, 频率 = 频率, 
     预测时间单位 = 预测时间单位, .模型选项 = .模型选项)

################ 频率 = 120 ##############
日内平滑指数数据120 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据120.rds')) %>% 
                     as.data.table()
基准 <- last(日内平滑指数数据120$年月日时分, 3)[1] %>% as_date()
时间索引 %<>% .[. >= 基准]
迭代基准 <- 样本[日期 %chin% 时间索引]$序列
频率 = 120

日内高频指数平滑(
     时间索引 = 时间索引, 样本 = 样本, 数据量 = 数据量, 频率 = 频率, 
     预测时间单位 = 预测时间单位, .模型选项 = .模型选项)

################ 频率 = 100 ##############
日内平滑指数数据100 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据100.rds')) %>% 
                     as.data.table()
基准 <- last(日内平滑指数数据100$年月日时分, 3)[1] %>% as_date()
时间索引 %<>% .[. >= 基准]
迭代基准 <- 样本[日期 %chin% 时间索引]$序列
频率 = 100

日内高频指数平滑(
     时间索引 = 时间索引, 样本 = 样本, 数据量 = 数据量, 频率 = 频率, 
     预测时间单位 = 预测时间单位, .模型选项 = .模型选项)

################ 频率 = 80 ###############
日内平滑指数数据80 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据80.rds')) %>% 
                     as.data.table()
基准 <- last(日内平滑指数数据80$年月日时分, 3)[1] %>% as_date()
时间索引 %<>% .[. >= 基准]
迭代基准 <- 样本[日期 %chin% 时间索引]$序列
频率 = 80

日内高频指数平滑(
     时间索引 = 时间索引, 样本 = 样本, 数据量 = 数据量, 频率 = 频率, 
     预测时间单位 = 预测时间单位, .模型选项 = .模型选项)

################ 频率 = 60 ###############
日内平滑指数数据60 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据60.rds')) %>% 
                     as.data.table()
基准 <- last(日内平滑指数数据60$年月日时分, 3)[1] %>% as_date()
时间索引 %<>% .[. >= 基准]
迭代基准 <- 样本[日期 %chin% 时间索引]$序列
频率 = 60

日内高频指数平滑(
     时间索引 = 时间索引, 样本 = 样本, 数据量 = 数据量, 频率 = 频率, 
     预测时间单位 = 预测时间单位, .模型选项 = .模型选项)

################ 频率 = 50 ###############
日内平滑指数数据50 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据50.rds')) %>% 
                     as.data.table()
基准 <- last(日内平滑指数数据50$年月日时分, 3)[1] %>% as_date()
时间索引 %<>% .[. >= 基准]
迭代基准 <- 样本[日期 %chin% 时间索引]$序列
频率 = 50

日内高频指数平滑(
     时间索引 = 时间索引, 样本 = 样本, 数据量 = 数据量, 频率 = 频率, 
     预测时间单位 = 预测时间单位, .模型选项 = .模型选项)

################ 频率 = 40 ###############
日内平滑指数数据40 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据40.rds')) %>% 
                     as.data.table()
基准 <- last(日内平滑指数数据40$年月日时分, 3)[1] %>% as_date()
时间索引 %<>% .[. >= 基准]
迭代基准 <- 样本[日期 %chin% 时间索引]$序列
频率 = 40

日内高频指数平滑(
     时间索引 = 时间索引, 样本 = 样本, 数据量 = 数据量, 频率 = 频率, 
     预测时间单位 = 预测时间单位, .模型选项 = .模型选项)

################ 频率 = 30 ###############
日内平滑指数数据30 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据30.rds')) %>% 
                     as.data.table()
基准 <- last(日内平滑指数数据30$年月日时分, 3)[1] %>% as_date()
时间索引 %<>% .[. >= 基准]
迭代基准 <- 样本[日期 %chin% 时间索引]$序列
频率 = 30

日内高频指数平滑(
     时间索引 = 时间索引, 样本 = 样本, 数据量 = 数据量, 频率 = 频率, 
     预测时间单位 = 预测时间单位, .模型选项 = .模型选项)

################ 频率 = 24 ###############
日内平滑指数数据24 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据24.rds')) %>% 
                     as.data.table()
基准 <- last(日内平滑指数数据24$年月日时分, 3)[1] %>% as_date()
时间索引 %<>% .[. >= 基准]
迭代基准 <- 样本[日期 %chin% 时间索引]$序列
频率 = 24

日内高频指数平滑(
     时间索引 = 时间索引, 样本 = 样本, 数据量 = 数据量, 频率 = 频率, 
     预测时间单位 = 预测时间单位, .模型选项 = .模型选项)

################ 频率 = 20 ###############
日内平滑指数数据20 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据20.rds')) %>% 
                     as.data.table()
基准 <- last(日内平滑指数数据20$年月日时分, 3)[1] %>% as_date()
时间索引 %<>% .[. >= 基准]
迭代基准 <- 样本[日期 %chin% 时间索引]$序列
频率 = 20

日内高频指数平滑(
     时间索引 = 时间索引, 样本 = 样本, 数据量 = 数据量, 频率 = 频率, 
     预测时间单位 = 预测时间单位, .模型选项 = .模型选项)

################ 频率 = 16 ###############
日内平滑指数数据16 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据16.rds')) %>% 
                     as.data.table()
基准 <- last(日内平滑指数数据16$年月日时分, 3)[1] %>% as_date()
时间索引 %<>% .[. >= 基准]
迭代基准 <- 样本[日期 %chin% 时间索引]$序列
频率 = 16

日内高频指数平滑(
     时间索引 = 时间索引, 样本 = 样本, 数据量 = 数据量, 频率 = 频率, 
     预测时间单位 = 预测时间单位, .模型选项 = .模型选项)

################ 频率 = 15 ###############
日内平滑指数数据15 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据15.rds')) %>% 
                     as.data.table()
基准 <- last(日内平滑指数数据15$年月日时分, 3)[1] %>% as_date()
时间索引 %<>% .[. >= 基准]
迭代基准 <- 样本[日期 %chin% 时间索引]$序列
频率 = 15

日内高频指数平滑(
     时间索引 = 时间索引, 样本 = 样本, 数据量 = 数据量, 频率 = 频率, 
     预测时间单位 = 预测时间单位, .模型选项 = .模型选项)

################ 频率 = 12 ###############
日内平滑指数数据12 <- readRDS(paste0(数据库蜀道, '日内平滑指数数据12.rds')) %>% 
                     as.data.table()
基准 <- last(日内平滑指数数据12$年月日时分, 3)[1] %>% as_date()
时间索引 %<>% .[. >= 基准]
迭代基准 <- 样本[日期 %chin% 时间索引]$序列
频率 = 12

日内高频指数平滑(
     时间索引 = 时间索引, 样本 = 样本, 数据量 = 数据量, 频率 = 频率, 
     预测时间单位 = 预测时间单位, .模型选项 = .模型选项)



