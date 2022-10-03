setwd('/home/englianhu/文档/GitHub/binary.com-interview-question-data')
数据库蜀道 <- paste0(getwd(), '/文艺数据库/fx/USDJPY/仓库/')
数据库文件夹 <- dir(数据库蜀道, pattern = '0$')


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

列表120 <- list.files(paste0(数据库蜀道, 数据库文件夹)[1], '*.rds')
saveRDS(列表120, paste0(数据库蜀道, '列表120.rds'))
日内平滑指数数据120 <- ldply(列表120, function(参数) readRDS(paste0(数据库蜀道, '120/', 参数)))

列表1200 <- list.files(paste0(数据库蜀道, 数据库文件夹)[2], '*.rds')
saveRDS(列表1200, paste0(数据库蜀道, '列表1200.rds'))
日内平滑指数数据1200 <- ldply(列表1200, function(参数) readRDS(paste0(数据库蜀道, '1200/', 参数)))

列表150 <- list.files(paste0(数据库蜀道, 数据库文件夹)[3], '*.rds')
saveRDS(列表150, paste0(数据库蜀道, '列表150.rds'))
日内平滑指数数据150 <- ldply(列表150, function(参数) readRDS(paste0(数据库蜀道, '150/', 参数)))

列表200 <- list.files(paste0(数据库蜀道, 数据库文件夹)[4], '*.rds')
saveRDS(列表200, paste0(数据库蜀道, '列表200.rds'))
日内平滑指数数据200 <- ldply(列表200, function(参数) readRDS(paste0(数据库蜀道, '200/', 参数)))

列表240 <- list.files(paste0(数据库蜀道, 数据库文件夹)[5], '*.rds')
saveRDS(列表240, paste0(数据库蜀道, '列表240.rds'))
日内平滑指数数据240 <- ldply(列表240, function(参数) readRDS(paste0(数据库蜀道, '240/', 参数)))

列表300 <- list.files(paste0(数据库蜀道, 数据库文件夹)[6], '*.rds')
saveRDS(列表300, paste0(数据库蜀道, '列表300.rds'))
日内平滑指数数据300 <- ldply(列表300, function(参数) readRDS(paste0(数据库蜀道, '300/', 参数)))

列表400 <- list.files(paste0(数据库蜀道, 数据库文件夹)[7], '*.rds')
saveRDS(列表400, paste0(数据库蜀道, '列表400.rds'))
日内平滑指数数据400 <- ldply(列表400, function(参数) readRDS(paste0(数据库蜀道, '400/', 参数)))

列表600 <- list.files(paste0(数据库蜀道, 数据库文件夹)[8], '*.rds')
saveRDS(列表600, paste0(数据库蜀道, '列表600.rds'))
日内平滑指数数据600 <- ldply(列表600, function(参数) readRDS(paste0(数据库蜀道, '600/', 参数)))


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


