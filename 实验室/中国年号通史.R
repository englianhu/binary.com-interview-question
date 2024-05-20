天朝 = '周朝'

## 秦国 China，秦人 Chinese
## 司马错得蜀既得楚
if (!exists('.蜀道')) {
  .蜀道 <- getwd() |> 
    {\(.) str_split(., '/')}() |> 
    {\(.) c('/', .[[1]][2:5])}() |> 
    {\(.) c(., 'binary.com-interview-question-data/')}() |> 
    {\(.) paste(., collapse = '/')}() |> 
    {\(.) substring(., 2)}()
}

if (!exists('.蜀道书轩')) .蜀道书轩 <- paste0(.蜀道, '诸子百家学府/中国年号通史/')
if (!dir.exists(.蜀道书轩))  dir.create(.蜀道书轩)

## 中国年号通史
干支 <- c('甲子', '乙丑', '丙寅', '丁卯', '戊辰', '己巳', '庚午', '辛未', '壬申', '癸酉', '甲戌', '乙亥', '丙子', '丁丑', '戊寅', '己卯', '庚辰', '辛巳', '壬午', '癸未', '甲申', '乙酉', '丙戌', '丁亥', '戊子', '己丑', '庚寅', '辛卯', '壬辰', '癸巳', '甲午', '乙未', '丙申', '丁酉', '戊戌', '己亥', '庚子', '辛丑', '壬寅', '癸卯', '甲辰', '乙巳', '丙午', '丁未', '戊申', '己酉', '庚戌', '辛亥', '壬子', '癸丑', '甲寅', '乙卯', '丙辰', '丁巳', '戊午', '己未', '庚申', '辛酉', '壬戌', '癸亥')

生肖 <- c('鼠', '牛', '虎', '兔', '龙', '蛇', '马', '羊', '猴', '鸡', '狗', '猪')

朝代 <- c('唐朝', '宋朝', '元朝', '明朝', '清朝', '周朝', '三皇五帝', '商朝', '东周', '夏朝', '汉朝', '大理', '春秋', '西周', '战国', '西汉', '辽国', '东汉', '北朝', '西夏', '南北朝', '南朝', '北宋', '晋朝', '南宋', '北魏', '十六国', '金朝', '东晋', '吴越', '前凉', '三国', '南朝宋', '南平', '南朝梁', '南汉', '五代十国', '孙吴', '西晋', '西秦', '曹魏', '马楚', '成汉', '前秦', '蜀汉', '北凉', '南唐', '隋朝', '闽国', '杨吴')

帝王 <- c('秦始皇嬴政', '清圣祖玄烨', '清高宗弘历', '清世宗胤禛', '明太祖朱元璋', '明成祖朱棣', '蒙古太祖铁木真', '西汉武帝刘彻', '西汉文帝刘恒', '元世祖忽必烈', '北宋徽宗赵佶', '北宋太祖赵匡胤', '唐玄宗李隆基', '周武曌武则天', '唐太宗李世民', '唐高宗李治', '隋文帝杨坚', '隋炀帝杨广', '东周赧王姬延', '秦昭襄王嬴稷', '西夏仁宗李仁孝', '西周穆王姬满', '西夏崇宗李乾顺', '东周平王姬宜臼', '东周显王姬扁', '辽圣宗耶律隆绪', '明神宗朱翊钧', '南朝梁高祖萧衍', '辽道宗耶律洪基', '明世宗朱厚熜', '西周宣王姬静', '东周敬王姬匄', '北宋仁宗赵祯', '南宋理宗赵昀', '三国蜀汉后主刘禅', '西周厉王姬胡', '南宋高宗赵构', '元惠宗妥欢帖睦尔', '清德宗载湉', '殷商武乙子瞿', '东周襄王姬郑', '东汉光武帝刘秀', '东汉献帝刘协', '成汉武帝李雄', '南宋宁宗赵扩', '清宣宗旻宁', '殷商纣王子辛', '北朝北魏世祖拓跋焘', '东周贞定王姬介', '金世宗完颜雍')

if (天朝 %in% 朝代) {
  ## 唐朝
  if (天朝 == '唐朝') {
    页 <- 14
    链 <- c('https://www.cidianwang.com/nianhao/tangchao_08e44.htm', paste0('https://www.cidianwang.com/nianhao/tangchao_08e44_', 1:页, '.htm'))
  }
  ## 宋朝
  if (天朝 == '宋朝') {
    页 <- 15
    链 <- c('https://www.cidianwang.com/nianhao/songchao_0e20a.htm', paste0('https://www.cidianwang.com/nianhao/songchao_0e20a_', 1:页, '.htm'))
  }
  ## 元朝
  if (天朝 == '元朝') {
    页 <- 4
    链 <- c('https://www.cidianwang.com/nianhao/yuanchao_25a79.htm', paste0('https://www.cidianwang.com/nianhao/yuanchao_25a79_', 1:页, '.htm'))
  }
  ## 明朝
  if (天朝 == '明朝') {
    页 <- 13
    链 <- c('https://www.cidianwang.com/nianhao/mingchao_8869e.htm', paste0('https://www.cidianwang.com/nianhao/mingchao_8869e_', 1:页, '.htm'))
  }
  ## 清朝
  if (天朝 == '清朝') {
    页 <- 13
    链 <- c('https://www.cidianwang.com/nianhao/qingchao_6c5aa.htm', paste0('https://www.cidianwang.com/nianhao/qingchao_6c5aa_', 1:页, '.htm'))
  }
  ## 周朝
  if (天朝 == '周朝') {
    页 <- 19
    链 <- c('https://www.cidianwang.com/nianhao/zhouchao_0a340.htm', paste0('https://www.cidianwang.com/nianhao/zhouchao_0a340_', 1:页, '.htm'))
  }
  ## 三皇五帝、三黄五帝
  if (天朝 %in% c('三皇五帝', '三黄五帝')) {
    页 <- 19
    链 <- c('https://www.cidianwang.com/nianhao/sanhuangwudi_3504e.htm', paste0('https://www.cidianwang.com/nianhao/sanhuangwudi_3504e_', 1:页, '.htm'))
  }
  ## 商朝
  if (天朝 == '商朝') {
    页 <- 19
    链 <- c('https://www.cidianwang.com/nianhao/shangchao_a2dbe.htm', paste0('https://www.cidianwang.com/nianhao/shangchao_a2dbe_', 1:页, '.htm'))
  }
  ## 东周
  if (天朝 == '东周') {
    页 <- 19
    链 <- c('https://www.cidianwang.com/nianhao/dongzhou_7c3e4.htm', paste0('https://www.cidianwang.com/nianhao/dongzhou_7c3e4_', 1:页, '.htm'))
  }
  ## 夏朝
  if (天朝 == '夏朝') {
    页 <- 19
    链 <- c('https://www.cidianwang.com/nianhao/xiachao_0b343.htm', paste0('https://www.cidianwang.com/nianhao/xiachao_0b343_', 1:页, '.htm'))
  }
  ## 汉朝
  if (天朝 == '汉朝') {
    页 <- 19
    链 <- c('https://www.cidianwang.com/nianhao/hanchao_c8903.htm', paste0('https://www.cidianwang.com/nianhao/hanchao_c8903_', 1:页, '.htm'))
  }
  ## 大理
  if (天朝 == '大理') {
    页 <- 15
    链 <- c('https://www.cidianwang.com/nianhao/dali_7c50a.htm', paste0('https://www.cidianwang.com/nianhao/dali_7c50a_', 1:页, '.htm'))
  }
  ## 春秋
  if (天朝 == '春秋') {
    页 <- 14
    链 <- c('https://www.cidianwang.com/nianhao/chunqiu_ebdf2.htm', paste0('https://www.cidianwang.com/nianhao/chunqiu_ebdf2_', 1:页, '.htm'))
  }
  ## 西周
  if (天朝 == '西周') {
    页 <- 13
    链 <- c('https://www.cidianwang.com/nianhao/xizhou_ac6be.htm', paste0('https://www.cidianwang.com/nianhao/xizhou_ac6be_', 1:页, '.htm'))
  }
  ## 战国
  if (天朝 == '战国') {
    页 <- 12
    链 <- c('https://www.cidianwang.com/nianhao/zhanguo_ebb8f.htm', paste0('https://www.cidianwang.com/nianhao/zhanguo_ebb8f_', 1:页, '.htm'))
  }
  ## 西汉
  if (天朝 == '西汉') {
    页 <- 10
    链 <- c('https://www.cidianwang.com/nianhao/xihan_127fd.htm', paste0('https://www.cidianwang.com/nianhao/xihan_127fd_', 1:页, '.htm'))
  }
  ## 辽国
  if (天朝 == '辽国') {
    页 <- 10
    链 <- c('https://www.cidianwang.com/nianhao/liaoguo_e6927.htm', paste0('https://www.cidianwang.com/nianhao/liaoguo_e6927_', 1:页, '.htm'))
  }
  ## 东汉
  if (天朝 == '东汉') {
    页 <- 9
    链 <- c('https://www.cidianwang.com/nianhao/donghan_80711.htm', paste0('https://www.cidianwang.com/nianhao/donghan_80711_', 1:页, '.htm'))
  }
  ## 北朝
  if (天朝 == '北朝') {
    页 <- 10
    链 <- c('https://www.cidianwang.com/nianhao/beichao_b4c32.htm', paste0('https://www.cidianwang.com/nianhao/beichao_b4c32_', 1:页, '.htm'))
  }
  ## 周朝
  if (天朝 == '周朝') {
    页 <- 19
    链 <- c('https://www.cidianwang.com/nianhao/zhouchao_0a340.htm', paste0('https://www.cidianwang.com/nianhao/zhouchao_0a340_', 1:页, '.htm'))
  }
  ## 周朝
  if (天朝 == '周朝') {
    页 <- 19
    链 <- c('https://www.cidianwang.com/nianhao/zhouchao_0a340.htm', paste0('https://www.cidianwang.com/nianhao/zhouchao_0a340_', 1:页, '.htm'))
  }
  
  
  
}



年号 <- plyr::ldply(链, function(椠) {
  椠 <- 椠 %>% 
    read_html() %>% 
    html_element('.left') %>% 
    html_text2() %>% 
    str_split('\n') %>% 
    unlist()
  ## 第一次出现的朝代是选项，所以正则表达式匹配第二次出现的朝代标题。
  序 <- grep(paste0('^', 天朝, '$'), 椠)[2]
  椠[序:length(椠)]
  版 <- matrix(椠[-1], ncol = 2, byrow = TRUE, dimnames = list(NULL, c('年份', '明细')))
}, .progress = 'text')
# saveRDS(周朝年号, paste0(.蜀道书轩, '椠周朝年号.rds'))
# 周朝年号 <- readRDS(paste0(.蜀道书轩, '椠周朝年号.rds'))


