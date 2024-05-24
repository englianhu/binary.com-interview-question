中国年号通史 <- function(天朝 = '战国') {
  ## 秦国 China，秦人 Chinese
  ## 司马错得蜀既得楚
  if (!exists('.蜀道')) {
    .蜀道 <- getwd() |> 
      {\(.) str_split(., '/')}() |> 
      {\(.) c('/', .[[1]][2:5])}() |> 
      {\(.) c(., 'zhongkehongqi-cangku/')}() |> 
      {\(.) paste(., collapse = '/')}() |> 
      {\(.) substring(., 2)}()
  }
  if (!exists('.蜀道书轩')) .蜀道书轩 <- paste0(.蜀道, '诸子百家学府/中国年号通史/')
  if (!dir.exists(.蜀道书轩))  dir.create(.蜀道书轩)
  
  ## 中国年号通史
  干支 <- c('甲子', '乙丑', '丙寅', '丁卯', '戊辰', '己巳', '庚午', '辛未', '壬申', '癸酉', '甲戌', '乙亥', '丙子', '丁丑', '戊寅', '己卯', '庚辰', '辛巳', '壬午', '癸未', '甲申', '乙酉', '丙戌', '丁亥', '戊子', '己丑', '庚寅', '辛卯', '壬辰', '癸巳', '甲午', '乙未', '丙申', '丁酉', '戊戌', '己亥', '庚子', '辛丑', '壬寅', '癸卯', '甲辰', '乙巳', '丙午', '丁未', '戊申', '己酉', '庚戌', '辛亥', '壬子', '癸丑', '甲寅', '乙卯', '丙辰', '丁巳', '戊午', '己未', '庚申', '辛酉', '壬戌', '癸亥')
  
  生肖 <- c('鼠', '牛', '虎', '兔', '龙', '蛇', '马', '羊', '猴', '鸡', '狗', '猪')
  
  朝代 <- c('唐朝', '宋朝', '元朝', '明朝', '清朝', '周朝', '三皇五帝', '三黄五帝', '商朝', '东周', '夏朝', '汉朝', '大理', '春秋', '西周', '战国', '西汉', '辽国', '东汉', '北朝', '西夏', '南北朝', '南朝', '北宋', '晋朝', '南宋', '北魏', '十六国', '金朝', '东晋', '吴越', '前凉', '三国', '南朝宋', '南平', '南朝梁', '南汉', '五代十国', '孙吴', '西晋', '西秦', '曹魏', '马楚', '成汉', '前秦', '蜀汉', '北凉', '南唐', '隋朝', '闽国', '杨吴')
  
  帝王 <- c('秦始皇嬴政', '清圣祖玄烨', '清高宗弘历', '清世宗胤禛', '明太祖朱元璋', '明成祖朱棣', '蒙古太祖铁木真', '西汉武帝刘彻', '西汉文帝刘恒', '元世祖忽必烈', '北宋徽宗赵佶', '北宋太祖赵匡胤', '唐玄宗李隆基', '周武曌武则天', '唐太宗李世民', '唐高宗李治', '隋文帝杨坚', '隋炀帝杨广', '东周赧王姬延', '秦昭襄王嬴稷', '西夏仁宗李仁孝', '西周穆王姬满', '西夏崇宗李乾顺', '东周平王姬宜臼', '东周显王姬扁', '辽圣宗耶律隆绪', '明神宗朱翊钧', '南朝梁高祖萧衍', '辽道宗耶律洪基', '明世宗朱厚熜', '西周宣王姬静', '东周敬王姬匄', '北宋仁宗赵祯', '南宋理宗赵昀', '三国蜀汉后主刘禅', '西周厉王姬胡', '南宋高宗赵构', '元惠宗妥欢帖睦尔', '清德宗载湉', '殷商武乙子瞿', '东周襄王姬郑', '东汉光武帝刘秀', '东汉献帝刘协', '成汉武帝李雄', '南宋宁宗赵扩', '清宣宗旻宁', '殷商纣王子辛', '北朝北魏世祖拓跋焘', '东周贞定王姬介', '金世宗完颜雍')
  
  天朝 <- 天朝
  
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
    ## 西夏
    if (天朝 == '西夏') {
      页 <- 9
      链 <- c('https://www.cidianwang.com/nianhao/xixia_e9818.htm', paste0('https://www.cidianwang.com/nianhao/xixia_e9818_', 1:页, '.htm'))
    }
    ## 南北朝
    if (天朝 == '南北朝') {
      页 <- 8
      链 <- c('https://www.cidianwang.com/nianhao/nanbeichao_93342.htm', paste0('https://www.cidianwang.com/nianhao/nanbeichao_93342_', 1:页, '.htm'))
    }
    ## 南朝
    if (天朝 == '南朝') {
      页 <- 8
      链 <- c('https://www.cidianwang.com/nianhao/nanchao_662ae.htm', paste0('https://www.cidianwang.com/nianhao/nanchao_662ae_', 1:页, '.htm'))
    }
    ## 北宋
    if (天朝 == '北宋') {
      页 <- 8
      链 <- c('https://www.cidianwang.com/nianhao/beisong_d5762.htm', paste0('https://www.cidianwang.com/nianhao/beisong_d5762_', 1:页, '.htm'))
    }
    ## 晋朝
    if (天朝 == '晋朝') {
      页 <- 7
      链 <- c('https://www.cidianwang.com/nianhao/jinchao_78b45.htm', paste0('https://www.cidianwang.com/nianhao/jinchao_78b45_', 1:页, '.htm'))
    }
    ## 南宋
    if (天朝 == '南宋') {
      页 <- 7
      链 <- c('https://www.cidianwang.com/nianhao/nansong_76a07.htm', paste0('https://www.cidianwang.com/nianhao/nansong_76a07_', 1:页, '.htm'))
    }
    ## 北魏
    if (天朝 == '北魏') {
      页 <- 7
      链 <- c('https://www.cidianwang.com/nianhao/beiwei_982ed.htm', paste0('https://www.cidianwang.com/nianhao/beiwei_982ed_', 1:页, '.htm'))
    }
    ## 十六国
    if (天朝 == '十六国') {
      页 <- 6
      链 <- c('https://www.cidianwang.com/nianhao/shiliuguo_c0b84.htm', paste0('https://www.cidianwang.com/nianhao/shiliuguo_c0b84_', 1:页, '.htm'))
    }
    ## 金朝
    if (天朝 == '金朝') {
      页 <- 5
      链 <- c('https://www.cidianwang.com/nianhao/jinchao_2f4da.htm', paste0('https://www.cidianwang.com/nianhao/jinchao_2f4da_', 1:页, '.htm'))
    }
    ## 东晋
    if (天朝 == '东晋') {
      页 <- 5
      链 <- c('https://www.cidianwang.com/nianhao/dongjin_7fea2.htm', paste0('https://www.cidianwang.com/nianhao/dongjin_7fea2_', 1:页, '.htm'))
    }
    ## 吴越
    if (天朝 == '吴越') {
      页 <- 3
      链 <- c('https://www.cidianwang.com/nianhao/wuyue_6a285.htm', paste0('https://www.cidianwang.com/nianhao/wuyue_6a285_', 1:页, '.htm'))
    }
    ## 前凉
    if (天朝 == '前凉') {
      页 <- 3
      链 <- c('https://www.cidianwang.com/nianhao/qianliang_5de73.htm', paste0('https://www.cidianwang.com/nianhao/qianliang_5de73_', 1:页, '.htm'))
    }
    ## 三国
    if (天朝 == '三国') {
      页 <- 3
      链 <- c('https://www.cidianwang.com/nianhao/sanguo_36d4c.htm', paste0('https://www.cidianwang.com/nianhao/sanguo_36d4c_', 1:页, '.htm'))
    }
    ## 南朝宋
    if (天朝 == '南朝宋') {
      页 <- 2
      链 <- c('https://www.cidianwang.com/nianhao/nanchaosong_0e8df.htm', paste0('https://www.cidianwang.com/nianhao/nanchaosong_0e8df_', 1:页, '.htm'))
    }
    ## 南平
    if (天朝 == '南平') {
      页 <- 2
      链 <- c('https://www.cidianwang.com/nianhao/nanping_d301d.htm', paste0('https://www.cidianwang.com/nianhao/nanping_d301d_', 1:页, '.htm'))
    }
    ## 南朝梁
    if (天朝 == '南朝梁') {
      页 <- 2
      链 <- c('https://www.cidianwang.com/nianhao/nanchaoliang_dfe6c.htm', paste0('https://www.cidianwang.com/nianhao/nanchaoliang_dfe6c_', 1:页, '.htm'))
    }
    ## 南汉
    if (天朝 == '南汉') {
      页 <- 2
      链 <- c('https://www.cidianwang.com/nianhao/nanhan_c820a.htm', paste0('https://www.cidianwang.com/nianhao/nanhan_c820a_', 1:页, '.htm'))
    }
    ## 五代十国
    if (天朝 == '五代十国') {
      页 <- 2
      链 <- c('https://www.cidianwang.com/nianhao/wudaishiguo_5a803.htm', paste0('https://www.cidianwang.com/nianhao/wudaishiguo_5a803_', 1:页, '.htm'))
    }
    ## 孙吴
    if (天朝 == '孙吴') {
      页 <- 2
      链 <- c('https://www.cidianwang.com/nianhao/sunwu_92ea8.htm', paste0('https://www.cidianwang.com/nianhao/sunwu_92ea8_', 1:页, '.htm'))
    }
    ## 西晋
    if (天朝 == '西晋') {
      页 <- 2
      链 <- c('https://www.cidianwang.com/nianhao/xijin_8d2ec.htm', paste0('https://www.cidianwang.com/nianhao/xijin_8d2ec_', 1:页, '.htm'))
    }
    ## 西秦
    if (天朝 == '西秦') {
      页 <- 2
      链 <- c('https://www.cidianwang.com/nianhao/xiqin_4970a.htm', paste0('https://www.cidianwang.com/nianhao/xiqin_4970a_', 1:页, '.htm'))
    }
    ## 曹魏
    if (天朝 == '曹魏') {
      页 <- 2
      链 <- c('https://www.cidianwang.com/nianhao/caowei_20ec0.htm', paste0('https://www.cidianwang.com/nianhao/caowei_20ec0_', 1:页, '.htm'))
    }
    ## 马楚
    if (天朝 == '马楚') {
      页 <- 2
      链 <- c('https://www.cidianwang.com/nianhao/machu_f9150.htm', paste0('https://www.cidianwang.com/nianhao/machu_f9150_', 1:页, '.htm'))
    }
    ## 成汉
    if (天朝 == '成汉') {
      页 <- 2
      链 <- c('https://www.cidianwang.com/nianhao/chenghan_db547.htm', paste0('https://www.cidianwang.com/nianhao/chenghan_db547_', 1:页, '.htm'))
    }
    ## 前秦
    if (天朝 == '前秦') {
      页 <- 2
      链 <- c('https://www.cidianwang.com/nianhao/qianqin_1d2e0.htm', paste0('https://www.cidianwang.com/nianhao/qianqin_1d2e0_', 1:页, '.htm'))
    }
    ## 蜀汉
    if (天朝 == '蜀汉') {
      页 <- 2
      链 <- c('https://www.cidianwang.com/nianhao/shuhan_e2ebc.htm', paste0('https://www.cidianwang.com/nianhao/shuhan_e2ebc_', 1:页, '.htm'))
    }
    ## 北凉
    if (天朝 == '北凉') {
      页 <- 1
      链 <- c('https://www.cidianwang.com/nianhao/beiliang_fec3b.htm', paste0('https://www.cidianwang.com/nianhao/beiliang_fec3b_', 1:页, '.htm'))
    }
    ## 南唐
    if (天朝 == '南唐') {
      页 <- 1
      链 <- c('https://www.cidianwang.com/nianhao/nantang_b1086.htm', paste0('https://www.cidianwang.com/nianhao/nantang_b1086_', 1:页, '.htm'))
    }
    ## 隋朝
    if (天朝 == '隋朝') {
      页 <- 1
      链 <- c('https://www.cidianwang.com/nianhao/suichao_8e69b.htm', paste0('https://www.cidianwang.com/nianhao/suichao_8e69b_', 1:页, '.htm'))
    }
    ## 闽国
    if (天朝 == '闽国') {
      页 <- 1
      链 <- c('https://www.cidianwang.com/nianhao/minguo_a98ce.htm', paste0('https://www.cidianwang.com/nianhao/minguo_a98ce_', 1:页, '.htm'))
    }
    ## 杨吴
    if (天朝 == '杨吴') {
      页 <- 1
      链 <- c('https://www.cidianwang.com/nianhao/yangwu_81d1b.htm', paste0('https://www.cidianwang.com/nianhao/yangwu_81d1b_', 1:页, '.htm'))
    }
    
  }
  
  年号 <- plyr::ldply(1:length(链), function(迭) {
    椠 <- 链[迭] %>% 
      read_html() %>% 
      html_element('.left') %>% 
      html_text2() %>% 
      str_split('\n') %>% 
      unlist()
    
    ## 第一次出现的朝代是选项，所以正则表达式匹配第二次出现的朝代标题。
    序甲 <- grep(paste0('^', 天朝, '$'), 椠)[2]
    序乙 <- grep(paste0('^', 迭, '$'), 椠)
    椠 <- 椠[(序甲 + 3):(序乙 - 1)]
    椠 <- 椠[!椠 %in% 椠[grep('第一页|上一页', 椠)]]
    版 <- matrix(椠, ncol = 2, byrow = TRUE, dimnames = list(NULL, c('年份', '明细')))
  }, .progress = 'text')
  
  ## 椠明细
  简 <- strsplit(年号$明细, '，|：')
  
  if (!exists('天干')) 天干 <- c('甲', '乙', '丙', '丁', '戊', '己', '庾', '辛', '壬', '癸')
  if (!exists('地支')) 地支 <- c('子', '丑', '寅', '卯', '辰', '巳', '午', '未', '申', '酉', '戌', '亥')
  if (!exists('干支')) 干支 <- paste0(rep(天干, 6), rep(地支, 5))
  
  ## 序列号
  朝代序 <- lapply(简, function(牒) grep('朝代', 牒)) %>% 
    unlist
  君主序 <- lapply(简, function(牒) grep('在位皇帝', 牒)) %>% 
    unlist
  年号序 <- lapply(简, function(牒) grep('年号', 牒)) %>% 
    unlist
  
  ## 朝代列表
  朝代列 <- lapply(1:length(简), function(迭) 简[[迭]][朝代序[迭]:(君主序[迭] - 1)])
  限 <- 朝代列 %>% 
    lapply(length) %>% 
    unlist() %>% 
    max()
  朝代列 <- lapply(1:length(简), function(迭) {
    空 <- rep(NA, 限 - length(朝代列[[迭]]))
    牒 <- c(朝代列[[迭]], 空)
    names(牒) <- c('朝代', paste0('朝代', 天干[1:(length(牒) - 1)]))
    牒
  })
  
  ## 君主列表
  君主列 <- lapply(1:length(简), function(迭) 简[[迭]][君主序[迭]:(年号序[迭] - 1)])
  限 <- 君主列 %>% 
    lapply(length) %>% 
    unlist() %>% 
    max()
  君主列 <- lapply(1:length(简), function(迭) {
    空 <- rep(NA, 限 - length(君主列[[迭]]))
    牒 <- c(君主列[[迭]], 空)
    names(牒) <- c('在位皇帝', paste0('在位皇帝', 天干[1:(length(牒) - 1)]))
    牒
  })
  
  ## 年号列表
  年号列 <- lapply(1:length(简), function(迭) 简[[迭]][(年号序[迭]):length(简[[迭]])])
  限 <- 年号列 %>% 
    lapply(length) %>% 
    unlist() %>% 
    max()
  年号列 <- lapply(1:length(简), function(迭) {
    空 <- rep(NA, 限 - length(年号列[[迭]]))
    牒 <- c(年号列[[迭]], 空)
    names(牒) <- c('年号', paste0('年号', 天干[1:(length(牒) - 1)]))
    牒
  })
  
  年号通史 <- ldply(1:length(简), function(迭) {
    牒 <- 简[[迭]][1:(朝代序[[迭]] - 1)]
    names(牒) = c('农历干支', '干支', '十二生肖', '生肖')
    牒 <- c(牒, 朝代列[[迭]], 君主列[[迭]], 年号列[[迭]])
  }) %>% 
    as_tibble()
  
  年号乙 <- 年号 %>% 
    mutate(
      年份乙 = 年份 %>% 
        str_extract_all('[0-9]') %>% 
        plyr::ldply(., function(迭) {
          paste0(迭, collapse = '') %>% 
            as.numeric() %>% 
            cnum::num2c(lang = 'sc')}) %>% 
        unlist(), 
      年份 = if_else(substr(年份, 1, 3) == '公元前', paste0('公元前', 年份乙, '年'), 年份乙), 
      年份乙 = -cnum::c2num(年份乙))
  
  年号通史 <- cbind(年号乙[c('年份', '年份乙')], 年号通史) %>% 
    as_tibble()
  rm(简, 朝代序, 朝代列, 君主序, 君主列, 年号序, 年号列, 年号, 年号乙)
  
  if (levels(年号通史$农历干支) == '农历干支') 年号通史$农历干支 <- NULL
  if (levels(年号通史$十二生肖) == '生肖') 年号通史$十二生肖 <- NULL
  if (levels(年号通史$朝代) == '朝代') 年号通史$朝代 <- NULL
  if (levels(年号通史$在位皇帝) == '在位皇帝') 年号通史$在位皇帝 <- NULL
  if (levels(年号通史$年号) == '年号') 年号通史$年号 <- NULL
  
  return(年号通史)
}

