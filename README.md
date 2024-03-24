
<img src='诸子百家考工记/世博量化.png' height='100'> <img src='诸子百家考工记/大秦赋 - 北京大学.png' height='100'>

<img src='诸子百家考工记/binary-logo.png' width='240'>

---

[<img src='诸子百家考工记/红旗礼逆袭.gif' height='20' width='47'>](https://www.chinaredflag.cn) [<img src='诸子百家考工记/赢家黄氏江夏堂.png' height='20'>](https://englianhu.wordpress.com) [<img src='诸子百家考工记/赢家ξng黄氏江夏堂 - 量化对冲（中）.png' height='20'>](https://gitee.com/englianhu) [<img src='诸子百家考工记/大秦赋 - 北京大学.png' height='20'>](https://www.pku.edu.cn)

# 次元期权（binary.com）<span style='color:#DE5D83; background-color:black;'>**量化分析员/量化交易员**面试题</span>

**大秦赋 (Chinese Emperor)**<br>
春秋战国《*礼记•经解*》<br>
孔子曰：『君子慎始，差若毫厘，缪以千里。』

> <span style='color:#FFEBCD; background-color:#991B00;'>**《礼记•经解》孔子曰：**</span><span style='color:#DimGrey; background-color:#D52600;'>*「君子慎始，差若毫厘，谬以千里。」*</span>[^1]

*引用：[「快懂百科」《礼记•经解》](https://www.baike.com/wikiid/2225522569881832051?view_id=2tt3iw3blkq000)和[第一范文网：差之毫厘，谬以千里的故事](https://www.diyifanwen.com/chengyu/liuziyishangchengyugushi/2010051523105152347092749890.htm)和[「百度百科」春秋时期孔子作品《礼记•经解》](https://baike.baidu.com/item/%E7%A4%BC%E8%AE%B0%C2%B7%E7%BB%8F%E8%A7%A3/2523092)和[「當代中國」差之毫釐 謬以千里](https://www.ourchinastory.com/zh/2962/%E5%B7%AE%E4%B9%8B%E6%AF%AB%E9%87%90%20%E8%AC%AC%E4%BB%A5%E5%8D%83%E9%87%8C)*

[^1]: [HTML Color Codes](https://html-color.codes)

## 面试题

应征次元期权（法人马企）面试入门测验。借鉴西蒙·柯林斯的 https://matchodds.org （或詹姆斯·西蒙斯的高频量化对冲基金---文艺复兴科技）愚生尝试编写个自动采撷数据、科研回测、筹算、算卜预测、自动下单、结算、显示盈亏、风险管理报告、评估再改良高频量化对冲投资战略的一条龙服务的智能网页应用。愚生于此尝试着手于科研多元化计数/机数建模，再评估有效性与可行性，并参阅[硕士程度量化作业（英）](https://github.com/englianhu/Quant-Strategies-HFT)。盼受禄于**次元期权**。

## 第一题

### 第一题第一章）解答

愚生使用从阳历二零一四年一月一日至二零一七年一月廿日的每日**美日兑换**阴阳烛加交易量数据，再通过以下一些计数/机数建模来算卜预测最高价与最低价：

- 自回归移动平均模型
- 指数平滑模型
- 单变量广义自回归条件异方差模型
- 加权指数移动平均模型
- <s>蒙迪卡洛马尔科夫链</s>
- <s>贝叶斯时间序列</s>
- <s>[混频抽样回归](https://zhuanlan.zhihu.com/p/343307253) / [MIDAS：混频数据回归](https://www.lianxh.cn/news/8ab193e6f04cc.html)</s>

请查阅[次元期权面试题一（英）](http://rpubs.com/englianhu/binary-Q1) ([旧链接](https://englianhu.github.io/2017/09/binary-forex-trading-Q1.html)或[备用网址](http://rpubs.com/englianhu/binary-forex-trading-Q1)或[备用网址二（添加均方误差，比较计数/机数模型的精准度）](http://rpubs.com/englianhu/binary-Q1-Added))。

此栏开始以下相关科研文献所使用的阴阳烛加交易量数据有七种货币兑换，从阳历二零一三年一月一日至二零一七年八月卅一日：

- 澳美兑换（AUDUSD）
- 欧美兑换（EURUSD）
- 英美兑换（GBPUSD）
- 美加兑换（USDCAD）
- 美瑞兑换（USDCHF）
- 美中兑换（USDCNY）
- 美日兑换（USDJPY）

文献如下：

- 一）[次元期权面试题一 - 延伸版（英）](http://rpubs.com/englianhu/binary-Q1E) or ([备用网址](http://rpubs.com/englianhu/316133))尝试使用多元计数/机数模型来评估与比较多元货币每日兑换率。
- 二）[广义自回归条件异方差模型中的`ARIMA(p,d,q)`参数最优化](http://rpubs.com/englianhu/binary-Q1FiGJRGARCH)筹算出规律`p,d,q`最优值，并将之应用于广义自回归条件异方差模型提升计数/机数模型的算卜/预测精准度。[次元期权面试试题一 - 广义自回归条件异方差模型中的`ARCH in Mean`](http://rpubs.com/englianhu/binary-Q1-archm)比较ARCHM和非ARCHM的原模型。
- 三）[次元期权面试题一 - 单变量广义自回归条件异方差模型（英）](http://rpubs.com/englianhu/binary-Q1Uni-GARCH)比较了十四个广义自回归条件异方差模型系列（有缺失值，不够工整数据）和过滤后的九个计数/机数模型如下（从阳历二零一三年一月一日至二零一七年八月卅一日，工整数据）的算卜/预测值的精准度。

  - sGARCH
  - fGARCH.GARCH
  - fGARCH.TGARCH
  - fGARCH.NGARCH
  - fGARCH.NAGARCH
  - fGARCH.GJRGARCH
  - gjrGARCH
  - iGARCH
  - csGARCH

- 四）[Q1App](https://beta.rstudioconnect.com/content/3073)是个通过应用程序编程接口采撷某网站实时汇价的闪霓应用，而[Q1App2](https://beta.rstudioconnect.com/content/3138)是个金融投注的闪霓应用。
- 五）[次元期权面试题一 - 多变量广义自回归条件异方差模型（英）](http://rpubs.com/englianhu/binary-Q1Multi-GARCH)介绍、评估并比较多变量广义自回归条件异方差模型系列如下：

  - symmetric DCC
  - asymmetric DCC
  - Flexible DCC
  - <s>GO-GARCH</s>
  - <s>Copula-GARCH</s>

为了着手于高频量化对冲数据计数/机数建模，尝试审查并整顿数据，文献[次元期权面试试题一 - 单变量数据缺失值管理](http://rpubs.com/englianhu/handle-missing-value)和文献[次元期权面试试题一 - 多变量数据缺失值管理（乙）](http://rpubs.com/englianhu/handle-multivariate-missing-value)但单变量建模出现一些错误（一些是人为的美国洋番黑客洲际入侵犯罪），文献中使用多种弥补数据缺失值的计数/机数筹算方法如`interpolatan`、`kalman`、`locf`和`ma`。. The [次元期权面试题一 - 日间高频交易计数/机数建模比较（英）](http://rpubs.com/englianhu/binary-Q1Inter-HFT)比较了ts、msts、SARIMA、mcsGARCH、<s>midasr、midas-garch、Levy process</s> 计数/机数模型。

### 第一题第二章）<span style='color:red'>幕后花絮</span>

Initially, I wrote a shiny app (as showing in below gif file) but it is heavily budden for loading. Kindly browse over [ShinyApp](https://beta.rstudioconnect.com/content/2367/) (Kindly refer to [binary.com Interview Question I - Lasso, Elastic-Net and Ridge Regression](http://rpubs.com/englianhu/binary-Q1L-EN-R) for more information) which contain the questions and answers of 3 questions. For the staking model, I simply forecast the highest and lowest price, and then : 

- Kelly criterion and using highest or lowest price for closing transaction, otherwise using closing price if the forecasted lowest/highest price is not occur.
- Placed $100 an each of the forecasted variance value and do the settlement based on the real variance value. 

<img src='诸子百家考工记/20170113_104005.gif' width='360'>

Secondly, I wrote another app [testRealTimeTransc](https://beta.rstudioconnect.com/content/3775) trial version to test the real time trading, and a completed version is [Q1App2](https://beta.rstudioconnect.com/content/3138).

Due to the paper [Binary.com Interview Q1 - Tick-Data-HiLo For Daily Trading <span style='color:red'>(Blooper)</span>](http://rpubs.com/englianhu/binary-Q1TD) simulated the data and then only noticed I not yet updated the new function, then I wrote **广义自回归条件异方差模型中的`ARIMA(p,d,q)`参数最优化** to compare the accuracy. However my later paper simulated dataset doesn't save the $fit$ in order to retrieve the $\sigma^2$ and VaR values for stop-loss pips when I got the idea. Here I put it as blooper and start **binary-Q1 Multivariate GARCH Models** and later on will write another **FOREX Day Trade Simulation** which will simulate all tick-data but not only HiLo data.

### 第一题第三章）闪霓应用

- **shinyApp** : `shiny::runGitHub('englianhu/binary.com-interview-question')` - Application which compare the accuracy of multiple `lasso`, `ridge` and `elastic net` models (blooper).
- **Q1App** : `shiny::runGitHub('englianhu/binary.com-interview-question', subdir = 'Q1')` - the application gather, calculate and forecast price. Once the user select currency and the forecast day, the system will auto calculate and plot the graph.
- **testRealTimeTransc** : `shiny::runGitHub('englianhu/binary.com-interview-question', subdir = 'testRealTimeTransc')` - real time trading system which auto gather, calculate the forecast price, and also place orders, as well as settlement and plot P&L everyday.
- **Q1App2** : `shiny::runGitHub('englianhu/binary.com-interview-question', subdir = 'Q1App2')` - The application contain the Banker and Punter section which applied aboved statistical modelling.

## 第二题

### 第二题第一章）解答

第二题，尝试编写个闪霓应用[Q2App](https://beta.rstudioconnect.com/content/3089)，双变量或三变量泊松计数/机数模型可以运用在分析投资者在资本投入与题款/脱售基金上的概率，方便管理投资基金的整体投资基金资本与资金流动性。奈何并无任何投资基金的投资者资金流动数据可供科研用途。

### 第二题第二章）闪霓应用

**Q2**：运行代码`shiny::runGitHub('englianhu/binary.com-interview-question', subdir = 'Q2')`来通过闪霓应用实践[排队论（运筹学理论）](https://baike.baidu.com/item/排队论/938889)。

## 第三题

For question 3, due to the question doesn't states we only bet on the matches which overcame a certain edge, therefore I just simply list the scenario. Kindly refer to [Betting strategy](http://rpubs.com/englianhu/317677) for more informtion.

## 参考资源

### 4.1)第一题

01. [**Stock Market Forecasting Using LASSO Linear Regression Model** *by Sanjiban Sekhar Roy, Dishant Mital, Avik Basu, Ajith Abraham (2015)*](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/reference/Stock%20Market%20Forecasting%20Using%20LASSO%20Linear%20Regression%20Model.pdf)❤‍🔥
02. [**Using LASSO from lars (or glmnet) package in R for variable selection** *by Juancentro (2014)*](http://stats.stackexchange.com/questions/58531/using-lasso-from-lars-or-glmnet-package-in-r-for-variable-selection?answertab=votes#tab-top)
03. [**Difference between glmnet() and cv.glmnet() in R?** *by Amrita Sawant (2015)*](https://stackoverflow.com/questions/29311323/difference-between-glmnet-and-cv-glmnet-in-r?answertab=votes#tab-top)
04. [**Testing Kelly Criterion and Optimal f in R** *by Roy Wei (2012)*](https://alphaism.wordpress.com/2012/04/13/testing-kelly-criterion-and-optimal-f-in-r) ❤‍🔥
05. [**Portfolio Optimization and Monte Carlo Simulation** *by Magnus Erik Hvass Pedersen (2014)*](https://raw.githubusercontent.com/scibrokes/kelly-criterion/master/references/Portfolio%20Optimization%20and%20Monte%20Carlo%20Simulation.pdf) ❤‍🔥
06. [**Glmnet Vignette** *by Trevor Hastie and Junyang Qian (2014)*](https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html)
07. [**lasso怎么用算法实现？** *by shuaihuang (2010)*](https://d.cosx.org/d/101533-101533/5)
08. [**The Sparse Matrix and {glmnet}** *by Manuel Amunategui (2014)*](http://amunategui.github.io/sparse-matrix-glmnet/)
09. [**Regularization and Variable Selection via the Elastic Net** *by Hui Zou and Trevor Hastie*](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/reference/Regularization%20and%20Variable%20Selection%20via%20the%20Elastic%20Net.pdf)
10. [LASSO, Ridge, and Elastic Net](http://www4.stat.ncsu.edu/~post/josh/LASSO_Ridge_Elastic_Net_-_Examples.html) ❤‍🔥
11. [**热门数据挖掘模型应用入门（一）: LASSO回归** *by 侯澄钧 (2016)*](https://cosx.org/2016/10/data-mining-1-lasso)
12. [The Lasso Page](http://statweb.stanford.edu/~tibs/lasso.html)
13. [**Call_Valuation.R** *by Mariano (2016)*](https://api.rpubs.com/Mariano/call)
14. [Lecture 6 – Stochastic Processes and Monte Carlo](http://zorro-trader.com/manual/en/Lecture%206.htm) ([<span style='color:blue'>http://zorro-trader.com/manual</span>](http://zorro-trader.com/manual)) ❤‍🔥 ❤‍🔥
15. [**The `caret` Package** *by Max Kuhn (2017)*](http://topepo.github.io/caret/index.html) ❤‍🔥
16. [Time Series Cross Validation](https://rpubs.com/crossxwill/time-series-cv) ❤‍🔥
17. [Character-Code.com](http://character-code.com/)
18. [**Size Matters – Kelly Optimization** *by Roy Wei (2012)*](https://alphaism.wordpress.com/2012/03/26/size-matters-kelly-optimization/) ❤‍🔥
19. [**Time Series Cross Validation** *by William Chiu (2015)*](https://rpubs.com/crossxwill/time-series-cv) ❤‍🔥
20. [**Forecasting Volatility** *by Stephen Figlewski (2004)*](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/fcad2844d7f10c486f3601af9932f49973548e4b/reference/Focasting%20Volatility.pdf)
21. [**Successful Algorithmic Trading** *by Michael Halls Moore (2015)*](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/fcad2844d7f10c486f3601af9932f49973548e4b/reference/Successful%20Algorithmic%20Trading.pdf) ❤‍🔥 ❤‍🔥
22. [**Financial Risk Modelling and Portfolio Optimization with R (2nd Edt)** *by Bernhard Praff (2016)*](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/reference/Financial%20Risk%20Modelling%20and%20Portfolio%20Optimization%20with%20R%20(2nd%20Edt).pdf) ❤‍🔥
23. [**Analyzing Financial Data and Implementing Financial Models using R** *by Clifford S.Ang (2015)*](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/eec3bbe99c61b4e2e2f4a2b1c47e7a2fca6106c4/reference/Analyzing%20Financial%20Data%20and%20Implementing%20Financial%20Models%20using%20R.pdf) ❤‍🔥

### 4.2) 第二题

01. [Queueing model 534 in Excel](https://github.com/englianhu/binary.com-interview-question/blob/master/reference/Queue-534.xls) ❤‍🔥
02. [Queueing model macro in Excel](https://github.com/englianhu/binary.com-interview-question/blob/master/reference/QueueMacros.xls) ❤‍🔥
03. [Queueing up in R, (continued)](https://www.r-bloggers.com/queueing-up-in-r-continued)
04. [Waiting in line, waiting on R](https://www.r-bloggers.com/waiting-in-line-waiting-on-r)
05. [Simulating a Queue in R](https://www.r-bloggers.com/simulating-a-queue-in-r/)
06. [What is the queue data structure in R?](https://www.researchgate.net/post/What_is_the_queue_data_structure_in_R#59d5b01b404854fdc9168902)
07. [Implementing a Queue as a Reference Class](https://www.r-bloggers.com/implementing-a-queue-as-a-reference-class/)
08. [queue implementation?](http://r.789695.n4.nabble.com/queue-implementation-td2529272.html)
09. [Queueing Theory Calculator](http://www.supositorio.com/rcalc) ❤‍🔥
10. [ **The Pith of Performance** *by Neil Gunther (2010)*](http://perfdynamics.blogspot.my/2010/05/simulating-queue-in-r.html?m=1)
11. [Computationally Efficient Simulation of Queues - The R Package queuecomputer](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/reference/Computationally%20Efficient%20Simulation%20of%20Queues%20-%20The%20R%20Package%20queuecomputer.pdf)
12. [Waiting-Line Models](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/reference/Waiting-Line%20Models.pdf)
13. [Queues with Breakdowns and Customer Discouragement](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/reference/Queues%20with%20Breakdowns%20and%20Customer%20Discouragement.pdf)

### 4.3) 第三题

01. [Data APIs/feeds available as packages in R](http://stats.stackexchange.com/questions/12670/data-apis-feeds-available-as-packages-in-r)
02. [Application of Kelly Criterion model in Sportsbook Investment](https://github.com/scibrokes/kelly-criterion)

---

<img src='诸子百家考工记/deriv.png' width='260'>

[<img src='诸子百家考工记/红旗礼逆袭.gif' height='20' width='47'>](https://www.chinaredflag.cn) [<img src='诸子百家考工记/赢家黄氏江夏堂.png' height='20'>](https://englianhu.wordpress.com) [<img src='诸子百家考工记/赢家ξng黄氏江夏堂 - 量化对冲（中）.png' height='20'>](https://gitee.com/englianhu) [<img src='诸子百家考工记/大秦赋 - 北京大学.png' height='20'>](https://www.pku.edu.cn)

# 量化交易

### 一）<span style='color:RoyalBlue'>简介</span>

- 从农历二零一八年[Binary.com is Rebranding to Deriv.com](https://derivdotcom.medium.com/binary-com-is-rebranding-to-deriv-com-and-here-is-everything-you-need-to-know-6f4a8513c84b)科研项目耽搁两年有余，二零二零年的[二元期权/次元期权（Binary.com） → 金融衍生/金融起源（Deriv.com）](https://englianhu.medium.com/binary-com-deriv-com-6058cdbfc3a1)文章中阐明一些已发布的科研论文，并继续科研对冲基金高频量化交易，不过在此并非面试Deriv.com而是科研用途。
- [金融衍生/金融起源（Deriv.com）和次元期权（Binary.com）加入金融委员会](https://caifuhao.eastmoney.com/news/20200609155259637111910)
- 科研项目中所使用的汇价原始数据，皆由FXCM官网下载，储存至[猫城：世博量化/real-time-fxcm](https://github.com/scibrokes/real-time-fxcm)回测科研统计建模用途。

<span style='color:red'>**已将次元期权科研项目相关数据，一律迁移至[「数据仓库」次元期权（binary.com）量化分析员/量化交易员面试题](https://github.com/englianhu/binary.com-interview-question-data)，并继续科研高频量化对冲计数/机数建模。**</span>

季节性时间序列与高频量化对冲计数/机数模型如下：

- [Deriv.com - Interday High Frequency Trading Models Comparison <span style='color:RoyalBlue'>Review (Part I)</span>](https://rpubs.com/englianhu/binary-Q1Inter-HFT-RV1)（或[RStudioConnect.com备用网址](https://beta.rstudioconnect.com/content/16240/binary-Q1Inter-HFT-RV1.html)）
- [Deriv.com - Interday & Intraday High Frequency Trading Models Comparison <span style='color:#4E79A7'>**Review (Part II)**</span>](https://rpubs.com/englianhu/742275)（[或[RStudioConnect.com备用网址](https://beta.rstudioconnect.com/content/16442/binary-Q1Inter-HFT-RV2.html)）
- 金融衍生 - 筛选日内高频量化交易统计模型（第III部）
  - [金融衍生 - 筛选日内高频量化交易统计模型 <span style='color:#4E79A7'>**（第III部）**</span>](https://rpubs.com/englianhu/HFT-RV3)
  - [金融衍生 - 筛选日内高频量化交易统计模型 <span style='color:#4E79A7'>**第三部（衍生版）**</span>](https://englianhu.github.io/民国一百一十三年（甲辰年）/桃月/binary-Q1Inter-HFT-RV3E.html)
- 金融衍生 - 筛选日内高频量化交易统计模型（丁）

### 二）<span style='color:red'>幕后花絮</span>

[Deriv.com - Interday High Frequency Trading Models Comparison <span style='color:red'>Blooper</span>](https://rpubs.com/englianhu/binary-Q1Inter-HFT)着手于季节性计数/机数建模，而文献中提及的一些计数/机数模型mcsGARCH、midasr、midas-garch、Levy process会在日后继续科研。

<br><br>

---

[<img src='诸子百家考工记/世博量化.png' height='14'/> Sςιβrοκεrs Trαdιηg®](http://www.scibrokes.com)<br>
<span style='color:RoyalBlue'>**[<img src='诸子百家考工记/世博量化.png' height='14'/> 世博量化®](http://www.scibrokes.com)企业知识产权®及版权®所有，盗版必究。**</span>
