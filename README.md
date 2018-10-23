
![](www/binary-logo.jpg)

# Job Application - Quantitative Analyst

## 0) Interview Sample Question

The sample question for Interview a job in Binary.com. Here I try to write a web application which is automatically gather data, calculate, forecast, place orders, settlement and also P&L report from tip-to-toe. Here I also conducting few research tasks to test the efficiency of some statistical models, and also refer to a [Master Degree level quantitave assignment](https://github.com/englianhu/Quant-Strategies-HFT) as my studies. Hope that I can be shortlisted to be a member of Binary.com.

## 1) Question I

### 1.1) Answer

I use daily OHLCV USDJPY data (from 2014-01-01 to 2017-01-20) and application of some models to forecast the highest and lowest price :

- Auto Arima models
- Exponential Time Series
- Univariate Garch models
- Exponential Weighted Moving Average
- <s>Monte Carlo Markov Chain</s>
- <s>Bayesian Time Series</s>
- <s>Midas</s>

Kindly refer to [Binary.com Interview Q1](http://rpubs.com/englianhu/binary-Q1) ([Old link](https://englianhu.github.io/2017/09/binary-forex-trading-Q1.html) or [Alternate link](http://rpubs.com/englianhu/binary-forex-trading-Q1) or [Alternate link 2 (Added MSE comparison)](http://rpubs.com/englianhu/binary-Q1-Added)) for more information.

Well, dataset for below papers daily OHLCV of 7 currencies from 2013-01-01 to 2017-08-31:

- AUDUSD
- EURUSD
- GBPUSD
- USDCAD
- USDCHF
- USDCNY
- USDJPY

1) Here I wrote another extention page for Q1 which is analyse the multiple currencies and also models <s>from minutes to</s> daily. You are feel free to browse over [Binary.com Interview Q1 (Extention)](http://rpubs.com/englianhu/binary-Q1E) or ([Alternate link](http://rpubs.com/englianhu/316133)).

2) Here I also find the optimal arma order for GARCH models as you can refer to [GARCH模型中的`ARIMA(p,d,q)`参数最优化](http://rpubs.com/englianhu/binary-Q1FiGJRGARCH). [binary.com 面试试题 I - GARCH模型中的`ARCH in Mean`](http://rpubs.com/englianhu/binary-Q1-archm) compares the ARCHM with previous Non-ARCHM models.

3) You can also refer to [binary.com Interview Question I - Comparison of Univariate GARCH Models](http://rpubs.com/englianhu/binary-Q1Uni-GARCH) which compares the prediction accuracy of 14 GARCH models (not completed) and 9 models (mostly completed from 2013-01-01 to 2017-08-30).

- sGARCH
- fGARCH.GARCH
- fGARCH.TGARCH
- fGARCH.NGARCH
- fGARCH.NAGARCH
- fGARCH.GJRGARCH
- gjrGARCH
- iGARCH
- csGARCH

Besides, I wrote a shinyApp which display the real-time price through API. Kindly refer to [Q1App](https://beta.rstudioconnect.com/content/3073/) where [Q1App2](https://beta.rstudioconnect.com/content/3138/) is another app for financial value betting.

[binary.com Interview Question I - Multivariate GARCH Models](http://rpubs.com/englianhu/binary-Q1Multi-GARCH) introduce few multi-variate GARCH models.

- symmetric DCC
- asymmetric DCC
- Flexible DCC
- <s>GO-GARCH</s>
- <s>Copula-GARCH</s>

In order to started the high-frequency-trading statistical modelling, I inspect the dataset via [binary.com面试试题 I - 单变量数据缺失值管理](http://rpubs.com/englianhu/handle-missing-value) and also [binary.com 面试试题 I - 多变量数据缺失值管理 II](http://rpubs.com/englianhu/handle-multivariate-missing-value) but the univariate modelling caused some statistical error. The papers compares multi-methods like `interpolatan`, `kalman`, `locf` and `ma`. The [binary.com Interview Question I - Interday High Frequency Trading Models Comparison](http://rpubs.com/englianhu/binary-Q1Inter-HFT) compares SARIMA, mcsGARCH, <s>midasr, midas-garch, Levy process</s> models.

### 1.2) <span style='color:red'>Blooper</span>

Initially, I wrote a shiny app (as showing in below gif file) but it is heavily budden for loading. Kindly browse over [ShinyApp](https://beta.rstudioconnect.com/content/2367/) which contain the questions and answers of 3 questions. For the staking model, I simply forecast the highest and lowest price, and then : 
  
- Kelly criterion and using highest or lowest price for closing transaction, otherwise using closing price if the forecasted lowest/highest price is not occur.
- Placed $100 an each of the forecasted variance value and do the settlement based on the real variance value. 

<img src='www/20170113_104005.gif' width='360'>

Secondly, I wrote another app [testRealTimeTransc](https://beta.rstudioconnect.com/content/3775/) trial version to test the real time trading, and a completed version is [Q1App2](https://beta.rstudioconnect.com/content/3138/).

Due to the paper [Binary.com Interview Q1 - Tick-Data-HiLo For Daily Trading <span style='color:red'>(Blooper)</span>](http://rpubs.com/englianhu/binary-Q1TD) simulated the data and then only noticed I not yet updated the new function, then I wrote **GARCH模型中的`ARIMA(p,d,q)`参数最优化** to compare the accuracy. However my later paper simulated dataset doesn't save the $fit$ in order to retrieve the $\sigma^2$ and VaR values for stop-loss pips when I got the idea. Here I put it as blooper and start **binary-Q1 Multivariate GARCH Models** and later on will write another **FOREX Day Trade Simulation** which will simulate all tick-data but not only HiLo data.

### 1.3) Shiny Application

- **shinyApp** : `shiny::runGitHub('englianhu/binary.com-interview-question')` - Application which compare the accuracy of multiple `lasso`, `ridge` and `elastic net` models (blooper).
- **Q1App** : `shiny::runGitHub('englianhu/binary.com-interview-question', subdir = 'Q1')` - the application gather, calculate and forecast price. Once the user select currency and the forecast day, the system will auto calculate and plot the graph.
- **testRealTimeTransc** : `shiny::runGitHub('englianhu/binary.com-interview-question', subdir = 'testRealTimeTransc')` - real time trading system which auto gather, calculate the forecast price, and also place orders, as well as settlement and plot P&L everyday.
- **Q1App2** : `shiny::runGitHub('englianhu/binary.com-interview-question', subdir = 'Q1App2')` - The application contain the Banker and Punter section which applied aboved statistical modelling.

## 2) Question II

### 2.1) Answer

For question 2, I simply write an app, kindly use [Q2App](https://beta.rstudioconnect.com/content/3089/). The bivariate or trivariate poisson model might useful for analyse the probability of fund-in and fund-out by investors in order to manage whole investment pool. Unfortunately there has no such dataset avaiable for fund pool management modelling.

### 2.2) Shiny Application

- **Q2** : `shiny::runGitHub('englianhu/binary.com-interview-question', subdir = 'Q2')` - An application which applied queuing theory.

## 3) Question III

For question 3, due to the question doesn't states we only bet on the matches which overcame a certain edge, therefore I just simply list the scenario. Kindly refer to [Betting strategy](http://rpubs.com/englianhu/317677) for more informtion.

## 4) Reference

### 4.1) Question I

01. [**Stock Market Forecasting Using LASSO Linear Regression Model** *by Sanjiban Sekhar Roy, Dishant Mital, Avik Basu, Ajith Abraham (2015)*](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/reference/Stock%20Market%20Forecasting%20Using%20LASSO%20Linear%20Regression%20Model.pdf)<img src='www/hot.jpg' width='20'>
02. [**Using LASSO from lars (or glmnet) package in R for variable selection** *by Juancentro (2014)*](http://stats.stackexchange.com/questions/58531/using-lasso-from-lars-or-glmnet-package-in-r-for-variable-selection?answertab=votes#tab-top)
03. [**Difference between glmnet() and cv.glmnet() in R?** *by Amrita Sawant (2015)*](https://stackoverflow.com/questions/29311323/difference-between-glmnet-and-cv-glmnet-in-r?answertab=votes#tab-top)
04. [**Testing Kelly Criterion and Optimal f in R** *by Roy Wei (2012)*](https://alphaism.wordpress.com/2012/04/13/testing-kelly-criterion-and-optimal-f-in-r) <img src='www/hot.jpg' width='20'>
05. [**Portfolio Optimization and Monte Carlo Simulation** *by Magnus Erik Hvass Pedersen (2014)*](https://raw.githubusercontent.com/scibrokes/kelly-criterion/master/references/Portfolio%20Optimization%20and%20Monte%20Carlo%20Simulation.pdf) <img src='www/hot.jpg' width='20'>
06. [**Glmnet Vignette** *by Trevor Hastie and Junyang Qian (2014)*](https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html)
07. [**lasso怎么用算法实现？** *by shuaihuang (2010)*](https://d.cosx.org/d/101533-101533/5)
08. [**The Sparse Matrix and {glmnet}** *by Manuel Amunategui (2014)*](http://amunategui.github.io/sparse-matrix-glmnet/)
09. [**Regularization and Variable Selection via the Elastic Net** *by Hui Zou and Trevor Hastie*](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/reference/Regularization%20and%20Variable%20Selection%20via%20the%20Elastic%20Net.pdf)
10. [LASSO, Ridge, and Elastic Net](http://www4.stat.ncsu.edu/~post/josh/LASSO_Ridge_Elastic_Net_-_Examples.html) <img src='www/hot.jpg' width='20'>
11. [**热门数据挖掘模型应用入门（一）: LASSO回归** *by 侯澄钧 (2016)*](https://cosx.org/2016/10/data-mining-1-lasso)
12. [The Lasso Page](http://statweb.stanford.edu/~tibs/lasso.html)
13. [**Call_Valuation.R** *by Mariano (2016)*](https://api.rpubs.com/Mariano/call)
14. [Lecture 6 – Stochastic Processes and Monte Carlo](http://zorro-trader.com/manual/en/Lecture%206.htm) ([<span style='color:blue'>http://zorro-trader.com/manual</span>](http://zorro-trader.com/manual)) <img src='www/hot.jpg' width='20'> <img src='www/hot.jpg' width='20'>
15. [**The `caret` Package** *by Max Kuhn (2017)*](http://topepo.github.io/caret/index.html) <img src='www/hot.jpg' width='20'>
16. [Time Series Cross Validation](https://rpubs.com/crossxwill/time-series-cv) <img src='www/hot.jpg' width='20'>
17. [Character-Code.com](http://character-code.com/)
18. [**Size Matters – Kelly Optimization** *by Roy Wei (2012)*](https://alphaism.wordpress.com/2012/03/26/size-matters-kelly-optimization/) <img src='www/hot.jpg' width='20'>
19. [**Time Series Cross Validation** *by William Chiu (2015)*](https://rpubs.com/crossxwill/time-series-cv) <img src='www/hot.jpg' width='20'>
20. [**Forecasting Volatility** *by Stephen Figlewski (2004)*](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/fcad2844d7f10c486f3601af9932f49973548e4b/reference/Focasting%20Volatility.pdf)
21. [**Successful Algorithmic Trading** *by Michael Halls Moore (2015)*](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/fcad2844d7f10c486f3601af9932f49973548e4b/reference/Successful%20Algorithmic%20Trading.pdf) <img src='www/hot.jpg' width='20'> <img src='www/hot.jpg' width='20'>
22. [**Financial Risk Modelling and Portfolio Optimization with R (2nd Edt)** *by Bernhard Praff (2016)*](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/reference/Financial%20Risk%20Modelling%20and%20Portfolio%20Optimization%20with%20R%20(2nd%20Edt).pdf) <img src='www/hot.jpg' width='20'>
23. [**Analyzing Financial Data and Implementing Financial Models using R** *by Clifford S.Ang (2015)*](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/eec3bbe99c61b4e2e2f4a2b1c47e7a2fca6106c4/reference/Analyzing%20Financial%20Data%20and%20Implementing%20Financial%20Models%20using%20R.pdf) <img src='www/hot.jpg' width='20'>

### 4.2) Question II

01. [Queueing model 534 in Excel](https://github.com/englianhu/binary.com-interview-question/blob/master/reference/Queue-534.xls) <img src='www/hot.jpg' width='20'>
02. [Queueing model macro in Excel](https://github.com/englianhu/binary.com-interview-question/blob/master/reference/QueueMacros.xls) <img src='www/hot.jpg' width='20'>
03. [Queueing up in R, (continued)](https://www.r-bloggers.com/queueing-up-in-r-continued)
04. [Waiting in line, waiting on R](https://www.r-bloggers.com/waiting-in-line-waiting-on-r)
05. [Simulating a Queue in R](https://www.r-bloggers.com/simulating-a-queue-in-r/)
06. [What is the queue data structure in R?](https://www.researchgate.net/post/What_is_the_queue_data_structure_in_R#59d5b01b404854fdc9168902)
07. [Implementing a Queue as a Reference Class](https://www.r-bloggers.com/implementing-a-queue-as-a-reference-class/)
08. [queue implementation?](http://r.789695.n4.nabble.com/queue-implementation-td2529272.html)
09. [Queueing Theory Calculator](http://www.supositorio.com/rcalc) <img src='www/hot.jpg' width='20'>
10. [ **The Pith of Performance** *by Neil Gunther (2010)*](http://perfdynamics.blogspot.my/2010/05/simulating-queue-in-r.html?m=1)
11. [Computationally Efficient Simulation of Queues - The R Package queuecomputer](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/reference/Computationally%20Efficient%20Simulation%20of%20Queues%20-%20The%20R%20Package%20queuecomputer.pdf)
12. [Waiting-Line Models](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/reference/Waiting-Line%20Models.pdf)
13. [Queues with Breakdowns and Customer Discouragement](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/reference/Queues%20with%20Breakdowns%20and%20Customer%20Discouragement.pdf)

### 4.3) Question III

01. [Data APIs/feeds available as packages in R](http://stats.stackexchange.com/questions/12670/data-apis-feeds-available-as-packages-in-r)
02. [Application of Kelly Criterion model in Sportsbook Investment](https://github.com/scibrokes/kelly-criterion)

---

<span style='color:RoyalBlue'>**Powered by - Copyright® Intellectual Property Rights of [<img src='www/scb_logo.jpg' width='64'>®](http://www.scibrokes.com)個人の経営企業**</span>
