
![](www/binary-logo.jpg)

# Job Application - Quantitative Analyst

## Interview Sample Question

  The sample question for Interview a job in Binary options.

### Question I

  I use daily OHLCV USDJPY data (from 2014-01-01 to 2017-01-20) and application of some models to forecast the highest and lowest price :
  
  - Auto Arima models
  - Exponential Time Series
  - Univariate Garch models
  - Exponential Weighted Moving Average
  - <s>Monte Carlo Markov Chain</s>
  - <s>Bayesian Time Series</s>
  - <s>Midas</s>
  
  For the staking model, I simply forecast the highest and lowest price, and then : 
  
  - Kelly criterion and using highest or lowest price for closing transaction, otherwise using closing price if the forecasted lowest/highest price is not occur.
  - Placed $100 an each of the forecasted variance value and do the settlement based on the real variance value. 

  Kindly refer to [Binary.com Interview Q1](https://englianhu.github.io/2017/09/binary-forex-trading-Q1.html) ([Alternate link](http://rpubs.com/englianhu/binary-forex-trading-Q1))
  
  Besides, I wrote a shinyApp which display the real-time price through API. Kindly refer to [Q1App](https://beta.rstudioconnect.com/content/3073/) where [Q1App2](https://beta.rstudioconnect.com/content/3138/) is another app for financial value betting.

<span style='color:red'>**Blooper...**</span>

  Initially, I wrote a shiny app (as showing in below gif file) but it is heavily budden for loading. Kindly browse over [Q1 ShinyApp](https://beta.rstudioconnect.com/content/2367/).

<img src='www/20170113_104005.gif' width='360'>

  Secondly, I wrote another app [testRealTimeTransc](https://beta.rstudioconnect.com/content/3775/) trial version to test the real time trading, and a completed version is [Q1App2](https://beta.rstudioconnect.com/content/3138/).

  Here I wrote another extention page for Q1 which is analyse the multiple currencies and also models from minutes to daily. You are feel free to browse over [Binary.com Interview Q1E](http://rpubs.com/englianhu/316133).

### Question II

  For question 2, I simply write an app, kindly use [Q2App](https://beta.rstudioconnect.com/content/3089/).

### Question III

  For question 3, due to the question doesn't states we only bet on the matches which overcame a certain edge, therefore I just simply list the scenario. Kindly refer to [Betting strategy](http://rpubs.com/englianhu/317677) for more informtion.

## Reference

### Question I

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
  19. [**Time Series Cross Validation** *by William Chiu (2015)*](https://rpubs.com/crossxwill/time-series-cv) <img src='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/www/hot.jpg?raw=true' width='20'>
  20. [**Forecasting Volatility** *by Stephen Figlewski (2004)*](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/fcad2844d7f10c486f3601af9932f49973548e4b/reference/Focasting%20Volatility.pdf)
  21. [**Successful Algorithmic Trading** *by Michael Halls Moore (2015)*](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/fcad2844d7f10c486f3601af9932f49973548e4b/reference/Successful%20Algorithmic%20Trading.pdf) <img src='www/hot.jpg' width='20'> <img src='www/hot.jpg' width='20'>
  22. [**Financial Risk Modelling and Portfolio Optimization with R (2nd Edt)** *by Bernhard Praff (2016)*](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/reference/Financial%20Risk%20Modelling%20and%20Portfolio%20Optimization%20with%20R%20(2nd%20Edt).pdf) <img src='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/www/hot.jpg?raw=true' width='20'>
  23. [**Analyzing Financial Data and Implementing Financial Models using R** *by Clifford S.Ang (2015)*](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/eec3bbe99c61b4e2e2f4a2b1c47e7a2fca6106c4/reference/Analyzing%20Financial%20Data%20and%20Implementing%20Financial%20Models%20using%20R.pdf) <img src='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/www/hot.jpg?raw=true' width='20'>

### Question II

  01. [Queueing model 534 in Excel](https://github.com/englianhu/binary.com-interview-question/blob/master/reference/Queue-534.xls) <img src='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/www/hot.jpg?raw=true' width='20'>
  02. [Queueing model macro in Excel](https://github.com/englianhu/binary.com-interview-question/blob/master/reference/QueueMacros.xls) <img src='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/www/hot.jpg?raw=true' width='20'>
  03. [Queueing up in R, (continued)](https://www.r-bloggers.com/queueing-up-in-r-continued)
  04. [Waiting in line, waiting on R](https://www.r-bloggers.com/waiting-in-line-waiting-on-r)
  05. [Simulating a Queue in R](https://www.r-bloggers.com/simulating-a-queue-in-r/)
  06. [What is the queue data structure in R?](https://www.researchgate.net/post/What_is_the_queue_data_structure_in_R#59d5b01b404854fdc9168902)
  07. [Implementing a Queue as a Reference Class](https://www.r-bloggers.com/implementing-a-queue-as-a-reference-class/)
  08. [queue implementation?](http://r.789695.n4.nabble.com/queue-implementation-td2529272.html)
  09. [Queueing Theory Calculator](http://www.supositorio.com/rcalc) <img src='https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/www/hot.jpg?raw=true' width='20'>
  10. [ **The Pith of Performance** *by Neil Gunther (2010)*](http://perfdynamics.blogspot.my/2010/05/simulating-queue-in-r.html?m=1)
  11. [Computationally Efficient Simulation of Queues - The R Package queuecomputer](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/reference/Computationally%20Efficient%20Simulation%20of%20Queues%20-%20The%20R%20Package%20queuecomputer.pdf)
  12. [Waiting-Line Models](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/reference/Waiting-Line%20Models.pdf)
  13. [Queues with Breakdowns and Customer Discouragement](https://raw.githubusercontent.com/englianhu/binary.com-interview-question/master/reference/Queues%20with%20Breakdowns%20and%20Customer%20Discouragement.pdf)

### Question III

  01. [Data APIs/feeds available as packages in R](http://stats.stackexchange.com/questions/12670/data-apis-feeds-available-as-packages-in-r)
  02. [Application of Kelly Criterion model in Sportsbook Investment](https://github.com/scibrokes/kelly-criterion)


