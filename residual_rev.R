###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
# http://systematicinvestor.wordpress.com/?s=residual
###############################################################################
# purpose: draw lines of 3 factor model dynamic factor exposures 
#===============================================================================
rm(list=ls())
#setInternet2(TRUE)
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod') 
require("PerformanceAnalytics")
library(stargazer)
library(lubridate)
library(reshape2)
library(psych)
library(fBasics)
library(Hmisc)
#library(xtsExtra)  #if you get error, please install xtsExtra from r-forge as shown in the top line
library(xts)
#install.packages("xtsExtra", repos="http://R-Forge.R-project.org", type="source")
require(RColorBrewer)
library(magrittr)
library(scales)
library(Rglpk)
library(fPortfolio)
#install.packages("xtsExtra", repos="http://R-Forge.R-project.org")
#install.packages("xtsExtra", repos="http://R-Forge.R-project.org", type="source")
#library(xtsExtra)
library(sandwich)
library(lmtest)
library(broom)
library(dynlm)
library(huxtable)
library(xtable)
library(ggplot2)
#library(timetk)
load.packages('scales')
load.packages('RColorBrewer')
#jbTest(rnorm(100, mean = 3, sd = 5))
#*************************************************************************
# Try to use HAC function by Ledoit to test the equality of Sharpe ratios
# Jobson-Korkie test modified by Ledoit "Robust Performance Hypothesis Testing with
# the Sharpe Ratio""
#**************************************************************************
#load("D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/Sharpe/Sharpe.RData")
#********************************************************************
# read monthly closing price for listed stock in TWSE
# TEJ choose listed stocks without TRD and F-stocks
# TEJ下載之資料要先另存為UTF-8才能讀入
#===========================================================================
#setwd("~/residual reversal")
m.price = read.table("~/residual_strategy/data/TWSE_1990_2017_m_close1.TXT")
m.price<-m.price[,-2]
colnames(m.price)<-c("id", "date", "close")
# use dcast to reorder dataframe by date;
mprice.reorder = dcast(m.price,date~id)
dim(mprice.reorder)
#nr<-dim(mprice.reorder)[1]
#mprice.reorder = mprice.reorder[-nr,]
tail(mprice.reorder,1)
date<-seq(as.Date("1990-02-01"), length=336, by="1 month") - 1
date
#rownames(mprice.reorder)<-date
mprice.reorder.xts<-xts(mprice.reorder[,-1], order.by = date)
head(mprice.reorder.xts)
#storage.mode(mprice.reorder.xts)<-"numeric"
#head(mprice.reorder.xts)
tail(mprice.reorder.xts,1)
dim(mprice.reorder.xts)
#=====================================================
# import mkt capitalizaton
# TEJ choose listed stocks without TDR and F-stocks
#=====================================================
m.cap = read.table("~/residual_strategy/data/TWSE_1990_2017_m_cap.txt", fileEncoding = 'UTF-8')
m.cap<-m.cap[,-2]
colnames(m.cap)<-c("id", "date", "cap")
# use dcast to reorder dataframe by date;
mcap.reorder = dcast(m.cap,date~id)
dim(mcap.reorder)
#nr<-dim(mprice.reorder)[1]
#mprice.reorder = mprice.reorder[-nr,]
tail(mcap.reorder,1)
str(mcap.reorder)
# delete last row because it is empty
#mcap.reorder = mcap.reorder[-dim(mcap.reorder)[1],]
mcap.reorder = mcap.reorder[,-1]
mcap.reorder[]<-lapply(mcap.reorder, function(x) as.numeric(as.character(x)))
str(mcap.reorder)
#rownames(mprice.reorder)<-date
date<-seq(as.Date("1990-02-01"), length=336, by="1 month") - 1
date
mcap.reorder.xts<-xts(mcap.reorder, order.by = date)
head(mcap.reorder.xts)
#storage.mode(mprice.reorder.xts)<-"numeric"
#head(mprice.reorder.xts)
tail(mcap.reorder.xts,1)
dim(mcap.reorder.xts)
head(mcap.reorder.xts)
str(mcap.reorder.xts)
#====================================================
# import Fama French 4 factors monthly return series;
# rmf, size, bm, mom
# code in TEJ: Y9999:所有上市公司
#====================================================
# ff6f.tw = read.table("D:/data/FF6F_1990_2017_m.txt", stringsAsFactors = FALSE)
ff4f.tw = read.table("~/residual_strategy/data/FF4F_1990_2017_m.txt", stringsAsFactors = FALSE, fileEncoding = 'UTF-8')
colnames(ff4f.tw)<-c("id", "name","yearmonth", "rmf", "size", "bm", "mom", "rf")
ff4f.tw <-ff4f.tw[-1,]
ff4f.tw = ff4f.tw[,c(-1,-2)]
head(ff4f.tw)
tail(ff4f.tw)
dim(ff4f.tw)
ff4f.tw[]<-lapply(ff4f.tw, function(x) as.numeric(as.character(x)))
#ff4f.tw%>%lapply(function(x) as.numeric(as.character(x)))
str(ff4f.tw)
#convert to xts
ff4f.tw = ff4f.tw[,-1]
ff4f.tw.xts<-xts(ff4f.tw, order.by = date)
head(ff4f.tw.xts)
#===============================================================================
# import adjusted monthly closing price and log returns from 198912 to 201212
#===============================================================================
#ret.tw = read.csv("D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/調整股價ln月報酬/adj_m_ln_ret_reorder1.csv", header=TRUE)
#price.tw=read.csv("D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/調整股價ln月報酬/adj_m_price_reorder2.csv", header=TRUE)
#price.tw = price.tw[-1,]
#head(price.tw[1:10,1:5])
# price.tw1 = read.csv("D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/1990_2011monprices.csv", header=TRUE)
# Fama-French 3 factor returns
# ff3.tw = read.csv("D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/ff3factor.csv", header=TRUE)
#===========================================================================================
# import stock index
twse = read.table("~/residual_strategy/data/TWSE_index_1990_2017_m_close.txt", fileEncoding = 'UTF-8')
twse = twse[-1, c(-1, -2, -3)]
colnames(twse)<-c("index", "ret", "logret")
twse[]<-lapply(twse, function(x) as.numeric(as.character(x)))
twse.xts<-xts(twse, order.by=date)
head(twse.xts)
#================================
#Last Observation Carried Forward
#================================
#x <- xts(1:10, Sys.Date()+1:10)
#x[c(1,2,5,9,10)] <- NA
#na.locf(x)
#na.locf(x, fromLast=TRUE)
#na.locf(x, na.rm=TRUE, fromLast=TRUE)
#================================
price.tw.xts<-mprice.reorder.xts
tickers.tw<-colnames(price.tw.xts)
tickers.tw
tickers.n<-length(tickers.tw)
tickers.n
# sample data range: starting date = 199001; end date=20171231
price.tw.sample <- price.tw.xts["199001/201712"]
ff.tw.sample <- ff4f.tw.xts["199001/201712"]
twse.sample<- twse.xts["199001/201712"]
mcap.sample<- mcap.reorder.xts["199001/201712"]
twse.close<-twse.sample[,1]
head(twse.close)
head(price.tw.sample)
#head(ret.tw.xts)
#head(ff.tw.sample)
#################################
#equal weight for stocks in TWSE 
################################
#資料處理
#將沒有資料的股票刪除(即全部是NA資料者)=>fprices:filtered prices;
# #dim(data.tw$prices)
# data.tw.fprices<-data.tw$prices[, sapply(data.tw$prices, function(x) 
#   sum(is.na(x)))!=nrow(data.tw$prices)]
# #將價格資料多於36筆的股票留下
# data.tw.ffprices<-data.tw.fprices[, sapply(data.tw.fprices, function(x) 
#   sum(!is.na(x)))>36]                                  
# 
# head(data.tw.ffprices[1:10,1:5])
# dim(data.tw.ffprices) 
#由原本的839檔縮減為749檔股票

#***********************************************************
#models.tw<-list()
#data.tw <- new.env()
#將沒有資料的股票刪除(即全部是NA資料者)=>f == filtered prices;
price.tw.f<-price.tw.sample[, sapply(price.tw.sample, function(x) 
  sum(is.na(x)))!=nrow(price.tw.sample)]
#將價格資料多於36筆的股票留下
price.tw.sample<-price.tw.sample[, sapply(price.tw.f, function(x) 
  sum(!is.na(x)))>36]
dim(price.tw.sample)
# align capitalization data with price 
mcap.tw<-mcap.reorder.xts[, match(colnames(price.tw.sample), colnames(mcap.reorder.xts))]
dim(mcap.tw)
tail(mcap.tw)
mcap.tw[1:37, 1:15]
price.tw.sample[1:37, 1:15]
#由原本的931檔縮減為870檔股票
tickers.tw<-colnames(price.tw.sample)
tickers.tw
tickers.n<-length(tickers.tw)
tickers.n
tickers.cap<-colnames(mcap.tw)
tickers.cap
tickers.tw == tickers.cap
#=======================================================
# Import ROE at the end of each year
# In Blitz paper Table 5: variable defined as Y/B (profitability)
#=========================================================
# ROE = read.table("~/residual reversal/data/TWSE_1990_2017_ROE_1.txt")
# head(ROE)
# ROE<-ROE[,-2]
# colnames(ROE)<-c("id", "date", "roe")
# # use dcast to reorder dataframe by date;
# ROE.reorder = dcast(ROE, date~id)
# dim(ROE.reorder)
# tail(ROE.reorder,1)
# head(ROE.reorder,1)
# str(ROE.reorder)
# ROE.reorder[1:10, 1:5]
# #---------------------------------------------
# # take out year end data series:
# # 199012, 199112, 199212....
# #================================================
#  yearnum = seq(1990, 2017)
#  year_m = as.numeric(paste(yearnum, "12", sep=""))
#  ind1<-vector()
#  
#  #i=1
#  for (i in 1:length(year_m)) {
#        n<-grep(year_m[i], ROE.reorder[,1])
#        ind1 = c(ind1, n)
#  }
#  
# ROE.reorder1<-ROE.reorder[ind1,]
# ROE.reorder1[,1:5]
# dim(ROE.reorder1)
# ROE.reorder1
# #expand from year to year month series to align with stock returns
# # repeat 12 times for each year
# ROE.reorder12<-as.data.frame(lapply(ROE.reorder, rep, each = 3))
# dim(ROE.reorder12)
# ROE.reorder12[1:13,1:3]
# # convert to xts
# ROE.reorder12[]<-lapply(ROE.reorder12, function(x) as.numeric(as.character(x)))
# ROE.reorder12<-ROE.reorder12[,-1]
# ROE.reorder12.xts<-xts(ROE.reorder12, order.by = date)
# head(ROE.reorder12.xts[1:10, 1:5])
# dim(ROE.reorder12.xts)
# ROE.ticker<-unlist(lapply(colnames(ROE.reorder12.xts), substring, 2))
# ROE.ticker
# colnames(ROE.reorder12.xts)<-ROE.ticker

#*********************************************************************
# create a equally-weight portfolio based on all listed stocks
# Benchmark portfolio by market
#=====================================================================
models.tw<-list()
data.tw <- new.env()
data.tw$prices<-price.tw.sample
data.tw$weight<-price.tw.sample
data.tw$execution.price<-price.tw.sample
data.tw$execution.price[]<-NA
head(price.tw.sample,5)
#weight setting = equal weight;
#head(ntop(data.tw.sample,tickers.n))
data.tw$weight = ntop(price.tw.sample, tickers.n)
#head(data.tw$weight,5)
#sum(model.tw$weight[1,])
data.tw$weight[1:36,] = NA
#ret.tw<-data.tw$prices / mlag(data.tw$prices) - 1
#write.csv(data.tw$weight, file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/equal_weights.csv")
#write.csv(ret.tw, file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/ret_tw.csv")
#head(models.tw$weight[,1],40)
models.tw$equal.weight = bt.run(data.tw)
#sum(models.tw$weight[100,],na.rm=TRUE)
names(models.tw$equal.weight)
# weight
# get the structure of a list
str(models.tw$equal.weight$weight)
models.tw$equal.weight$weight$X1101
class(models.tw$equal.weight[1])
# ret = return of portfolio each period
models.tw$equal.weight$ret
# equity = accumulative return of the portfolio;
models.tw$equal.weight$equity
models.tw$equal.weight$worst
models.tw$equal.weight$best
models.tw$equal.weight$cagr
models.tw$equal.weight$dates.index
#===========================================
# create cap-weighted portfolios based on all listed stocks
# repeat the above by changing weight to capital weighting
#--------------------------------------------
capsum<-rowSums(mcap.tw, na.rm=T)
mcap.weight.tw<-mcap.tw * NA
for (i in 1:nrow(mcap.tw)){
  mcap.weight.tw[i,] = mcap.tw[i,]/capsum[i]
}
mcap.weight.tw[1:40, 1:10]
#rowSums(mcap.weight.tw, na.rm = T)
data.tw$weight = mcap.weight.tw
#head(data.tw$weight,5)
#sum(model.tw$weight[1,])
data.tw$weight[1:36,] = NA
#ret.tw<-data.tw$prices / mlag(data.tw$prices) - 1
#write.csv(data.tw$weight, file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/equal_weights.csv")
#write.csv(ret.tw, file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/ret_tw.csv")
#head(models.tw$weight[,1],40)
models.tw$cap.weight = bt.run(data.tw)
models.tw$cap.weight$equity
models.tw$cap.weight$worst
models.tw$cap.weight$best
models.tw$cap.weight$cagr
################################
#加權指數為benchmark;
################################
data.twse<-new.env()
data.twse$prices<-twse.close
data.twse$weight <- twse.close
data.twse$execution.price<-twse.close
data.twse$execution.price[,1]<-NA
data.twse$weight[,1] = 1
data.twse$weight[1:36,] = NA
#head(model.twse$prices)
#head(model.twse$weight)
#nrow(model.twse$prices)
models.tw$twse = bt.run(data.twse)
# ret = return of portfolio each period 
models.tw$twse[3]
# equity = accumulative return of the portfolio;
models.tw$twse$equity
# cagr = geometric mean of annual rate of return
models.tw$twse$cagr
####################################################################################
#Next let’s group stocks into Quantiles based on 1-Month returns and create back-test 
#for each Quantile. I will rely on the code in the Volatility Quantiles post to create 
#Quantiles.
#*****************************************************************
# Create Reversal Quantiles
#****************************************************************** 
# Step 1: 依股票過去一個月的報酬率，將股票分為十組，報酬率最低者列入第一組；
#最高者列入第十組；
# Step 2: 每組組內股票依等權重方式計算投資權重；
#########################################################################
n.quantiles = 10
start.t = 1 + 36
#equal weighting: weights.tw
quantiles.tw = weights.tw = coredata(price.tw.sample) * NA
# cap weighting: weights.cap
weights.cap = coredata(price.tw.sample) * NA
head(quantiles.tw)
one.month.tw = coredata(price.tw.sample / mlag(price.tw.sample))
head(one.month.tw[37,],20)

# t=37
for( t in start.t:nrow(weights.tw) ) {
  factor.tw = as.vector(one.month.tw[t,])
  ranking.tw = ceiling(n.quantiles * rank(factor.tw, na.last = 'keep','first') / count(factor.tw))
  cap.t = as.vector(mcap.tw[t,])
  quantiles.tw[t,] = ranking.tw
  weights.tw[t,] = 1/tapply(rep(1,tickers.n), ranking.tw, sum)[ranking.tw]  		
  weights.cap[t,] = cap.t/tapply(cap.t, ranking.tw, sum)[ranking.tw]
}

quantiles.tw = ifna(quantiles.tw,0)
#head(quantiles.tw[,1],140)
head(weights.tw)
quantiles.tw.ret = quantiles.tw
#weights.tw[37:40,]
#將分組組別寫出檔案
#write.csv(quantiles.tw, file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/quantiles_stocks_10Q.csv")
#write.csv(weights.tw, file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/weights_stocks_10Q.csv")

#**************************************************************************************
# Create backtest for each Quintile
# models.tw: conventional return reversal portfolios based on equal weights
# models.cap: conventional return reversal portfolios based on capitalization weights
#************************************************************************************** 
temp.tw = weights.tw * NA
temp.cap = weights.cap * NA
models.cap<-list()
#i=1
for( i in 1:n.quantiles) {
  temp.tw[] = 0
  temp.cap[] = 0
  temp.tw[quantiles.tw == i] = weights.tw[quantiles.tw == i]
  temp.cap[quantiles.tw == i] = weights.cap[quantiles.tw == i]
  data.tw$weight[] = NA
  data.tw$weight = temp.tw
  #將各組權重寫出另存為檔案
  #name1<-paste("D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/weights",i,sep='')
  #name2<-paste(name1,'.csv',sep='')
  #write.csv(temp.tw, file=name2)
  models.tw[[ paste('M1_Q',i,sep='') ]] = bt.run(data.tw, silent = T)
  #reset weights to be capial weighting and rerun the backtesting 
  data.tw$weight = temp.cap
  models.cap[[ paste('M1_Q',i,sep='') ]] = bt.run(data.tw, silent = T)
}

models.tw
names(models.tw)
#[1] "equal.weight" "cap.weight"   "twse"         "M1_Q1"        "M1_Q2"       
#[6] "M1_Q3"        "M1_Q4"        "M1_Q5"        "M1_Q6"        "M1_Q7"       
#[11] "M1_Q8"        "M1_Q9"        "M1_Q10"  
# "equal.weight": portfolio based on all sampled stocks with equal weighting
# "cap.weight": portfolio based on all sampled stocks with cap weighting
# "twse": TWSE index portfolio
names(models.tw[[1]]) #第一個模型:equal.weight內的成份;
#[1] "weight"      "type"        "ret"         "best"        "worst"       "equity"     
#[7] "cagr"        "dates.index"

#第1個模型:equal.weight內的第三個成份:ret
head(models.tw[[1]]$ret, 40)
head(models.tw[[1]]$equity, 40)
#第8個模型:the fifth quantile (M1_Q5) 內的第三個成份:ret
#models.tw[[7]][3]
models.tw$M1_Q5$ret

#依names(models.tw)之結果，依序將return合併
all.ret<-merge.xts(models.tw[[1]][3]$ret,models.tw[[2]]$ret,
                   models.tw[[3]]$ret,models.tw[[4]]$ret,
                   models.tw[[5]]$ret,models.tw[[6]]$ret,
                   models.tw[[7]]$ret,models.tw[[8]]$ret,
                   models.tw[[9]]$ret,models.tw[[10]]$ret,
                   models.tw[[11]]$ret,models.tw[[12]]$ret,
                   models.tw[[13]]$ret)
#names(all.ret)<-names(models.tw)
#head(all.ret)
#依names(models.tw)之結果，依序將equity合併
all.equity<-merge.xts(models.tw[[1]][6]$equity,models.tw[[2]][6]$equity,
                      models.tw[[3]][6]$equity,models.tw[[4]][6]$equity,
                      models.tw[[5]][6]$equity,models.tw[[6]][6]$equity,
                      models.tw[[7]][6]$equity,models.tw[[8]][6]$equity,
                      models.tw[[9]][6]$equity,models.tw[[10]][6]$equity,
                      models.tw[[11]][6]$equity,models.tw[[12]][6]$equity,
                      models.tw[[13]][6]$equity)
#----------------------------------------------------------------------------
all.cagr<- c(models.tw[[1]][7]$cagr,models.tw[[2]][7]$cagr,
             models.tw[[3]][7]$cagr,models.tw[[4]][7]$cagr,
             models.tw[[5]][7]$cagr,models.tw[[6]][7]$cagr,
             models.tw[[7]][7]$cagr,models.tw[[8]][7]$cagr,
             models.tw[[9]][7]$cagr,models.tw[[10]][7]$cagr,
             models.tw[[11]][7]$cagr,models.tw[[12]][7]$cagr,
             models.tw[[13]][7]$cagr)
#-----------------------------------------------------------------------------
names(all.equity)<-names(models.tw)
names(all.ret)<-names(models.tw)
names(all.cagr)<-names(models.tw)
head(all.equity,40)
tail(all.equity,1)
#------------------------------------------------------------------------------
# cap-weighted portfolios
#-----------------------------------------------------------------------------
#依names(models.tw)之結果，依序將return合併
all.ret.cap<-merge.xts(models.cap[[1]][3]$ret,models.cap[[2]]$ret,
                       models.cap[[3]]$ret,models.cap[[4]]$ret,
                       models.cap[[5]]$ret,models.cap[[6]]$ret,
                       models.cap[[7]]$ret,models.cap[[8]]$ret,
                       models.cap[[9]]$ret,models.cap[[10]]$ret)
#names(all.ret)<-names(models.tw)
#head(all.ret)
#依names(models.tw)之結果，依序將equity合併
all.equity.cap<-merge.xts(models.cap[[1]][6]$equity,models.cap[[2]][6]$equity,
                          models.cap[[3]][6]$equity,models.cap[[4]][6]$equity,
                          models.cap[[5]][6]$equity,models.cap[[6]][6]$equity,
                          models.cap[[7]][6]$equity,models.cap[[8]][6]$equity,
                          models.cap[[9]][6]$equity,models.cap[[10]][6]$equity)
#------------------------------------------------------------------------------
all.cagr.cap<- c(models.cap[[1]][7]$cagr,models.cap[[2]][7]$cagr,
                 models.cap[[3]][7]$cagr,models.cap[[4]][7]$cagr,
                 models.cap[[5]][7]$cagr,models.cap[[6]][7]$cagr,
                 models.cap[[7]][7]$cagr,models.cap[[8]][7]$cagr,
                 models.cap[[9]][7]$cagr,models.cap[[10]][7]$cagr)
all.cagr.cap
names(all.equity.cap)<-names(models.cap)
names(all.ret.cap)<-names(models.cap)
head(all.equity.cap, 40)
tail(all.equity.cap, 1)
#now convert xts into dataframe
fig.cap<-all.equity.cap["1993/2017"]
label.dat = fortify(tail(fig.cap,1), melt=TRUE)
fig.cap<-fortify(fig.cap, melt = TRUE)
head(fig.cap,5)
str(fig.cap)

#
title = "Cumulative returns of value-weighted decile portfolios ranked by previous one-month returns"
subtitle = "Q10 is the largest return portfolio in previous month"
# scale_shape_manual() reference: 
# https://stackoverflow.com/questions/16813278/cycling-through-point-shapes-when-more-than-6-factor-levels
#
p = ggplot(fig.cap, aes(x = Index, y = Value)) +
    geom_line(aes(linetype = Series)) +
    scale_shape_manual(values=c(seq(0,8), 15)) +
    geom_point(aes(shape = Series)) +
    scale_x_date(limits = as.Date(c(min(fig.cap$Index), max(fig.cap$Index))),
                   date_labels = "%Y", date_breaks = "3 years") +
    xlab("year") + ylab("cumulative returns")
p
#
p = p + geom_text(data = label.dat, aes(x = Index, y= Value, label = Series), 
                  hjust = 0.5, size=3, color="black")+
  ggtitle(label = title, subtitle = subtitle) +
  theme(plot.title=element_text(face="bold", size=12))+
  theme(legend.justification=c(0,0), legend.position=c(0,0.5))+
  theme(legend.text = element_text(colour="black", size = 9, face = "bold")) +
  geom_hline(yintercept=c(1,10),colour="#990000", linetype="dashed")
p
#path_p = paste("~/residual_strategy/output/", "10Q_equity_cap.pdf", sep="")
#pdf(file=path_p)
#p
#dev.off()
# Use function ExportPlot()! You have to run this function first!
path_p = paste("~/residual_strategy/output/", "10Q_equity_cap", sep="")
ExportPlot(p, path_p)
#-----------------------------------------------------------------------
Q1_Q10.ret.cap<-merge.xts(models.cap[[1]][3]$ret,models.cap[[10]]$ret)
Q1_Q10.ret.cap<-Q1_Q10.ret.cap['1993/2017']
colnames(Q1_Q10.ret.cap)<-c("Q1", "Q10")

#index(Q1_Q10.ret.cap)<-as.yearmon(index(Q1_Q10.ret.cap))
charts.PerformanceSummary(Q1_Q10.ret.cap, geometric=TRUE)
#
# drawdown<-PerformanceAnalytics:::Drawdowns(Q1_Q10.ret.cap)
# tmp.df <- as.data.frame(coredata(drawdown))
# tmp.df$Date <- as.POSIXct(index(drawdown))
# tmp.df.long <- melt(tmp.df, id.var="Date")
# colnames(drawdown)[1]
# tmp.df.long$asset <- rep(series.name, nrow(tmp.df.long))
# head(tmp.df.long)
# ggplot(tmp.df.long, aes(x=Date, y=value)) + 
#   geom_line(aes(linetype=variable))
# 
# #----------------------------------------------------------------------
# # Create cumulative return function
# # https://plot.ly/ggplot2/scale_x_date/
# # create function to clean returns if having NAs in data
# clean.rtn.xts <- function(univ.rtn.xts.obj,na.replace=0){
#   univ.rtn.xts.obj[is.na(univ.rtn.xts.obj)]<- na.replace
#   univ.rtn.xts.obj
# }
# # calculate accumulative returns
# cum.rtn <- function(clean.xts.obj, g = TRUE)
# {
#   x <- clean.xts.obj
#   if(g == TRUE){y <- cumprod(x+1)-1} else {y <- cumsum(x)}
#   y
# }
# 
# # Create function to calculate drawdowns
# dd.xts <- function(clean.xts.obj, g = TRUE)
# {
#   x <- clean.xts.obj
#   if(g == TRUE){y <- PerformanceAnalytics:::Drawdowns(x)} else {y <- PerformanceAnalytics:::Drawdowns(x,geometric = FALSE)}
#   y
# }
# 
# # create a function to create a dataframe to be usable in ggplot to replicate charts.PerformanceSummary
# cps.df <- function(xts.obj, geometric)
# {
#   x <- clean.rtn.xts(xts.obj)
#   series.name <- colnames(xts.obj)[1]
#   tmp <- cum.rtn(x, geometric)
#   tmp$rtn <- x
#   tmp$dd <- dd.xts(x, geometric)
#   colnames(tmp) <- c("Index","Return","Drawdown") # names with space
#   tmp.df <- as.data.frame(coredata(tmp))
#   tmp.df$Date <- as.POSIXct(index(tmp))
#   tmp.df.long <- melt(tmp.df,id.var="Date")
#   tmp.df.long$asset <- rep(series.name, nrow(tmp.df.long))
#   tmp.df.long
# }
# 
# # create a function to create a dataframe to be usable in 
# # ggplot to replicate charts.PerformanceSummary
# cps.df <- function(xts.obj, g)
# {
#   x <- clean.rtn.xts(xts.obj)
#   series.name <- colnames(xts.obj)[1]
#   tmp <- cum.rtn(x, geometric)
#   tmp$rtn <- x
#   tmp$dd <- dd.xts(x, geometric)
#   colnames(tmp) <- c("Index","Return","Drawdown") # names with space
#   tmp.df <- as.data.frame(coredata(tmp))
#   tmp.df$Date <- as.POSIXct(index(tmp))
#   tmp.df.long <- melt(tmp.df, id.var="Date")
#   tmp.df.long$asset <- rep(series.name, nrow(tmp.df.long))
#   tmp.df.long
# }
# #---------------------------------
# # using the cps.df function
# df <- cps.df(Q1_Q10.ret.cap, g)
# # adding in a title string if need be


#============================
# output graph in pdf
#============================
ExportPlot <- function(gplot, filename, width=11.69, height=8.27) {
  # Export plot in PDF and EPS.
  # Notice that A4: width=11.69, height=8.27
  ggsave(paste(filename, '.pdf', sep=""), gplot, width = width, height = height)
  postscript(file = paste(filename, '.eps', sep=""), width = width, height = height)
  print(gplot)
  dev.off()
  png(file = paste(filename, '_.png', sep=""), width = width * 100, height = height * 100)
  print(gplot)
  dev.off()
}


#write.csv(as.data.frame(all.ret), file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/all.ret_10Q.csv")
#write.csv(as.data.frame(all.equity), file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/all.equity_10Q.csv")
#*****************************************************************
# Create Report: return reversal strategy
#******************************************************************   
plotbt.custom.report.part1(models.tw)
plotbt.custom.report(models.tw)
strategy.performance.snapshoot(models.tw, T)
plotbt.strategy.sidebyside(models.tw, return.table=T, make.plot = T)
plotbt(models.tw, plotX = T, plottype = 'line')
mtext('Cumulative Performance', side = 2, line = 1)
plotbt(models.tw)
#==============================================================
# using ggplot to plot cumulative returns
# k = 'equal.weight'
dim(all.equity["1993/2017"])
head(all.equity["1993/2017"],1)
data.pg = fortify(all.equity["1993/2017"], melt = TRUE)
dim(data.pg)
head(data.pg)
label.dat = fortify(tail(all.equity,1), melt=TRUE)
title = paste("Cumulative returns of TWSE and equal-weighted, cap-weighted and Q1-Q10 return decile portfolios")
#title = paste(title, 'based on quintiles of residual returns')
gp= ggplot(data.pg, aes(x=Index, y=Value, group = Series)) +
  geom_line(aes(linetype=Series, color = Series)) +
  scale_shape_manual(values=seq(0, 13)) + 
  geom_point(aes(shape=Series)) +
  scale_x_date(limits = as.Date(c(min(data.pg$Index), max(data.pg$Index))),
               date_labels = "%Y", date_breaks = "3 years") +
  xlab("Index") + ylab("cumulative returns")+
  #scale_x_datetime(breaks = date_breaks("1 year"),labels = date_format("%Y"))+
  geom_text(data = label.dat, aes(x=Index,y=Value, label = Series), hjust = 0.5)+
  ggtitle(title) +
  theme(plot.title=element_text(face="bold", size=10))+
  theme(legend.justification=c(0,0), legend.position=c(0,0.5))+
  theme(legend.text = element_text(colour="blue", size = 9, face = "bold")) +
  geom_hline(yintercept=c(1,10),colour="#990000", linetype="dashed")
#
gp
#  
path_gp = paste("~/residual_strategy/output/", "10Q_equity", sep="")

ExportPlot(gp, path_gp) # run ExportPlot() function first!
# export statistics table
#models.m = rev(models.tw[[m]]) #reverse the order to be Q10, Q9, ..., Q1.
#  plotbt(models, plotX = T, log = 'y', LeftMargin = 3)            
#  mtext('Cumulative Performance', side = 2, line = 1)
pbt = plotbt.strategy.sidebyside(models.tw, return.table = TRUE)
path_risk = paste("~/residual_strategy/output/", "risk_equity.csv", sep="")
write.csv(pbt, path_risk)
#============================
# output graph in pdf
#============================
ExportPlot <- function(gplot, filename, width=11.69, height=8.27) {
  # Export plot in PDF and EPS.
  # Notice that A4: width=11.69, height=8.27
  ggsave(paste(filename, '.pdf', sep=""), gplot, width = width, height = height)
  postscript(file = paste(filename, '.eps', sep=""), width = width, height = height)
  print(gplot)
  dev.off()
  png(file = paste(filename, '_.png', sep=""), width = width * 100, height = height * 100)
  print(gplot)
  dev.off()
}
#===========================================

####################################################################
# head(quantiles.tw==5)
# head(weights.tw)
# head(temp.tw)
# test<-weights.tw[quantiles.tw == 5]
# test
#*****************************************************************
# Create spread portfolio (Q1-Q10 ) based on return quantile
#****************************************************************** 
temp.tw[] = 0
temp.tw[quantiles.tw == 1] = weights.tw[quantiles.tw == 1]
temp.tw[quantiles.tw == n.quantiles] = -weights.tw[quantiles.tw == n.quantiles]

data.tw$weight[] = NA
data.tw$weight = temp.tw
models.tw$spread = bt.run(data.tw, silent = T)
models.tw$spread
names(models.tw$spread)
models.tw$spread[[6]]
ret.equity.spread<-merge.xts(models.tw$spread[[3]],models.tw$spread[[6]])
head(ret.equity.spread)
#write.csv(ret.equity.spread, file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/ret_equity_spread.csv")
all.ret<-merge.xts(all.ret, models.tw$spread[[3]])
names(all.ret)<-names(models.tw)
head(all.ret)
all.equity<-merge.xts(all.equity, models.tw$spread[[6]])
names(all.equity)<-names(models.tw)
head(all.equity)
tail(all.equity,1)
#----------------------------------------------------------------------------------------
# create report of conventional return reversal strategy
# using equal-weighting, cap-weighting, spread
#----------------------------------------------------------------------------------------
ret.stats = matrix(data = NA, nrow=14, ncol =10)
#所有股票等權重之投資組合月報酬率
#equal.ret = models.tw[[1]][3]$ret
# x = rnorm(1000)
# ksnormTest(x)
# out1 = jarqueberaTest(x)
# out2 = jbTest(x)
# str(out1)
# str(out2)

i=1
for (i in 1:14){
  one.month.ret  = all.ret[,i]["199302/201712"]
  annual.factor <-length(one.month.ret)/12
  cagr = (tail(cumprod(as.vector(one.month.ret)+1),1))^(1/annual.factor)-1
  cagr = cagr * 100
  #
  avgD = as.numeric(AverageDrawdown(one.month.ret))*100
  maxD = as.numeric(maxDrawdown(one.month.ret))*100
  #equal.ret = equal.ret["199302/201212"]
  #one.month.ret = merge(one.month.ret, equal.ret)
  RF.tw = ff4f.tw.xts$"rf"["199302/201712"]/100
  average.ret = mean(one.month.ret)
  stdev.ret = sd(one.month.ret)
  skew = skew(one.month.ret)
  kurtosis = kurtosi(one.month.ret)
  jbtest = jarqueberaTest(coredata(one.month.ret))
  #jbtest1 = jbTest(coredata(one.month.ret))
  #average.ret = mean(quantiles.tw$one.month[[i]][3]$ret)
  #stdev.ret = sd(quantiles.tw$one.month[[i]][3]$ret)
  #RF.tw<-data.fa.tw$factors$"RF"
  SR = SharpeRatio.annualized(one.month.ret, Rf = RF.tw/12, scale=12, geometric=FALSE)
  #SR.eq = SharpeRatio.annualized(equal.ret, Rf = RF.tw/12, scale=12, geometric=FALSE)
  ret.stats[i,] = rbind(c(as.numeric(average.ret), as.numeric(stdev.ret), skew, as.numeric(kurtosis), 
                          jbtest@test$statistic, as.numeric(jbtest@test$p.value), 
                          as.numeric(SR), as.numeric(cagr), avgD, maxD))
}
ret.stats
ret.stats.df = as.data.frame(ret.stats)
rownames(ret.stats.df)<-names(all.ret)
colnames(ret.stats.df) = c("mean", "stdev", "skew", "kurtosis",
                           "jb-test", "jb-pvalue","Sharpe ratio", "CAGR", "AvgD", "MaxD")
#-----------------------------------------------------------------------------------------------------------------------
# Table 1: Summary statistics for conventional return reversal and residual reversal portfolios based on equal weighting
# Panel A: Conventional reversal strategy (equal weighting)
#-----------------------------------------------------------------------------------------------------------------------
options(digits = 4)
ret.stats.df
# write output to latex file
my.xtable<-xtable(x = ret.stats.df, 
                  label = 'tab:returnRevStats',
                  caption = "Descriptive statistics for return reversal based equal weighting",
                  digits = 4)

print(my.xtable, include.rownames = TRUE,
      file = '~/residual_strategy/output/table1_panelA_return_stats.tex',
      type = 'latex')

#-----------------------------------------------------------------------------------------
#write.csv(as.data.frame(all.ret), file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/all.ret_all.csv")
#write.csv(as.data.frame(all.equity), file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/all.equity_all.csv")
#*****************************************************************
# Create Report
#******************************************************************   
plotbt.custom.report.part1(models.tw[spl('twse,equal.weight,spread,M1_Q1,M1_Q2,M1_Q3')])
plotbt(models.tw[spl('twse,equal.weight,spread,M1_Q1,M1_Q2,M1_Q3')])
mtext('Cumulative Performance', side = 2, line = 1)
#----------------------------------------------------------------------
#*****************************************************************
# Create spread portfolio (Q1-Q10 ) based on return quantile 
# using cap weighted 
#****************************************************************** 
temp.cap[] = 0
temp.cap[quantiles.tw == 1] = weights.cap[quantiles.tw == 1]
temp.cap[quantiles.tw == n.quantiles] = -weights.cap[quantiles.tw == n.quantiles]

data.tw$weight[] = NA
data.tw$weight = temp.cap
models.cap$spread = bt.run(data.tw, silent = T)
models.cap$spread
names(models.cap$spread)
models.cap$spread[[6]]
ret.equity.spread.cap<-merge.xts(models.cap$spread[[3]],models.cap$spread[[6]])
head(ret.equity.spread.cap)
#write.csv(ret.equity.spread, file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/ret_equity_spread.csv")
all.ret.cap<-merge.xts(all.ret.cap, models.cap$spread[[3]])
names(all.ret.cap)<-names(models.cap)
head(all.ret.cap,40)
all.equity.cap<-merge.xts(all.equity.cap, models.tw$spread[[6]])
names(all.equity.cap)<-names(models.cap)
head(all.equity.cap)
tail(all.equity.cap,1)
#----------------------------------------------------------------------------------------
# create report of cap weighted porfolios
#----------------------------------------------------------------------------------------
#******************************************************
# Summary statistics:
# return reversal strategy
#*******************************************************
ret.stats.cap = matrix(data = NA, nrow=11, ncol =10)
#所有股票等權重之投資組合月報酬率
#cap.ret = models.tw[[1]][3]$ret
# x = rnorm(1000)
# ksnormTest(x)
# out1 = jarqueberaTest(x)
# out2 = jbTest(x)
# str(out1)
# str(out2)

i=1
for (i in 1:11){
  one.month.ret  = all.ret.cap[,i]["199302/201712"]
  annual.factor <-length(one.month.ret)/12
  cagr = (tail(cumprod(as.vector(one.month.ret)+1),1))^(1/annual.factor)-1
  cagr = cagr * 100
  #
  avgD = as.numeric(AverageDrawdown(one.month.ret))*100
  maxD = as.numeric(maxDrawdown(one.month.ret))*100
  #equal.ret = equal.ret["199302/201212"]
  #one.month.ret = merge(one.month.ret, equal.ret)
  RF.tw = ff4f.tw.xts$"rf"["199302/201712"]/100
  average.ret = mean(one.month.ret)
  stdev.ret = sd(one.month.ret)
  skew = skew(one.month.ret)
  kurtosis = kurtosi(one.month.ret)
  jbtest = jarqueberaTest(coredata(one.month.ret))
  #jbtest1 = jbTest(coredata(one.month.ret))
  #average.ret = mean(quantiles.tw$one.month[[i]][3]$ret)
  #stdev.ret = sd(quantiles.tw$one.month[[i]][3]$ret)
  #RF.tw<-data.fa.tw$factors$"RF"
  SR = SharpeRatio.annualized(one.month.ret, Rf = RF.tw/12, scale=12, geometric=FALSE)
  #SR.eq = SharpeRatio.annualized(equal.ret, Rf = RF.tw/12, scale=12, geometric=FALSE)
  ret.stats.cap[i,] = rbind(c(as.numeric(average.ret), as.numeric(stdev.ret), skew, as.numeric(kurtosis), 
                              as.numeric(jbtest@test$statistic), as.numeric(jbtest@test$p.value), 
                              as.numeric(SR), as.numeric(cagr), avgD, maxD))
}
ret.stats.cap
ret.stats.cap.df = as.data.frame(ret.stats.cap, row.names = colnames(all.ret.cap))
colnames(ret.stats.cap.df) = c("mean", "stdev", "skew", "kurtosis",
                               "jb-test", "jb-pvalue","Sharpe ratio", "CAGR", "avgD", "maxD")

#------------------------------------------------------------------------------------------------------------------------
# Table 2: Summary statistics for conventional return reversal and residual reversal portfolios based on value weighting
# Panel A: Conventional reversal strategy (value weighting)
#------------------------------------------------------------------------------------------------------------------------
options(digits = 4)
ret.stats.cap.df
# write output to latex file
my.xtable<-xtable(x = ret.stats.cap.df, 
                  label = 'tab:returnRevStats',
                  caption = "Descriptive statistics for return reversal based on captial weighting",
                  digits = 4)

print(my.xtable, include.rownames = TRUE,
      file = '~/residual_strategy/output/table2_panelA_ret_stats_cap.tex',
      type = 'latex')

###################################################################
#以上為依報酬率分十組所得之績效結果
###################################################################
#================================================================================================
# Followings are residual reversal strategy portfolio
#================================================================================================

#############################################################################
# function to compute additional custom stats for factor.rolling.regression
#############################################################################
factor.rolling.regression.custom.stats <- function(x,y,fit) {
  n = len(y)
  e = y - x %*% fit$coefficients
  se = sd(e)
  return(c(e[n], e[n]/se))
}

##########################
# add factors and align
##########################
data.fa.tw <- new.env()
# 資料內容說明：包含weight, price, factors, and individual stock 
# data.fa.tw[["X1101"]]:1101股票價格
# data.fa.tw$factors: 三因子模型資料
# data.fa.tw$prices: 所有檔數股票價格
#i="X1101"
#head(data.tw$prices[,i])
#將沒有資料的股票刪除(即全部是NA資料者)=>fprices:filtered prices;
dim(data.tw$prices)
data.tw.fprices<-data.tw$prices[, sapply(data.tw$prices, function(x) 
  sum(is.na(x)))!=nrow(data.tw$prices)]
#將價格資料多於36筆的股票留下
data.tw.ffprices<-data.tw.fprices[, sapply(data.tw.fprices, function(x) 
  sum(!is.na(x)))>36]                                  

head(data.tw.ffprices[1:10,1:5])
dim(data.tw.ffprices) 
#由原本的920檔縮減為870檔股票
tickers.tw.f<-colnames(data.tw.ffprices)
tickers.tw.f
tickers.n.f<-length(tickers.tw.f)
tickers.n.f
#
i = "1101"
for(i in tickers.tw.f) data.fa.tw[[i]] = data.tw.ffprices[,i]
#data.fa.tw$factors = ff3f.tw.sample / 100
ff3f.tw.xts<-ff4f.tw.xts[, -which(names(ff4f.tw.xts) == 'mom')]
data.fa.tw$factors = ff3f.tw.xts / 100
head(data.fa.tw$factors)
colnames(data.fa.tw$factors)<-c("mkp", "SML", "HML", "RF")
#convert to annaul risk free rate monthly rate
data.fa.tw$factors$RF<-data.fa.tw$factors$RF/12
head(data.fa.tw$factors)
#bt.prep(data.fa.tw, align='remove.na')
head(data.fa.tw[[i]])
data.fa.tw$prices<-data.tw.ffprices
data.fa.tw$weight<-data.tw.ffprices
head(data.fa.tw$prices)
ncol(data.fa.tw$prices)

#test<-data.fa.tw$symbolnames[-grep('factor', data.fa.tw$symbolnames)]
#test[1]
#*****************************************************************
# Compute Factor Attribution for each ticker
#******************************************************************   
temp = NA * data.tw.ffprices
head(temp)
factors.tw	= list()
factors.tw$last.e = temp
factors.tw$last.e_s = temp
#FF factor regression estimated coefficients
coeff.tw = list()
coeff.tw$b0 = temp #alpha
coeff.tw$b1 = temp #market
coeff.tw$b2 = temp #size
coeff.tw$b3 = temp #book
coeff.tw$r2 = temp #R-squared

i="1101"
j=1 # j: factor coefficients
for(i in tickers.tw.f) {
  cat(i, '\n')
  
  # Factor Loadings Regression
  obj.tw = factor.rolling.regression(data.fa.tw, i, 36, silent=T,
                                     factor.rolling.regression.custom.stats)
  
  for(j in 1:len(factors.tw))		
    factors.tw[[j]][,i] = obj.tw$fl$custom[,j]
  
  for (k in 1:len(coeff.tw))
    coeff.tw[[k]][,i] = obj.tw$fl$estimate[,k]
  # obj.tw$fl$estimate[1:40, 1:5]
}
#-------------------------------------------------------
# check residual data 
# head(data.fa.tw$factors)
# head(data.fa.tw$'1101')
# yi = ROC(data.fa.tw$'1101', type = "discrete")
# head(yi)
# y = yi[2:37]
# x = data.fa.tw$factors[2:37,1:3]
# test<-lm(y~x)
# summary(test)
# residuals(test)
#-----------------------------------------------------------
names(coeff.tw)
# [1] "b0" "b1" "b2" "b3" "r2"
names(coeff.tw[[1]])
head(coeff.tw[[1]])
tail(coeff.tw[[1]])
tail(coeff.tw[[2]])
#write.csv(coeff.tw[[1]], file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/coeff_b0_10Q.csv")
# data = data.fa.tw
names(factors.tw)
#[1] "last.e"   "last.e_s" "one.month"
names(factors.tw[[2]])
last(factors.tw[[2]])
#將最後一期各個股票的殘差及標準化後的殘差值寫出為檔案
#write.csv(factors.tw[[1]], file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/last_e_10Q.csv")
#write.csv(factors.tw[[2]], file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/last_es_10Q.csv")
#write.csv(factors.tw[[3]], file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/last_1m.csv")
# add base strategy
factors.tw$one.month = coredata(data.tw.ffprices / mlag(data.tw.ffprices))
#*******************************************************************************************
#Next let’s group stocks into Quantiles based on 1-Month Reversal factors and create reports.
#*******************************************************************************************

#*****************************************************************
# Create Quantiles
#****************************************************************** 
quantiles.tw = list()
index.tw = match(index(data.tw.ffprices), index(data.tw$prices) )
head(index.tw)
head(coredata(data.tw.ffprices))
data.fa.tw$execution.price<-data.tw.ffprices
data.fa.tw$execution.price[]<-NA
#name_10q = list() #將每組股票名單分別列出;

#**************************
#此為分組函數, 將資料由小到大分為10組, output為組別 
#**************************
# position.score = coredata(factors.tw$last.e)
# period.ends = index.tw
# data = data.fa.tw
# t= 37
#=================================
# Include weights.cap which is computed in previous coding 
# temp.cap = weights.cap * NA
#--------------------------------------------------------------
# modify bt function to account for cap-weighting porfolios
#------------------------------------------------------------------
# Note: This function returns optional results: models and models_cap
# models: are based on equal weighting 
# models_cap: are based on capital weighting
# you have to modify mcap_tw in function return() to return which results you need
#===============================================================
position.score <-factors.tw[['last.e_s']]
mcap_tw<-mcap.tw
data <- data.fa.tw
period.ends <- index.tw
start.t = 37
#

bt.make.quintiles<-function(
  position.score,
  mcap_tw, 
  data,
  period.ends,
  n.quantiles = 10,
  start.t = 2,
  prefix = ''
)
{
  n = ncol(position.score)
  position.score = coredata(position.score)
  mcap_tw = coredata(mcap_tw)
  quantiles = weights = weights.cap.t= position.score * NA
  # t = 37
  for( t in start.t:nrow(weights) ) {
    factor = as.vector(position.score[t,])
    ranking = ceiling(n.quantiles * rank(factor, na.last = 'keep','first') / count(factor))
    quantiles[t,] = ranking
    weights[t,] = 1/tapply(rep(1,n), ranking, sum)[ranking]
    # testing: start from row 37 to check which one belongs to quantile 9th
    # and sum(weights[37,which(quantiles[37,]==9)]) should be equal to 1
    cap.t = as.vector(mcap_tw[t,])
    weights.cap.t[t,] = cap.t/tapply(cap.t, ranking, sum)[ranking]
  }
  quantiles = ifna(quantiles,0)
  temp = weights * NA
  temp_cap = weights.cap.t * NA
  models = list()
  models_cap = list()
  stock_names = list()
  #i=1
  for( i in 1:n.quantiles) {
    temp[] = 0
    temp_cap[] = 0
    #weights[quantiles == 1]:利用"quantiles == 1"將第一組的股票的index找出，進而篩選出來對應股票權重的資料，
    #並放入temp[]中；
    temp[quantiles == i] = weights[quantiles == i]
    temp_cap[quantiles == i] = weights.cap.t[quantiles == i]
    data$weight[] = NA
    data$weight[period.ends,] = temp
    models[[ paste(prefix,'Q',i,sep='') ]] = bt.run(data, silent = T)
    data$weight[] = NA
    data$weight[period.ends,] = temp_cap
    models_cap[[ paste(prefix,'Q',i,sep='') ]] = bt.run(data, silent = T)
  }
  temp[] = 0
  temp_cap[] = 0
  temp[quantiles == 1] = weights[quantiles == 1]
  temp_cap[quantiles == 1] = weights.cap.t[quantiles == 1]
  temp[quantiles == n.quantiles] = -weights[quantiles == n.quantiles]
  temp_cap[quantiles == n.quantiles] = -weights.cap.t[quantiles == n.quantiles]
  data$weight[] = NA
  data$weight[period.ends,] = temp
  models$spread = bt.run(data, silent = T)
  data$weight[] = NA
  data$weight[period.ends,] = temp_cap
  models_cap$spread = bt.run(data, silent = T)
  models$quantiles = quantiles
  models_cap$quantiles = quantiles
  # write.csv(quintiles, file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/last_e_10Q.csv")
  models2<-list(models = models, models_cap = models_cap)
  return(models2) 
}


name = "last.e"
for(name in names(factors.tw)) {
  cat(name, '\n')
  quantiles.tw[[name]] = bt.make.quintiles(factors.tw[[name]], mcap.tw, data.fa.tw, index.tw, start.t =  1+36, prefix=paste(name,'_',sep=''))
  #quantiles.tw[[name]] = bt.make.quintiles(factors.tw[[name]], mcap.tw, data.fa.tw, index.tw, start.t =  1+36, prefix=paste(name,'_',sep=''))
  #filename1=paste(name,"_10Q_ranking.csv",sep="")
  #filename2=paste("D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/", filename1,sep="")
  #write.csv(quantiles.tw[[name]]$quantiles, file=filename2)
}  

names(quantiles.tw$last.e_s)
# [1] "models"     "models_cap"
#--------------------------------------------------------
# report equal weighted portfolios based on residual return
#--------------------------------------------------------
names(quantiles.tw$last.e_s$models)

all.ret.es.10Q<-merge.xts(quantiles.tw$last.e_s$models[[1]][3]$ret,
                          quantiles.tw$last.e_s$models[[2]][3]$ret,
                          quantiles.tw$last.e_s$models[[3]][3]$ret,
                          quantiles.tw$last.e_s$models[[4]][3]$ret,
                          quantiles.tw$last.e_s$models[[5]][3]$ret,
                          quantiles.tw$last.e_s$models[[6]][3]$ret,
                          quantiles.tw$last.e_s$models[[7]][3]$ret,
                          quantiles.tw$last.e_s$models[[8]][3]$ret,
                          quantiles.tw$last.e_s$models[[9]][3]$ret,
                          quantiles.tw$last.e_s$models[[10]][3]$ret,
                          quantiles.tw$last.e_s$models[[11]][3]$ret)
dim(all.ret.es.10Q)
#
all.equity.es.10Q<-merge.xts(quantiles.tw$last.e_s$models[[1]][6]$equity,
                             quantiles.tw$last.e_s$models[[2]][6]$equity,
                             quantiles.tw$last.e_s$models[[3]][6]$equity,
                             quantiles.tw$last.e_s$models[[4]][6]$equity,
                             quantiles.tw$last.e_s$models[[5]][6]$equity,
                             quantiles.tw$last.e_s$models[[6]][6]$equity,
                             quantiles.tw$last.e_s$models[[7]][6]$equity,
                             quantiles.tw$last.e_s$models[[8]][6]$equity,
                             quantiles.tw$last.e_s$models[[9]][6]$equity,
                             quantiles.tw$last.e_s$models[[10]][6]$equity,
                             quantiles.tw$last.e_s$models[[11]][6]$equity)
dim(all.equity.es.10Q)

names(all.equity.es.10Q)<-c(names(quantiles.tw$last.e_s)[1:11])
names(all.ret.es.10Q)<-c(names(quantiles.tw$last.e_s)[1:11])
#---------------------------------------------------------------
res.stats = matrix(data = NA, nrow=11, ncol =10)
#所有股票等權重之投資組合月報酬率
#equal.ret = models.tw[[1]][3]$ret
# x = rnorm(1000)
# ksnormTest(x)
# out1 = jarqueberaTest(x)
# out2 = jbTest(x)
# str(out1)
# str(out2)

i=1
for (i in 1:11){
  one.month.ret  = all.ret.es.10Q[,i]["199302/201712"]
  annual.factor <-length(one.month.ret)/12
  cagr = (tail(cumprod(as.vector(one.month.ret)+1),1))^(1/annual.factor)-1
  cagr = cagr * 100
  avgD = as.numeric(AverageDrawdown(one.month.ret))*100
  maxD = as.numeric(maxDrawdown(one.month.ret))*100
  #equal.ret = equal.ret["199302/201212"]
  #one.month.ret = merge(one.month.ret, equal.ret)
  RF.tw = ff4f.tw.xts$"rf"["199302/201712"]/100
  average.ret = mean(one.month.ret)
  stdev.ret = sd(one.month.ret)
  skew = skew(one.month.ret)
  kurtosis = kurtosis(one.month.ret)
  jbtest = jarqueberaTest(coredata(one.month.ret))
  #jbtest1 = jbTest(coredata(one.month.ret))
  #average.ret = mean(quantiles.tw$one.month[[i]][3]$ret)
  #stdev.ret = sd(quantiles.tw$one.month[[i]][3]$ret)
  #RF.tw<-data.fa.tw$factors$"RF"
  SR = SharpeRatio.annualized(one.month.ret, Rf = RF.tw/12, scale=12, geometric=FALSE)
  #SR.eq = SharpeRatio.annualized(equal.ret, Rf = RF.tw/12, scale=12, geometric=FALSE)
  res.stats[i,] = rbind(c(as.numeric(average.ret), as.numeric(stdev.ret), skew, as.numeric(kurtosis), 
                          jbtest@test$statistic, as.numeric(jbtest@test$p.value), 
                          as.numeric(SR), as.numeric(cagr), avgD, maxD))
}
res.stats
res.stats.df = as.data.frame(res.stats, row.names = names(all.ret)[-1:-3])
colnames(res.stats.df) = c("mean", "stdev", "skew", "kurtosis",
                           "jb-test", "jb-pvalue","Sharpe ratio", "CAGR", "avgD", "maxD")


#-----------------------------------------------------------------------------------------------------------------------
# Table 1: Summary statistics for conventional return reversal and residual reversal portfolios based on equal weighting
# Panel B: Residual reversal strategy (equal weighting)
#-----------------------------------------------------------------------------------------------------------------------
options(digits = 4)
res.stats.df
# write output to latex file
my.xtable<-xtable(x = res.stats.df, 
                  label = 'tab:returnRevStats',
                  caption = "Descriptive statistics for residual reversal based on equal weighting",
                  #caption = "Descriptive statistics for residual reversal based on cap weighting",
                  digits = 4)

print(my.xtable, include.rownames = TRUE,
      file = '~/residual_strategy/output/table1_res_stats_equalw.tex',
      #file = '~/residual reversal/output/tables/table1_res_stats_capw.tex',
      type = 'latex')

#--------------------------------------------------------
# report cap weighted portfolios based on residuals
#--------------------------------------------------------
all.ret.es.cap.10Q<-merge.xts(quantiles.tw$last.e_s$models_cap[[1]][3]$ret,
                              quantiles.tw$last.e_s$models_cap[[2]][3]$ret,
                              quantiles.tw$last.e_s$models_cap[[3]][3]$ret,
                              quantiles.tw$last.e_s$models_cap[[4]][3]$ret,
                              quantiles.tw$last.e_s$models_cap[[5]][3]$ret,
                              quantiles.tw$last.e_s$models_cap[[6]][3]$ret,
                              quantiles.tw$last.e_s$models_cap[[7]][3]$ret,
                              quantiles.tw$last.e_s$models_cap[[8]][3]$ret,
                              quantiles.tw$last.e_s$models_cap[[9]][3]$ret,
                              quantiles.tw$last.e_s$models_cap[[10]][3]$ret,
                              quantiles.tw$last.e_s$models_cap[[11]][3]$ret)
dim(all.ret.es.cap.10Q)

all.equity.es.cap.10Q<-merge.xts(quantiles.tw$last.e_s$models_cap[[1]][6]$equity,
                                 quantiles.tw$last.e_s$models_cap[[2]][6]$equity,
                                 quantiles.tw$last.e_s$models_cap[[3]][6]$equity,
                                 quantiles.tw$last.e_s$models_cap[[4]][6]$equity,
                                 quantiles.tw$last.e_s$models_cap[[5]][6]$equity,
                                 quantiles.tw$last.e_s$models_cap[[6]][6]$equity,
                                 quantiles.tw$last.e_s$models_cap[[7]][6]$equity,
                                 quantiles.tw$last.e_s$models_cap[[8]][6]$equity,
                                 quantiles.tw$last.e_s$models_cap[[9]][6]$equity,
                                 quantiles.tw$last.e_s$models_cap[[10]][6]$equity,
                                 quantiles.tw$last.e_s$models_cap[[11]][6]$equity)
dim(all.equity.es.cap.10Q)

names(all.equity.es.cap.10Q)<-c(names(quantiles.tw$last.e_s)[1:11])
names(all.ret.es.cap.10Q)<-c(names(quantiles.tw$last.e_s)[1:11])
tail(all.equity.es.cap.10Q,1)
all.ret.es.cap.10Q[1:40, 1:5]
#-------------------------------------------------------------------
res.cap.stats = matrix(data = NA, nrow=11, ncol =10)
#所有股票等權重之投資組合月報酬率
#equal.ret = models.tw[[1]][3]$ret
# x = rnorm(1000)
# ksnormTest(x)
# out1 = jarqueberaTest(x)
# out2 = jbTest(x)
# str(out1)
# str(out2)

i=1
for (i in 1:11){
  one.month.ret  = all.ret.es.cap.10Q[,i]["199302/201712"]
  annual.factor <-length(one.month.ret)/12
  cagr = (tail(cumprod(as.vector(one.month.ret)+1),1))^(1/annual.factor)-1
  cagr = cagr * 100
  avgD = as.numeric(AverageDrawdown(one.month.ret))*100
  maxD = as.numeric(maxDrawdown(one.month.ret))*100
  #equal.ret = equal.ret["199302/201212"]
  #one.month.ret = merge(one.month.ret, equal.ret)
  RF.tw = ff4f.tw.xts$"rf"["199302/201712"]/100
  average.ret = mean(one.month.ret)
  stdev.ret = sd(one.month.ret)
  skew = skew(one.month.ret)
  kurtosis = kurtosis(one.month.ret)
  jbtest = jarqueberaTest(coredata(one.month.ret))
  #jbtest1 = jbTest(coredata(one.month.ret))
  #average.ret = mean(quantiles.tw$one.month[[i]][3]$ret)
  #stdev.ret = sd(quantiles.tw$one.month[[i]][3]$ret)
  #RF.tw<-data.fa.tw$factors$"RF"
  SR = SharpeRatio.annualized(one.month.ret, Rf = RF.tw/12, scale=12, geometric=FALSE)
  #SR.eq = SharpeRatio.annualized(equal.ret, Rf = RF.tw/12, scale=12, geometric=FALSE)
  res.cap.stats[i,] = rbind(c(as.numeric(average.ret), as.numeric(stdev.ret), skew, as.numeric(kurtosis), 
                              jbtest@test$statistic, as.numeric(jbtest@test$p.value), 
                              as.numeric(SR), as.numeric(cagr), avgD, maxD))
}
res.cap.stats
res.cap.stats.df = as.data.frame(res.cap.stats, row.names = names(all.ret)[-1:-3])
colnames(res.cap.stats.df) = c("mean", "stdev", "skew", "kurtosis",
                               "jb-test", "jb-pvalue","Sharpe ratio", "CAGR", "avgD", "maxD")

#-----------------------------------------------------------------------------------------------------------------------
# Table 2: Summary statistics for conventional return reversal and residual reversal portfolios based on cap weighting
# Panel B: Residual reversal strategy (cap weighting)
#-----------------------------------------------------------------------------------------------------------------------
options(digits = 4)
res.cap.stats.df
# write output to latex file
my.xtable<-xtable(x = res.cap.stats.df, 
                  label = 'tab:returnRevStats',
                  caption = "Descriptive statistics for residual reversal based on cap weighting",
                  digits = 4)

print(my.xtable, include.rownames = TRUE,
      file = '~/residual_strategy/output/table2_res_stats_capw.tex',
      type = 'latex')

#--------------------------------------------------------------------------
# Fig. 1: Cumulated returns of monthly return loser (retQ1) and residual loser (resQ1)
# plot equal weighting conventional reversal Q1 vs. residual reversal Q1 
# accumulative returns
#--------------------------------------------------------------------------
ret_res_equal<-merge(all.equity$M1_Q1, quantiles.tw$last.e_s$models[[1]][6]$equity)
tail(ret_res_equal)
colnames(ret_res_equal)<-c("ret Q1", "res Q1")
ret_res_equal = ret_res_equal["1993/2017"]
label.dat1 = fortify(tail(ret_res_equal,1), melt=TRUE)
ret_res_equal.long<-fortify(ret_res_equal, melt = TRUE)
#
gg20<- ggplot(ret_res_equal.long, aes(x=Index, y=Value, group = Series)) +
       geom_line(aes(linetype=Series)) +
       scale_x_date(limits = as.Date(c(min(ret_res_equal.long$Index), max(ret_res_equal.long$Index))),
               date_labels = "%Y", date_breaks = "3 years") + 
       geom_hline(yintercept=c(1),colour="#990000", linetype="dashed")+
       ggtitle("Accumulated returns of convential reversal (ret Q1) vs. residual reversal (res Q1)
          portfolios based on equal weighting")+
       xlab("year") + ylab("value")
#
gg20
#
path_gg20 = paste("~/residual_strategy/output/", "ret_res_equal", sep="")
ExportPlot(gg20, path_gg20)
#------------------------------------------------------------------------------
# Plot multiple months by year for conventional return reversal vs. residual reversal
# monthly returns
#-----------------------------------------------------------------------------
ret_res_equal.ret<-merge(models.tw[[1]][3]$ret, quantiles.tw$last.e_s$models[[1]][3]$ret)
ret_res_equal.ret<-ret_res_equal.ret['1993/2017']
colnames(ret_res_equal.ret)<-c('ret_Q1', 'res_Q1')
ret_res_equal.ret.long<-fortify(ret_res_equal.ret, melt=TRUE)
head(ret_res_equal.ret.long)
ret_res_equal.ret.long$Year = year(ret_res_equal.long$Index)
ret_res_equal.ret.long$Month = factor(month(ret_res_equal.long$Index))
head(ret_res_equal.ret.long)
#
gg21<-ggplot(ret_res_equal.ret.long, aes(x=Month, y=Value, group = Series)) +
  geom_line(aes(linetype=Series)) +
  facet_wrap(~ Year, nc = 3)
#
gg21

# Density distribution of ret_Q1 and res_Q1
ggplot(ret_res_equal.ret.long, aes(Value, color= Series)) +
    geom_density()

#geom_text(data = label.dat, aes(x = Index, y= Value, label = Series), 
#          hjust = 0.1, size=3, color= 'black', parse = TRUE)+
#geom_hline(yintercept=c(1, 0.5, 0),colour="#990000", linetype="dashed")+
#ggtitle(title)
#----------------------------------------------------------------------------
# calculate mean return for each year for conventional return reversal and residual 
# reversal strategy
#----------------------------------------------------------------------------
tmp.xts = ret_res_equal.ret
#tmp.xts$date = index(ret_res_equal.ret)
tmp.xts$year = year(index(tmp.xts))
tmp.xts$month = month(index(tmp.xts))
tmp.xts
#
i=1993
mean.out<-NULL
sharperatio<-NULL

for (i in seq(1993, 2017)){
  tmpi<-tmp.xts[tmp.xts$year==i, 1:2]
  meani = apply(tmpi, 2, mean)
  sharp1 = SharpeRatio(tmpi[,1], scale=12)
  sharp2 = SharpeRatio(tmpi[,2], scale=12)
  mean.out<-rbind(mean.out, meani)
  sharperatio = rbind(sharperatio, sharp1[1], sharp2[1])
}
rownames(mean.out)<-c(seq(1993, 2017))
mean.out
sharperatio<-matrix(sharperatio, ncol=2, byrow=T)
rownames(sharperatio)<-c(seq(1993, 2017))
sharperatio
#tmpi.df<-as.data.frame(tmpi)
#SharpeRatio(tmpi.df[,"ret_Q1", drop=FALSE],Rf=0, scale=252)

#===============================================================================
# save 3 factor model estimated coefficients across 10 portfoilos ranked by ret
# in cf.ret.qi 
#===============================================================================
# We start from portfolio returns based on cap weighting
q1.ret = quantiles.tw$one.month$models_cap$one.month_Q1$ret
q10.ret = quantiles.tw$one.month$models_cap$one.month_Q10$ret
head(data.fa.tw$factors)
head(q1.ret)
q1.ret.factors = merge(q1.ret, data.fa.tw$factors[,-4])
q10.ret.factors = merge(q10.ret, data.fa.tw$factors[,-4])
head(q1.ret.factors)
q1.ret.factors = q1.ret.factors["199302/201712"]
q10.ret.factors = q10.ret.factors["199302/201712"]
head(q1.ret.factors)
#write.csv(es.q1.ret.factors, file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/es_q1_ret_factors.csv")
#es.q1 = es.q1.ret.factors[,-3:-4]
cf.ret.cap.qi<-list()
# i = 1
for (i in 1:10){
  temp.ret<-quantiles.tw$one.month$models_cap[[i]]$ret 
  temp.qi.ret.factors = merge(temp.ret, data.fa.tw$factors[,-4])
  temp.qi.ret.factors = temp.qi.ret.factors["199302/201712"]
  rolling <- function(x) coef(lm(X1101 ~ ., data = as.data.frame(x)))
  #cf.es.q1 = rollapplyr(es.q1.ret.factors, 36, rolling, by.column = FALSE)
  cf.ret.cap.qi[[i]] <- rollapplyr(temp.qi.ret.factors, 36, rolling, by.column = FALSE)
  #cf.es.q10 = rollapplyr(es.q10.ret.factors, 36, rolling, by.column = FALSE)
  #head(cf.es.qi[[1]],40)
  #tail(cf.ret.qi[[1]])
}  

head(cf.ret.cap.qi[[10]], 40)
tail(cf.ret.cap.qi[[10]], 1)
par(mfrow=c(1,1))
plot(na.trim(cf.ret.cap.qi[[2]][, 2:4]), ylim = c(-0.8, 1.5))
#
#colnames(cf.ret.cap.qi[[1]][,2:4])<-c("mkt", "SMB", "HML")
tmp<-cf.ret.cap.qi[[1]][, 2:4]["1996/2017"]
colnames(tmp)<-c("mkt", "SMB", "HML")
label.dat = fortify(tail(tmp,1), melt=TRUE)
cf.ret.cap.q1.long<-fortify(tmp, melt = TRUE)
head(cf.ret.cap.q1.long)
title = paste("Coefficients of three factor models for conventional 
              value-weighted return portfolios for Q1")
#
gg1<-ggplot(cf.ret.cap.q1.long, aes(x=Index, y=Value, group = Series, type = Series)) +
     geom_line(aes(linetype=Series)) + 
     scale_x_date(limits = as.Date(c(min(cf.ret.cap.q1.long$Index), max(cf.ret.cap.q1.long$Index))),
               date_labels = "%Y", date_breaks = "3 years") +
     geom_text(data = label.dat, aes(x = Index, y= Value, label = Series), 
            hjust = 0.1, size=3, color= 'black', parse = TRUE)+
     geom_hline(yintercept=c(1, 0.5, 0),colour="#990000", linetype="dashed")+
     ggtitle(title)+
     #  geom_point(aes(shape=Series))+
     xlab("year") + ylab("coefficients")
#
gg1
#
path_gg1 = paste("~/residual_strategy/output/", "cf_ret_cap_q1", sep="")
ExportPlot(gg1, path_gg1)
#-------------------------------------------------------------
# Table 1 panel A run rolling regression by eq(15)
# conventional return reversal model based on cap weighting
#-------------------------------------------------------------
# create interaction variables with three factors:
data.fa.tw$factors.lag<-lag(data.fa.tw$factors)
data.fa.tw$factors.ind<-apply(data.fa.tw$factors.lag, 2, function(x) ifelse((x>0), x, 0))
head(data.fa.tw$factors.ind)
# delete RF column
data.fa.tw$factors.ind<-data.fa.tw$factors.ind[,-4]
data.fa.tw$factors.ind<-as.xts(data.fa.tw$factors.ind)
# lag ahead one period: lag = -1 (by Blitz paper)
data.fa.tw$factors.ind<-lag(data.fa.tw$factors.ind, -1)
head(data.fa.tw$factors.ind)
data.fa.tw$factors.ind<-as.xts(data.fa.tw$factors.ind)["199302/201712"]
head(data.fa.tw$factors.ind)
colnames(data.fa.tw$factors.ind)<-c("mkp_up", "SML_up", "HML_up")
# 
est.parm<-matrix(data = NA, nrow=30, ncol =8)
# i=1
for (i in 1:10){
  ret.qi<-quantiles.tw$one.month$models_cap[[i]]$ret 
  ret.qi.ret.factors<-merge(ret.qi, data.fa.tw$factors[,-4])['199302/201712']
  ret.qi.ret.factors_up<-merge(ret.qi.ret.factors, data.fa.tw$factors.ind)
  #head(ret.q1.ret.factors_up)
  # run EQ(15) regression
  eq15.ret.qi<-lm(X1101 ~ mkp+SML+HML+mkp_up+SML_up+HML_up, data=as.data.frame(ret.qi.ret.factors_up))
  #summary(eq15.ret.q1)
  # create regression results in tables using huxreg()
  #huxreg(eq15.ret.q1)
  # Newey West standard errors correction for serial correlation
  nw.t.out<-coeftest(eq15.ret.qi, vcov=NeweyWest(eq15.ret.qi, verbose=T))
  #nw.t.out
  #nw.t.out[1,]
  #huxreg(nw.t.out)
  #print_latex(eq15.ret.Q1)
  #print_screen(eq15.ret.q1)
  #output summary into table
  options(digits = 4)
  options(scipen=30)
  #eq15.ret.qi<-rbind(out, adjR2 = summary(eq15.ret.qi)$adj.r.squared)
  #options("scipen"=30, "digits"=4)
  #eq15.ret.q1
  #est.parm<-matrix(data = NA, nrow=30, ncol =7)
  est.parm[3*(i-1)+1, 8]<-summary(eq15.ret.qi)$adj.r.squared
  # estimated coefficients in the first row
  est.parm[3*(i-1)+1, 1:7]<-nw.t.out[,1]
  # estimated t values in the second row
  est.parm[3*(i-1)+2, 1:7]<-nw.t.out[,3]
  # p values in the third row
  est.parm[3*(i-1)+3, 1:7]<-nw.t.out[,4]
}

est.parm.df<-as.data.frame(est.parm)
colnames(est.parm.df)<-c("alpha","mkp", "SML", "HML", "mkp_up", "SML_up", "HML_up", "adjR2")
#rownames(est.parm.df)<-rep(c("coef", "t-value", "p-value"), 10)
est.parm.df
options("scipen"=10, "digits"= 3)
#options("digits"= 3)
est.parm.df
# http://www.tablesgenerator.com/latex_tables
# we can use latex table generator to generate tables
#-----------------------------------------------------------------
# 2. repeat the above codings again to compute conventional reversal 
# strategy based on equal weighting
#------------------------------------------------------------------
est.parm.eq<-matrix(data = NA, nrow=30, ncol =8)
# i=1
eq15.ls = list()
for (i in 1:10){
  ret.qi<-quantiles.tw$one.month$models[[i]]$ret 
  ret.qi.ret.factors<-merge(ret.qi, data.fa.tw$factors[,-4])['199302/201712']
  ret.qi.ret.factors_up<-merge(ret.qi.ret.factors, data.fa.tw$factors.ind)
  #head(ret.q1.ret.factors_up)
  # run EQ(15) regression
  eq15.ret.qi<-lm(X1101 ~ mkp+SML+HML+mkp_up+SML_up+HML_up, data=as.data.frame(ret.qi.ret.factors_up))
  eq15.ls[[i]]<-eq15.ret.qi
   #summary(eq15.ret.q1)
  # create regression results in tables using huxreg()
  #huxreg(eq15.ret.q1)
  # Newey West standard errors correction for serial correlation
  nw.t.out<-coeftest(eq15.ret.qi, vcov=NeweyWest(eq15.ret.qi, verbose=T))
  #nw.t.out
  #nw.t.out[1,]
  #huxreg(nw.t.out)
  #print_latex(eq15.ret.Q1)
  #print_screen(eq15.ret.q1)
  #output summary into table
  options(digits = 4)
  options(scipen=30)
  #eq15.ret.qi<-rbind(out, adjR2 = summary(eq15.ret.qi)$adj.r.squared)
  #options("scipen"=30, "digits"=4)
  #eq15.ret.q1
  #est.parm<-matrix(data = NA, nrow=30, ncol =7)
  est.parm.eq[3*(i-1)+1, 8]<-summary(eq15.ret.qi)$adj.r.squared
  # estimated coefficients
  est.parm.eq[3*(i-1)+1, 1:7]<-nw.t.out[,1]
  # estimated t values
  est.parm.eq[3*(i-1)+2, 1:7]<-nw.t.out[,3]
  # p values
  est.parm.eq[3*(i-1)+3, 1:7]<-nw.t.out[,4]
}

stargazer(eq15.ls, flip=TRUE)
# stargazer(eq15.ls, type="text", 
#           dep.var.labels=c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10"), 
#           covariate.labels=c("alpha", "RMRF", "SMB", 
#                              "HML", "RMRF_up", "SMB_up", "HML_up", "Adj-R2"), 
#           flip = TRUE)

est.parm.eq.df<-as.data.frame(est.parm.eq)
colnames(est.parm.eq.df)<-c("alpha","mkp", "SML", "HML", "mkp_up", "SML_up", "HML_up", "adjR2")
#rownames(est.parm.df)<-rep(c("coef", "t-value", "p-value"), 10)
est.parm.eq.df
options("scipen"=10, "digits"= 3)
#options("digits"= 3)
est.parm.eq.df
# http://www.tablesgenerator.com/latex_tables
# we can use latex table generator to generate tables
#============================================================================

#============================================================================
# 3 .Repeat the above codings again to compute residual reversal 
# strategy based on equal weighting
#------------------------------------------------------------------
est.parm.es.eq<-matrix(data = NA, nrow=30, ncol =8)
# i=1
for (i in 1:10){
  ret.qi<-quantiles.tw$last.e_s$models[[i]]$ret 
  ret.qi.ret.factors<-merge(ret.qi, data.fa.tw$factors[,-4])['199302/201712']
  ret.qi.ret.factors_up<-merge(ret.qi.ret.factors, data.fa.tw$factors.ind)
  #head(ret.q1.ret.factors_up)
  # run EQ(15) regression
  eq15.ret.qi<-lm(X1101 ~ mkp+SML+HML+mkp_up+SML_up+HML_up, data=as.data.frame(ret.qi.ret.factors_up))
  #summary(eq15.ret.q1)
  # create regression results in tables using huxreg()
  #huxreg(eq15.ret.q1)
  # Newey West standard errors correction for serial correlation
  nw.t.out<-coeftest(eq15.ret.qi, vcov=NeweyWest(eq15.ret.qi, verbose=T))
  #nw.t.out
  #nw.t.out[1,]
  #huxreg(nw.t.out)
  #print_latex(eq15.ret.Q1)
  #print_screen(eq15.ret.q1)
  #output summary into table
  options(digits = 4)
  options(scipen=30)
  #eq15.ret.qi<-rbind(out, adjR2 = summary(eq15.ret.qi)$adj.r.squared)
  #options("scipen"=30, "digits"=4)
  #eq15.ret.q1
  #est.parm<-matrix(data = NA, nrow=30, ncol =7)
  est.parm.es.eq[3*(i-1)+1, 8]<-summary(eq15.ret.qi)$adj.r.squared
  # estimated coefficients
  est.parm.es.eq[3*(i-1)+1, 1:7]<-nw.t.out[,1]
  # estimated t values
  est.parm.es.eq[3*(i-1)+2, 1:7]<-nw.t.out[,3]
  # p values
  est.parm.es.eq[3*(i-1)+3, 1:7]<-nw.t.out[,4]
}

est.parm.es.eq.df<-as.data.frame(est.parm.es.eq)
colnames(est.parm.es.eq.df)<-c("alpha","mkp", "SML", "HML", "mkp_up", "SML_up", "HML_up", "adjR2")
#rownames(est.parm.df)<-rep(c("coef", "t-value", "p-value"), 10)
est.parm.es.eq.df
options("scipen"=10, "digits"= 5)
#options("digits"= 3)
est.parm.es.eq.df
# http://www.tablesgenerator.com/latex_tables
# we can use latex table generator to generate tables

#===============================================================================
# Blitz Table 4: here we don't need to differentiate captial from equal weighting as
# we just focus on stocks coefficients not portfolio coefficients
# save 3 factor model estimated coefficients across 10 portfoilos ranked by es
# in cf.es.qi
#===============================================================================
# We start from portfolio returns based on cap weighting
es.q1.ret = quantiles.tw$last.e_s$models_cap$last.e_s_Q1$ret
es.q10.ret = quantiles.tw$last.e_s$models_cap$last.e_s_Q10$ret
head(data.fa.tw$factors)
head(es.q1.ret)
#es.q1.ret.factors = merge(es.q1.ret, data.fa.tw$factors[,-4])
#es.q10.ret.factors = merge(es.q10.ret, data.fa.tw$factors[,-4])
#head(es.q1.ret.factors)
#es.q1.ret.factors = es.q1.ret.factors["199302/201712"]
#es.q10.ret.factors = es.q10.ret.factors["199302/201712"]
#head(es.q1.ret.factors)
#write.csv(es.q1.ret.factors, file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/es_q1_ret_factors.csv")
#es.q1 = es.q1.ret.factors[,-3:-4]
cf.es.qi <- list()
cf.ret.qi <- list()
# i = 1
for (i in 1:10){
  temp.es.qi.ret.factors <- quantiles.tw$last.e_s$models_cap[[i]]$ret %>% 
                          merge(., data.fa.tw$factors[, -4]) %>% 
                          .["199302/201712"]
  temp.ret.qi.ret.factors <- quantiles.tw$one.month$models_cap[[i]]$ret %>% 
                          merge(., data.fa.tw$factors[, -4]) %>% 
                          .["199302/201712"]
  rolling <- function(x) coef(lm(X1101 ~ ., data = as.data.frame(x)))
  cf.es.qi[[i]] <- rollapplyr(temp.es.qi.ret.factors, 36, rolling, by.column = FALSE)
  cf.ret.qi[[i]] <- rollapplyr(temp.ret.qi.ret.factors, 36, rolling, by.column = FALSE)
}  

head(cf.es.qi[[10]], 40)
#
plot(na.trim(cf.es.qi[[1]][, 2:4]), ylim = c(-0.8, 1.5))
# 
cf.es.pg = fortify(na.trim(cf.es.qi[[1]][,2:4]), melt = TRUE)
cf.ret.pg = fortify(na.trim(cf.ret.qi[[1]][,2:4]), melt = TRUE)
dim(cf.es.pg)
dim(cf.ret.pg)
head(cf.es.pg)
label.dat = fortify(tail(cf.es.qi[[1]][,2:4],1), melt = TRUE)
title = "Dynamic factor exposures for residual reversal portfolio (Q1)"
#title = paste(title, 'based on quintiles of residual returns')
#gp= ggplot(cf.es.pg, aes(x=Index, y=Value, group = Series)) +
cf.es.ret.Q1 <- ggplot(cf.es.pg, aes(x = Index, y = Value, group = Series)) +  
                geom_line(aes(linetype = Series)) +
                geom_line(data = cf.ret.pg, aes(linetype = Series), color = "red")+
                scale_linetype_manual(values=c("solid", "dotdash",  "longdash"))+
                #scale_color_manual(values=c( "black", "#666666", "grey","black", "#666666", "grey")+
                #geom_point(aes(shape=Series))+
                xlab("") + ylab("Factor exposures")+
                #scale_x_datetime(breaks = date_breaks("1 year"),labels = date_format("%Y"))+
                geom_text(data = label.dat, aes(x = Index, y = Value, label = Series), hjust = -0.1)+
                ggtitle(title) +
                theme(plot.title = element_text(face = "bold", size = 12))+
                theme(legend.position = c(0.8, 0.8)) +
                #theme(legend.position="right")
                #theme(legend.justification=c(0,0), legend.position=c(0,0.6))+
                theme(legend.text = element_text(colour = "blue", size = 10, face = "bold")) +
                geom_hline(yintercept = c(0,1), colour = "#990000", linetype = "solid")
#
path_gp = paste("~/residual_strategy/output/", "cf_es_ret_Q1", sep= "")
ExportPlot(cf.es.ret.Q1, path_gp) # run Expo
#-----------------------------------------------------------------
# use plotly to plot 
#-----------------------------------------------------------------
library(plotly)
cf.data<-cbind(cf.ret = cf.ret.qi[[1]][,2:4],
               cf.res = cf.es.qi[[1]][,2:4])
cf.data<-data.frame(date=index(cf.data), coredata(cf.data))
cf.data<-na.trim(cf.data)
colnames(cf.data)<-c("date", "mkp", "SMB", "HML", "mkp.1", "SMB.1", "HML.1")
#=============================================================================
p1 <- plot_ly(data=cf.data, x=~date, y=~mkp, name='mkp_ret',type = 'scatter', mode='lines') %>%
      add_trace(y=~mkp.1, name='mkp_res', mode = 'lines+markers')
p1
#
p2 <- plot_ly(data=cf.data, x=~date, y=~SMB, name='SMB_ret',type = 'scatter', mode='lines') %>%
      add_trace(y=~SMB.1, name='SMB_res', mode = 'lines+markers')
p2
#
p3 <- plot_ly(data=cf.data, x=~date, y=~HML, name='HML_ret',type = 'scatter', mode='lines') %>%
      add_trace(y=~HML.1, name='HML_res', mode = 'lines+markers')
p3
#
p123 <- subplot(p1, p2, p3)
p123
#---------------------------------------------------------------------------------------
#ggplot(cf.data, aes(x = dens, fill = lines)) + geom_density(alpha = 0.5)
cf.data.gg<-cbind(cf.ret = cf.ret.qi[[1]][,2:4],
                  cf.res = cf.es.qi[[1]][,2:4])
head(cf.data.gg)
colnames(cf.data.gg)<-c("mkp", "SMB", "HML", "mkp.1", "SMB.1", "HML.1")
cf.data.gg<-na.trim(cf.data.gg)
#------------------------------------------------------
#mkp vs. mkp.1
cf.data.pg1 = fortify(cf.data.gg[,c(1,4)], melt = TRUE)
head(cf.data.pg1)
#------------------------------------------------------
cf_dist_pg1 <- ggplot(cf.data.pg1, aes(x = Value, fill = Series)) + geom_density(alpha = 0.3)+
               scale_x_continuous(limits =c(0.6, 1.5))

#
# SMB vs. SMB.1
cf.data.pg2 = fortify(cf.data.gg[,c(2,5)], melt = TRUE)
head(cf.data.pg2)
#------------------------------------------------------
cf_dist_pg2 <- ggplot(cf.data.pg2, aes(x = Value, fill = Series)) + geom_density(alpha = 0.3) +
               scale_x_continuous(limits =c(-0.6, 1.5))
# 
# HML vs. HML.1
cf.data.pg3 = fortify(cf.data.gg[,c(3,6)], melt = TRUE)
head(cf.data.pg3)
#------------------------------------------------------
cf_dist_pg3 <- ggplot(cf.data.pg3, aes(x = Value, fill = Series)) + geom_density(alpha = 0.3) +
               scale_x_continuous(limits =c(-1.0, 1.5))

#-------------------------------------------------------------
#計算Table 1 panel A run rolling regression by eq(15)
# residual reversal model based on cap weighting
#-------------------------------------------------------------
# create interaction variables with three factors:
data.fa.tw$factors.lag<-lag(data.fa.tw$factors)
data.fa.tw$factors.ind<-apply(data.fa.tw$factors.lag, 2, function(x) ifelse((x>0), x, 0))
head(data.fa.tw$factors.ind)
# delete RF column
data.fa.tw$factors.ind<-data.fa.tw$factors.ind[,-4]
data.fa.tw$factors.ind<-as.xts(data.fa.tw$factors.ind)["199302/201712"]
head(data.fa.tw$factors.ind)
colnames(data.fa.tw$factors.ind)<-c("mkp_up", "SML_up", "HML_up")
# 
est.parm.es<-matrix(data = NA, nrow=30, ncol =8)
for (i in 1:10){
  ret.qi<-quantiles.tw$last.e_s$models_cap[[i]]$ret 
  ret.qi.ret.factors<-merge(ret.qi, data.fa.tw$factors[,-4])['199302/201712']
  ret.qi.ret.factors_up<-merge(ret.qi.ret.factors, data.fa.tw$factors.ind)
  #head(ret.q1.ret.factors_up)
  # run EQ(15) regression
  eq15.ret.qi<-lm(X1101 ~ mkp+SML+HML+mkp_up+SML_up+HML_up, data=as.data.frame(ret.qi.ret.factors_up))
  #summary(eq15.ret.q1)
  # create regression results in tables using huxreg()
  #huxreg(eq15.ret.q1)
  # Newey West standard errors correction for serial correlation
  nw.t.out<-coeftest(eq15.ret.qi, vcov=NeweyWest(eq15.ret.qi, verbose=T))
  #nw.t.out
  #nw.t.out[1,]
  #huxreg(nw.t.out)
  #print_latex(eq15.ret.Q1)
  #print_screen(eq15.ret.q1)
  #output summary into table
  options(digits = 4)
  options(scipen=30)
  #eq15.ret.qi<-rbind(out, adjR2 = summary(eq15.ret.qi)$adj.r.squared)
  #options("scipen"=30, "digits"=4)
  #eq15.ret.q1
  #est.parm<-matrix(data = NA, nrow=30, ncol =7)
  est.parm.es[3*(i-1)+1, 8]<-summary(eq15.ret.qi)$adj.r.squared
  # estimated coefficients
  est.parm.es[3*(i-1)+1, 1:7]<-nw.t.out[,1]
  # estimated t values
  est.parm.es[3*(i-1)+2, 1:7]<-nw.t.out[,3]
  # p values
  est.parm.es[3*(i-1)+3, 1:7]<-nw.t.out[,4]
}

est.parm.es.df<-as.data.frame(est.parm.es)
colnames(est.parm.es.df)<-c("alpha","mkp", "SML", "HML", "mkp_up", "SML_up", "HML_up", "adjR2")
#rownames(est.parm.df)<-rep(c("coef", "t-value", "p-value"), 10)
est.parm.es.df
options("scipen"=10, "digits"= 3)
#options("digits"= 3)
est.parm.es.df
# http://www.tablesgenerator.com/latex_tables
# we can use latex table generator to generate tables
# add parenthesis to t-values
for (i in 1:10){
  est.parm.es.df[3*(i-1)+2, ] <- paste0("(", format(unlist(est.parm.es.df[3*(i-1)+2,])),")")
}

est.parm.es.df
# est.parm.df
my.xtable<-xtable(x = est.parm.es.df, 
                  label = 'tab:ParmResRevCap',
                  caption = "Dynamic factor exposures based on cap-weighted residual reversal",
                  digits = 3)
my.xtable

print(my.xtable, include.rownames = TRUE,
      file = '~/residual reversal/output/tables/table3_factor_param_res_capw.tex',
      type = 'latex')




#write.csv(eq15.ret.q1, "~/residual reversal/output/eq15.ret.Q1.csv")
#-----------------------------------------------------------------------

#***************************************
# Replicate Table 4 in Blitz paper
#計算估計FF model係數之中位數之平均值
# Based on es-sorted portfolios
#***************************************
# 1. 針對標準化後的殘差值分組資料進行計算
quantiles.es = coredata(quantiles.tw$last.e_s$models[[12]])
dim(quantiles.es)
quantiles.es[1:40, 1:5]
# create a matrix to place average of medians of estimated coefficients: b0, b1, b2, b3, r2
coeff.meds.es = matrix(data = NA, nrow=length(coeff.tw), ncol = 10)
coeff.meds.es.10<-list()
k=1
i=1
for (k in 1:5){
  coeff = coredata(coeff.tw[[k]])
  for( i in 1:n.quantiles) {
    #temp.coeff[] = 0
    temp.coeff = quantiles.es * NA
    temp.coeff[quantiles.es == i] = coeff[quantiles.es == i]
    rmeds = apply(temp.coeff, 1, median, na.rm = TRUE)     ## get row medians
    rmeds.avg = mean(rmeds, na.rm = TRUE)
    coeff.meds.es[k,i] = rmeds.avg
  }
}
coeff.meds.es.df = as.data.frame(coeff.meds.es, row.names = c("b0","b1","b2","b3","r2"))
colnames(coeff.meds.es.df) = c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10")
coeff.meds.es.df
write.csv(coeff.meds.es, file="~/residual reversal/output/coeff_meds_es_10Q_1006.csv")

#***************************************
# Replicate Table 4 in Blitz paper
#計算估計FF model係數之中位數之平均值
# Based on ret-sorted portfolios
#***************************************
# 1. 針對標準化後的殘差值分組資料進行計算
quantiles.ret = coredata(quantiles.tw$one.month$models[[12]])
# create a matrix to place average of medians of estimated coefficients: b0, b1, b2, b3, r2
coeff.meds.ret = matrix(data = NA, nrow=length(coeff.tw), ncol = 10)
k=1
i=1
for (k in 1:length(coeff.tw)){
  coeff = coredata(coeff.tw[[k]])
  for( i in 1:n.quantiles) {
    #temp.coeff[] = 0
    temp.coeff = quantiles.ret * NA
    temp.coeff[quantiles.ret == i] = coeff[quantiles.ret == i]
    rmeds = apply(temp.coeff, 1, median, na.rm = TRUE)     ## get row medians
    rmeds.avg = mean(rmeds, na.rm = TRUE)
    coeff.meds.ret[k,i] = rmeds.avg
  }
}
coeff.meds.ret.df = as.data.frame(coeff.meds.ret, row.names = c("b0","b1","b2","b3","r2"))
colnames(coeff.meds.ret.df) = c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10")
coeff.meds.ret.df
write.csv(coeff.meds.ret, file="~/residual reversal/output/coeff_meds_ret_10Q.csv")
#write.csv(coeff.meds.df, file="~/residual reversal/output/coeff_meds_10Q.csv")

#***********************************
# Replicate Figure 1 in Blitz paper
#***********************************
#1. 先分析殘差組之投資組合
#------------------------------------


# looping thru market, size and book
i=1
cf.es.q1 = cf.es.qi[[i]]
i=10
cf.es.q10 cf.es.qi[[10]]
for (i in 1:3){
  cf.q1 = cf.es.q1[,i+1]
  cf.q10 = cf.es.q10[,i+1]
  factor.ret = lag(es.q1.ret.factors[,i+1],1)
  #head(factor.ret)
  reg.q1  = lm(cf.q1~factor.ret)
  reg.q10 = lm(cf.q10~factor.ret)
  par(cex=.8)
  beta.q1.q10 = merge(merge(factor.ret, cf.q1), cf.q10)
  #tail(beta.q1.q10.market)
  colnames(beta.q1.q10) = c(colnames(factor.ret),"loser", "winner")
  beta.q1.q10 = coredata(na.omit(beta.q1.q10))
  #head(beta.q1.q10)
  data1 = as.data.frame(beta.q1.q10)
  data2 = melt(data1, id.vars = colnames(factor.ret), variable.name = "type") 
  colnames(data2) = c(colnames(factor.ret),"type","beta")
  #cf.market.q1.q10 = merge.xts(cf.market.q1, cf.market.q10)
  #colnames(cf.market.q1.q10) =  c("q1","q10")
  #cf.market.q1.q10.df = as.data.frame(cf.market.q1.q10)
  # beta.q1 = coredata(cf.market.q1[!is.na(cf.market.q1), ])
  # beta.q10 = coredata(cf.market.q10[!is.na(cf.market.q10),])
  # market.ret.trun = coredata(market.ret["199601/201212"])
  # head(beta.q1)
  # length(beta.q1)
  # length(beta.q10)
  # beta = data.frame(x=rbind(market.ret.trun, market.ret.trun),
  #                   y=rbind(beta.q1, beta.q10), 
  #                   cat = rep(c("winner","loser"), each = length(beta.q1)))
  # colnames(beta) = c("market","beta","type")
  # ggplot(data = beta, aes(x=market, y=beta, colour=type))+ 
  #                     geom_smooth(method = "lm") +
  #                     geom_point()+ 
  # #                   scale_colour_hue(h = c(180,0))
  #                     scale_colour_grey()
  #factor = data2[,1]
  f1 = ggplot(data = data2, aes(x=data2[,1], y=beta, shape=type, linetype = type))+ 
    geom_smooth(method = "lm", se=FALSE, fullrange=T, alpha=0.2) +
    geom_point() +
    scale_x_continuous(colnames(factor.ret))+
    scale_y_continuous(paste(colnames(factor.ret),"beta" ))+
    scale_shape_manual(values=c(1,2))+
    scale_colour_manual(values = c('gray','black'))
  #       scale_colour_hue(h = c(180,0))
  #       scale_colour_grey()
  path = "~/residual reversal/output/"
  file1 = paste("es_f",i, sep="")
  file2 = paste(file1, "_Market.jpeg", sep="")
  file3 = paste(path, file2)
  ggsave(f1, file = file3, scale=2)
}

#***********************************
# Replicate Figure 1 in Blitz paper
#***********************************
# 2. 以下為依報酬率分組後之投資組合分析
# 此部份程式只利用第一組及最第十組資料來做分析
# 但Blitz paper是將資料只分為二組：低於樣本平均報酬率為一組，高於樣本平均報酬率為一組
#-------------------------------------
ret.q1.ret = quantiles.tw$one.month$one.month_Q1$ret
ret.q10.ret = quantiles.tw$one.month$one.month_Q10$ret
head(data.fa.tw$factors)
head(ret.q1.ret)
ret.q1.ret.factors = merge(ret.q1.ret, data.fa.tw$factors[,-4])
ret.q10.ret.factors = merge(ret.q10.ret, data.fa.tw$factors[,-4])
head(ret.q1.ret.factors)
ret.q1.ret.factors = ret.q1.ret.factors["199302/201712"]
ret.q10.ret.factors = ret.q10.ret.factors["199302/201712"]
head(ret.q1.ret.factors)
#---------------------------------------------------------------------
rolling <- function(x) coef(lm(X1101 ~ ., data = as.data.frame(x)))
cf.ret.q1 = rollapplyr(ret.q1.ret.factors, 36, rolling, by.column = FALSE)
cf.ret.q10 = rollapplyr(ret.q10.ret.factors, 36, rolling, by.column = FALSE)
head(cf.ret.q1,37)
tail(cf.ret.q1)
tail(cf.ret.q10)

# looping thru market, size and book
i=1
for (i in 1:3){
  cf.q1 = cf.ret.q1[,i+1]
  cf.q10 = cf.ret.q10[,i+1]
  factor.ret = lag(ret.q1.ret.factors[,i+1],1)
  #head(factor.ret)
  reg.q1  = lm(cf.q1~factor.ret)
  reg.q10 = lm(cf.q10~factor.ret)
  par(cex=.8)
  beta.q1.q10 = merge(merge(factor.ret, cf.q1), cf.q10)
  #tail(beta.q1.q10.market)
  colnames(beta.q1.q10) = c(colnames(factor.ret),"loser", "winner")
  beta.q1.q10 = coredata(na.omit(beta.q1.q10))
  #head(beta.q1.q10)
  data1 = as.data.frame(beta.q1.q10)
  # head(data1)
  data2 = melt(data1, id.vars = colnames(factor.ret), variable.name = "type") 
  # head(data2)
  colnames(data2) = c(colnames(factor.ret),"type","beta")
  f1 = ggplot(data = data2, aes(x=data2[,1], y=beta, shape=type, linetype = type))+ 
    geom_point() + 
    geom_smooth(method = "lm", se=FALSE, fullrange=T, alpha=0.2) +
    scale_x_continuous(colnames(factor.ret))+
    scale_y_continuous(paste(colnames(factor.ret),"beta" ))+
    scale_shape_manual(values=c(1,2))+
    scale_colour_manual(values = c('gray','black'))
  #       scale_colour_hue(h = c(180,0))
  #       scale_colour_grey()
  path = "~/residual reversal/output/"
  file1 = paste("ret_f",i, sep="")
  file2 = paste(file1, "_Market.jpeg", sep="")
  file3 = paste(path, file2)
  ggsave(f1, file = file3, scale=2)
}
#===============================================================
# 以下為仿Blitz的做法，將股票分為4組，並嘗試重做Figure 1的部份
#****************************************************************
#此為分組函數, 將資料由小到大分為4組, output為組別 
#***************************************************************
# position.score = coredata(factors.tw$last.e)
# period.ends = index.tw
# data = data.fa.tw
# t= 37
bt.make.quintiles4<-function(
  position.score,
  data,
  period.ends,
  n.quantiles = 4,
  start.t = 2,
  prefix = ''
)
{
  n = ncol(position.score)
  position.score = coredata(position.score)
  quantiles = weights = position.score * NA
  
  for( t in start.t:nrow(weights) ) {
    factor = as.vector(position.score[t,])
    ranking = ceiling(n.quantiles * rank(factor, na.last = 'keep','first') / count(factor))
    quantiles[t,] = ranking
    weights[t,] = 1/tapply(rep(1,n), ranking, sum)[ranking]
  }
  quantiles = ifna(quantiles,0)
  temp = weights * NA
  models = list()
  stock_names = list()
  #i=1
  for( i in 1:n.quantiles) {
    temp[] = 0
    #weights[quantiles == 1]:利用"quantiles == 1"將第一組的股票的index找出，進而篩選出來對應股票權重的資料，
    #並放入temp[]中；
    temp[quantiles == i] = weights[quantiles == i]
    data$weight[] = NA
    data$weight[period.ends,] = temp
    models[[ paste(prefix,'Q',i,sep='') ]] = bt.run(data, silent = T)
  }
  temp[] = 0
  temp[quantiles == 1] = weights[quantiles == 1]
  temp[quantiles == n.quantiles] = -weights[quantiles == n.quantiles]
  data$weight[] = NA
  data$weight[period.ends,] = temp
  models$spread = bt.run(data, silent = T)
  models$quantiles = quantiles
  # write.csv(quintiles, file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/last_e_10Q.csv")
  return(models) 
}

quantiles4.tw = list()
name = "last.e"
for(name in names(factors.tw)) {
  cat(name, '\n')
  quantiles4.tw[[name]] = bt.make.quintiles4(factors.tw[[name]], data.fa.tw, index.tw, start.t =  1+36, prefix=paste(name,'_',sep=''))
  #filename1=paste(name,"_10Q_ranking.csv",sep="")
  #filename2=paste("D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/", filename1,sep="")
  #write.csv(quantiles.tw[[name]]$quantiles, file=filename2)
}  
names(quantiles4.tw)
names(quantiles4.tw$one.month)

#**************************************************************************
# Replicate Figure 1 in Blitz paper and Table 1 panel A based on eq(15)
#**************************************************************************
# 2. 以下為將報酬率分4組 (winner and loser groups) 投資組合分析
# ref: Blitz paper是將資料分為低於樣本平均報酬率為一組 (Q1)，高於樣本平均報酬率為一組(Q2)
#但本程式只分為前25%與後25%的股票
#-------------------------------------
ret.Q1.ret = quantiles4.tw$one.month$one.month_Q1$ret
ret.Q4.ret = quantiles4.tw$one.month$one.month_Q4$ret
head(data.fa.tw$factors)
head(ret.Q1.ret)
ret.Q1.ret.factors = merge(ret.Q1.ret, data.fa.tw$factors[,-4])
ret.Q4.ret.factors = merge(ret.Q4.ret, data.fa.tw$factors[,-4])
head(ret.Q1.ret.factors)
ret.Q1.ret.factors = ret.Q1.ret.factors["199302/201712"]
ret.Q4.ret.factors = ret.Q4.ret.factors["199302/201712"]
head(ret.Q1.ret.factors)
# compute annualized mean returns, annualized standard deviation and Sharpe ratio
# ret.Q1.mean<- (1+mean(ret.Q1.ret.factors[,1]))^12-1
ret.Q1.mean<- mean(ret.Q1.ret.factors[,1])
ret.Q1.mean
ret.Q4.mean<- mean(ret.Q4.ret.factors[,1])
ret.Q4.mean
# to compute Newey West t statistics of the hypothesis for the mean value equal to 0 
out<-lm(as.vector(ret.Q1.ret.factors[,1])~1)
out.1<-lm(as.vector(ret.Q4.ret.factors[,1])~1)
summary(out)
summary(out.1)
coeftest(out, vcov=NeweyWest(out, verbose=T))
coeftest(out.1, vcov=NeweyWest(out.1, verbose=T))
#ret.Q1.std<- sd(ret.Q1.ret.factors[,1])*sqrt(12)
ret.Q1.std<- sd(ret.Q1.ret.factors[,1])
ret.Q1.std
SR<-SharpeRatio.annualized(ret.Q1.ret.factors[,1], 
                           Rf = data.fa.tw$factors[,4]/12, 
                           scale=12, geometric=FALSE)
SR
#---------------------------------
ret.Q4.std<- sd(ret.Q4.ret.factors[,1])
ret.Q4.std
SR.1<-SharpeRatio.annualized(ret.Q4.ret.factors[,1], 
                             Rf = data.fa.tw$factors[,4]/12, 
                             scale=12, geometric=FALSE)
SR.1

#--------------------------------
rolling <- function(x) coef(lm(X1101 ~ ., data = as.data.frame(x)))
cf.ret.Q1 = rollapplyr(ret.Q1.ret.factors, 36, rolling, by.column = FALSE)
cf.ret.Q4 = rollapplyr(ret.Q4.ret.factors, 36, rolling, by.column = FALSE)
head(cf.ret.Q1,37)
tail(cf.ret.Q1)
tail(cf.ret.Q4)
#-------------------------------------------------------------
#計算Table 1 panel A eq(15)
#-------------------------------------------------------------
# create interaction variables with three factors:
#data.fa.tw$factors.lag<-lag(data.fa.tw$factors)
#data.fa.tw$factors.ind<-apply(data.fa.tw$factors.lag, 2, function(x) ifelse((x>0), x, 0))
#head(data.fa.tw$factors.ind)
# delete RF column
#data.fa.tw$factors.ind<-data.fa.tw$factors.ind[,-4]
#data.fa.tw$factors.ind<-as.xts(data.fa.tw$factors.ind)["199302/201712"]
#head(data.fa.tw$factors.ind)
#colnames(data.fa.tw$factors.ind)<-c("mkp_up", "SML_up", "HML_up")
ret.Q1.ret.factors_up<-merge(ret.Q1.ret.factors, data.fa.tw$factors.ind)
head(ret.Q1.ret.factors_up)
#
ret.Q4.ret.factors_up<-merge(ret.Q4.ret.factors, data.fa.tw$factors.ind)
head(ret.Q4.ret.factors_up)
# run EQ(15) regression
eq15.ret.Q1<-lm(X1101 ~ mkp+SML+HML+mkp_up+SML_up+HML_up, data=as.data.frame(ret.Q1.ret.factors_up))
summary(eq15.ret.Q1)
huxreg(eq15.ret.Q1)
#
eq15.ret.Q4<-lm(X1101 ~ mkp+SML+HML+mkp_up+SML_up+HML_up, data=as.data.frame(ret.Q4.ret.factors_up))
summary(eq15.ret.Q4)
huxreg(eq15.ret.Q4)



# Newey West standard errors correction for serial correlation
out<-coeftest(eq15.ret.Q1, vcov=NeweyWest(eq15.ret.Q1, verbose=T))
huxreg(out)
#print_latex(eq15.ret.Q1)
print_screen(eq15.ret.Q1)
#output summary into table
options(digits = 4)
options(scipen=999)
eq15.ret.Q1<-rbind(out, adjR2 = summary(eq15.ret.Q1)$adj.r.squared)
eq15.ret.Q1
write.csv(eq15.ret.Q1, "~/residual reversal/output/eq15.ret.Q1.csv")
#-----------------------------------------------------------------------
# Repeat above coding by using q1 (quintile portfolio) to see if there is 
# any difference between q1 and Q1
#----------------------------------------------------------------------
ret.q1.ret.factors_up<-merge(ret.q1.ret.factors, data.fa.tw$factors.ind)
head(ret.q1.ret.factors_up)
# run EQ(15) regression
eq15.ret.q1<-dynlm(X1101 ~ mkp+SML+HML+mkp_up+SML_up+HML_up, data=as.data.frame(ret.q1.ret.factors_up))
summary(eq15.ret.q1)
out<-coeftest(eq15.ret.q1, vcov=NeweyWest(eq15.ret.q1, verbose=T))
#output summary into table
options(digits = 4)
options(scipen=999)
eq15.ret.Q1<-rbind(out, adjR2 = summary(eq15.ret.Q1)$adj.r.squared)
write.csv(eq15.ret.q1, "~/residual reversal/output/eq15.ret.Q1.4.csv")

#--------------------------------------------------------------------------
# Here we plot the figure
# looping thru market, size and book
#----------------------------------------------------------------------------
i=1
for (i in 1:3){
  cf.Q1 = cf.ret.Q1[,i+1]
  cf.Q2 = cf.ret.Q2[,i+1]
  factor.ret = lag(ret.Q1.ret.factors[,i+1],1)
  #head(factor.ret)
  reg.Q1  = lm(cf.Q1~factor.ret)
  reg.Q2 = lm(cf.Q2~factor.ret)
  par(cex=.8)
  beta.Q1.Q2 = merge(merge(factor.ret, cf.Q1), cf.Q2)
  #tail(beta.q1.q10.market)
  colnames(beta.Q1.Q2) = c(colnames(factor.ret),"loser", "winner")
  beta.Q1.Q2 = coredata(na.omit(beta.Q1.Q2))
  #head(beta.q1.q10)
  data1 = as.data.frame(beta.Q1.Q2)
  # head(data1)
  data2 = melt(data1, id.vars = colnames(factor.ret), variable.name = "type") 
  # head(data2)
  colnames(data2) = c(colnames(factor.ret),"type","beta")
  f1 = ggplot(data = data2, aes(x=data2[,1], y=beta, shape=type, linetype = type))+ 
    geom_point() + 
    geom_smooth(method = "lm", se=FALSE, fullrange=T, alpha=0.2) +
    scale_x_continuous(colnames(factor.ret))+
    scale_y_continuous(paste(colnames(factor.ret),"beta" ))+
    scale_shape_manual(values=c(1,2))+
    scale_colour_manual(values = c('gray','black'))
  #       scale_colour_hue(h = c(180,0))
  #       scale_colour_grey()
  path = "~/residual reversal/output/"
  file1 = paste("ret_f_Q12_",i, sep="")
  file2 = paste(file1, "_Market.jpeg", sep="")
  file3 = paste(path, file2)
  ggsave(f1, file = file3, scale=2)
}

#====================================================
# 2.1 以下為依標準化後殘差值分為2組後之投資組合分析
# ref: Blitz paper是將資料分為低於樣本平均報酬率為一組 (Q1)，高於樣本平均報酬率為一組(Q2)
#但本程式只分為前50%與後50%的股票
#-------------------------------------
res.Q1.ret = quantiles4.tw$last.e_s$last.e_s_Q1$ret
res.Q4.ret = quantiles4.tw$last.e_s$last.e_s_Q4$ret
head(data.fa.tw$factors)
head(res.Q1.ret)
res.Q1.ret.factors = merge(res.Q1.ret, data.fa.tw$factors[,-4])
res.Q4.ret.factors = merge(res.Q4.ret, data.fa.tw$factors[,-4])
head(res.Q1.ret.factors)
res.Q1.ret.factors = res.Q1.ret.factors["199302/201712"]
res.Q4.ret.factors = res.Q4.ret.factors["199302/201712"]
head(res.Q1.ret.factors)

rolling <- function(x) coef(lm(X1101 ~ ., data = as.data.frame(x)))
cf.res.Q1 = rollapplyr(res.Q1.ret.factors, 36, rolling, by.column = FALSE)
cf.res.Q4 = rollapplyr(res.Q4.ret.factors, 36, rolling, by.column = FALSE)
head(cf.res.Q1,37)
tail(cf.res.Q1)
tail(cf.res.Q4)
#-------------------------------------------------------------
#計算Table 1 panel B eq(15)
#-------------------------------------------------------------
res.Q1.ret.avg<-mean(res.Q1.ret.factors[,1])
res.Q1.ret.avg
#
res.Q4.ret.avg<-mean(res.Q4.ret.factors[,1])
res.Q4.ret.avg
#
res.Q1.std<- sd(res.Q1.ret.factors[,1])
res.Q1.std
SR<-SharpeRatio.annualized(res.Q1.ret.factors[,1], 
                           Rf = data.fa.tw$factors[,4]/12, 
                           scale=12, geometric=FALSE)
SR
#
res.Q4.std<- sd(res.Q4.ret.factors[,1])
res.Q4.std
SR.2<-SharpeRatio.annualized(res.Q4.ret.factors[,1], 
                             Rf = data.fa.tw$factors[,4]/12, 
                             scale=12, geometric=FALSE)
SR.2
#
out<-lm(as.vector(res.Q1.ret.factors[,1])~1)
summary(out)
coeftest(out, vcov=NeweyWest(out, verbose=T))
#
out.2<-lm(as.vector(res.Q4.ret.factors[,1])~1)
summary(out.2)
coeftest(out.2, vcov=NeweyWest(out.2, verbose=T))
#
res.Q1.ret.factors_up<-merge(res.Q1.ret.factors, data.fa.tw$factors.ind)
head(res.Q1.ret.factors_up)
#
res.Q4.ret.factors_up<-merge(res.Q4.ret.factors, data.fa.tw$factors.ind)
head(res.Q4.ret.factors_up)
# run EQ(15) regression
eq15.res.Q1<-dynlm(X1101 ~ mkp+SML+HML+mkp_up+SML_up+HML_up, data=as.data.frame(res.Q1.ret.factors_up))
summary(eq15.res.Q1)
out<-coeftest(eq15.res.Q1, vcov=NeweyWest(eq15.res.Q1, verbose=T))
#
eq15.res.Q4<-dynlm(X1101 ~ mkp+SML+HML+mkp_up+SML_up+HML_up, data=as.data.frame(res.Q4.ret.factors_up))
summary(eq15.res.Q4)
out.2<-coeftest(eq15.res.Q4, vcov=NeweyWest(eq15.res.Q4, verbose=T))
#output summary into table
options(digits = 4)
options(scipen=999)
eq15.res.Q1<-rbind(out, adjR2 = summary(eq15.res.Q1)$adj.r.squared)
eq15.res.Q1
eq15.res.Q4<-rbind(out.2, adjR2 = summary(eq15.res.Q4)$adj.r.squared)
eq15.res.Q4


write.csv(eq15.res.Q1, "~/residual reversal/output/eq15.res.Q1.csv")
#-----------------------------------
#------------------------------------------------------------------------
# Repeat above coding by using q1 (quintile portfolio) to see if there is 
# any difference between q1 and Q1 
#------------------------------------------------------------------------
es.q1.ret.factors_up<-merge(es.q1.ret.factors, data.fa.tw$factors.ind)
head(es.q1.ret.factors_up)
# run EQ(15) regression
eq15.es.q1<-lm(X1101 ~ ., data=as.data.frame(es.q1.ret.factors_up))
summary(eq15.es.q1)
summary(eq15.es.q1)$adj.r.squared
#output summary into table
options(digits = 4)
options(scipen=999)
eq15.es.q1<-rbind(data.frame(summary(eq15.es.q1)$coefficients), adjR2 = summary(eq15.es.q1)$adj.r.squared)
write.csv(eq15.es.q1, "~/residual reversal/output/eq15.es.Q1.csv")
#------------------------------------------------------------------------
# looping thru market, size and book
i=1
for (i in 1:3){
  cf.Q1 = cf.res.Q1[,i+1]
  cf.Q2 = cf.res.Q2[,i+1]
  factor.ret = lag(res.Q1.ret.factors[,i+1],1)
  #head(factor.ret)
  reg.Q1  = lm(cf.Q1~factor.ret)
  reg.Q2 = lm(cf.Q2~factor.ret)
  par(cex=.8)
  beta.Q1.Q2 = merge(merge(factor.ret, cf.Q1), cf.Q2)
  #tail(beta.q1.q10.market)
  colnames(beta.Q1.Q2) = c(colnames(factor.ret),"loser", "winner")
  beta.Q1.Q2 = coredata(na.omit(beta.Q1.Q2))
  #head(beta.q1.q10)
  data1 = as.data.frame(beta.Q1.Q2)
  # head(data1)
  data2 = melt(data1, id.vars = colnames(factor.ret), variable.name = "type") 
  # head(data2)
  colnames(data2) = c(colnames(factor.ret),"type","beta")
  f1 = ggplot(data = data2, aes(x=data2[,1], y=beta, shape=type, linetype = type))+ 
    geom_point() + 
    geom_smooth(method = "lm", se=FALSE, fullrange=T, alpha=0.2) +
    scale_x_continuous(colnames(factor.ret))+
    scale_y_continuous(paste(colnames(factor.ret),"beta" ))+
    scale_shape_manual(values=c(1,2))+
    scale_colour_manual(values = c('gray','black'))
  #       scale_colour_hue(h = c(180,0))
  #       scale_colour_grey()
  path = "~/residual reversal/output/"
  file1 = paste("res_f_Q12_",i, sep="")
  file2 = paste(file1, "_Market.jpeg", sep="")
  file3 = paste(path, file2)
  ggsave(f1, file = file3, scale=2)
}

#*************************************************************
#將股票名稱依十組分別取出, 先從last.es的分組開始
#*************************************************************
#i=1
#j=37
#>names(quantiles.tw$last.e_s):
#[1] "last.e_s_Q1"  "last.e_s_Q2"  "last.e_s_Q3"  "last.e_s_Q4"  "last.e_s_Q5"  "last.e_s_Q6"  "last.e_s_Q7" 
#[8] "last.e_s_Q8"  "last.e_s_Q9"  "last.e_s_Q10" "spread"       "quantiles"
#> names(quantiles.tw$last.e_s[[1]])
#[1] "weight"      "type"        "ret"         "best"        "worst"       "equity"      "cagr"       
#[8] "dates.index"

#********************************
#將分組名單股票分別寫出檔案: 依es分組
# output data: name_stock[[i]], i=1,...,10
#********************************
# For example: definition of name_stock[[1]]: it is a 336*870 matrix. Starting from month (row) 37,
# we can find the stock id listed in the 1-st quantile portfolio. Row 38 shows the stock ids listed in 
# the 1-st quantile portfolio in month 38. The rest applies. 
quantiles = quantiles.tw$last.e_s$models[[12]]
temp = NA*coredata(quantiles)
name_stock_es<-list()
i=1
j=37
for (i in 1:n.quantiles){
  name_stock_es[[i]] = temp
  for (j in 37:length(index.tw)){
    n_list<-names(which(quantiles.tw$last.e_s$models[[12]][j,]==i))
    name_stock_es[[i]][j,1:length(n_list)]<-n_list  
  }
  colnames(name_stock_es[[i]])<-NULL
  filename1=paste(i,"q_es_stocknames.csv",sep="")
  filename2=paste("~/residual reversal/output/", filename1,sep="")
  write.csv(name_stock_es[[i]], file=filename2)
}


#write.table(name_stock[[1]],"D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/1q_stock.names.txt",
#             col.names=F,sep="\t", quote = FALSE)

#*******************************************************************************
#將分組名單股票分別寫出檔案: 傳統報酬率ret分組, 輸出10組股票名單的時間數列資料
# name_stock.ret[[i]], i=1,..., 10
#*******************************************************************************
quantiles.ret = quantiles.tw$one.month$models[[12]]
temp.ret = NA*coredata(quantiles.ret)
name_stock.ret<-list()
i=1
j=37
for (i in 1:n.quantiles){
  name_stock.ret[[i]] = temp.ret
  for (j in 37:length(index.tw)){
    n_list<-names(which(quantiles.tw$one.month$models[[12]][j,]==i))
    name_stock.ret[[i]][j,1:length(n_list)]<-n_list  
  }
  colnames(name_stock.ret[[i]])<-NULL
  filename1=paste(i,"q_ret_stocknames.csv",sep="")
  filename2=paste("~/residual reversal/output/", filename1,sep="")
  write.csv(name_stock.ret[[i]], file=filename2)
}


#******************************************************************
# Replicate Table 4 in Blitz paper
# 1.1 每組個股過去36個month報酬率取出，並計算其波動率,先從last.es的分組開始
# Compute the time-series averages of the median volatility based on residual-sorted 
# portfolios and are estimated
# using the 36 months prior to formation date. 
#******************************************************************
hist.returns = ROC(data.fa.tw$prices[,tickers.tw.f], type = 'discrete')
head(hist.returns[,1:10],10)
returns.std = rollapply(data = hist.returns,
                        width = 36,
                        FUN = sd, na.rm = T)
std.meds.es = matrix(data = NA, nrow=length(index.tw), ncol = 10)
std.meds.avg.es = vector()
# use complete.case to remove NA in stock names
i=1
j=37
for (i in 1:n.quantiles){
  for (j in 37:length(index.tw)){
    id = complete.cases(name_stock_es[[i]][j,]) 
    name = name_stock_es[[i]][j,id]
    temp.std = returns.std[,match(name, colnames(hist.returns))]
    std.meds = apply(temp.std[j,], 1, median, na.rm = TRUE) # get row median
    std.meds.es[j,i] = coredata(std.meds)
  }
  std.meds.avg.es[i] = coredata(mean(std.meds.es[,i], na.rm = TRUE))
}
# average medians of volatility for 10 portfolios
options(digits = 4)
std.meds.avg.es
write.csv(std.meds.avg.es, file="~/residual reversal/output/std_meds_avg_es.csv")


#******************************************************************
#Replicate Table 4 in Blitz paper
#1.2 每組個股過去36個報酬率取出，並計算其波動率,再從one.month的分組開始
# 計算平均時間數列的中位數
#******************************************************************
std.meds.ret = matrix(data = NA, nrow=length(index.tw), ncol = 10)
std.meds.avg.ret = vector()
# use complete.case to remove NA in stock names
i=1
j=37
for (i in 1:n.quantiles){
  for (j in 37:length(index.tw)){
    id = complete.cases(name_stock.ret[[i]][j,]) 
    name = name_stock.ret[[i]][j,id]
    temp.std = returns.std[,match(name, colnames(hist.returns))]
    std.meds = apply(temp.std[j,], 1, median, na.rm = TRUE) # get row median
    std.meds.ret[j,i] = coredata(std.meds)
  }
  std.meds.avg.ret[i] = coredata(mean(std.meds.ret[,i], na.rm = TRUE))
}
std.meds.avg.ret
#
write.csv(std.meds.avg.ret, file="~/residual reversal/output/std_meds_avg_ret.csv")

#******************************************************************
# Replicate Table 4 in Blitz paper
# 2.1 每組個股資本市值，並取各組中位數的時間數列平均 based on es-sorted portfolios
#******************************************************************
cap.meds.es = matrix(data = NA, nrow=length(index.tw), ncol = 10)
cap.mean.es = matrix(data = NA, nrow=length(index.tw), ncol = 10)
cap.meds.avg.es = vector()
cap.mean.avg.es = vector()
# use complete.case to remove NA in stock names
i=1
j=37
for (i in 1:n.quantiles){
  for (j in 37:length(index.tw)){
    id = complete.cases(name_stock_es[[i]][j,]) 
    name = name_stock_es[[i]][j,id]
    temp.cap = mcap.tw[,match(name, colnames(mcap.tw))]
    cap.meds = apply(temp.cap[j,], 1, median, na.rm = TRUE) # get row median
    cap.avg = apply(temp.cap[j,], 1, mean, na.rm = TRUE) # get row mean
    cap.meds.es[j,i] = coredata(cap.meds)
    cap.mean.es[j,i] = coredata(cap.avg)
  }
  cap.meds.avg.es[i] = coredata(mean(cap.meds.es[,i], na.rm = TRUE))
  cap.mean.avg.es[i] = coredata(mean(cap.mean.es[,i], na.rm = TRUE))
}
# average medians of market capitalization for 10 portfolios
cap.meds.avg.es
cap.mean.avg.es
write.csv(cap.meds.avg.es, file="~/residual reversal/output/cap_meds_avg_es.csv")
write.csv(cap.mean.avg.es, file="~/residual reversal/output/cap_mean_avg_es.csv")
#******************************************************************
# Replicate Table 4 in Blitz paper
# 2.2 每組個股資本市值，並取各組中位數的時間數列平均 based on ret-sorted portfolios
#******************************************************************
cap.meds.ret = matrix(data = NA, nrow=length(index.tw), ncol = 10)
cap.mean.ret = matrix(data = NA, nrow=length(index.tw), ncol = 10)
cap.meds.avg.ret = vector()
cap.mean.avg.ret = vector()
# use complete.case to remove NA in stock names
i=1
j=37
for (i in 1:n.quantiles){
  for (j in 37:length(index.tw)){
    id = complete.cases(name_stock.ret[[i]][j,]) 
    name = name_stock.ret[[i]][j,id]
    temp.cap = mcap.tw[,match(name, colnames(mcap.tw))]
    cap.meds = apply(temp.cap[j,], 1, median, na.rm = TRUE) # get row median
    cap.avg = apply(temp.cap[j,], 1, mean, na.rm = TRUE) # get row mean
    cap.meds.ret[j,i] = coredata(cap.meds)
    cap.mean.ret[j,i] = coredata(cap.avg)
  }
  cap.meds.avg.ret[i] = coredata(mean(cap.meds.ret[,i], na.rm = TRUE))
  cap.mean.avg.ret[i] = coredata(mean(cap.mean.ret[,i], na.rm = TRUE))
}
# average medians of market capitalization for 10 portfolios
cap.meds.avg.ret
cap.mean.avg.ret
write.csv(cap.meds.avg.ret, file="~/residual reversal/output/cap_meds_avg_ret.csv")
write.csv(cap.mean.avg.ret, file="~/residual reversal/output/cap_mean_avg_ret.csv")
#=====================================================================================
# Create Blitz paper Table 4 panel 2
#=====================================================================================
#table4<-matrix(data=NA, nrow = 5, ncol = 10)
#colnames(table4)<-c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10")
table4.2<-coeff.meds.es.df[2:4,]
table4.2<-rbind(std.meds.avg.es, table4.2)
table4.2<-rbind(table4.2, cap.meds.avg.es/30) #convert to US million dollars
table4.2<-rbind(table4.2, cap.mean.avg.es/30) #convert to US million dollars
rownames(table4.2)<-c("volatility", "mkp", "SMB", "HML", "MktCap_med", "MktCap_avg")
options(scipen = 999)
table4.2

my.xtable<-xtable(x = table4.2, 
                  label = 'tab:PortChar',
                  caption = "Portfolio characteristics (residual strategy)",
                  digits = 4)
my.xtable

print(my.xtable, include.rownames = TRUE,
      file = '~/residual reversal/output/tables/table4_portchar_res.tex',
      type = 'latex')


#=====================================================================================
# Create Blitz paper Table 4 panel 1 (based on return-sorted portfolios)
#=====================================================================================
#table4<-matrix(data=NA, nrow = 5, ncol = 10)
#colnames(table4)<-c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10")
table4.1<-coeff.meds.ret.df[2:4,]
table4.1<-rbind(std.meds.avg.ret, table4.1)
table4.1<-rbind(table4.1, cap.meds.avg.ret/30) #convert to US million dollars
table4.1<-rbind(table4.1, cap.mean.avg.ret/30) #convert to US million dollars
rownames(table4.1)<-c("volatility", "mkp", "SMB", "HML", "MktCap_med", "MktCap_avg")
options(scipen = 999)
table4.1

my.xtable<-xtable(x = table4.1, 
                  label = 'tab:PortChar',
                  caption = "Portfolio characteristics (conventional strategy)",
                  digits = 4)
my.xtable

print(my.xtable, include.rownames = TRUE,
      file = '~/residual reversal/output/tables/table4_portchar_ret.tex',
      type = 'latex')






#******************************************************************
names(quantiles.tw)
#[1] "last.e"    "last.e_s"  "one.month"
names(quantiles.tw$last.e_s)
#[1] "last.e_s_Q1"  "last.e_s_Q2"  "last.e_s_Q3"  "last.e_s_Q4"  "last.e_s_Q5"  "last.e_s_Q6" 
#[7] "last.e_s_Q7"  "last.e_s_Q8"  "last.e_s_Q9"  "last.e_s_Q10" "spread" "quantiles"        
# quantiles.tw$last.e_s 中第一個成份(Q1)內容如下：
names(quantiles.tw$last.e_s[[1]])
#[1] "weight"      "type"        "ret"         "best"        "worst"       "equity"     
#[7] "cagr"        "dates.index"
#quantiles.tw$last.e_s[[12]]：代表分十組的組別(quantiles);
dim(quantiles.tw$last.e_s[[12]])
head(quantiles.tw$last.e_s[[1]]["weight"])
names(quantiles.tw$one.month)
#[1] "one.month_Q1"  "one.month_Q2"  "one.month_Q3"  "one.month_Q4"  "one.month_Q5" 
#[6] "one.month_Q6"  "one.month_Q7"  "one.month_Q8"  "one.month_Q9"  "one.month_Q10"
#[11] "spread"        "quantiles"    
names(quantiles.tw$one.month[[1]])
names(quantiles.tw$one.month$one.month_Q1)
#[1] "weight"      "type"        "ret"         "best"        "worst"       "equity"     
#[7] "cagr"        "dates.index"

#es:代表standardized residuals; e:代表residuals;，最後一項為equal weight for all stocks' portfolio
all.ret.es.10Q<-merge.xts(quantiles.tw$last.e_s[[1]][3]$ret,quantiles.tw$last.e_s[[2]][3]$ret,
                          quantiles.tw$last.e_s[[3]][3]$ret,quantiles.tw$last.e_s[[4]][3]$ret,
                          quantiles.tw$last.e_s[[5]][3]$ret,quantiles.tw$last.e_s[[6]][3]$ret,
                          quantiles.tw$last.e_s[[7]][3]$ret,quantiles.tw$last.e_s[[8]][3]$ret,
                          quantiles.tw$last.e_s[[9]][3]$ret,quantiles.tw$last.e_s[[10]][3]$ret,
                          quantiles.tw$last.e_s[[11]][3]$ret,models.tw[[1]][3]$ret)
dim(all.ret.es.10Q)

all.equity.es.10Q<-merge.xts(quantiles.tw$last.e_s[[1]][6]$equity,quantiles.tw$last.e_s[[2]][6]$equity,
                             quantiles.tw$last.e_s[[3]][6]$equity,quantiles.tw$last.e_s[[4]][6]$equity,
                             quantiles.tw$last.e_s[[5]][6]$equity,quantiles.tw$last.e_s[[6]][6]$equity,
                             quantiles.tw$last.e_s[[7]][6]$equity,quantiles.tw$last.e_s[[8]][6]$equity,
                             quantiles.tw$last.e_s[[9]][6]$equity,quantiles.tw$last.e_s[[10]][6]$equity,
                             quantiles.tw$last.e_s[[11]][6]$equity, models.tw[[1]][6]$equity)
dim(all.equity.es.10Q)

names(all.equity.es.10Q)<-c(names(quantiles.tw$last.e_s)[1:11],"equalw")
names(all.ret.es.10Q)<-c(names(quantiles.tw$last.e_s)[1:11],"equalw")

#以下為未標準化的殘差資料分組結果;，最後一項為equal weight for all stocks' portfolio
# all.ret.e.10Q<-merge.xts(quantiles.tw$last.e[[1]][3]$ret,quantiles.tw$last.e[[2]][3]$ret,
#                          quantiles.tw$last.e[[3]][3]$ret,quantiles.tw$last.e[[4]][3]$ret,
#                          quantiles.tw$last.e[[5]][3]$ret,quantiles.tw$last.e[[6]][3]$ret,
#                          quantiles.tw$last.e[[7]][3]$ret,quantiles.tw$last.e[[8]][3]$ret,
#                          quantiles.tw$last.e[[9]][3]$ret,quantiles.tw$last.e[[10]][3]$ret,
#                          quantiles.tw$last.e[[11]][3]$ret,models.tw[[1]][3]$ret)

# all.equity.e.10Q<-merge.xts(quantiles.tw$last.e[[1]][6]$equity,quantiles.tw$last.e[[2]][6]$equity,
#                             quantiles.tw$last.e[[3]][6]$equity,quantiles.tw$last.e[[4]][6]$equity,
#                             quantiles.tw$last.e[[5]][6]$equity,quantiles.tw$last.e[[6]][6]$equity,
#                             quantiles.tw$last.e[[7]][6]$equity,quantiles.tw$last.e[[8]][6]$equity,
#                             quantiles.tw$last.e[[9]][6]$equity,quantiles.tw$last.e[[10]][6]$equity,
#                             quantiles.tw$last.e[[11]][6]$equity, models.tw[[1]][6]$equity)
# 
# names(all.equity.e.10Q)<-c(names(quantiles.tw$last.e)[1:11],"equalw")
# names(all.ret.e.10Q)<-c(names(quantiles.tw$last.e)[1:11],"equalw")

#以下為依股票報酬率排序分組之結果，最後一項為equal weight for all stocks' portfolio
all.equity.ret.10Q<-merge.xts(quantiles.tw$one.month[[1]][6]$equity,quantiles.tw$one.month[[2]][6]$equity,
                              quantiles.tw$one.month[[3]][6]$equity,quantiles.tw$one.month[[4]][6]$equity,
                              quantiles.tw$one.month[[5]][6]$equity,quantiles.tw$one.month[[6]][6]$equity,
                              quantiles.tw$one.month[[7]][6]$equity,quantiles.tw$one.month[[8]][6]$equity,
                              quantiles.tw$one.month[[9]][6]$equity,quantiles.tw$one.month[[10]][6]$equity,
                              quantiles.tw$one.month[[11]][6]$equity,models.tw[[1]][6]$equity)

all.return.ret.10Q<-merge.xts(quantiles.tw$one.month[[1]][3]$ret,quantiles.tw$one.month[[2]][3]$ret,
                              quantiles.tw$one.month[[3]][3]$ret,quantiles.tw$one.month[[4]][3]$ret,
                              quantiles.tw$one.month[[5]][3]$ret,quantiles.tw$one.month[[6]][3]$ret,
                              quantiles.tw$one.month[[7]][3]$ret,quantiles.tw$one.month[[8]][3]$ret,
                              quantiles.tw$one.month[[9]][3]$ret,quantiles.tw$one.month[[10]][3]$ret,
                              quantiles.tw$one.month[[11]][3]$ret, models.tw[[1]][3]$ret)

names(all.return.ret.10Q)<-c(names(quantiles.tw$one.month)[1:11],"equalw")
names(all.equity.ret.10Q)<-c(names(quantiles.tw$one.month)[1:11],"equalw")
tail(all.equity.ret.10Q,1)
#將依分組報酬率資料寫出
write.csv(as.data.frame(all.ret.es.10Q), file="~/residual reversal/output/all.ret_es_10Q.csv")
write.csv(as.data.frame(all.equity.es.10Q), file="~/residual reversal/output/all.equity_es_10Q.csv")
write.csv(as.data.frame(all.ret.e.10Q), file="~/residual reversal/output/all.ret_e_10Q.csv")
write.csv(as.data.frame(all.equity.e.10Q), file="~/residual reversal/output/all.equity_e_10Q.csv")
write.csv(as.data.frame(all.equity.ret.10Q), file="~/residual reversal/output/all.equity_ret_10Q.csv")
write.csv(as.data.frame(all.return.ret.10Q), file="~/residual reversal/output/all.return_ret_10Q.csv")
#maxDD<-compute.max.drawdown(quantiles.tw$one.month[[1]][6]$equity)
#cagr<-compute.cagr(quantiles.tw$one.month[[1]][6]$equity)

#*****************************************************************
# Create Report
#******************************************************************   				
plotbt.custom.report.part1(quantiles.tw$one.month$spread,quantiles.tw$last.e$spread,quantiles.tw$last.e_s$spread)

plotbt.strategy.sidebyside(quantiles.tw$one.month$spread,quantiles.tw$last.e$spread,quantiles.tw$last.e_s$spread)

plotbt.custom.report.part1(quantiles.tw$one.month$one.month_Q1, 
                           quantiles.tw$last.e$last.e_Q1, 
                           quantiles.tw$last.e_s$last.e_s_Q1)

#*******************************************************
# replicate Table 1 in Blitz  
#*******************************************************
#******************************************************
# Summary statistics:
# 1. return reversal strategy
#*******************************************************
ret.stats = matrix(data = NA, nrow=12, ncol =8)
#所有股票等權重之投資組合月報酬率
equal.ret = models.tw[[1]][3]$ret
# x = rnorm(1000)
# ksnormTest(x)
# out1 = jarqueberaTest(x)
# out2 = jbTest(x)
# str(out1)
# str(out2)

i=1
for (i in 1:12){
  one.month.ret  = all.return.ret.10Q[,i]["199302/201712"]
  annual.factor <-length(one.month.ret)/12
  cagr = (tail(cumprod(as.vector(one.month.ret)+1),1))^(1/annual.factor)-1
  cagr = cagr * 100
  #equal.ret = equal.ret["199302/201212"]
  #one.month.ret = merge(one.month.ret, equal.ret)
  RF.tw = data.fa.tw$factors$"RF"["199302/201712"]
  average.ret = mean(one.month.ret)
  stdev.ret = sd(one.month.ret)
  skew = skew(one.month.ret)
  kurtosis = kurtosi(one.month.ret)
  jbtest = jarqueberaTest(coredata(one.month.ret))
  #jbtest1 = jbTest(coredata(one.month.ret))
  #average.ret = mean(quantiles.tw$one.month[[i]][3]$ret)
  #stdev.ret = sd(quantiles.tw$one.month[[i]][3]$ret)
  #RF.tw<-data.fa.tw$factors$"RF"
  SR = SharpeRatio.annualized(one.month.ret, Rf = RF.tw/12, scale=12, geometric=FALSE)
  #SR.eq = SharpeRatio.annualized(equal.ret, Rf = RF.tw/12, scale=12, geometric=FALSE)
  ret.stats[i,] = rbind(c(as.numeric(average.ret), as.numeric(stdev.ret), skew, as.numeric(kurtosis), 
                          jbtest@test$statistic, as.numeric(jbtest@test$p.value), 
                          as.numeric(SR), as.numeric(cagr)))
}
ret.stats
ret.stats.df = as.data.frame(ret.stats, row.names = names(all.return.ret.10Q))
colnames(ret.stats.df) = c("average month ret", "month stdev", "skew", "kurtosis",
                           "jb-test", "jb-pvalue","Annual. Sharpe ratio", "CAGR")
options(digits = 4)
ret.stats.df
# write output to latex file
my.xtable<-xtable(x = ret.stats.df, 
                  label = 'tab:returnRevStats',
                  caption = "Descriptive statistics for return reversal",
                  digits = 4)

print(my.xtable, include.rownames = TRUE,
      file = '~/residual reversal/output/tables/table1_return_stats.tex',
      type = 'latex')
#write.csv(ret.stats.df, file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/return_ret_10Q_stats.csv")

#***********************************************
# Summary statistics:
# 2. residual return reversal strategy
#***********************************************
res.stats = matrix(data = NA, nrow=12, ncol =8)

for (i in 1:12){
  one.month.es  = all.ret.es.10Q[,i]["199302/201712"]
  annual.factor <-length(one.month.es)/12
  cagr = (tail(cumprod(as.vector(one.month.es)+1),1))^(1/annual.factor)-1
  cagr = cagr * 100
  RF.tw = data.fa.tw$factors$"RF"["199302/201712"]
  average.ret.es = mean(one.month.es)
  stdev.ret.es = sd(one.month.es)
  skew = skew(one.month.es)
  kurtosis = kurtosi(one.month.es)
  jbtest = jarqueberaTest(coredata(one.month.es))
  #average.ret = mean(quantiles.tw$one.month[[i]][3]$ret)
  #stdev.ret = sd(quantiles.tw$one.month[[i]][3]$ret)
  #RF.tw<-data.fa.tw$factors$"RF"
  SR = SharpeRatio.annualized(one.month.es, Rf = RF.tw/12, scale=12, geometric=FALSE)
  #SR.eq = SharpeRatio.annualized(equal.ret, Rf = RF.tw/12, scale=12, geometric=FALSE)
  res.stats[i,] = rbind(c(as.numeric(average.ret.es), as.numeric(stdev.ret.es), skew, as.numeric(kurtosis), 
                          jbtest@test$statistic, as.numeric(jbtest@test$p.value), 
                          as.numeric(SR), as.numeric(cagr)))
}
res.stats
res.stats.df = as.data.frame(res.stats, row.names = names(all.equity.es.10Q))
colnames(res.stats.df) = c("average month ret", "month stdev", "skew", "kurtosis","jb-test", 
                           "jb-pvalue","Annual. Sharpe ratio", "CAGR")
res.stats.df
# write output to latex file
my.xtable1<-xtable(x = res.stats.df, 
                   label = 'tab:residualRevStats',
                   caption = "Descriptive statistics for resiidual reversal",
                   digits = 4)

print(my.xtable1, include.rownames = TRUE,
      file = '~/residual reversal/output/tables/table1_residual_stats.tex',
      type = 'latex')

#write.csv(res.stats.df, file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/return_res_10Q_stats.csv")
#***********************************************************************************

#******************************************************************
# Compute ROE statistics for es-sorted portfolio
# 
#******************************************************************
ROE.meds.es = matrix(data = NA, nrow=length(index.tw), ncol = 10)
ROE.mean.es = matrix(data = NA, nrow=length(index.tw), ncol = 10)
ROE.meds.avg.es = vector()
ROE.mean.avg.es = vector()
# use complete.case to remove NA in stock names
i=1
j=37
for (i in 1:n.quantiles){
  for (j in 37:length(index.tw)){
    id = complete.cases(name_stock[[i]][j,]) 
    name = name_stock[[i]][j,id]
    temp.ROE = ROE.reorder12.xts[, match(name, colnames(ROE.reorder12.xts))]
    ROE.meds = apply(temp.ROE[j,], 1, median, na.rm = TRUE) # get row median
    ROE.avg = apply(temp.ROE[j,], 1, mean, na.rm = TRUE) # get row mean
    ROE.meds.es[j,i] = coredata(ROE.meds)
    ROE.mean.es[j,i] = coredata(ROE.avg)
  }
  ROE.meds.avg.es[i] = coredata(mean(ROE.meds.es[,i], na.rm = TRUE))
  ROE.mean.avg.es[i] = coredata(mean(ROE.mean.es[,i], na.rm = TRUE))
}
# average medians of market capitalization for 10 portfolios
ROE.meds.avg.es
ROE.mean.avg.es
write.csv(ROE.meds.avg.es, file="~/residual reversal/output/ROE_meds_avg_es.csv")
write.csv(ROE.mean.avg.es, file="~/residual reversal/output/ROE_mean_avg_es.csv")














#******************************************************************
# Performance analysis
#******************************************************************
# Event lists - FOR BEST RESULTS, KEEP THESE DATES IN ORDER
risk.dates = c(
  "1987-10-19",
  "1995-07-21",
  "1997-10-27",
  "1998-08-31",
  "2000-04-14",
  "2001-09-11",
  "2008-09-29")
risk.labels = c(
  "Black Monday",
  "Taiwan Strait Missle Crisis",
  "Asia Turmoi",
  "Russian Dfault",
  "Dotcom Collapse",
  "911 Terror Attack",
  "U.S. Financial Crisis"
)

#****************************
# 利用plot.xts繪圖
# 1. create report on all.return.ret.10Q
#****************************
colnames(all.equity.ret.10Q) = c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8",
                                 "Q9","Q10","spread","EQ")
head(all.equity.ret.10Q)
#function add alpha or transparency to colors
addalpha <- function(cols,alpha=180) {
  rgbcomp <- col2rgb(cols)
  rgbcomp[4] <- alpha
  return(rgb(rgbcomp[1],rgbcomp[2],rgbcomp[3],rgbcomp[4],maxColorValue=255))
}

#postscript("c:/whatever.eps")
#plot(rnorm(100,5), type = "l", main="Hey Some Data")
#dev.off()


# export figure 
#jpeg("c:/10Qret.jpeg",width=640,height=567,units="px")
#pdf("c:/10Qret.pdf")
#postscript("c:/10Qret.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 10, width = 10)

setEPS()
postscript("c:/10Qret.eps", horizontal = FALSE, onefile = FALSE, paper = "a4",
           colormodel = "rgb") # here we have include "colormodel = "rgb" " so that
# we can import .eps file into word doc.
plot.xts(all.equity.ret.10Q, 
         screens=c(1,1,1,1,1,1,1,1,1,1,2,2), #screens=1 probably most appropriate for this application
         layout.screens=c(1,1,2), 
         events = list(  #get events to plot from above risk.date in list form
           time = as.Date(risk.dates),
           label = risk.labels,
           col = "black"), 
         lwd = c(2, 1.8, 1.6, 1.4, 1.2, 1.6, 1.2, 1.8, 1.2, 1.2, 2,1.8),
         lty = c(1,2,3,4,5,6,7,8,9,10,2,1),
         col = c(1,2,3,4,5,1,7,1,9,10,1,2), #第6,8組為黑色，強調其績效較佳
         major.format = "%m-%Y", minor.ticks = FALSE,
         legend.loc = "topleft", auto.legend=TRUE,
         main = NA
         #main="Quintile, spread and equal weight portfolios" )
         #main="依報酬率分組、反轉策略及等權投資組合")
)
title(main = "Traditional reverse strategy", outer = TRUE, adj = 0.5, line =-1 )
title(ylab = "cumulative returns")
dev.off()


#******************************************
# 利用plot.xts繪圖
# 2. create report on all.equity.es.10Q
#******************************************
colnames(all.equity.es.10Q) = c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8",
                                "Q9","Q10","spread","EQ")
head(all.equity.es.10Q)
#function add alpha or transparency to colors
addalpha <- function(cols,alpha=180) {
  rgbcomp <- col2rgb(cols)
  rgbcomp[4] <- alpha
  return(rgb(rgbcomp[1],rgbcomp[2],rgbcomp[3],rgbcomp[4],maxColorValue=255))
}

#postscript("c:/whatever.eps")
#plot(rnorm(100,5), type = "l", main="Hey Some Data")
#dev.off()


# export figure 
#jpeg("c:/10Qes.jpeg",width=640,height=567,units="px")
#pdf("c:/10Qes.pdf")
#postscript("c:/10Qes.eps", horizontal = FALSE, onefile = FALSE, paper = "special", height = 10, width = 10)

#setEPS()
postscript("c:/10Qes.eps", horizontal = FALSE, onefile = FALSE, paper = "special",
           colormodel = "rgb") # here we have include "colormodel = "rgb" " so that
# we can import .eps file into word doc.
plot.xts(all.equity.es.10Q, 
         screens=c(1,1,1,1,1,1,1,1,1,1,2,2), #screens=1 probably most appropriate for this application
         layout.screens=c(1,1,2), 
         events = list(  #get events to plot from above risk.date in list form
           time = as.Date(risk.dates),
           label = risk.labels,
           col = "black"), 
         lwd = c(2, 1.8, 1.6, 1.4, 1.2, 1.6, 1.2, 1.8, 1.2, 1.2, 2,1.8),
         lty = c(1,2,3,4,5,6,7,8,9,10,2,1),
         col = c(1,2,3,4,5,1,7,1,9,10,1,2), #第6,8組為黑色，強調其績效較佳
         major.format = "%m-%Y", minor.ticks = FALSE,
         legend.loc = "topleft", auto.legend=TRUE,
         main = NA
         #main="Quintile, spread and equal weight portfolios" )
         #main="依報酬率分組、反轉策略及等權投資組合")
)
title(main = "Residual reversal strategy", outer = TRUE, adj = 0.5, line =-1 )
title(ylab = "cumulative returns")
dev.off()

#*******************************************************************************
# Cumulative returns, Expected shortfall and drawdowns
# for return.Q1, es.Q1, es.spread and EQ;  
# Reference: http://timelyportfolio.blogspot.tw/2012/08/plotxts-is-wonderful.html
#********************************************************************************
library(calibrate)
R = merge(all.return.ret.10Q$"one.month_Q1", all.ret.es.10Q$"last.e_s_Q1",
          all.ret.es.10Q$"spread",all.ret.es.10Q$"equalw")
colnames(R) = c("ret.Q1", "es.Q1", "es.spread", "EQ")
head(R,38)
R = R[-1:-37,]
#write.csv(as.data.frame(R), file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/R_month_ret.csv")
Return.cumulative = cumprod(1+R) - 1
head(Return.cumulative,38)
tail(Return.cumulative)

cumulreturn.panel  <- function(...) {
  mtext("Cumulative Return", side=1, adj=1, cex = 0.7,line=-3)
  #textxy(x=index(tail(Return.cumulative[,1],1)), y=11.0236, labs = "ret.Q1")
  default.panel(...)
  abline(h=pretty(c(par("yaxp")[1],par("yaxp")[2]),n=par("yaxp")[3]),col="gray60",lty=3)
  abline(h=0, col="black")
}

es.panel <- function(index,x,...) {
  mtext("Expected Shortfall", side=1, adj=1, cex = 0.7, line=-3) 
  default.panel(index,x,...)
  #silly to do this but if we wanted just certain points like every 4 months 
  #we could do something like this
  #default.panel(index[seq(1,NROW(index),by=4)],coredata(x[seq(1,NROW(index),by=4)]),...)
  #abline(h=0, col="black")
  abline(h=pretty(c(par("yaxp")[1],par("yaxp")[2]),n=par("yaxp")[3]),col="gray60",lty=3)
  abline(h=par("yaxp")[1], col="black")
}

drawdown.panel <-  function(index,x,...) {  
  mtext("Drawdown", side=1, adj=1, cex = 0.7,line=-3) 
  default.panel(index,x,...)
  #silly to do this but if we wanted just certain points like every 4 months we could do something like this
  #default.panel(index[seq(1,NROW(index),by=4)],coredata(x[seq(1,NROW(index),by=4)]),...)
  #abline(h=0, col="black")
  abline(h=pretty(c(par("yaxp")[1],par("yaxp")[2]),n=par("yaxp")[3]),col="gray60",lty=3)
  abline(h=par("usr")[3], col="black")
}

#get some risk measurements to add for Performance Summary style plot
Risk.drawdown <- Drawdowns(R)

Risk.es <- rollapplyr(R,FUN="ES",width=36,p=0.95,na.pad=TRUE)
head(Risk.es,37)
ES
#take care of NA with 0 at beginning and interpolation at end
Risk.es <- apply(Risk.es,MARGIN=2,FUN=na.fill,fill=c(0,"extend"))
#something wrong with returned value from apply.rolling so indexes don't work properly
data.to.plot <- as.xts(cbind(coredata(Return.cumulative),Risk.es,coredata(Risk.drawdown)),order.by=index(R))

#png("chartsPerformanceSummary.png",width=640,height=600,units="px")
postscript("c:/drawdowns.eps", horizontal = FALSE, onefile = FALSE, paper = "special",
           colormodel = "rgb")
plot.xts(data.to.plot,
         lwd = c(2,2.5,1.8,1.7), #do this to show how arguments are recycled
         col = brewer.pal(n=9,"Greys")[c(9,7,6,5)], # from darkest to light;
         lty = c(2,1,1,1),
         events = list(  #get events to plot from above risk.date in list form
           time = as.Date(risk.dates),
           label = risk.labels,
           col = "red"), 
         auto.grid = TRUE, #usually auto.grid works just fine but turn off for example purposes
         las = 1,yax.loc = "right",  # yax.loc could also be flip or left in this case
         screens = c(1,1,1,1,2,2,2,2,3,3,3,3),  #4 series for each so first 4 in panel 1 second 4 in panel 2 and last 4 in panel 3
         layout.screens = c(1,1,2,3), #panel 1 take up first 2 of 4 so 50% and panels 2 and 3 each 25%
         bty = "n", 
         panel = c(cumulreturn.panel,es.panel,drawdown.panel), #c(first.panel,"auto"), #panel cycles through by panel rather than each series
         ylab = NA, major.format = "%m-%Y", minor.ticks = FALSE,
         legend.loc = c("topleft","bottomleft","bottomleft"), auto.legend = TRUE,
         #legend.loc = c("topleft",NA,NA), auto.legend = TRUE,
         #legend.pars = list(bty = "n", horiz=TRUE),  #make legend box transparent
         cex.axis = 0.9, 
         main = NA)

title(main = "Performance Summary of two reversal strategies", adj = 0.5, outer = TRUE, line = -1)
dev.off()
#********************************************************************

#*****************************
# calcualte returns by month
# 將四個策略依每個月報酬率分別計算平均值
#*****************************
ret.bymonth = matrix(data = NA, nrow=12, ncol =4)
t.stats = matrix(data = NA, nrow=12, ncol =4)
p.value = matrix(data = NA, nrow=12, ncol =4)
star.df = data.frame(p.value)*NA
i=1
j=1
for (i in 1:4){
  for (j in 1:12){
    R.i = R[,i]["2003/2012"]
    #R.i = R[,i]["1993/2012"]
    ret.bymonth[j,i] = mean(R.i[.indexmon(R.i)==j-1]) # 0 for Jan, 1 for Feb,...
    t.stats[j,i] = t.test(R.i[.indexmon(R.i)==j-1], mean = 0)$statistic
    p.value[j,i] = t.test(R.i[.indexmon(R.i)==j-1])$p.value
    if (p.value[j,i]<0.01){
      star.df[j,i] = "***"
    }
    else if (p.value[j,i]<0.05){
      star.df[j,i] = "**"
    }
    else if (p.value[j,i]<0.1){
      star.df[j,i] = "*"
    }
    else  {
      star.df[j,i] = ""
    }
    
  }
}

ret.bymonth.df = as.data.frame(ret.bymonth)
t.stats.df = as.data.frame(t.stats)
p.value.df = as.data.frame(p.value)
colnames(ret.bymonth.df) = colnames(R)
colnames(t.stats.df) = colnames(R)
colnames(p.value.df) = colnames(R)
head(ret.bymonth.df)
star.df

#newcol<-sapply(colnames(t.stats.df), function(cn){t.stats.df[match(cn, colnames(star.df)), 1]})
#test = matrix(paste(ret.bymonth.df[,1:2], star.df[,1:2], sep=""), ncol=2, nrow=12)
#combine star with t-stats values;
bracket.star = t.stats.df * NA
i=1
for (i in 1:4){
  bracket = paste(paste( "(",round(t.stats.df[,i],digits =4), sep=""), ")", sep="")
  bracket.star[,i] = paste(bracket, star.df[,i], sep="")
}
bracket.star


#write.csv(ret.bymonth.df, file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/ret_bymonth_1993_2012.csv")
#write.csv(t.stats.df, file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/t_stats_1993_2012.csv")
#write.csv(p.value.df, file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/p_value_1993_2012.csv")
#write.csv(bracket.star, file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/star_value_1993_2012.csv")
#*******************************************************************

#*******************************************
# create grouped monthly (2002-2012) barplot with R
#*******************************************
library(ggplot2)
ret.bymonth.df1 = cbind(month=1:12, ret.bymonth.df)
#months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
#ret.bymonth.df1 = cbind(month=months, ret.bymonth.df)
head(ret.bymonth.df1)
ret.bymonth.mdf = melt(ret.bymonth.df1,id = 1)
head(ret.bymonth.mdf,20)
colnames(ret.bymonth.mdf) = c("month","type","returns")
#ggplot(ret.bymonth.mdf, aes(months, returns, fill = type)) + geom_bar(position = "dodge")+
#  labs(title="Returns by month")
setEPS()
postscript("c:/ret_bymonth_2003_2012.eps", horizontal = FALSE, onefile = FALSE, paper = "special",
           colormodel = "rgb") # here we have include "colormodel = "rgb" " so that
# we can import .eps file into word doc.
qplot(factor(ret.bymonth.mdf$month),data=ret.bymonth.mdf,geom="bar",
      fill=type,weight=returns,position="dodge",
      main = "Return by month (2003-2012)", xlab="month",ylab="Returns")+
  scale_fill_grey()
dev.off()      

#ggplot(ret.bymonth.mdf, aes(month, fill=returns) + geom_bar(position="dodge") +
#  labs(title="Examplary Grouped Barplot")

#ggplot(ret.bymonth.mdf,aes(x = month,y = returns)) +  geom_bar(stat= identity, fill = type, position = "dodge")

#*******************************************
# create grouped annual returns (1993-2001) barplot with R
#*******************************************
#R.cum.y = data.frame[]
temp = matrix(rep(0,80), nrow=20, ncol=4)
year.index = seq(as.Date("1993-12-31"), length=20, by="1 year")
year.xts = as.xts(temp, order.by = year.index)
# colnames(temp.xts) = colnames(R)
i="2007"
for (i in 1993:2012){
  R.yi = R[as.character(i)]
  R.cum.yi = cumprod(1+R.yi) - 1
  year.xts[i-1992,]=tail(R.cum.yi,1)
}
colnames(year.xts) = colnames(R)
#coredata(year.xts)

year.range = "2002/2012"
year.xts = year.xts[year.range]
ret.byyr.df = cbind(year=2002:2012,as.data.frame(coredata(year.xts), colnames(R)))
#colnames(ret.byyr.df)= colnames("year", colnames(year.xts))
ret.byyr.mdf = melt(ret.byyr.df,id = 1)
head(ret.byyr.mdf,20)
colnames(ret.byyr.mdf) = c("year","type","returns")

setEPS()
postscript("c:/ret_byyr.eps", horizontal = FALSE, onefile = FALSE, paper = "special",
           colormodel = "rgb") # here we have include "colormodel = "rgb" " so that
# we can import .eps file into word doc.
qplot(factor(ret.byyr.mdf$year),data=ret.byyr.mdf,geom="bar",
      fill=type,weight=returns,position="dodge",
      main = "Return by year (2002-2012)", xlab="year",ylab="Returns")+
  scale_fill_grey()
dev.off() 

#***************************************************************************
# boxplot by strategies
#****************************************************************************
setEPS()
postscript("c:/ret_boxplot_2002_2012.eps", horizontal = FALSE, onefile = FALSE, paper = "special",
           colormodel = "rgb") 
chart.Boxplot(R["2002/2012"], names=T, horizontal=TRUE, colorset="darkgreen", as.Tufte =F,
              mean.symbol = 21, median.symbol="|", 
              main="Return Distributions Comparison (2002-2012)",
              element.color = "darkgray", outlier.symbol = 20, 
              xlab="monthly discrete Returns", sort.ascending=T)
dev.off()
#
setEPS()
postscript("c:/ret_boxplot_1993_2012.eps", horizontal = FALSE, onefile = FALSE, paper = "special",
           colormodel = "rgb") 
chart.Boxplot(R, names=T, horizontal=TRUE, colorset="darkgreen", as.Tufte =F,
              mean.symbol = 21, median.symbol="|", 
              main="Return Distributions Comparison (1993-2012)",
              element.color = "darkgray", outlier.symbol = 20, 
              xlab="monthly discrete Returns", sort.ascending=T)
dev.off()

#********************************************************************
# boxplot by month
#*******************************************************************
period.ends = endpoints(R, 'months')
period.ends = period.ends[period.ends > 0]   
dates = index(R)

month = date.month(dates[period.ends])
R.month = cbind(month, coredata(R))
head(R.month)
R.bymonth.mdf = melt(as.data.frame(R.month), id = 1)
head(R.bymonth.mdf)
colnames(R.bymonth.mdf) = c("month", "type", "returns")
head(R.bymonth.mdf)

setEPS()
postscript("c:/ret_bymonth_box.eps", horizontal = FALSE, onefile = FALSE, paper = "special",
           colormodel = "rgb") # here we have include "colormodel = "rgb" " so that
# we can import .eps file into word doc.
R.bymonth.mdf$month = factor(R.bymonth.mdf$month)
R.bymonth.mdf$type = factor(R.bymonth.mdf$type)

ggplot(data=R.bymonth.mdf, aes(x = month, y = returns)) +
  geom_boxplot(aes(fill = type), width = 1) + theme_bw()
# + stat_summary(fun.y=mean, geom="point")
# + scale_fill_grey() 

dev.off() 
#*******************************
# Seasonal investment strategy
#********************************
month.index = seq(as.Date("1993-02-01"), length=240, by="1 month") - 1
price.xts = xts(matrix(data = NA, nrow = length(month.index), ncol = 4), 
                order.by = month.index)


R.cum = cumprod(1+R)
R.cum2price = 100*R.cum 
R.cum2price
price.xts[-1,] = R.cum2price
price.xts[1,] = c(100,100,100,100)

colnames(price.xts) = names(R.cum)
head(price.xts)
period.ends = endpoints(price.xts, 'months')
period.ends = period.ends[period.ends > 0]   
dates = index(price.xts)

months = date.month(dates[period.ends])
length(months)
#
strategy <- new.env()
strategy$prices = price.xts
strategy$dates = index(price.xts)
#prices = strategy$prices 
#dates = strategy$dates  
models = list()
strategy$weight = strategy$prices*NA
strategy$execution.price = strategy$prices*NA
strategy$weight$ret.Q1 = 1
models$ret.Q1  = bt.run.share(strategy, clean.signal=F) 
ls(models$ret.Q1)
models$ret.Q1$equity
models$ret.Q1$ret
models$ret.Q1$best
models$ret.Q1$worst
models$ret.Q1$weight
# es.Q1 from the October[10] close through the March[3] close and cash otherwise (es.Q1 /Cash)
strategy$weight =strategy$prices*NA

strategy$weight$ret.Q1[period.ends] = iif( months >= 10 | months <= 3, 1, 0)
strategy$weight$cash[period.ends] = iif( !(months >= 10 | months <= 3), 1, 0)
ls(strategy$weight)
strategy$ret.Q1_Cash  = bt.run.share(strategy, clean.signal=F)  
#********************
# bt.run.share() 
#********************
b = strategy

bt.run.share =
  function
(
  b,
  prices = b$prices,
  clean.signal = T,
  trade.summary = F,
  do.lag = 1,
  do.CarryLastObservationForwardIfNA = TRUE,
  silent = F,
  capital = 100000,
  commission = 0,
  weight = b$weight,
  dates = 1:nrow(b$prices)
)
  {
  prices[] = bt.apply.matrix(coredata(prices), ifna.prev)
  weight = mlag(weight, do.lag - 1)
  do.lag = 1
  if(clean.signal) {
    weight[] = (capital / prices) * bt.exrem(weight)
  } else {
    weight[] = (capital / prices) * weight
  }
  bt.run(b,
         trade.summary = trade.summary,
         do.lag = do.lag,
         do.CarryLastObservationForwardIfNA = do.CarryLastObservationForwardIfNA,
         type='share',
         silent = silent,
         capital = capital,
         commission = commission,
         weight = weight,
         dates = dates)
}

bt.exrem = 
  function(weight)
  {
    bt.apply.matrix(weight, exrem)
  }

#****************************************************
# Create Regression Results in Table 1 in Blitz paper
#****************************************************
library(dynlm)
library(sandwich)
library(lmtest)
#head(ff3f.tw.xts)
lag1.ff3f.tw.xts = lag(ff3f.tw.xts[,-4])
head(lag1.ff3f.tw.xts)
# create dummy variables
#dummy.ff3f.tw = ifelse(coredata(lag1.ff3f.tw.xts)>0,1,0)
dummy.ff3f.tw.xts = (lag1.ff3f.tw.xts>0)*1
head(dummy.ff3f.tw.xts)
regression.data = merge(R,ff3f.tw.xts/100,dummy.ff3f.tw.xts)
head(regression.data)
#delete rows with NA;
data1 = regression.data[!is.na(regression.data[,1]),]
head(data1)
data1 = data1["1993/2012"]
ret.Q1_RF = data1$ret.Q1-data1$RF/12
head(ret.Q1_RF)
fit.ret <- dynlm(ret.Q1_RF~ market + size + book + market.1 + size.1 + book.1, data=data1)
summary(fit.ret)
fit.ret.summ <- summary(fit.ret)
fit.ret.summ$coefficients <- unclass(coeftest(fit.ret, vcov. = NeweyWest))

# es.Q1 data
es.Q1_RF = data1$es.Q1-data1$RF/12
head(es.Q1_RF)
fit.es <- lm(es.Q1_RF~ market + size + book + market.1 + size.1 + book.1, data=data1)
summary(fit.es)
# es.spread data
es.spread_RF = data1$es.spread-data1$RF/12
head(es.spread_RF)
fit.es.spread <- lm(es.spread_RF~ market + size + book + market.1 + size.1 + book.1, data=data1)
summary(fit.es.spread)
# EQ data
EQ_RF = data1$EQ-data1$RF/12
head(EQ_RF)
fit.EQ <- lm(EQ_RF~ market + size + book + market.1 + size.1 + book.1, data=data1)
summary(fit.EQ)

###########################################################################
#Writes the regression coefficients in a csv file 
###########################################################################
## reg_model is the regression model, fname is the name of the csv file you want 
regr_tab <- function(reg_model, fname){
  
  # coefficients in dataframe
  regr_tab <- data.frame(summary(reg_model)$coefficients)
  
  # grab the coefficients
  colnames(regr_tab) <- colnames(summary(reg_model)$coefficients)
  # get the p-vals 
  regr_tab[ ,4] <- ifelse(regr_tab[ ,4] < .001, "< 0.001", 
                          ifelse(regr_tab[ ,4] < .01, "< 0.01", 
                                 round(regr_tab[ ,4], 3)))
  
  # format the table
  summary = format(regr_tab, autoformat = 1)
  
  # write it as a csv file 
  write.csv(summary, paste(fname,"_model_coeff.csv", sep=''))
}

regr_tab(fit.ret,"D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/fit_ret")
regr_tab(fit.es,"D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/fit_es")
regr_tab(fit.es.spread,"D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/fit_es_spread")
regr_tab(fit.EQ,"D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/fit_EQ")

#*******************************************************************

names(quantiles.tw$one.month$spread)
names(quantiles.tw$last.e$spread)
names(quantiles.tw$last.e_s$spread)
#將要分析的資料合併;
ret.risk<-merge.xts(models.tw[[1]][3]$ret,
                    models.tw[[2]][3]$ret,
                    quantiles.tw$one.month$spread[3]$"ret",
                    quantiles.tw$last.e$spread[3]$"ret",
                    quantiles.tw$last.e_s$spread[3]$"ret",
                    quantiles.tw$one.month[[1]][3]$ret,
                    quantiles.tw$last.e[[1]][3]$ret,
                    quantiles.tw$last.e_s[[1]][3]$ret,
                    quantiles.tw$one.month[[10]][3]$ret,
                    quantiles.tw$last.e[[10]][3]$ret,
                    quantiles.tw$last.e_s[[10]][3]$ret)
names(ret.risk)<-c("equal.weight","TWSE","spread.ret","e.spread","es.spread",
                   "Q1.ret","Q1.e","Q1.es","Q10.ret","Q10.e","Q10.es")

equity.risk<-merge.xts(models.tw[[1]][6]$equity,
                       models.tw[[2]][6]$equity,
                       quantiles.tw$one.month$spread[6]$"equity",
                       quantiles.tw$last.e$spread[6]$"equity",
                       quantiles.tw$last.e_s$spread[6]$"equity",
                       quantiles.tw$one.month[[1]][6]$equity,
                       quantiles.tw$last.e[[1]][6]$equity,
                       quantiles.tw$last.e_s[[1]][6]$equity,
                       quantiles.tw$one.month[[10]][6]$equity,
                       quantiles.tw$last.e[[10]][6]$equity,
                       quantiles.tw$last.e_s[[10]][6]$equity)
names(equity.risk)<-c("equal.weight","TWSE","spread.ret","e.spread","es.spread",
                      "Q1.ret","Q1.e","Q1.es","Q10.ret","Q10.e","Q10.es")
ret.risk<-ret.risk["199501/201112"]
equity.risk<-equity.risk["199501/201112"]
head(ret.risk)
head(equity.risk)

#***************************
# VaR
#***************************
performance.df<-data.frame()
var.df<-VaR(ret.risk, p=.95, method="modified")
#performance.df

#************************
# Expected shortfall
#************************
eshortfall<-ES(ret.risk, p=.95, method="modified")
performance.df<-as.data.frame(rbind(var.df, eshortfall))

#as.vector(test)
#*********************
# Sharpe Ratio
#*********************
RF.tw<-data.fa.tw$factors$"RF"
SR<-SharpeRatio.annualized(ret.risk, Rf = 0, scale=12, geometric=FALSE)
SR
SR1<-SharpeRatio(ret.risk, Rf = 0, p = 0.95, FUN = "StdDev")
SR1
performance.df<-rbind(performance.df, SR)
performance.df
#******************************************
# Benchmark return: Information ratio
#******************************************
#equal.weight.bn<-models.tw[[1]][3]$ret["199501/201112"]
#twse.bn<-models.tw[[2]][3]$ret["199501/201112"]
equal.weight.bn<-ret.risk[,1]
twse.bn<-ret.risk[,2]
InformationRatio(ret.risk[,-c(1,2)], twse.bn, scale=12)
IR<-InformationRatio(ret.risk[,-c(1,2)], equal.weight.bn, scale=12)
performance.df[4,c(-1,-2)]<-IR
#******************************************
# maxDrawdown
#*****************************************
mdd<-maxDrawdown(ret.risk, invert=FALSE)
chart.Drawdown(ret.risk,legend.loc="bottomleft")
drawdown.series<-Drawdowns(ret.risk)
write.csv(as.data.frame(drawdown.series), file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/drawdown.csv")
#write.csv(as.data.frame(performance.df), file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/perfromance.csv")
#****************************************
# Sortino ratio
#****************************************
sortino<-SortinoRatio(ret.risk,MR=twse.bn)
sortino



position.score=factors.tw[["one.month"]]
# write.csv(position.score, file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/last.e.csv")
n = ncol(position.score)
n
position.score = coredata(position.score)
head(position.score[,1],37)
quantiles.tm = weights.tm = position.score * NA
start.t=37
prefix = ''
for( t in start.t:nrow(weights.tm) ) {
  factor.tm = as.vector(position.score[t,])
  ranking.tm = ceiling(n.quantiles * rank(factor.tm, na.last = 'keep','first') / count(factor.tm))
  quantiles.tm[t,] = ranking.tm
  weights.tm[t,] = 1/tapply(rep(1,n), ranking.tm, sum)[ranking.tm]
}
quantiles.tm = ifna(quantiles.tm,0)
#write.csv(quantiles.tm, file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/1m_quantiles.csv")
#write.csv(weights.tm, file="D:/亞洲大學碩士班指導論文/雅萍香玫/香玫/1m_weights.csv")

#************************************************************************************
temp = weights.tm * NA
models_tw = list()

i=1
for( i in 1:n.quantiles) {
  temp[] = 0
  temp[quantiles.tm == i] = weights.tm[quantiles.tm == i]
  data.fa.tw$weight[] = NA
  data.fa.tw$weight[index.tw,] = temp
  models_tw[[ paste(prefix,'Q',i,sep='') ]] = bt.run(data.fa.tw, silent = T)
  
  #*****************************************************************
  # Create Report
  #****************************************************************** 					
  plotbt.custom.report.part1(quantiles$one.month$spread,quantiles$last.e$spread,quantiles$last.e_s$spread)
  
  plotbt.strategy.sidebyside(quantiles$one.month$spread,quantiles$last.e$spread,quantiles$last.e_s$spread)
  
  plotbt.custom.report.part1(quantiles$last.e_s)
  
  
  
  
  #################################
  #挑選歷史資料長度有36個月的股票；
  #################################
  subset.36m<-index(data.tw.sample)[1:36]
  subset.36m
  data.tw.sample[subset.36m,1:10]
  rm.index = which(sapply(tickers.tw, function(x) sum(!is.na(data.tw.sample[subset.36m,x])) ==36 ))
  names(rm.index)
  insample.y<-data.tw.sample[subset.36m, rm.index]
  insample.x<-ff.tw.sample[subset.36m,]
  
  #to.monthly(data.tw.sample)
  #head(data.tw.sample)
  #head(data.tw.sample,5)
  #myxts<-xts(rnorm(31),as.Date("2008-12-31")+(0:30)*31)
  #date.str<-as.Date(as.character(data.tw[,1]))
  #format(date.str, "%Y-%m")
  #test<-aggregate(data.tw.xts, format(index(data.tw.xts), "%Y-%m"))
  #test<-data.tw.sample[,1]
  #head(test)
  #subset.36m<-index(test)[1:36]
  #test[subset.36m]
  #test[.indexmon(test)==0] # January for all years (note zero-based indexing!)
  #format(as.Date("2000-01-31")+1:36)
  #as.POSIXct(format(as.Date("2000-01-01")+1:10))
  
  #########################
  # 計算雅萍Sharpe ratio
  ########################
  # bnh = buy and hold portfolio
  #portfolio.performance.df<-data.frame()
  bnh.tw=read.csv("D:/亞洲大學碩士班指導論文/雅萍香玫/雅萍/buy_and_hold.csv", header=TRUE)
  rownames(bnh.tw)=bnh.tw[,1]
  bnh.xts<-as.xts(bnh.tw[,-1])
  bnh.sr<-SharpeRatio(bnh.xts, Rf = 0, p = 0.95, FUN = "StdDev")
  bnh.sr
  # Annualized Sharpe ratio
  bnh.sr1<-SharpeRatio.annualized(bnh.xts, Rf = 0, scale=360, geometric=FALSE)
  bnh.sr1  
  # 10 day moving average  
  d10.ma=read.csv("D:/亞洲大學碩士班指導論文/雅萍香玫/雅萍/10d_ma.csv", header=TRUE)
  rownames(d10.ma)=d10.ma[,1]
  d10.ma.xts<-as.xts(d10.ma[,-1])
  d10.ma.sr<-SharpeRatio(d10.ma.xts, Rf = 0, p = 0.95, FUN = "StdDev")
  d10.ma.sr  
  
  # Annualized Sharpe ratio
  d10.ma.sr1<-SharpeRatio.annualized(d10.ma.xts, Rf = 0, scale=360, geometric=FALSE)
  d10.ma.sr1  
  performance.port<-rbind(bnh.sr, bnh.sr1,d10.ma.sr, d10.ma.sr1) 
  write.csv(performance.port, file="D:/亞洲大學碩士班指導論文/雅萍香玫/雅萍/sharpe.csv")