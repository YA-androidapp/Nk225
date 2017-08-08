# Copyright (c) 2017 YA-androidapp(https://github.com/YA-androidapp) All rights reserved.

# install.packages('RCurl')
# install.packages('XML')
# install.packages('forecast')
# install.packages('xts')
# install.packages('lubridate')
library(RCurl) # HTML取得
library(XML) # HTMLから表をパース
library(tcltk) # プログレスバー
library(forecast) # 時系列の作図と予測
library(xts) # 時系列
library(lubridate) # 時系列ボックスプロット
library(ggplot2)



# 日経平均株価ヒストリカルデータを取得する
# 1949/5以降のデータを取得可能
# tableurl <- 'https://indexes.nikkei.co.jp/nkave/statistics/dataload?list=daily&year=2017&month=8'
tableurl.1 <- 'https://indexes.nikkei.co.jp/nkave/statistics/dataload?list=daily&year='
tableurl.2 <- '&month='

today <- Sys.Date()
today.year.n <- as.numeric(format(today, '%Y'))
today.year <- as.character(today.year.n)
today.month <- as.character(as.numeric(format(today, '%m')))



# 年月ごとに取得処理を行う
since <- 1949
progressBar <- txtProgressBar(min = 0, max = 12*(today.year.n-since), style = 3)

result <- NULL
for (y in today.year:since) {
  for (m in 12:1) {
    # cat( formatC(y, width=4, flag="0"), '/', formatC(m, width=2, flag="0"))
    setTxtProgressBar(progressBar, (12*(today.year.n-y) + (12 - m)))

    # 将来分のデータは存在しないためスキップ
    diff <- difftime(as.Date(paste(y,m,'1', sep='-')),today, units = 'auto')
    if(diff>0){
      next;
    }

    tableurl <- paste(tableurl.1, y, tableurl.2, m, sep='')

    contents <- getURL(tableurl)
    doc <- htmlParse(contents, encoding = 'UTF-8')
    tabs <- readHTMLTable(doc, stringsAsFactors = F)
    table <- as.data.frame(tabs$`NULL`)
    table$始値 <- as.numeric(sub(",","",table$始値))
    table$高値 <- as.numeric(sub(",","",table$高値))
    table$安値 <- as.numeric(sub(",","",table$安値))
    table$終値 <- as.numeric(sub(",","",table$終値))
    table$日付 <- as.Date(gsub('[.]', '-', table$日付))
    rownames(table) <- gsub('[.]', '-', table$日付)

    if(is.null(result)){
      result <- table
    } else {
      result <- rbind(result, table)
    }

    Sys.sleep(0.5)
  }
}

sort <- order(result$日付)
nk225 <- result[sort,]
head(nk225) # 確認
write.csv(nk225, "./nk225.csv",row.names=T) # CSV出力

# 時系列分析のためにxtsパッケージへ変換する
nk225.xts <- as.xts(read.zoo(nk225))


# 移動平均
tail(rollapply(nk225.xts, 5, mean)) # 5日移動平均

# 月別集計
endpoints <- endpoints(nk225.xts, on='months')
period.apply(nk225.xts, endpoints, function(x) sapply(x, max))
period.apply(nk225.xts, endpoints, function(x) sapply(x, mean))
period.apply(nk225.xts, endpoints, function(x) sapply(x, median))
period.apply(nk225.xts, endpoints, function(x) sapply(x, min))

# 最終日
aggregate(nk225.xts, as.yearmon, last)

# 期別集計
endpoints.6 <- endpoints(nk225.xts, on='months', k=6)
period.apply(nk225.xts, endpoints.6, function(x) sapply(x, max))
period.apply(nk225.xts, endpoints.6, function(x) sapply(x, mean))
period.apply(nk225.xts, endpoints.6, function(x) sapply(x, median))
period.apply(nk225.xts, endpoints.6, function(x) sapply(x, min))



# 欠損値補間

# 最近接値
na.locf(nk225.xts)                   # 前の行で補間
na.locf(nk225.xts, fromLast=T)       # 後の行で補間
t(na.locf(t(nk225.xts)))             # 前の列で補間
t(na.locf(t(nk225.xts), fromLast=T)) # 後の列で補間

# 平均値
na.approx(nk225.xts)                 # 行方向の前後の平均値で補間
t(na.approx(t(nk225.xts)))           # 列方向の前後の平均値で補間

# 行列方向の前後の平均値で補間
( na.approx(nk225.xts) + t(na.approx(t(nk225.xts))) ) / 2

# スプライン補間
na.spline(nk225.xts)

# カルマンフィルタ
na.StructTS(nk225.xts)



# 作図して傾向を捉える
plot(nk225.xts[,4])

nk225.xts.ip <- na.locf(na.approx(nk225.xts), fromLast=T)

acf(nk225.xts.ip[,4]) # 自己相関プロット

plot(decompose(nk225.xts[,4])) # 成分別プロット



# ボックスプロット
nk225.xts.date <- index(nk225.xts)
nk225.df <- nk225 # 複製
nk225.df$close <- nk225.df[,4]
nk225.df$year <- factor(year(nk225.xts.date))
nk225.df$month <- factor(month(nk225.xts.date))
nk225.df$wday <- factor(weekdays(nk225.xts.date, abbreviate=T), levels=c('日', '月', '火', '水', '木', '金', '土'))
p <- ggplot(data=nk225.df, aes(x=wday, y=close)) + facet_grid(month ~ year) + geom_boxplot() + xlab('Weekday') + ylab('Nk225')
print(p)



# 訓練用データと評価用データに分割
nk225.xts.train <- nk225.xts['::2017-03-31']
nk225.xts.eval  <- nk225.xts['2017-04-01::']
