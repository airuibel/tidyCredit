# riskTools

##  自述文件

1. 纯R语言风控模型包，整体风格受谢博士scorecard启发(https://github.com/ShichenXie/scorecard)
2. 支持的最优分箱方法：卡方最优分箱、cart决策树最优分箱、BestKs最优分箱
3. 其他分箱方法：聚类分箱、等频分箱、等距分箱、遍历分箱
4. 效果上目前测试不亚于市面上任何包(binningsChimerge YYDS)，且支持输出单调趋势、自动调整为单调或正U倒U、自动调整单调

##

## downloan

1. devtools::install_github('https://github.com/XianglinZhang-risker/tidyCredit')

##
## example 1

### load data

library(data.table)

data("germancredit", package="scorecard")

data <- germancredit

###  Convert Y labels

data <- transformResponse(data,y = 'creditability')

dataList <- splitDt(data = data,ratio = c(0.7,0.3),seed = 520)

labelList <- lapply(dataList, function(x) x$y)

###  bins

binsChi <- binningsChimerge(
  data = data
  , y = 'y'
)

binsChiDt <- rbindlist(binsChi)

### woe

woeData <- woeTrans(data,y = 'y',bins = binsChi)

woeDataList <- lapply(dataList,function(x) woeTrans(x,y = 'y',bins = binsChi))

### glm

fit = glm(
  y ~ .
  , family = binomial()
  , data = woeDataList$train
)

summary(fit)

### stepwise by aic

fitStepAic <- step(
  fit
  , direction = 'both'
  , k = 2
  , trace = FALSE
)

summary(fitStepAic)

### pred

predList <- lapply(woeDataList, function(x) predict(fitStepAic, x, type='response'))

### performance

modelPerformance <- modelEffect(
  pred = predList
  , label = labelList
  , ord = "desc"
)

modelPerformance

###  score and scorecard

#### Only output the final model score

modelScoreRes <- lapply(predList, function(x) modelScore(x))

#### scorecard

sc <- scoreCard(
  bins = binsChi
  , logitModel = fitStepAic
)

scDt <- rbindlist(sc, fill = T)

riskForm(
 pred = modelScoreRes
 , label = labelList
 , ord = 'asc'
)

##  所有函数介绍
#### binningsBestKS：BestKs最优分箱(数据越离散效果越好，与卡方分箱不相上下,建议设置自动调整为1)
#### binningsCartReg：cart回归树最优分箱
#### binningsChimerge：卡方最优分箱(表现最好，但时间稍长)
#### binningsCluster：聚类最优分箱
#### binningsTraverse:遍历分箱，适用于特定场景，变量变量所有值
#### binningsEqual：等频分箱
#### binningsWidth：等距分箱
#### binsBestKS：BestKs最优分箱-只输出分割点
#### binsCartReg：cart回归树最优分箱-只输出分割点
#### binsCluster：聚类最优分箱-只输出分割点
#### binsEqual：等频分箱-只输出分割点
#### binsWidth：等距分箱-只输出分割点
#### describeData：快速的描述性统计
#### modelEffect：输出模型效果
#### modelScore：转换逻辑回归预测概率为得分
#### riskForm：分箱输出结果效果
#### scoreCard：输出入模变量及其得分
#### splitDt：切分数据集，可选训练测试或K折交叉
#### transformResponse：帮助转换Y标签
#### woePlot：分箱可视化-echarts4r
#### woePlotGG：分箱可视化-ggplot2
#### woeTrans:原始数据转换为woe








