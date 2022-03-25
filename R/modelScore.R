#' Title modelScore
#'
#' Probability transition score for observations.
#'
#' @param prob The prediction result of the predict function
#' @param points0 Target points, default 600.
#' @param odds Target odds, default 1/19. Odds = p/(1-p).
#' @param pdo Points to Double the Odds, default 50.
#' @param scipen Fractions with decimal places.
#'
#' @return Probability transition score for observations.
#'
#' @import data.table
#'
#' @export
#'
#' @examples
#'
#' library(data.table)
#'
#' data("germancredit", package="scorecard")
#'
#' data <- germancredit
#'
#' ##  Convert Y labels
#'
#' data <- transformResponse(data,y = 'creditability')
#'
#' dataList <- splitDt(data = data,ratio = c(0.7,0.3),seed = 520)
#'
#' labelList <- lapply(dataList, function(x) x$y)
#'
#' ##  bins
#'
#' binsChi <- binningsChimerge(
#'   data = data
#'   , y = 'y'
#' )
#'
#' binsChiDt <- rbindlist(binsChi)
#'
#' ## woe
#'
#' woeData <- woeTrans(data,y = 'y',bins = binsChi)
#'
#' woeDataList <- lapply(dataList,function(x) woeTrans(x,y = 'y',bins = binsChi))
#'
#' ## glm
#'
#' fit = glm(
#'   y ~ .
#'   , family = binomial()
#'   , data = woeDataList$train
#' )
#'
#' summary(fit)
#'
#' ## stepwise by aic
#'
#' fitStepAic <- step(
#'   fit
#'   , direction = 'both'
#'   , k = 2
#'   , trace = FALSE
#' )
#'
#' summary(fitStepAic)
#'
#' # ## stepwise by bic
#' #
#' # fitStepBic <- step(
#' #   fit
#' #   , direction = 'both'
#' #   , k = log(nrow(woeDataList$train)))
#' #   , trace = FALSE
#' # )
#' #
#' # summary(fitStepBic)
#'
#' ## pred
#'
#' predList <- lapply(woeDataList, function(x) predict(fitStepAic, x, type='response'))
#'
#' ## performance
#'
#' modelPerformance <- modelEffect(
#'   pred = predList
#'   , label = labelList
#'   , ord = "desc"
#' )
#'
#' modelPerformance
#'
#' ##  score and scorecard
#'
#' ### Only output the final model score
#'
#' modelScoreRes <- lapply(predList, function(x) modelScore(x))
#'
#' ### scorecard
#'
#' sc <- scoreCard(
#'   bins = binsChi
#'   , logitModel = fitStepAic
#' )
#'
#' scDt <- rbindlist(sc, fill = T)
#'
#' riskForm(
#'  pred = modelScoreRes
#'  , label = labelList
#'  , ord = 'asc'
#' )
#'
modelScore <- function(prob,points0 = 600,odds = 1/19,pdo = 50,scipen = 0){

  # """
  #   prob: 类别概率
  #   points0:在这个odds下的分数
  #   odds：设定的坏好比
  #   pdo: 好坏翻倍比
  #   scipen: 分数保留小数位数
  #
  # """

  B <- pdo/log(2)

  A <- points0 + B*log(odds)

  y <- log(prob/(1-prob))

  score = round(A-B*y,scipen)

  return (
    score
  )

}
