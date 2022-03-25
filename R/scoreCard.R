#' Title scoreCard
#'
#' Model scores for each bin interval.
#'
#' @param bins Variable binning, which can be a list or a data frame.
#' @param logitModel Results of the base glm logistic regression model
#' @param points0 Target points, default 600.
#' @param odds Target odds, default 1/19. Odds = p/(1-p).
#' @param pdo Points to Double the Odds, default 50.
#' @param scipen Fractions with decimal places.
#'
#' @return Scorecard conversion scores for all observations.
#'
#' @import data.table
#'
#' @export
#'
#' @examples
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
#'
scoreCard <- function(bins,logitModel,points0=600, odds=1/19, pdo=50 ,scipen = 0){

  scVar <- function(varOdds,points0 = points0,odds = odds,pdo = pdo){

    # """
    #   varOdds: 类别概率
    #   points0:在这个odds下的分数
    #   odds：设定的坏好比
    #   pdo: 好坏翻倍比
    #
    # """

    B <- pdo/log(2)

    A <- points0 + B*log(odds)

    y <- varOdds

    score = A-B*y

    return (
      list(
        score = score
        , A = A
        , B = B
      )
    )
  }

  standardPoints <- scVar(
    varOdds = 0
    , points0 = points0
    , odds = odds
    , pdo = pdo
  )

  modelCoef <- data.table(
    vars = names(logitModel$coefficients)
    , coefValue = logitModel$coefficients
  )

  modelBins <- bins[
    modelCoef[vars != '(Intercept)',vars]
  ]

  modelBinsCoef <- lapply(
    modelBins
    , function(x) x[
      modelCoef
      , on = .(vars)
      , nomatch = NULL
    ]
  )

  interceptScore <- list(
    interceptScore =
      data.table(
        vars = 'intercept'
        , varScore = round(
          scVar(
            modelCoef[vars == '(Intercept)',coefValue]
            , points0 = points0
            , odds = odds
            , pdo = pdo
          )$score
          , scipen
        )
      )
  )

  modelBinsCoefScore <- lapply(
    modelBinsCoef
    , function(x) x[
      , `:=`(
        varScore = round(
          woe * coefValue * (-standardPoints$B)
          , scipen
        )
      )
    ]
  )

  modelBinsCoefScoreRes <- append(
    interceptScore
    , modelBinsCoefScore
  )

  return(modelBinsCoefScoreRes)

}
