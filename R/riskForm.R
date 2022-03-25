#' Title riskForm
#'
#' Risk table for each bin interval
#'
#' @param pred The prediction result of the predict function
#' @param label Y label
#' @param method 'freq' 'width' 'chimerge'
#' @param nbins Number of bins
#' @param ord Fractional or probabilistic monotonic direction,'desc' or 'asc'.
#' @param labelSign Response value label name.Default is c('bad','1','no','response').
#' @param specify  Whether to specify special binning, if specified please enter a numeric vector.
#' @param showCols Column of results,The full column name is c('vars', 'bins', 'count', 'countRate', 'nmlCount', 'nmlCountCum', 'nmlCountCumRate', 'rpsCount', 'rpsCountCum', 'rpsCountCumRate', 'rpsRate', 'woe', 'iv', 'totalIv', 'totalIvAdj', 'binsLift', 'shape', 'splits', 'isSpecial').
#'
#' @return Risk table for each bin interval
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
riskForm <- function(
  pred
  , label
  , method = 'freq' # 'freq' 'width' 'chimerge'
  , nbins = 10
  , ord = 'desc' # desc or asc
  , labelSign = c('bad','1','no','response')
  , specify = NULL
  , showCols = c(
    'predCut'
    , 'count'
    , 'countRate'
    , 'countRateCum'
    , 'nmlCount'
    , 'nmlCountCum'
    , 'nmlCountCumRate'
    , 'rpsCount'
    , 'rpsCountCum'
    , 'rpsCountCumRate'
    , 'rpsRate'
    , 'woe'
    , 'iv'
    , 'totalIv'
    , 'binsLift'
    , 'accumLift'
  )
){

  if (is.list(pred)){

    predLen <- length(names(pred))

    labelLen <- length(names(label))

    if (predLen != labelLen){

      stop('The result is inconsistent with the number of label groups.')

    }

    modelData <- list()

    for (i in names(pred)){

      modelData[i] <- list(
        data.table(
          pred = pred[i][[1]], label = label[i][[1]] , type = i
        )
      )

    }

    modelData <- lapply(modelData, function(x) x[
      , y := fifelse(label %in% labelSign,'bad','good')
    ][
      , yValue := fifelse(y == 'bad',1,0)
    ])

    if (ord == 'desc'){

      modelData <- lapply(modelData, function(x) x[order(-pred)])

    } else if (ord == 'asc'){

      modelData <- lapply(modelData, function(x) x[order(pred)])

    } else{

      stop('This parameter only supports desc or asc.')

    }

    modelDataDt <- rbindlist(modelData)

    if (is.null(specify)){

      if (method == 'freq'){

        # print(modelData)

        modelDataRes <-
          lapply(
            modelData
            , function(x) x[
              , `:=`(
                predCut = cut(
                  pred
                  , breaks = binsEqual(data = modelDataDt,x = 'pred',nbins = nbins)
                  , right = F
                  , dig.lab = 10
                )
              )
            ]
          )

        modelDataResDs <- lapply(
          modelDataRes
          , function(x) riskFormFunc(x,ord = ord)
        )

        modelDataResDs <- lapply(
          modelDataResDs,
          function(x) x[
            , showCols
            , with = F
          ]
        )

      } else if (method == 'width'){

        modelDataRes <-
          lapply(
            modelData
            , function(x) x[
              , `:=`(
                predCut = cut(
                  pred
                  , breaks = binsWidth(data = modelDataDt,x = 'pred',nbins = nbins)
                  , right = F
                  , dig.lab = 10
                )
              )
            ]
          )

        modelDataResDs <- lapply(
          modelDataRes
          , function(x) riskFormFunc(x, ord = ord)
        )

        modelDataResDs <- lapply(
          modelDataResDs,
          function(x) x[
            , showCols
            , with = F
          ]
        )

      } else if (method == 'chimerge'){

        modelDataRes <-
          lapply(
            modelData
            , function(x) x[
              , `:=`(
                predCut = cut(
                  pred
                  , breaks = chiMergeNum(data = modelDataDt,y = 'y',x = 'pred')
                  , right = F
                  , dig.lab = 10
                )
              )
            ]
          )

        modelDataResDs <- lapply(
          modelDataRes
          , function(x) riskFormFunc(x, ord = ord)
        )

        modelDataResDs <- lapply(
          modelDataResDs,
          function(x) x[
            , showCols
            , with = F
          ]
        )

      } else {

        stop('Only freq, width, chimerge are supported.')

      }

    } else {

      modelDataRes <-
        lapply(
          modelData
          , function(x) x[
            , `:=`(
              predCut = cut(
                pred
                , breaks = sort(specify)
                , right = F
                , dig.lab = 10
              )
            )
          ]
        )

      modelDataResDs <- lapply(
        modelDataRes
        , function(x) riskFormFunc(x,ord = ord)
      )

      modelDataResDs <- lapply(
        modelDataResDs,
        function(x) x[
          , showCols
          , with = F
        ]
      )


    }




  } else {

    modelData <- data.table(
      pred = pred
      , label = label
    )

    modelData[
      , y := fifelse(label %in% labelSign,'bad','good')
    ][
      , yValue := fifelse(y == 'bad',1,0)
    ]

    if (ord == 'desc'){

      modelData <- modelData[order(-pred)]

    } else if (ord == 'asc'){

      modelData <- modelData[order(pred)]

    } else{

      stop('This parameter only supports desc or asc.')

    }

    if (is.null(specify)){

      if (method == 'freq'){

        modelDataRes <- copy(
          modelData[
            , `:=`(
              predCut = cut(
                pred
                , breaks = binsEqual(data = modelData,x = 'pred',nbins = nbins)
                , right = F
                , dig.lab = 10
              )
            )
          ]
        )

        modelDataResDs <- riskFormFunc(modelDataRes, ord = ord)

        modelDataResDs <-
          modelDataResDs[
            , showCols
            , with = F
          ]

      } else if (method == 'width'){

        modelDataRes <- copy(
          modelData[
            , `:=`(
              predCut = cut(
                pred
                , breaks = binsWidth(data = modelData,x = 'pred',nbins = nbins)
                , right = F
                , dig.lab = 10
              )
            )
          ]
        )

        modelDataResDs <- riskFormFunc(modelDataRes, ord = ord)

        modelDataResDs <-
          modelDataResDs[
            , showCols
            , with = F
          ]

      } else if (method == 'chimerge'){

        modelDataRes <- copy(
          modelData[
            , `:=`(
              predCut = cut(
                pred
                , breaks = chiMergeNum(data = modelData,y = 'y',x = 'pred')
                , right = F
                , dig.lab = 10
              )
            )
          ]
        )

        modelDataResDs <- riskFormFunc(modelDataRes, ord = ord)

        modelDataResDs <-
          modelDataResDs[
            , showCols
            , with = F
          ]

      } else {

        stop('Only freq, width, chimerge are supported.')

      }

    }else {

      modelDataRes <- copy(
        modelData[
          , `:=`(
            predCut = cut(
              pred
              , breaks = sort(specify)
              , right = F
              , dig.lab = 10
            )
          )
        ]
      )

      modelDataResDs <- riskFormFunc(modelDataRes, ord = ord)

      modelDataResDs <-
        modelDataResDs[
          , showCols
          , with = F
        ]

    }


  }

  return(modelDataResDs)

}
