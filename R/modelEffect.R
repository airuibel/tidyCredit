#' Title modelEffect
#'
#' Evaluate model performance.
#'
#' @param pred The prediction result of the predict function
#' @param label Y label
#' @param ord Fractional or probabilistic monotonic direction,'desc' or 'asc'.
#' @param labelSign Response value label name.Default is c('bad','1','no','response').
#' @param show c('ks','roc','lift','density')
#'
#' @return ks','roc','lift','density' and some other metrics
#'
#' @import data.table echarts4r
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
modelEffect <- function(
  pred
  , label
  , ord = 'desc' # desc or asc
  , labelSign = c('bad','1','no','response')
  , show = NULL # c('ks','roc','lift','density') # 仅控制大图
){

  ## mse
  mse = function(data) {

    return(
      data[
        , mean((yValue - pred)^2)
      ]
    )

  }

  # mse(modelData)

  ## rmse
  rmse = function(data) {

    return(sqrt(mse(data)))

  }

  # rmse(modelData)

  ## logloss
  logloss = function(data) {
    setDT(data)
    return(
      data[
        , logloss :=
          yValue * log(pred) + (1-yValue) * log(1-pred)
      ][
        , mean(-logloss)
      ]
    )
  }

  # logloss(modelData)

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

    ##  AUC 计算

    if (ord == 'desc'){

      modelDataLink <- lapply(
        modelData
        , function(x) outer(x[yValue == 1,pred], x[yValue == 0,pred], "-")
      )

      aucValue <- lapply(
        modelDataLink
        , function(x) round(mean((x>0) + .5*(x==0)),4)
      )

    } else if (ord == 'asc'){

      modelDataLink <- lapply(
        modelData
        , function(x) outer(x[yValue == 1,pred], x[yValue == 0,pred], "-")
      )

      aucValue <- lapply(
        modelDataLink
        , function(x) 1 - round(mean((x>0) + .5*(x==0)),4)
      )

    }

    ##  KS LIFT 计算

    modelData <- lapply(
      modelData
      , function(x) x[
        , `:=`(
          count = .I
          , gcum = cumsum(ifelse(yValue == 0,1,0))
          , bcum = cumsum(ifelse(yValue == 1,1,0))
        )
      ][
        , `:=`(
          countRate = count/max(count)
          , gcumRate = round(gcum / max(gcum,na.rm = T),4)
          , bcumRate = round(bcum / max(bcum,na.rm = T),4)
        )
      ][
        , `:=`(
          ks = abs(gcumRate - bcumRate)
          , lift = round(bcumRate / countRate,4)
        )
      ]
    )

    modelDataDt <- rbindlist(modelData)

    ksCha <- list(
      paste0(
        unique(modelDataDt[,type])[1]
        , "Ks Max Value: "
        , max(modelDataDt[type == unique(modelDataDt[,type])[1],ks],na.rm = T)
      )
    )

    for (i in 2:length(unique(modelDataDt[,type]))){

      ksCha <- append(
        ksCha
        , paste0(
          unique(modelDataDt[,type])[i]
          , "Ks Max Value: "
          , max(modelDataDt[type == unique(modelDataDt[,type])[i],ks],na.rm = T)
        )
      )
    }

    rocCha <- list(
      paste0(
        names(aucValue)[1]
        , "Auc Value: "
        , aucValue[[1]]
      )
    )

    for (i in 2:length(names(aucValue))){

      rocCha <- append(
        rocCha
        , paste0(
          names(aucValue)[i]
          , "Auc Value: "
          , aucValue[[i]]
        )
      )
    }

    liftCha <- list(
      paste0(
        unique(modelDataDt[,type])[1]
        , "Lift Max Value: "
        , max(modelDataDt[type == unique(modelDataDt[,type])[1],lift],na.rm = T)
      )
    )

    for (i in 2:length(unique(modelDataDt[,type]))){

      liftCha <- append(
        liftCha
        , paste0(
          unique(modelDataDt[,type])[i]
          , "Lift Max Value: "
          , max(modelDataDt[type == unique(modelDataDt[,type])[i],lift],na.rm = T)
        )
      )
    }

    ks <- modelDataDt |>
      group_by(type) |>
      e_charts(countRate) |>
      e_title('KS'
              ,ksCha) |>
      e_line(gcumRate,symbol='none', smooth = TRUE) |>
      e_line(bcumRate,symbol='none', smooth = TRUE) |>
      e_line(ks,symbol='none', smooth = TRUE) |>
      e_theme("westeros") |>
      e_datazoom(x_index = c(0, 1)) |>
      e_tooltip(trigger = "axis") |>
      e_legend(show = T,position = 'left') |>
      e_mark_point(serie = 'ks',data = list(name = "Max",type = "max"))

    ksmini <- modelDataDt |>
      group_by(type) |>
      e_charts(
        countRate
        # , height = 250
        # , width = 250
      ) |>
      e_dims(height = "auto", width = "auto")|>
      e_title('KS'
              ,ksCha) |>
      e_line(gcumRate,symbol='none', smooth = TRUE) |>
      e_line(bcumRate,symbol='none', smooth = TRUE) |>
      e_line(ks,symbol='none', smooth = TRUE) |>
      e_theme("westeros") |>
      e_datazoom(x_index = c(0, 1)) |>
      e_tooltip(trigger = "axis") |>
      e_legend(show = T) |>
      e_mark_point(serie = 'ks',data = list(name = "Max",type = "max"))

    roc <- modelDataDt |>
      group_by(type) |>
      e_charts(gcumRate) |>
      e_title('ROC', rocCha) |>
      e_line(bcumRate, symbol='none', smooth = TRUE) |> # add a line
      # e_area(bcumRate, symbol='none', smooth = TRUE) |>
      e_theme("westeros") |>
      e_datazoom(x_index = c(0, 1)) |>
      e_legend(show = T) |>
      e_tooltip(trigger = "axis")

    rocmini <- modelDataDt |>
      group_by(type) |>
      e_charts(
        gcumRate
        # , height = 250
        # , width = 250
      ) |>
      e_dims(height = "auto", width = "auto")|>
      e_title('ROC', rocCha) |>
      e_line(bcumRate, symbol='none', smooth = TRUE) |> # add a line
      # e_area(bcumRate, symbol='none', smooth = TRUE) |>
      e_theme("westeros") |>
      e_datazoom(x_index = c(0, 1)) |>
      e_legend(show = T) |>
      e_tooltip(trigger = "axis")

    lift <- modelDataDt |>
      group_by(type) |>
      e_charts(countRate) |>
      e_title('LIFT',liftCha) |>
      e_line(lift, symbol='none', smooth = TRUE) |> # add a line
      # e_area(bcumRate, symbol='none', smooth = TRUE) |>
      e_theme("westeros") |>
      e_datazoom(x_index = c(0, 1)) |>
      e_legend(show = T) |>
      e_tooltip(trigger = "axis")

    liftmini <- modelDataDt |>
      group_by(type) |>
      e_charts(
        countRate
        # , height = 250
        # , width = 250
      ) |>
      e_dims(height = "auto", width = "auto")|>
      e_title('LIFT',liftCha) |>
      e_line(lift, symbol='none', smooth = TRUE) |> # add a line
      # e_area(bcumRate, symbol='none', smooth = TRUE) |>
      e_theme("westeros") |>
      e_datazoom(x_index = c(0, 1)) |>
      e_legend(show = T) |>
      e_tooltip(trigger = "axis")

    density <- modelDataDt |>
      group_by(type,y) |>
      e_charts(countRate) |>
      e_title(
        'Density'
        # , paste0("Lift Max Value: ",max(modelData$lift,na.rm = T))
      ) |>
      e_density(pred) |>
      e_theme("westeros") |>
      e_datazoom() |>
      e_legend(show = T) |>
      e_tooltip(trigger = "axis")

    densitymini <- modelDataDt |>
      group_by(type,y) |>
      e_charts(
        countRate
        # , height = 250
        # , width = 250
      ) |>
      e_dims(height = "auto", width = "auto")|>
      e_title(
        'Density'
        # , paste0("Lift Max Value: ",max(modelData$lift,na.rm = T))
      ) |>
      e_density(pred) |>
      e_theme("westeros") |>
      e_datazoom() |>
      e_legend(show = T) |>
      e_tooltip(trigger = "axis")

    # print(modelData)

    modelDataImp <- list(
      ksInfo = data.table(
        type = 'ks'
        , kind = lapply(modelData, function(x) unique(x[, type]))
        , value = lapply(modelData, function(x) max(x[is.na(ks) == F,ks]))
        , CumulativProportion = lapply(
          modelData, function(x) x[ks == max(ks,na.rm = T),countRate]
        )
      )
      , rocInfo = data.table(
        type = 'roc'
        , kind = lapply(modelData, function(x) unique(x[, type]))
        , value = lapply(aucValue, function(x) x)
      )
      , liftInfo = data.table(
        type = 'lift'
        , kind = lapply(modelData, function(x) unique(x[, type]))
        , value = lapply(modelData, function(x) max(x[is.na(lift) == F,lift]))
      )
      , giniInfo = data.table(
        type = 'gini'
        , kind = lapply(modelData, function(x) unique(x[, type]))
        , value = lapply(aucValue, function(x) 2*x-1)
      )
      , mse = data.table(
        type = 'mse'
        , kind = lapply(modelData, function(x) unique(x[, type]))
        , value = lapply(modelData, function(x) mse(x))
      )
      , rmse = data.table(
        type = 'rmse'
        , kind = lapply(modelData, function(x) unique(x[, type]))
        , value = lapply(modelData, function(x) rmse(x))
      )
      , logloss = data.table(
        type = 'logloss'
        , kind = lapply(modelData, function(x) unique(x[, type]))
        , value = lapply(modelData, function(x) logloss(x))
      )
    )

    if (is.null(show)){

    }else{

      for (i in show){

        print(get(i))

      }

    }

    # e_arrange(ks,roc,lift,density)

    print(e_arrange(ksmini,rocmini,liftmini,densitymini,rows = 2,cols = 2))

  } else{
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

    ##  AUC 计算

    if (ord == 'desc'){

      modelDataLink <- outer(modelData[yValue == 1,pred], modelData[yValue == 0,pred], "-")

      aucValue <- round(mean((modelDataLink>0) + .5*(modelDataLink==0)),4)

    } else if (ord == 'asc'){

      modelDataLink <- outer(modelData[yValue == 1,pred], modelData[yValue == 0,pred], "-")

      aucValue <- 1 - round(mean((modelDataLink>0) + .5*(modelDataLink==0)),4)

    }

    ##  KS LIFT 计算

    modelData[
      , `:=`(
        count = .I
        , gcum = cumsum(ifelse(yValue == 0,1,0))
        , bcum = cumsum(ifelse(yValue == 1,1,0))
      )
    ][
      , `:=`(
        countRate = count/max(count)
        , gcumRate = round(gcum / max(gcum,na.rm = T),4)
        , bcumRate = round(bcum / max(bcum,na.rm = T),4)
      )
    ][
      , `:=`(
        ks = abs(gcumRate - bcumRate)
        , lift = round(bcumRate / countRate,4)
      )
    ]

    ks <- modelData |>
      e_charts(
        countRate
      ) |>
      e_title('KS',paste0("Ks Max Value: ",max(modelData$ks,na.rm = T))) |>
      e_line(gcumRate,symbol='none', smooth = TRUE) |>
      e_line(bcumRate,symbol='none', smooth = TRUE) |>
      e_line(ks,symbol='none', smooth = TRUE) |>
      e_theme("westeros") |>
      e_datazoom(x_index = c(0, 1)) |>
      e_tooltip(trigger = "axis") |>
      e_legend(show = F) |>
      e_mark_point(serie = 'ks',data = list(name = "Max",type = "max"))
    # e_mark_point(data = point)

    ksmini <- modelData |>
      e_charts(
        countRate
        # , height = 250
        # , width = 250
      ) |>
      e_dims(height = "auto", width = "auto")|>
      e_title('KS',paste0("Ks Max Value: ",max(modelData$ks,na.rm = T))) |>
      e_line(gcumRate,symbol='none', smooth = TRUE) |>
      e_line(bcumRate,symbol='none', smooth = TRUE) |>
      e_line(ks,symbol='none', smooth = TRUE) |>
      e_theme("westeros") |>
      e_datazoom(x_index = c(0, 1)) |>
      e_tooltip(trigger = "axis") |>
      e_legend(show = F) |>
      e_mark_point(serie = 'ks',data = list(name = "Max",type = "max"))

    roc <- modelData |>
      # group_by(type) |>
      e_charts(gcumRate) |>
      e_title('ROC',paste0("AUC Value: ",aucValue)) |>
      e_line(bcumRate, symbol='none', smooth = TRUE) |> # add a line
      # e_area(bcumRate, symbol='none', smooth = TRUE) |>
      e_theme("westeros") |>
      e_datazoom(x_index = c(0, 1)) |>
      e_legend(show = F) |>
      e_tooltip(trigger = "axis")

    rocmini <- modelData |>
      # group_by(type) |>
      e_charts(
        gcumRate
        # , height = 250
        # , width = 250
      ) |>
      e_dims(height = "auto", width = "auto")|>
      e_title('ROC',paste0("AUC Value: ",aucValue)) |>
      e_line(bcumRate, symbol='none', smooth = TRUE) |> # add a line
      # e_area(bcumRate, symbol='none', smooth = TRUE) |>
      e_theme("westeros") |>
      e_datazoom(x_index = c(0, 1)) |>
      e_legend(show = F) |>
      e_tooltip(trigger = "axis")

    lift <- modelData |>
      # group_by(type) |>
      e_charts(countRate) |>
      e_title('LIFT',paste0("Lift Max Value: ",max(modelData$lift,na.rm = T))) |>
      e_line(lift, symbol='none', smooth = TRUE) |> # add a line
      # e_area(bcumRate, symbol='none', smooth = TRUE) |>
      e_theme("westeros") |>
      e_datazoom(x_index = c(0, 1)) |>
      e_legend(show = F) |>
      e_tooltip(trigger = "axis")

    liftmini <- modelData |>
      # group_by(type) |>
      e_charts(
        countRate
        # , height = 250
        # , width = 250
      ) |>
      e_dims(height = "auto", width = "auto")|>
      e_title('LIFT',paste0("Lift Max Value: ",max(modelData$lift,na.rm = T))) |>
      e_line(lift, symbol='none', smooth = TRUE) |> # add a line
      # e_area(bcumRate, symbol='none', smooth = TRUE) |>
      e_theme("westeros") |>
      e_datazoom(x_index = c(0, 1)) |>
      e_legend(show = F) |>
      e_tooltip(trigger = "axis")

    density <- modelData |>
      group_by(y) |>
      e_charts(countRate) |>
      e_title(
        'Density'
        # , paste0("Lift Max Value: ",max(modelData$lift,na.rm = T))
      ) |>
      e_density(pred) |>
      e_theme("westeros") |>
      e_datazoom() |>
      e_legend(show = F) |>
      e_tooltip(trigger = "axis")

    densitymini <- modelData |>
      group_by(y) |>
      e_charts(
        countRate
        # , height = 250
        # , width = 250
      ) |>
      e_dims(height = "auto", width = "auto")|>
      e_title('Density') |> # , paste0("Lift Max Value: ",max(modelData$lift,na.rm = T))
      e_density(pred) |>
      e_theme("westeros") |>
      e_datazoom() |>
      e_legend(show = F) |>
      e_tooltip(trigger = "axis")



    modelDataImp <- list(
      ksInfo = data.table(
        type = 'ks'
        , value = max(modelData[is.na(ks) == F,ks])
        , CumulativProportion = modelData[ks == max(ks,na.rm = T),countRate]
      )
      , rocInfo = data.table(
        type = 'roc'
        , value = aucValue
      )
      , liftInfo = data.table(
        type = 'lift'
        , value = max(modelData[is.na(lift) == F,lift])
      )
      , giniInfo = data.table(
        type = 'gini'
        , value = 2*aucValue-1
      )
      , mse = data.table(
        type = 'mse'
        , value = mse(modelData)
      )
      , rmse = data.table(
        type = 'rmse'
        , value = rmse(modelData)
      )
      , logloss = data.table(
        type = 'logloss'
        , value = logloss(modelData)
      )
    )

    if (is.null(show)){

    }else{

      for (i in show){

        print(get(i))

      }

    }

    # e_arrange(ks,roc,lift,density)

    print(e_arrange(ksmini,rocmini,liftmini,densitymini,rows = 2,cols = 2))
  }

  return(modelDataImp)

}
