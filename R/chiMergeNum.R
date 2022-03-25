#' Title
#'
#' @param data
#' @param y
#' @param x
#' @param alpha
#' @param limit
#' @param breaksList
#' @param minBinsNumInitial
#' @param dataLimitCondition
#' @param responseSign
#' @param specialValue
#' @param ifSpecialNeeds
#' @param specialVarValue
#' @param ifSpecialVarNeeds
#' @param binsNumLimit
#' @param autoAdjust
#' @param ifMonotonous
#' @param maxIterationLimit
#'
#' @return
#' @export
#'
#' @examples
chiMergeNum <- function(
  data
  , y
  , x
  , alpha = 0.1
  , limit = 0.05
  , breaksList = NULL
  , minBinsNumInitial = NULL # 一般建议将值设置在20，最大不超过全部数据行的1/10，minBinsNumInitial优先级更高
  , dataLimitCondition = 'all' #  all or DelNa or DelNaSpe 如果不含特殊值，DelNa和DelNaSpe结果一样;默认NULL = DelNaSpe
  , responseSign = c('bad','1','no','response')
  , specialValue = NULL  # should be vector
  , ifSpecialNeeds = 0 # 全局特殊值是否纳入分箱计算
  , specialVarValue = NULL # should be list
  , ifSpecialVarNeeds = 0 # 变量特殊值是否纳入计算
  , binsNumLimit = 8 # 设定最小分箱值，大于这个分箱将继续分箱
  , autoAdjust = 0 # 是否自动调整分箱，自动调整将会让分箱单调或呈正"U"、倒"U"，不建议与ifMonotonous参数同用
  , ifMonotonous = 0 # 判断是否单调 0：单调；1：不单调；
  , maxIterationLimit = Inf # 最大迭代次数
){

  y_ <- y

  x_ <- x

  ## 选取要计算的变量值和响应值

  data <- data[
    , .(y = get(y_),x = get(x_))
  ]

  ## -Inf和Inf处理

  if (
    length(
      intersect(c(-Inf,Inf),data[ ,x])
    ) > 0
  ){
    warning(
      paste0(
        x_
        , "The data contains -Inf, Inf fields, it is recommended to replace it with a special value, which will be processed according to the value by default."
      )
    )
  }

  ## NaN处理

  if (
    length(
      intersect(c(NaN),data[ ,x])
    ) > 0
  ){

    data[is.nan(data)] <- NA

    warning("The data contains a NaN field, it is recommended to replace it with a special value. By default, it will be replaced by Na, and it will appear in the Missing binning.")
  }

  ##  必须先声明标签

  yUni <- unique(data[,y])

  if (
    length(
      intersect(yUni,responseSign)
    ) == 0
  ){

    stop("The response value must be included in the parameter responseSign.")

  }

  MinBinsNumLimit1 <- nrow(data)

  MinBinsNumLimit2 <- nrow(data[!(is.na(x)),])

  data[
    , y := fifelse(y %in% responseSign,'bad','good')
  ][
    , yValue := fifelse(y == 'bad',1,0)
  ]

  ##  是否将全局特殊值纳入分箱计算,在这里也会讲缺失值刨去

  if (is.null(specialValue)){

    data <- data[!(is.na(x)),]

  } else if (is.null(specialValue) == F & ifSpecialNeeds == 0) {

    data <- data[!(x %in% specialValue) & !(is.na(x)),]

  } else if (is.null(specialValue) == F & ifSpecialNeeds == 1) {

    data <- data[!(is.na(x)),]

  } else if (!(ifSpecialNeeds %in% c(0,1))){

    stop("ifSpecialNeeds must be any value between 0 and 1.")

  }

  ##  是否将变量特殊值纳入分箱计算

  if (x_ %in% names(specialVarValue)){

    # print(specialVarValue[x_])

    if (is.null(specialVarValue)){

      data <- data

    } else if (is.null(specialVarValue) == F & ifSpecialVarNeeds == 0) {

      data <- data[!(x %in% specialVarValue),]

    } else if (is.null(specialVarValue) == F & ifSpecialVarNeeds == 1) {

      data <- data

    } else if (!(ifSpecialVarNeeds %in% c(0,1))){

      stop("ifSpecialVarNeeds must be any value between 0 and 1.")

    }

  }

  ## 判断条件最小分箱数据量，受参数limit控制

  MinBinsNumLimit3 <- nrow(data)

  if (is.null(minBinsNumInitial)){

    if (is.null(dataLimitCondition)){

      MinBinsNumLimit <- MinBinsNumLimit3 * limit

    } else if (dataLimitCondition == 'all'){

      MinBinsNumLimit <- MinBinsNumLimit1 * limit

    } else if (dataLimitCondition == 'DelNa'){

      MinBinsNumLimit <- MinBinsNumLimit2 * limit

    } else if (dataLimitCondition == 'DelNaSpe'){

      MinBinsNumLimit <- MinBinsNumLimit3 * limit

    } else {

      stop("The dataLimitCondition parameter can only be any one of all, DelNa, and DelNaSpe.")

    }

  } else if (is.numeric(minBinsNumInitial) == F){

    stop("The minBinsNumInitial parameter must be NULL or a numeric variable.")

  } else if (minBinsNumInitial >= MinBinsNumLimit1){

    stop("The value of minBinsNumInitial is greater than or equal to the maximum number of rows of the data, making this parameter meaningless, and the function will perform meaningless operations.")

  } else if (minBinsNumInitial > MinBinsNumLimit1 * 0.1 | minBinsNumInitial < 0){

    stop(paste0("For this dataset, the minBinsNumInitial parameter value should be limited to[0,",MinBinsNumLimit1 * 0.1,"]."))

  } else{

    MinBinsNumLimit <- minBinsNumInitial

  }

  ##  变量值去重排序

  valueSort <- sort(unique(data[, as.numeric(x)]))

  ##  获取切割点 -- 可以改成函数

  midPoint <- c(-Inf)

  if (length(valueSort) <= 1){

    midPoint <- midPoint

  }else{

    for (num in 2:length(valueSort)) {
      midPoint[num] <- (valueSort[num - 1] + valueSort[num])/2
    }

  }

  midPoint <- unique(c(midPoint,Inf))

  # print(midPoint)

  ## 按照最小分箱占比或数量对初始切割点进行处理

  if (length(midPoint) > 2){          ##  防止Inf,-Inf情况出现
    if (is.null(minBinsNumInitial)){

      midPoint[midPoint <= as.numeric(quantile(x = sort(data[, as.numeric(x)]),probs = limit))] <-
        as.numeric(quantile(x = midPoint,probs = limit))

      midPoint[midPoint >= as.numeric(quantile(x = sort(data[, as.numeric(x)]),probs = 1- limit))] <-
        as.numeric(quantile(x = midPoint,probs = 1 - limit))

      midPoint <- unique(c(-Inf,midPoint,Inf))

    }else{

      midPoint[midPoint <= as.numeric(sort(data[, as.numeric(x)])[minBinsNumInitial])] <-
        as.numeric(sort(data[, as.numeric(x)])[minBinsNumInitial])

      midPoint[midPoint >= as.numeric(sort(data[, as.numeric(x)])[nrow(data) - minBinsNumInitial])] <-
        as.numeric(as.numeric(sort(data[, as.numeric(x)])[nrow(data) - minBinsNumInitial]))

      midPoint <- unique(c(-Inf,midPoint,Inf))

    }
  }

  # print(midPoint)

  ##  卡方阈值

  threshold <- qchisq(1 - alpha, 1)

  ## 卡方值、分箱数值初始化

  MinChisqValue <- MinBinsNum <- 0

  ## 初始分箱数

  binsNum <- length(midPoint) - 1

  ## 计算相关性代码

  if (length(unique(data[, x])) == 1){

    corValue <- 0

  }else{

    corValue <- fifelse(
      is.na(cor(data[, x],data[, yValue],method = 'spearman',use = 'pairwise.complete.obs'))
      , 0
      , cor(data[, x],data[, yValue],method = 'spearman',use = 'pairwise.complete.obs')
    )

  }

  ## 删除 yValue

  data[, yValue := NULL]

  ## 迭代次数初始化

  maxIterationNum <- 0

  ## 是否有属于特殊分箱的变量

  if (is.null(breaksList) == F & x_ %in% names(breaksList)){

    midPoint <- breaksList[[x_]]

    midPoint <- midPoint[midPoint < max(data[, x],na.rm = T)]

    midPoint <- midPoint[midPoint > min(data[, x],na.rm = T)]

    midPoint <- c(-Inf,midPoint,Inf)

  } else{

    ## 循环获得最优解

    repeat{

      maxIterationNum = maxIterationNum + 1

      ##  可限制最大迭代次数

      if (maxIterationNum > maxIterationLimit){

        stop("The maximum number of iterations is reached, please adjust the maximum number of iterations or use the default value of Inf.")

      }

      ##  常数列 -- 不可能存在

      if (setequal(midPoint,c(-Inf,Inf))){

        break

      } else if (binsNum == 2){

        ##  分箱

        dataCutbins <- data[
          ,
          cutbins := cut(
            data[, x]
            , breaks = midPoint
            , include.lowest = TRUE
            , right = F
            , dig.lab = 20
          )
        ]

        ##  分箱长转宽

        dataCutbinsDs <- dcast(
          dataCutbins
          , cutbins ~ y
          , value.var = 'x'
          , fun.aggregate = length
        )

        ##  处理缺失列

        missCols <- setdiff(
          c('cutbins','good','bad')
          , names(dataCutbinsDs)
        )

        if (length(missCols) > 0){

          helpDataCutbinsDs <- data.table(
            matrix(
              0
              , nrow = nrow(dataCutbinsDs)
              , ncol = length(missCols)
            )
          )

          names(helpDataCutbinsDs) <- missCols

          dataCutbinsDs <- cbind(dataCutbinsDs,helpDataCutbinsDs)

          dataCutbinsDs <- dataCutbinsDs[,.(cutbins,good,bad)]

        }

        ##  计算各指标值

        dataCutbinsDs[
          ,`:=`(
            badLead = shift(as.numeric(bad),n = 1L,fill = Inf,type = 'lead')
            , goodLead = shift(as.numeric(good),n = 1L,fill = Inf,type = 'lead')
            , BinsNum = bad + good
          )
        ]

        if (min(dataCutbinsDs[,BinsNum]) < MinBinsNumLimit){

          midPoint <- c(-Inf,Inf)

        } else{

          midPoint <- midPoint

        }

        break

      } else if (binsNum > 2){

        # print("binsNum > 2")

        ##  分箱

        dataCutbins <- data[
          ,
          cutbins := cut(
            data[, x]
            , breaks = midPoint
            , include.lowest = TRUE
            , right = F
            , dig.lab = 20
          )
        ]

        ##  分箱长转宽

        dataCutbinsDs <- dcast(
          dataCutbins
          , cutbins ~ y
          , value.var = 'x'
          , fun.aggregate = length
        )

        # print(dataCutbinsDs)

        missCols <- setdiff(
          c('cutbins','good','bad')
          , names(dataCutbinsDs)
        )

        # print(missCols)

        if (length(missCols) > 0){

          helpDataCutbinsDs <- data.table(
            matrix(
              0
              , nrow = nrow(dataCutbinsDs)
              , ncol = length(missCols)
            )
          )

          names(helpDataCutbinsDs) <- missCols

          dataCutbinsDs <- cbind(dataCutbinsDs,helpDataCutbinsDs)

          dataCutbinsDs <- dataCutbinsDs[,.(cutbins,good,bad)]

        }

        ##  计算各指标值

        dataCutbinsDs[
          ,`:=`(
            badLead = shift(as.numeric(bad),n = 1L,fill = Inf,type = 'lead')
            , goodLead = shift(as.numeric(good),n = 1L,fill = Inf,type = 'lead')
            , BinsNum = bad + good
            , BinsBadRate = bad / (bad + good)
          )
        ][
          , `:=`(
            BinsBadRateLead = shift(BinsBadRate,n=1L,fill = Inf,type = 'lead')
            , BinsBadRateShift = shift(BinsBadRate,n=1L,fill = Inf,type = 'shift')
          )
        ][
          , `:=`(
            BinsBadRateLeadDiff = BinsBadRate - BinsBadRateLead
            , BinsBadRateShiftDiff = BinsBadRate - BinsBadRateShift
          )
        ][
          , chisqValue := apply(
            .SD
            , 1
            , function(x){

              x <- as.matrix(x)

              # n(ad-bc)^2/(a+b)(c+d)(a+c)(b+d)

              (as.numeric(x[2]) + as.numeric(x[3]) + as.numeric(x[4]) + as.numeric(x[5])) *
                ((as.numeric(x[2]) * as.numeric(x[5]) - as.numeric(x[3]) * as.numeric(x[4]))^2)/
                ((
                  (
                    as.numeric(x[2]) + as.numeric(x[3])
                  ) * (
                    as.numeric(x[4]) + as.numeric(x[5])
                  ) * (
                    as.numeric(x[2]) + as.numeric(x[4])
                  ) * (
                    as.numeric(x[3]) + as.numeric(x[5])
                  )
                ) + 1e-10)
            }
          )
        ][
          , `:=`(
            # mp = midPoint[-1]
            mp = fcase(
              cutbins %like% 'Inf)' == T | cutbins %like% 'Inf]' == T
              , Inf
              , cutbins %like% 'Inf)' == F
              , as.numeric(
                sub(
                  ','
                  , ''
                  , regmatches(
                    regexpr('(\\,\\d*\\.?\\d+)|(\\,\\-d*\\.?\\d+)',cutbins,perl = T)
                    , x = cutbins
                  )
                )
              )
            )
          )
          , by = cutbins
        ]

        # print(dataCutbinsDs)

        ##  自动调整

        BinsBadRateLeadDiffProduct <- list()

        for (i in 1:(nrow(dataCutbinsDs) - 2)){

          BinsBadRateLeadDiffProduct[[i]] <-
            dataCutbinsDs[i,BinsBadRateLeadDiff] *
            dataCutbinsDs[i + 1,BinsBadRateLeadDiff]
        }

        BinsBadRateLeadDiffProduct <- unlist(BinsBadRateLeadDiffProduct)

        ##  其他参数条件

        MinChisqValue <- min(dataCutbinsDs[,chisqValue],na.rm = T)

        binsNum <- dataCutbinsDs[,.N]

        MinBinsNum <- min(dataCutbinsDs[,BinsNum])

        ##  初始化单调判断条件

        maxBinsBadRateLeadDiff <- 0
        minBinsBadRateLeadDiff <- 1

        if (corValue > 0){

          maxBinsBadRateLeadDiff <- max(
            dataCutbinsDs[,BinsBadRateLeadDiff],na.rm = T
          )

        }else{

          minBinsBadRateLeadDiff <- min(
            dataCutbinsDs[BinsBadRateLeadDiff != -Inf,BinsBadRateLeadDiff],na.rm = T
          )

        }

        if (ifMonotonous == 0){

          if (MinBinsNum < MinBinsNumLimit){

            # print(1)

            dataIndices <-
              dataCutbinsDs[
                BinsNum == min(dataCutbinsDs[,BinsNum])
                , which = TRUE
              ]

            delDataIndices <- dataCutbinsDs[
              dataIndices
            ][
              ,`:=`(
                dataIndices = dataIndices
              )
            ][
              ,`:=`(
                upDownSign = fcase(
                  dataIndices == 1,0,
                  dataIndices == binsNum,1,
                  abs(BinsBadRateLeadDiff) <= abs(BinsBadRateShiftDiff),0,
                  abs(BinsBadRateLeadDiff) > abs(BinsBadRateShiftDiff),1,
                  default = 1
                )
              )
            ][
              ,`:=`(
                dataIndicesNew = dataIndices - upDownSign
              )
            ][,dataIndicesNew]

            midPoint <- c(
              -Inf
              , dataCutbinsDs[
                !(delDataIndices)
                , mp
              ]
            )

          } else if (MinChisqValue < threshold){

            # print(2)

            midPoint <- c(
              -Inf
              , dataCutbinsDs[
                chisqValue != min(dataCutbinsDs[!(mp %in% c(-Inf,Inf)),chisqValue],na.rm = T)
                , mp
              ]
            )

            if (Inf %in% midPoint){
              midPoint <- midPoint
            }else{
              midPoint <- c(midPoint,Inf)
            }

          } else if (binsNum > binsNumLimit){

            # print(3)

            midPoint <- c(
              -Inf
              , dataCutbinsDs[
                chisqValue != min(dataCutbinsDs[!(mp %in% c(-Inf,Inf)),chisqValue],na.rm = T)
                , mp
              ]
            )

            if (Inf %in% midPoint){
              midPoint <- midPoint
            }else{
              midPoint <- c(midPoint,Inf)
            }

          } else if (
            autoAdjust == 1 &
            (sum(BinsBadRateLeadDiffProduct < 0)) > 1
          ){

            midPoint <- c(
              -Inf
              , dataCutbinsDs[
                chisqValue != min(dataCutbinsDs[!(mp %in% c(-Inf,Inf)),chisqValue],na.rm = T)
                , mp
              ]
            )

            if (Inf %in% midPoint){
              midPoint <- midPoint
            }else{
              midPoint <- c(midPoint,Inf)
            }

          } else {break}

          binsNum <- length(midPoint) - 1

        } else if (ifMonotonous == 1){

          if (corValue > 0 & maxBinsBadRateLeadDiff > 0){

            midPoint <- c(-Inf,dataCutbinsDs[BinsBadRateLeadDiff < 0,mp])

            if (Inf %in% midPoint){
              midPoint <- midPoint
            }else{
              midPoint <- c(midPoint,Inf)
            }

          } else if (corValue <= 0 & minBinsBadRateLeadDiff <= 0){

            midPoint <- c(-Inf,dataCutbinsDs[BinsBadRateLeadDiff > 0,mp])

            if (Inf %in% midPoint){
              midPoint <- midPoint
            }else{
              midPoint <- c(midPoint,Inf)
            }

          } else if (MinBinsNum < MinBinsNumLimit){

            dataIndices <-
              dataCutbinsDs[
                BinsNum == min(dataCutbinsDs[,BinsNum])
                , which = TRUE
              ]

            delDataIndices <- dataCutbinsDs[
              dataIndices
            ][
              ,`:=`(
                dataIndices = dataIndices
              )
            ][
              ,`:=`(
                upDownSign = fcase(
                  dataIndices == 1,0,
                  dataIndices == binsNum,1,
                  abs(BinsBadRateLeadDiff) <= abs(BinsBadRateShiftDiff),0,
                  abs(BinsBadRateLeadDiff) > abs(BinsBadRateShiftDiff),1,
                  default = 1
                )
              )
            ][
              ,`:=`(
                dataIndicesNew = dataIndices - upDownSign
              )
            ][,dataIndicesNew]

            midPoint <- c(
              -Inf
              , dataCutbinsDs[
                !(delDataIndices)
                , mp
              ]
            )

          } else if (MinChisqValue < threshold){

            midPoint <- c(
              -Inf
              , dataCutbinsDs[
                chisqValue != min(dataCutbinsDs[!(mp %in% c(-Inf,Inf)),chisqValue],na.rm = T)
                , mp
              ]
            )

            if (Inf %in% midPoint){
              midPoint <- midPoint
            }else{
              midPoint <- c(midPoint,Inf)
            }

          } else if (binsNum > binsNumLimit){

            midPoint <- c(
              -Inf
              , dataCutbinsDs[
                chisqValue != min(dataCutbinsDs[!(mp %in% c(-Inf,Inf)),chisqValue],na.rm = T)
                , mp
              ]
            )

            if (Inf %in% midPoint){
              midPoint <- midPoint
            }else{
              midPoint <- c(midPoint,Inf)
            }

          } else if (
            autoAdjust == 1 &
            (sum(BinsBadRateLeadDiffProduct < 0)) > 1
          ){

            midPoint <- c(
              -Inf
              , dataCutbinsDs[
                chisqValue != min(dataCutbinsDs[!(mp %in% c(-Inf,Inf)),chisqValue],na.rm = T)
                , mp
              ]
            )

            if (Inf %in% midPoint){
              midPoint <- midPoint
            }else{
              midPoint <- c(midPoint,Inf)
            }

          } else {break}

          binsNum <- length(midPoint) - 1

        }

      }
    }

  }

  midPoint <- sort(midPoint)

  return(midPoint)

}





