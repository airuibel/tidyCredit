#' Title
#'
#' @param data
#' @param y
#' @param x
#' @param ksLimit
#' @param initialBins
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
#'
#' @import data.table
#'
#' @export
#'
#' @examples
#'

binsBestKS <- function(  ##  分裂时的最大值最小值可能等于样本的最大值最小值，需要更改
  data
  , y
  , x
  , ksLimit = 0.03
  , initialBins = NULL
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

  ksCal <- function(data,ksLimit,ksSplit = NULL) {

    if (is.null(ksSplit) == F){

      data <- data[!(x %in% unname(ksSplit))]

    }

    suppressWarnings(
      data[
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
        )
      ]
    )

    # print(data)

    suppressWarnings(
      ksMax <- min(
        data[
          ks == max(data[, ks],na.rm = T)
          , x
        ]
        , na.rm = T
      )
    )

    suppressWarnings(
      ksMaxValue <- min(
        data[
          ks == max(data[, ks],na.rm = T)
          , ks
        ]
      )
    )


    if (ksMaxValue <= ksLimit){

      ksMax <- NULL

    }

    return(ksMax)

  }

  data <- copy(data)

  setDT(data)

  y_ <- y

  x_ <- x

  ## 选取要计算的变量值和响应值

  data <- data[
    , .(y = get(y_),x = get(x_))
  ][order(x)]

  ## -Inf和Inf处理

  if (
    length(
      intersect(c(-Inf,Inf),data[ ,x])
    ) > 0
  ){

    data[is.infinite(data)] <- NA
    # warning("数据中包含-Inf,Inf字段，建议将其用特殊值替代，默认将会按照Na进行替换，将会出现在Missing分箱中。")
  }

  ## NaN处理

  if (
    length(
      intersect(c(NaN),data[ ,x])
    ) > 0
  ){

    data[is.nan(data)] <- NA

    # warning("数据中包含NaN字段，建议将其用特殊值替代，默认将会按照Na进行替换，将会出现在Missing分箱中。")
  }

  MinBinsNumLimit1 <- nrow(data)

  MinBinsNumLimit2 <- nrow(data[!(is.na(x)),])

  ##  y必须是两类别

  yUni <- unique(data[,y])

  if (length(yUni) != 2){

    stop("Y must be two-category.")

  }

  ## y不能含有NA

  if (sum(is.na(data[,y])) > 0){

    stop("Y cannot contain missing values.")

  }

  ##  必须先声明标签

  if (
    length(
      intersect(yUni,responseSign)
    ) == 0
  ){

    stop("The response value must be included in the parameter responseSign.")

  }

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

      stop("The dataLimitCondition parameter can only be any one of all, DelNa, DelNaSpe.")

    }

  } else if (is.numeric(minBinsNumInitial) == F){

    stop("The minBinsNumInitial parameter must be NULL or a numeric variable.")

  } else if (minBinsNumInitial >= MinBinsNumLimit1){

    stop("The value of minBinsNumInitial is greater than or equal to the maximum number of rows of the data, making this parameter meaningless, and the function will perform meaningless operations.")

  } else if (minBinsNumInitial > MinBinsNumLimit1 * 0.1 | minBinsNumInitial < 0){

    stop(paste0("For this dataset, the minBinsNumInitial parameter value should be limited to [0,",MinBinsNumLimit1 * 0.1,"]."))

  } else{

    MinBinsNumLimit <- minBinsNumInitial

  }

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

  ## 相关性阈值

  MonotonousThreshold <- -1

  ## 变量值去重排序

  valueSort <- sort(unique(data[, as.numeric(x)]))

  ## 迭代次数

  maxIterationNum <- 0

  ksSplit <- vector()

  ksMaxValue <- ksCal(data = data,ksLimit = ksLimit)

  # if (ksMaxValue == data[,min(x,na.rm = T)]){
  #
  #   ksMaxValue <- (valueSort[1] + valueSort[2]) / 2
  #
  # } else if (ksMaxValue == data[,max(x,na.rm = T)]){
  #
  #   ksMaxValue <- (valueSort[length(valueSort)] + valueSort[length(valueSort) - 1]) / 2
  #
  # }

  ksSplit <- append(
    ksSplit
    , ksMaxValue
  )

  nextLoop <- 0

  # print(ksSplit)

  repeat{

    maxIterationNum = maxIterationNum + 1

    ##  可限制最大迭代次数

    if (maxIterationNum > maxIterationLimit){

      stop("The maximum number of iterations is reached, please adjust the maximum number of iterations or use the default value Inf.")

    }

    if (is.null(unlist(ksMaxValue))){

      break

    }

    if (exists('dataList')){

      ksMaxValue <- lapply(
        dataList
        , function(x) ksCal(data = x,ksLimit,ksSplit = ksSplit)
      )

      # print(unname(ksMaxValue))
      #
      # print(data[,min(x,na.rm = T)])

      if (is.null(unlist(ksMaxValue))){

        break

      }

      if (
        length(
          setdiff(
            unlist(ksMaxValue)
            , unlist(ksSplit)
          )
        ) == 0
      ){

        # ksSplit <- dataCutbinsDs[,mp]

        # print(paste0('aaaaaaa',unname(ksSplit)))

        # break

        nextLoop <- 1

      }else{

        ksSplit <- append(
          ksSplit
          , unlist(ksMaxValue)
        )

        # print(unname(ksSplit))

      }
    }

    ##  分箱

    dataCutbins <- data[
      ,
      cutbins := cut(
        data[, x]
        , breaks = unique(c(-Inf,ksSplit,Inf))
        , right = F
        , dig.lab = 10
      )
    ]

    # print(dataCutbins)

    dataList <- split(
      data
      , by = 'cutbins'
    )

    # print(dataList)

    ##  分箱长转宽

    dataCutbinsDs <- dcast(
      data
      , cutbins ~ y
      , value.var = 'x'
      , fun.aggregate = length
    )

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
      , `:=`(
        mp = fcase(
          cutbins %like% 'Inf)' == T
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

    # as.numeric(
    #   sub(
    #     ','
    #     , ''
    #     , regmatches(
    #       regexpr('(\\,\\d*\\.?\\d+)',cutbins,perl = T)
    #       , x = cutbins
    #     )
    #   )
    # )

    # print(dataCutbinsDs)



    # [
    #   , `:=`(
    #     mp = c(sort(unlist(unique(ksSplit))),Inf)
    #   )
    # ][]

    # print(dataCutbinsDs)

    # print(ksSplitAssis)
    # print(ksSplit)
    #
    # if (identical(ksSplitAssis,ksSplit)){
    #
    #   break
    #
    # }
    #
    # ksSplitAssis <- ksSplit

    # ksSplit <- dataCutbinsDs[,mp]

    # print(unname(ksSplit))

    ##  其他参数条件

    binsNum <- dataCutbinsDs[,.N]

    MinBinsNum <- min(dataCutbinsDs[,BinsNum])

    if (is.null(initialBins)){

      if (length(unique(data[,x])) > 300){

        initialBins <- 35

      }else if (length(unique(data[,x])) > 150){

        initialBins <- 30

      }else if (length(unique(data[,x])) <= 25){

        initialBins <- 10

      }else {

        initialBins <- 20

      }

    }else {

      initialBins <- initialBins

    }

    if (
      # MinBinsNum < MinBinsNumLimit
      binsNum > initialBins
      | nextLoop == 1
    ){

      if (nrow(dataCutbinsDs) == 1){

        break

      }

      if (MinBinsNum < MinBinsNumLimit){

        # print(1)

        repeat{

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
                # dataIndices == 1 | dataIndices == binsNum,0,
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

          ksSplit <- dataCutbinsDs[
            !(delDataIndices)
            , mp
          ]

          # print(ksSplit)

          dataCutbins <- data[
            ,
            cutbins := cut(
              data[, x]
              , breaks = c(-Inf,unique(ksSplit))
              , right = F
              , dig.lab = 10
            )
          ]

          # print(ksSplit)

          ##  分箱长转宽

          dataCutbinsDs <- dcast(
            data
            , cutbins ~ y
            , value.var = 'x'
            , fun.aggregate = length
          )

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
            , `:=`(
              mp = fcase(
                cutbins %like% 'Inf)' == T
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
          ][]

          ##  其他参数条件

          binsNum <- dataCutbinsDs[,.N]

          MinBinsNum <- min(dataCutbinsDs[,BinsNum])

          # print(dataCutbinsDs)

          if (
            MinBinsNum >= MinBinsNumLimit |
            binsNum == 1
          ){
            break
          }

        }

      }

      if (binsNum > binsNumLimit){

        repeat{

          ksSplit <- dataCutbinsDs[
            abs(BinsBadRateLeadDiff) !=
              unique(
                suppressWarnings(
                  min(abs(dataCutbinsDs[BinsBadRateLeadDiff != -Inf,BinsBadRateLeadDiff]))
                )
              )
            , mp
          ]

          dataCutbins <- data[
            ,
            cutbins := cut(
              data[, x]
              , breaks = c(-Inf,unique(ksSplit))
              , right = F
              , dig.lab = 10
            )
          ]

          ##  分箱长转宽

          dataCutbinsDs <- dcast(
            data
            , cutbins ~ y
            , value.var = 'x'
            , fun.aggregate = length
          )

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
            , `:=`(
              mp = fcase(
                cutbins %like% 'Inf)' == T
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
          ][]

          ##  其他参数条件

          binsNum <- dataCutbinsDs[,.N]

          MinBinsNum <- min(dataCutbinsDs[,BinsNum])

          if (
            binsNum <= binsNumLimit |
            binsNum == 1
          ){
            break
          }
        }

      }

      ##  单调条件初始化

      maxBinsBadRateLeadDiff <- 0
      minBinsBadRateLeadDiff <- 1

      if (corValue > 0){
        MonotonousThreshold <-
          corValue * max(
            dataCutbinsDs[,BinsBadRateLeadDiff],na.rm = T
          )

        maxBinsBadRateLeadDiff <- max(
          dataCutbinsDs[,BinsBadRateLeadDiff],na.rm = T
        )

      }else{
        MonotonousThreshold <-
          corValue * suppressWarnings(
            min(
              dataCutbinsDs[BinsBadRateLeadDiff != -Inf,BinsBadRateLeadDiff],na.rm = T
            )
          )

        minBinsBadRateLeadDiff <-
          suppressWarnings(
            min(
              dataCutbinsDs[BinsBadRateLeadDiff != -Inf,BinsBadRateLeadDiff],na.rm = T
            )
          )

      }

      if (ifMonotonous == 1){

        repeat{
          if (corValue > 0 & maxBinsBadRateLeadDiff > 0){

            ksSplit <- c(dataCutbinsDs[BinsBadRateLeadDiff < 0,mp])

            if (Inf %in% ksSplit){
              ksSplit <- ksSplit
            }else{
              ksSplit <- c(ksSplit,Inf)
            }

          } else if (corValue <= 0 & minBinsBadRateLeadDiff <= 0){

            ksSplit <- c(dataCutbinsDs[BinsBadRateLeadDiff > 0,mp])

            if (Inf %in% ksSplit){
              ksSplit <- ksSplit
            }else{
              ksSplit <- c(ksSplit,Inf)
            }

          } else{break}

          dataCutbins <- data[
            ,
            cutbins := cut(
              data[, x]
              , breaks = c(-Inf,unique(ksSplit))
              , right = F
              , dig.lab = 10
            )
          ]

          if (nrow(dataCutbinsDs) <= 2){

            break

          }

          # print(ksSplit)

          ##  分箱长转宽

          dataCutbinsDs <- dcast(
            data
            , cutbins ~ y
            , value.var = 'x'
            , fun.aggregate = length
          )

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
            , `:=`(
              mp = fcase(
                cutbins %like% 'Inf)' == T
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
          ][]

          if (corValue > 0){
            MonotonousThreshold <-
              corValue * max(
                dataCutbinsDs[,BinsBadRateLeadDiff],na.rm = T
              )

            maxBinsBadRateLeadDiff <- max(
              dataCutbinsDs[,BinsBadRateLeadDiff],na.rm = T
            )

          }else{
            MonotonousThreshold <-
              corValue * suppressWarnings(
                min(
                  dataCutbinsDs[BinsBadRateLeadDiff != -Inf,BinsBadRateLeadDiff],na.rm = T
                )
              )

            minBinsBadRateLeadDiff <-
              suppressWarnings(
                min(
                  dataCutbinsDs[BinsBadRateLeadDiff != -Inf,BinsBadRateLeadDiff],na.rm = T
                )
              )
          }
        }
      }

      ##  自动调整参数初始化

      BinsBadRateLeadDiffProduct <- 1

      if (nrow(dataCutbinsDs) > 2){   ##  两个分箱一定单调

        BinsBadRateLeadDiffProduct <- list()

        # print(dataCutbinsDs)

        for (i in 1:(nrow(dataCutbinsDs) - 2)){

          # print(1)

          BinsBadRateLeadDiffProduct[[i]] <-
            dataCutbinsDs[i,BinsBadRateLeadDiff] *
            dataCutbinsDs[i + 1,BinsBadRateLeadDiff]
        }

        BinsBadRateLeadDiffProduct <- unlist(BinsBadRateLeadDiffProduct)

      }

      if (
        autoAdjust == 1 &
        (sum(BinsBadRateLeadDiffProduct < 0)) > 1
      ){

        repeat{

          ksSplit <- dataCutbinsDs[
            abs(BinsBadRateLeadDiff) !=
              unique(
                suppressWarnings(
                  min(abs(dataCutbinsDs[BinsBadRateLeadDiff != -Inf,BinsBadRateLeadDiff]))
                )
              )
            , mp
          ]

          dataCutbins <- data[
            ,
            cutbins := cut(
              data[, x]
              , breaks = c(-Inf,unique(ksSplit))
              , right = F
              , dig.lab = 10
            )
          ]

          ##  分箱长转宽

          dataCutbinsDs <- dcast(
            data
            , cutbins ~ y
            , value.var = 'x'
            , fun.aggregate = length
          )

          if (nrow(dataCutbinsDs) <= 3){

            break

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
            , `:=`(
              mp = fcase(
                cutbins %like% 'Inf)' == T
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
          ][]

          BinsBadRateLeadDiffProduct <- list()

          for (i in 1:(nrow(dataCutbinsDs) - 2)){

            BinsBadRateLeadDiffProduct[[i]] <-
              dataCutbinsDs[i,BinsBadRateLeadDiff] *
              dataCutbinsDs[i + 1,BinsBadRateLeadDiff]
          }

          BinsBadRateLeadDiffProduct <- unlist(BinsBadRateLeadDiffProduct)

          if (
            sum(BinsBadRateLeadDiffProduct < 0) <= 1 |
            binsNum == 1
          ){
            break
          }
        }

      }

      break

    }
  }

  # print(ksSplit)

  ksSplit <- sort(unique(c(-Inf,ksSplit,Inf)))

  ksSplitRes <- ksSplit

  for (i in 1:(length(ksSplit) - 1)){

    if (nrow(data[x >= ksSplit[i] & x < ksSplit[i + 1],]) == 0){

      # print(ksSplit[i])

      if (ksSplit[i] %in% (-Inf)){

        ksSplitRes <- ksSplitRes[!(ksSplitRes %in% ksSplitRes[i + 1])]

      }else {

        ksSplitRes <- ksSplitRes[!(ksSplitRes %in% ksSplitRes[i])]

      }

      # print(ksSplit)

    }

  }

  return(ksSplitRes)

}
