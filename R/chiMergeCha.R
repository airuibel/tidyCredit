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
#' @param maxIterationLimit
#'
#' @return
#' @export
#'
#' @examples
chiMergeCha <- function(
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
  , ifSpecialNeeds = 0 # 全局特殊值是否纳入分箱计算，类别型特殊值必须从全部样本中删除
  , specialVarValue = NULL # should be list
  , ifSpecialVarNeeds = 0 # 变量特殊值是否纳入计算，类别型特殊值必须从全部样本中删除
  , binsNumLimit = 8
  # , ifMonotonous = 0 # 判断是否单调 0：单调；1：不单调；
  , maxIterationLimit = Inf # 最大迭代次数
) {

  y_ <- y

  x_ <- x

  ## 选取要计算的变量值和响应值

  data <- data[
    , .(y = get(y_),x = get(x_))
  ]

  MinBinsNumLimit1 <- nrow(data)

  MinBinsNumLimit2 <- nrow(data[!(is.na(x)),])

  ## 空字符串处理

  if (
    nrow(data[x == '']) > 0
  ){

    data[x == '',x := NA]

    warning(
      paste0(
        x_
        , "The data contains empty strings, which will be treated as NA by default."
      )
    )
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

    MinBinsNumLimit <- MinBinsNumLimit3 * limit

    if (is.null(dataLimitCondition)){

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

  ##  卡方阈值

  threshold <- qchisq(1 - alpha, 1)

  ## 初始化判断条件的卡方值、分箱数、提升度值

  MinChisqValue <- MinBinsNum <- 0

  ## 初始分箱数

  binsNum <- length(as.character(unique(data[, x])))

  ## 迭代次数

  maxIterationNum <- 0

  if (is.null(breaksList) == F & x_ %in% names(breaksList)){

    midPoint <- c(breaksList[[x_]])

  } else{

    ## 循环获得最优解

    repeat{

      maxIterationNum = maxIterationNum + 1

      ##  可限制最大迭代次数

      if (maxIterationNum > maxIterationLimit){

        stop("The maximum number of iterations is reached, please adjust the maximum number of iterations or use the default value of Inf.")

      }

      midPoint = as.character(unique(data[, x]))

      if (length(midPoint) == 1){

        # print(1)

        break

      } else if (binsNum == 2){

        # print(2)

        ##  分箱

        dataCutbins <- data[
          , .N
          , by = .(x,y)
        ]

        ##  分箱长转宽

        dataCutbinsDs <- dcast(
          dataCutbins
          , x ~ y
          , value.var = 'N'
          , fun.aggregate = sum
        )

        missCols <- setdiff(
          c('good','bad')
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

          dataCutbinsDs <- dataCutbinsDs[,.(x,good,bad)]

        }

        ##  计算各指标值

        dataCutbinsDs[
          ,`:=`(
            BinsNum = bad + good
          )
        ]

        if (min(dataCutbinsDs[,BinsNum]) < MinBinsNumLimit){

          midPoint <- c(paste0(midPoint,collapse = '<,>'))

        } else{

          midPoint <- midPoint

        }

        break

      } else if (binsNum > 2){

        # print(3)

        ##  分箱

        dataCutbins <- data[
          , .N
          , by = .(x,y)
        ]

        ##  分箱长转宽

        dataCutbinsDs <- dcast(
          dataCutbins
          , x ~ y
          , value.var = 'N'
          , fun.aggregate = sum
        )

        # print(names(dataCutbinsDs))

        missCols <- setdiff(
          c('good','bad')
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

          dataCutbinsDs <- dataCutbinsDs[,.(x,good,bad)]

        }

        ##  计算各指标值

        dataCutbinsDs[
          ,`:=`(
            BinsBadRate = bad / (bad + good)
          )
        ]

        ##  按照响应率排序合并

        setorder(dataCutbinsDs,BinsBadRate)

        dataCutbinsDs[,BinsBadRate := NULL]

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
              # x <- as.matrix(x)

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
        ]

        # print(dataCutbinsDs)

        MinChisqValue <- min(dataCutbinsDs[,chisqValue],na.rm = T)

        binsNum <- dataCutbinsDs[,.N]

        MinBinsNum <- min(dataCutbinsDs[,BinsNum])

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
                dataIndices == 1 | dataIndices == binsNum,0,
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


          if (delDataIndices[1] == binsNum){

            needBind <- as.character(dataCutbinsDs[c(delDataIndices - 1,delDataIndices),x])

          }else{

            needBind <- as.character(dataCutbinsDs[c(delDataIndices,delDataIndices + 1),x])

          }

          data[x %in% needBind,x := paste(needBind,collapse = '<,>')]

        } else if (MinChisqValue < threshold){

          dataIndices <-
            dataCutbinsDs[
              chisqValue == min(dataCutbinsDs[,chisqValue],na.rm = T)
              , which = TRUE
            ]

          needBind <- as.character(dataCutbinsDs[c(dataIndices,dataIndices + 1),x])

          data[x %in% needBind,x := paste(needBind,collapse = '<,>')]

        }  else if (binsNum > binsNumLimit){

          dataIndices <-
            dataCutbinsDs[
              chisqValue == min(dataCutbinsDs[,chisqValue],na.rm = T)
              , which = TRUE
            ]

          needBind <- as.character(dataCutbinsDs[c(dataIndices,dataIndices + 1),x])

          data[x %in% needBind,x := paste(needBind,collapse = '<,>')]

        } else {break}
      }
    }

  }

  midPoint <- sort(midPoint)

  return(midPoint)

}
