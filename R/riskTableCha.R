#' Title
#'
#' @param data
#' @param y
#' @param x
#' @param showCols
#' @param specialValue
#' @param specialVarValue
#' @param responseSign
#' @param splitValue
#'
#' @return
#' @export
#'
#' @examples
riskTableCha <- function(
  data
  , y
  , x
  , showCols = c(
    'vars'
    , 'bins'
    , 'count'
    , 'countRate'
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
    , 'totalIvAdj' # 对于分箱提升度大于5倍以上提升度的不计算IV
    , 'binsLift'
    , 'shape'
    , 'splits'
    , 'isSpecial'
  )
  , specialValue = NULL
  , specialVarValue = NULL
  , responseSign = c('bad','1','no','response')
  , splitValue
){

  data <- copy(data)

  setDT(data)

  y_ <- y
  x_ <- x

  data <- data[
    , .(y = get(y_),x = get(x_))
  ][
    , y := fifelse(y %in% responseSign,1,0)
  ]

  ## 空字符串处理

  if (
    nrow(data[x == '']) > 0
  ){

    data[x == '',x := NA]

  }

  ## 拒绝特殊值合并的问题

  specialValue <- intersect(specialValue,data[,x])

  # specialVarValue <- intersect(specialVarValue[x_],data[,x])

  # specialValue <- sort(unique(c(specialValue,specialVarValue)))

  if (x_ %in% names(specialVarValue)){

    splitValue <- specialVarValue[x_][[1]]

  }


  ##  缺失值处理

  if (sum(is.na(data[,x])) > 0){

    riskTableMissing <- function(data ,y ,x){


      missData <- data[is.na(x) == T,.(x,y)]

      missDatay <- missData[
        ,.N,by = .(y)
      ]

      missDataResult <- data.table(
        vars = x
        , bins = 'Missing'
        , count = sum(missDatay[,N])
        , nmlCount = sum(missDatay[y == 0,N])
        , rpsCount = sum(missDatay[y == 1,N])
        , splits = 'Missing'
        , isSpecial = 'TRUE'
      )

      return(missDataResult)

    }

    riskTableMissingResult <- riskTableMissing(data, y , x)

  }

  ##  特殊值处理

  if (is.null(specialValue) == F){

    riskTableSpecial <- function(data ,y ,x ,specialValue){

      specialData <- data[x %in% specialValue,.(x,y)]

      specialDatay <- specialData[
        ,.(
          count = sum(fifelse(y %in% c(0,1),1,0)),
          nmlCount = sum(fifelse(y == 0,1,0)),
          rpsCount = sum(fifelse(y == 1,1,0))
        )
        ,by = .(bins = x)
      ][
        order(bins)
      ]

      specialDataResult <- cbind(
        data.table(
          vars = x
        )
        , specialDatay
        , data.table(
          splits = specialValue
        )
        , data.table(
          isSpecial = 'TRUE'
        )
      )

      return(specialDataResult)

    }

    riskTableSpecialResult <- riskTableSpecial(data, y, x, specialValue = specialValue)

  }

  ##  正常数据处理

  riskTableNormal <- function(data ,y ,x ,splitValue){

    normalData <- data[
      !(x %in% specialValue) & is.na(x) == F,.(x,y)
    ]

    splitTable <- data.table(splitValue,splitValueVec = strsplit(splitValue,"<,>"))

    for (i in 1:nrow(splitTable)){

      normalData[
        x %in% splitTable[i,splitValueVec][[1]]
        , x := splitTable[i,splitValue][[1]]
      ]

    }

    normalDatay <- normalData[
      ,.(
        count = sum(fifelse(y %in% c(0,1),1,0)),
        nmlCount = sum(fifelse(y == 0,1,0)),
        rpsCount = sum(fifelse(y == 1,1,0))
      )
      ,by = .(bins = x)
    ][
      order(bins)
    ]

    normalDataResult <- cbind(
      data.table(
        vars = x
      )
      , normalDatay
      , data.table(
        splits = normalDatay[, bins]
      )
      , data.table(
        isSpecial = 'FALSE'
      )
    )

    return(normalDataResult)

  }

  riskTableNormalResult <- riskTableNormal(data, y, x ,splitValue = splitValue)

  ##  数据合并

  if (sum(is.na(data[,.(x)])) > 0 & is.null(specialValue) == F){

    ## 可能存在不含缺失值的问题

    if (is.na(riskTableSpecialResult[,bins][1]) == F){

      riskTableResult <- rbind(
        riskTableMissingResult,
        riskTableSpecialResult,
        riskTableNormalResult
      )

    }else{

      riskTableResult <- rbind(
        riskTableMissingResult,
        riskTableNormalResult
      )

    }

  } else if (sum(is.na(data[,.(x)])) > 0 & is.null(specialValue) == T){

    riskTableResult <- rbind(
      riskTableMissingResult,
      riskTableNormalResult
    )
  } else if (sum(is.na(data[,.(x)])) == 0 & is.null(specialValue) == F){

    if (is.na(riskTableSpecialResult[,bins][1]) == F){
      riskTableResult <- rbind(
        riskTableSpecialResult,
        riskTableNormalResult
      )
    }else{
      riskTableResult <-
        riskTableNormalResult
    }

  } else if (sum(is.na(data[,.(x)])) == 0 & is.null(specialValue == T)){
    riskTableResult <-
      riskTableNormalResult
  } else{
    riskTableResult <-
      riskTableNormalResult
  }

  riskTableResult <- naZero(riskTableResult)

  ##  指标值计算

  riskTableResult[
    ,`:=`(
      countRate = round(count / sum(count),4)
      , nmlCountCum = cumsum(nmlCount)
      , rpsCountCum = cumsum(rpsCount)
    )
  ][
    ,`:=` (
      nmlCountCumRate = round(nmlCountCum / max(nmlCountCum),4)
      , rpsCountCumRate = round(rpsCountCum / max(rpsCountCum),4)
    )
  ][
    ,`:=`(
      rpsRate = rpsCount / (rpsCount + nmlCount)
      , woe = round(log((rpsCount / sum(rpsCount) + 1e-10) / (nmlCount / sum(nmlCount) + 1e-10)),4)
      , iv = round((rpsCount / sum(rpsCount) - nmlCount / sum(nmlCount)) * log((rpsCount / sum(rpsCount) + 1e-10) / (nmlCount / sum(nmlCount) + 1e-10)),4)
      , totalIv = round(sum((rpsCount / sum(rpsCount) - nmlCount / sum(nmlCount)) * log((rpsCount / sum(rpsCount) + 1e-10) / (nmlCount / sum(nmlCount) + 1e-10))),4)
      # , binsLift = round((rpsCount / max(rpsCountCum)) / (nmlCount / max(nmlCountCum)),4)
      , binsLift = round(
        (rpsCount / (rpsCount + nmlCount)) / (
          (max(rpsCountCum) / (max(nmlCountCum) + max(rpsCountCum)))  + 1e-10
        )
        , 4
      )
    )
  ][
    , `:=`(
      totalIvAdj = sum(
        fcase(
          binsLift < 5,iv,
          default = 0
        )
      )
    )
  ][]

  riskTableResult[
    , `:=`(
      BinsBadRateLead = shift(rpsRate,n=1L,fill = Inf,type = 'lead')
      , BinsBadRateShift = shift(rpsRate,n=1L,fill = Inf,type = 'shift')
    )
  ][
    , `:=`(
      BinsBadRateLeadDiff = rpsRate - BinsBadRateLead
    )
  ][]

  sumBinsBadRateLeadDiffAdd <- nrow(
    riskTableResult[isSpecial == 'FALSE',][BinsBadRateLeadDiff >= 0,]
  )

  sumBinsBadRateLeadDiffReduce <- nrow(
    riskTableResult[isSpecial == 'FALSE',][BinsBadRateLeadDiff < 0,]
  ) - 1  # 减-Inf

  if (nrow(riskTableResult[isSpecial == 'FALSE',]) == 1){

    riskTableResultShapeRes <- 'no binning'

  } else if(nrow(riskTableResult[isSpecial == 'FALSE',]) == 2){

    if (
      riskTableResult[isSpecial == 'FALSE',][1,rpsRate] >
      riskTableResult[isSpecial == 'FALSE',][2,rpsRate]
    ){

      riskTableResultShapeRes <- 'Monotonically decreasing'

    } else{

      riskTableResultShapeRes <- 'Monotonically increasing'

    }

  } else if(nrow(riskTableResult[isSpecial == 'FALSE',]) > 2){

    riskTableResultShape <- list()

    for (i in 1:(nrow(riskTableResult[isSpecial == 'FALSE',]) - 2)){

      riskTableResultShape[[i]] <-
        riskTableResult[isSpecial == 'FALSE',][i,BinsBadRateLeadDiff] *
        riskTableResult[isSpecial == 'FALSE',][i + 1,BinsBadRateLeadDiff]
    }

    riskTableResultShape <- unlist(riskTableResultShape)

    # print(riskTableResultShape)
    #
    # print(sumBinsBadRateLeadDiffAdd)
    #
    # print(sumBinsBadRateLeadDiffReduce)

    if (
      (sum(riskTableResultShape < 0)) == 0 &
      sumBinsBadRateLeadDiffAdd == nrow(riskTableResult[isSpecial == 'FALSE',]) - 1 &
      sumBinsBadRateLeadDiffReduce == 0
    ){

      riskTableResultShapeRes <- 'Monotonically decreasing'

    } else if(
      (sum(riskTableResultShape < 0)) == 0 &
      sumBinsBadRateLeadDiffAdd == 0 &
      sumBinsBadRateLeadDiffReduce == nrow(riskTableResult[isSpecial == 'FALSE',]) - 1
    ){

      riskTableResultShapeRes <- 'Monotonically increasing'

    } else if(
      (sum(riskTableResultShape < 0)) == 1 &
      riskTableResult[isSpecial == 'FALSE',][1,BinsBadRateLeadDiff] > 0
    ){

      riskTableResultShapeRes <- 'U shape'

    } else if(
      (sum(riskTableResultShape < 0)) == 1 &
      riskTableResult[isSpecial == 'FALSE',][1,BinsBadRateLeadDiff] < 0
    ){

      riskTableResultShapeRes <- 'inverted U shape'

    } else{

      riskTableResultShapeRes <- 'irregular'

    }

  }

  riskTableResult <- cbind(
    riskTableResult[
      , `:=`(
        BinsBadRateLead = NULL
        , BinsBadRateShift = NULL
        , BinsBadRateLeadDiff = NULL
      )
    ]
    , data.table(
      shape = riskTableResultShapeRes
    )
  )

  riskTableResult <- riskTableResult[
    , showCols
    , with = F
  ]

  return(riskTableResult)

}










