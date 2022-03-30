#' Title binningsWidth
#'
#' Equally spaced binning.
#'
#' @param data A data frame.
#' @param y Response value label column name.
#' @param x Variable value label column name.
#' @param nbins Number of bins.
#' @param showCols Column of results,The full column name is c('vars', 'bins', 'count', 'countRate', 'nmlCount', 'nmlCountCum', 'nmlCountCumRate', 'rpsCount', 'rpsCountCum', 'rpsCountCumRate', 'rpsRate', 'woe', 'iv', 'totalIv', 'totalIvAdj', 'binsLift', 'shape', 'splits', 'isSpecial')
#' @param responseSign Response value label name.Default is c('bad','1','no','response').
#' @param specialValue Values that need to be binned separately, which are globally valid for the data set.
#' @param ifSpecialNeeds Whether the global special value remains in the data when binning.
#' @param specialVarValue The value that needs to be binned separately, valid for the specified variable, must be a list.
#' @param ifSpecialVarNeeds Whether the special value of the variable dimension is included in the calculation when binning.
#' @param supplyCl The number of cores in the parallel backend, considering that most of the analysis will be performed on the server, in order to avoid affecting others, half of the core number is used by default.
#'
#'
#' @return A binned list of all variables.
#'
#' @import data.table foreach doFuture future parallel progressr
#' @importFrom stats setNames cor qchisq
#' @importFrom doFuture registerDoFuture
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom future plan cluster
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
#' ##  ALL
#'
#' binsWidth <- binningsWidth(
#'   data = data[,c(2,5,13,21)]
#'   , y = 'creditability'
#' )
#'
#' binsWidthDt <- rbindlist(binsWidth)
#'
#' ##  One
#'
#' binsWidthOne <- binningsWidth(
#'   data = data[,c(2,5,13,21)]
#'   , y = 'creditability'
#'   , x = 'duration.in.month'
#' )
binningsWidth <- function(
  data
  , y
  , x = NULL
  , nbins = 5
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
  , responseSign = c('bad','1','no','坏','response')
  , specialValue = NULL  # should be vector
  , ifSpecialNeeds = 0 # 全局特殊值是否纳入分箱计算
  , specialVarValue = NULL # should be list
  , ifSpecialVarNeeds = 0 # 变量特殊值是否纳入计算
  , supplyCl = NULL #  多线程的核心数,默认使用电脑一般核心数
){

  options(future.globals.maxSize = Inf)

  handlers(global = TRUE)
  handlers("progress")

  on.exit(stopCluster(cl))

  riskTableList <- list()

  if (is.null(x)){

    ##  确认线程核心数

    cores = detectCores()

    registerDoFuture()

    if (is.null(supplyCl)){

      cl <- makeCluster(cores)

      plan(cluster, workers = cl,earlySignal = TRUE)

    } else if (supplyCl > cores){

      cl <- makeCluster(cores)

      plan(cluster, workers = cl,earlySignal = TRUE)

      warning(paste0("supplyCl exceeds the maximum number of cores available, the maximum number of cores for this computer is：",cores))

    } else {

      # print(1)

      cl <- makeCluster(supplyCl)

      plan(cluster, workers = cl,earlySignal = TRUE)

    }


    ##  数据必须是数据框形式

    if (
      length(
        intersect(class(data),c('data.frame','data.table','tbl_df'))
      ) == 0
    ){

      # stop("数据类型必须是数据框类。")
      stop("The data type must be a data frame class.")

    }else{

      setDT(data)

    }

    ##  全部缺失变量剔除

    NasAllx <- NasCheck(data)

    if (length(NasAllx) >= 1){

      dataNames <- names(data)[!(names(data) %in% c(y,NasAllx))]

      warning(paste0(paste0(NasAllx,collapse = ',')),' .These variables are all empty values!')

    } else{

      dataNames <- names(data)[!(names(data) %in% y)]

    }

    ##  常数项变量删除

    ConstantNumT <- ConstantCheck(data)

    if (length(ConstantNumT) >= 1){

      dataNames <- dataNames[!(dataNames %in% c(y,ConstantNumT))]

      warning(paste0(paste0(ConstantNumT,collapse = ',')),' .These variables are constants!')

    } else{

      dataNames <- dataNames

    }

    ##  字符型变量删除

    ChaCheckNames <- ChaCheck(data[,-y,with = F])

    if (length(ChaCheckNames) >= 1){

      dataNames <- dataNames[!(dataNames %in% c(y,ChaCheckNames))]

      warning(
        paste0(paste0(setdiff(ChaCheckNames,y),collapse = ',')),
        ' .These variables are all character variables,
        this function only supports binning of numeric variables'
      )

    } else{

      dataNames <- dataNames

    }

    ##  NaN处理

    NaNSum <- sum(is.nan(data))

    if (NaNSum > 0){

      # warning("数据中包含NaN字段，建议将其用特殊值替代，默认将会按照Na进行替换，将会出现在Missing分箱中。")
      warning("The data contains a NaN field, it is recommended to replace it with a special value. By default, it will be replaced by Na, and it will appear in the Missing binning.")

    }

    ##  Inf,-Inf处理

    InfSum <- sum(is.infinite(data))

    if (InfSum > 0){

      # warning("数据中包含-Inf,Inf字段，建议将其用特殊值替代，默认将会按照数值进行处理。")
      warning("The data contains -Inf, Inf fields, it is recommended to replace it with a special value, which will be processed according to the value by default.")

    }

    ##  foreach循环

    p <- progressor(along = 1:length(dataNames))

    riskTableList <- foreach(
      i = dataNames
      , .export = c("riskTableNum","binsWidth","is.nan.data.frame","is.infinite.data.frame")
      , .packages = c("data.table")
      , .final = function(x) setNames(x, dataNames)
    ) %dopar%{

      p()

      riskTableNum(
        data = data
        , y = y
        , x = i
        , showCols = showCols
        , specialValue = specialValue
        , specialVarValue = specialVarValue
        , responseSign = responseSign
        , splitValue = binsWidth(
          data
          , i
          , nbins = nbins
          , specialValue = specialValue  # should be vector
          , ifSpecialNeeds = ifSpecialNeeds # 全局特殊值是否纳入分箱计算
          , specialVarValue = specialVarValue # should be list
          , ifSpecialVarNeeds = ifSpecialVarNeeds # 变量特殊值是否纳入计算
        )
      )

    }

    # stopCluster(cl)

  }else {

    cl <- makeCluster(1)

    plan(cluster, workers = cl,earlySignal = TRUE)

    ##  数据必须是数据框形式

    if (
      length(
        intersect(class(data),c('data.frame','data.table','tbl_df'))
      ) == 0
    ){

      # stop("数据类型必须是数据框类。")
      stop("The data type must be a data frame class.")

    }else{

      setDT(data)

    }

    ##  全部缺失变量剔除

    NasAllx <- NasCheck(data.table(data[,get(x)]))

    if (length(NasAllx) >= 1){

      dataNames <- names(data)[!(names(data) %in% c(y,NasAllx))]

      warning(paste0(paste0(NasAllx,collapse = ',')),' .These variables are all empty values!')

    }else{

      dataNames <- names(data)[!(names(data) %in% y)]

    }

    ##  常数项变量删除

    ConstantNumT <- ConstantCheck(data.table(data[,get(x)]))

    if (length(ConstantNumT) >= 1){

      dataNames <- dataNames[!(dataNames %in% c(y,ConstantNumT))]

      warning(paste0(paste0(ConstantNumT,collapse = ',')),' .These variables are constants!')

    }else{

      dataNames <- dataNames

    }

    ##  字符型变量删除

    ChaCheckNames <- ChaCheck(data.table(data[,get(x)]))

    if (length(ChaCheckNames) >= 1){

      dataNames <- dataNames[!(dataNames %in% c(y,ChaCheckNames))]

      warning(
        paste0(paste0(setdiff(ChaCheckNames,y),collapse = ',')),
        ' .These variables are all character variables,
        this function only supports binning of numeric variables'
      )

    } else{

      dataNames <- dataNames

    }

    ##  NaN处理

    NaNSum <- sum(is.nan(data.table(data[,get(x)])))

    if (NaNSum > 0){

      # warning("数据中包含NaN字段，建议将其用特殊值替代，默认将会按照Na进行替换，将会出现在Missing分箱中。")
      warning("The data contains a NaN field, it is recommended to replace it with a special value. By default, it will be replaced by Na, and it will appear in the Missing binning.")

    }

    ##  Inf,-Inf处理

    InfSum <- sum(is.infinite(data.table(data[,get(x)])))

    if (InfSum > 0){

      # warning("数据中包含-Inf,Inf字段，建议将其用特殊值替代，默认将会按照数值进行处理。")
      warning("The data contains -Inf, Inf fields, it is recommended to replace it with a special value, which will be processed according to the value by default.")

    }

    riskTableList[[x]] <- riskTableNum(
      data = data
      , y = y
      , x = x
      , showCols = showCols
      , specialValue = specialValue
      , specialVarValue = specialVarValue
      , responseSign = responseSign
      , splitValue = binsWidth(
        data
        , x
        , nbins = nbins
        , specialValue = specialValue  # should be vector
        , ifSpecialNeeds = ifSpecialNeeds # 全局特殊值是否纳入分箱计算
        , specialVarValue = specialVarValue # should be list
        , ifSpecialVarNeeds = ifSpecialVarNeeds # 变量特殊值是否纳入计算
      )
    )

  }

  return(riskTableList)

}
