#' Title binningsChimerge
#'
#' Chi-square optimal binning function,A bottom-up approach to merging.
#'
#' @param data A data frame.
#' @param y Response value label column name.
#' @param x Variable value label column name.
#' @param alpha Chi-square test threshold.
#' @param limit The smallest proportion of bins.
#' @param showCols Column of results,The full column name is c('vars', 'bins', 'count', 'countRate', 'nmlCount', 'nmlCountCum', 'nmlCountCumRate', 'rpsCount', 'rpsCountCum', 'rpsCountCumRate', 'rpsRate', 'woe', 'iv', 'totalIv', 'totalIvAdj', 'binsLift', 'shape', 'splits', 'isSpecial')
#' @param breaksList Custom binning value, must be in list form.
#' @param minBinsNumInitial The minimum number of bins, the priority is above the limit parameter.
#' @param dataLimitCondition When using the limit parameter, how to choose the number of data sets, "all" or "DelNa" or "DelNaSpe" is optional.
#' @param responseSign Response value label name.Default is c('bad','1','no','response').
#' @param specialValue Values that need to be binned separately, which are globally valid for the data set.
#' @param ifSpecialNeeds Whether the global special value remains in the data when binning.
#' @param specialVarValue The value that needs to be binned separately, valid for the specified variable, must be a list.
#' @param ifSpecialVarNeeds Whether the special value of the variable dimension is included in the calculation when binning.
#' @param binsNumLimit The minimum number of bins is limited, does not include missing values, special values.
#' @param autoAdjust Whether to adjust automatically, automatic adjustment can adjust the final binning result to monotonous response rate or U-shaped.
#' @param ifMonotonous Whether the binning is monotonous, the final binning result must be monotonous and optimal, which is very effective for some situations, and it is not recommended to be used in conjunction with autoAdjust.
#' @param maxIterationLimit Regarding the maximum number of iterations for a single variable, some computers cannot withstand the limitation of too many iterations. In most cases, it is not necessary to set it.
#' @param supplyCl The number of cores in the parallel backend, considering that most of the analysis will be performed on the server, in order to avoid affecting others, half of the core number is used by default.
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
#' binsChi <- binningsChimerge(
#'  data = data
#'   , y = 'creditability'
#' )
#'
#' binsChiDt <- rbindlist(binsChi)
#'
#' ##  One
#'
#' binsChiOne <- binningsChimerge(
#'   data = data
#'   , y = 'creditability'
#'   , x = 'duration.in.month'
#' )
#'

binningsChimerge <- function(
  data
  , y
  , x = NULL
  , alpha = 0.1
  , limit = 0.05
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
  , breaksList = NULL
  , minBinsNumInitial = NULL # 一般建议将值设置在20，最大不超过全部数据行的1/10，minBinsNumInitial优先级更高
  , dataLimitCondition = 'all' #  all or DelNa or DelNaSpe 如果不含特殊值，DelNa和DelNaSpe结果一样
  , responseSign = c('bad','1','no','response')
  , specialValue = NULL  # should be vector
  , ifSpecialNeeds = 0 # 全局特殊值是否纳入分箱计算,类别型特殊值必须从全部样本中删除
  , specialVarValue = NULL # should be list
  , ifSpecialVarNeeds = 0 # 变量特殊值是否纳入计算,类别型特殊值必须从全部样本中删除
  , binsNumLimit = 8
  , autoAdjust = 0 # 是否自动调整分箱，自动调整将会让分箱单调或呈正"U"、倒"U"，不建议与ifMonotonous参数同用
  , ifMonotonous = 0 # 判断是否单调 0：单调；1：不单调；
  , maxIterationLimit = Inf # 最大迭代次数
  , supplyCl = NULL #  多线程的核心数,默认使用电脑一般核心数
){

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

    }else{

      dataNames <- names(data)[!(names(data) %in% y)]

    }

    ##  常数项变量删除

    ConstantNumT <- ConstantCheck(data)

    if (length(ConstantNumT) >= 1){

      dataNames <- dataNames[!(dataNames %in% c(y,ConstantNumT))]

      warning(paste0(paste0(ConstantNumT,collapse = ',')),' .These variables are constants!')

    }else{

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
      , .export = c(
        "riskTableCha"
        , "riskTableNum"
        , "chiMergeNum"
        , "chiMergeCha"
        , "reciprocal"
        , "is.nan.data.frame"
        , "is.infinite.data.frame"
      )
      , .packages = c("data.table")
      , .final = function(x) setNames(x, dataNames)
      # , .errorhandling = 'pass'
    ) %dopar%{

      p()

      if (is.numeric(data[,get(i)]) == F){

        riskTableCha(
          data = data
          , y = y
          , x = i
          , showCols = showCols
          , specialValue = specialValue
          , specialVarValue = specialVarValue
          , responseSign = responseSign
          , splitValue = chiMergeCha(
            data
            , y
            , i
            , alpha = alpha
            , limit = limit
            , breaksList = breaksList
            , minBinsNumInitial = minBinsNumInitial
            , dataLimitCondition = dataLimitCondition
            , responseSign = responseSign
            , specialValue = specialValue
            , ifSpecialNeeds = 0
            , specialVarValue = specialVarValue
            , ifSpecialVarNeeds = 0
            , binsNumLimit = binsNumLimit
            , maxIterationLimit = maxIterationLimit
          )
        )

      } else{

        riskTableNum(
          data = data
          , y = y
          , x = i
          , showCols = showCols
          , specialValue = specialValue
          , specialVarValue = specialVarValue
          , responseSign = responseSign
          , splitValue = chiMergeNum(
            data
            , y
            , i
            , alpha = alpha
            , limit = limit
            , breaksList = breaksList
            , minBinsNumInitial = minBinsNumInitial
            , dataLimitCondition = dataLimitCondition
            , responseSign = responseSign
            , specialValue = specialValue
            , ifSpecialNeeds = ifSpecialNeeds
            , specialVarValue = specialVarValue
            , ifSpecialVarNeeds = ifSpecialVarNeeds
            , binsNumLimit = binsNumLimit
            , autoAdjust = autoAdjust
            , ifMonotonous = ifMonotonous
            , maxIterationLimit = maxIterationLimit
          )
        )

      }

    }

    # on.exit(stopCluster(cl))
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

    if (is.numeric(data[,get(x)]) == F){

      riskTableList[[x]] <- riskTableCha(
        data = data
        , y = y
        , x = x
        , showCols = showCols
        , specialValue = specialValue
        , specialVarValue = specialVarValue
        , responseSign = responseSign
        , splitValue = chiMergeCha(
          data
          , y
          , x
          , alpha = alpha
          , limit = limit
          , breaksList = breaksList
          , minBinsNumInitial = minBinsNumInitial
          , dataLimitCondition = dataLimitCondition
          , responseSign = responseSign
          , specialValue = specialValue
          , ifSpecialNeeds = 0
          , specialVarValue = specialVarValue
          , ifSpecialVarNeeds = 0
          , binsNumLimit = binsNumLimit
          , maxIterationLimit = maxIterationLimit
        )
      )

    } else{

      riskTableList[[x]] <- riskTableNum(
        data = data
        , y = y
        , x = x
        , showCols = showCols
        , specialValue = specialValue
        , specialVarValue = specialVarValue
        , responseSign = responseSign
        , splitValue = chiMergeNum(
          data
          , y
          , x
          , alpha = alpha
          , limit = limit
          , breaksList = breaksList
          , minBinsNumInitial = minBinsNumInitial
          , dataLimitCondition = dataLimitCondition
          , responseSign = responseSign
          , specialValue = specialValue
          , ifSpecialNeeds = ifSpecialNeeds
          , specialVarValue = specialVarValue
          , ifSpecialVarNeeds = ifSpecialVarNeeds
          , binsNumLimit = binsNumLimit
          , autoAdjust = autoAdjust
          , ifMonotonous = ifMonotonous
          , maxIterationLimit = maxIterationLimit
        )
      )

    }

  }

  return(riskTableList)

}
