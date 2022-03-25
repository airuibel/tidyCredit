#' Title  binsEqual
#'
#' Equal frequency binning
#'
#' @param data
#' @param x
#' @param nbins
#' @param specialValue
#' @param ifSpecialNeeds
#' @param specialVarValue
#' @param ifSpecialVarNeeds
#'
#' @return Returns the binned values of equal frequency bins.
#'
#' @import data.table
#' @importFrom stats quantile
#'
#' @export
#'
#' @examples
binsEqual = function(
  data
  , x
  , nbins = 5
  , specialValue = NULL  # should be vector
  , ifSpecialNeeds = 0 # 全局特殊值是否纳入分箱计算
  , specialVarValue = NULL # should be list
  , ifSpecialVarNeeds = 0 # 变量特殊值是否纳入计算
) {

  ##  分箱数必须大于等于2

  stopifnot(nbins >= 2)

  data <- copy(data)

  setDT(data)

  x_ <- x

  ## 选取要计算的变量值和响应值

  data <- data[
    , .(x = get(x_))
  ]


  ## NaN处理

  if (
    length(
      intersect(c(NaN),data[ ,x])
    ) > 0
  ){

    data[is.nan(data)] <- NA

    # warning("The data contains a NaN field, it is recommended to replace it with a special value. By default, it will be replaced by Na, and it will appear in the Missing binning.")
  }

  if (is.null(specialValue)){

    data <- data[!(is.na(x))]

  } else if (is.null(specialValue) == F & ifSpecialNeeds == 0) {

    data <- data[!(is.na(x)) & !(x %in% specialValue)]

  } else if (is.null(specialValue) == F & ifSpecialNeeds == 1) {

    data <- data[!(is.na(x))]

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

  breaks <- unique(
    as.numeric(
      quantile(data[, x], probs = seq(0, 1, length.out = nbins + 1), na.rm = TRUE)
    )
  )

  breaksLength <- length(breaks)

  if (breaksLength == 1){ # #实际不会出现，单独做函数的时候使用。

    resultCuts <- NA

  } else{

    resultCuts <- sort(c(-Inf,breaks[2:(breaksLength - 1)],Inf))

    ##  判断Inf前一个值是不是最大值,是的话删除，用Inf代替

    if (
      resultCuts[breaksLength - 1] == max(data[, x],na.rm = T)
    ){
      resultCuts <- resultCuts[-(breaksLength - 1)]
    }

    ##  判断第二个值是不是最小值,是的话删除，用-Inf代替

    # print(resultCuts)

    if (
      resultCuts[2] == min(data[, x],na.rm = T)
    ){
      resultCuts <- resultCuts[-2]
    }

    # print(resultCuts)

  }

  return(unique(sort(resultCuts)))

}

