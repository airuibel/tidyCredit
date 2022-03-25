#' Title
#'
#' @param data
#' @param x
#' @param nbins
#' @param specialValue
#' @param ifSpecialNeeds
#' @param specialVarValue
#' @param ifSpecialVarNeeds
#'
#' @return
#'
#' @import data.table
#' @importFrom stats setNames cor qchisq kmeans quantile
#'
#' @export
#'
#' @examples
binsWidth = function(
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
      intersect(c(NaN),data[, x])
    ) > 0
  ){

    data[is.nan(data)] <- NA

    # warning("数据中包含NaN字段，建议将其用特殊值替代，默认将会按照Na进行替换，将会出现在Missing分箱中。")
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

      stop("ifSpecialVarNeeds必须是0和1之间任意一个值。")

    }

  }

  maxValue <- max(data[!(x %in% c(-Inf,Inf)), x] ,na.rm = T)

  minValue <- min(data[!(x %in% c(-Inf,Inf)), x] ,na.rm = T)

  xRange <- maxValue - minValue

  nbinsRange <- xRange / nbins

  nbinsWidthList <- list()

  for (nbinsWidth in 1:nbins){

    nbinsWidthList[[nbinsWidth]] <- minValue + nbinsRange * nbinsWidth

    if (
      nbinsWidth > 1
    ){

      if (
        nrow(
          data[
            x >= nbinsWidthList[[nbinsWidth - 1]] &
            x < nbinsWidthList[[nbinsWidth]],
          ]
        ) == 0
      ){

        nbinsWidthList[[nbinsWidth]] <- NULL

      }

    }

  }

  ##  删除初始值

  nbinsWidthList <- unique(unlist(nbinsWidthList))

  nbinsWidthList <- nbinsWidthList[nbinsWidthList > minValue & minValue < maxValue]

  if (
    nbinsWidthList[length(nbinsWidthList)] >= max(data[, x],na.rm = T)
  ){
    nbinsWidthList <- nbinsWidthList[-(length(nbinsWidthList))]
  }

  if (
    nbinsWidthList[1] <= min(data[, x],na.rm = T)
  ){
    nbinsWidthList <- nbinsWidthList[-1]
  }

  breaks = unique(sort(c(-Inf,nbinsWidthList,Inf)))

  return(breaks)

}
