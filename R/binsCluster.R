#' Title
#'
#' @param data
#' @param x
#' @param nbins
#' @param method
#' @param specialValue
#' @param ifSpecialNeeds
#' @param specialVarValue
#' @param ifSpecialVarNeeds
#'
#' @return
#'
#' @import data.table mclust cluster
#' @importFrom stats kmeans
#' @importFrom mclust Mclust
#' @importFrom cluster pam
#'
#' @export
#'
#' @examples
binsCluster = function(
  data
  , x
  , nbins = 5
  , method = 'K-Medoids'  # kmeans、K-Medoids、EM算法可选
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

  ##

  if (method == 'kmeans'){

    tryCatch(
      {

        kmeans(as.matrix(data),centers = nbins - 1,iter.max = 20,nstart = 10)

      }, error = function(err.msg){

        stop(
          'Try the K-Medoids algorithm.'
        )

      }

    )

    km_cuts <- kmeans(as.matrix(data),centers = nbins - 1,iter.max = 20,nstart = 10)

    km_cuts <- sort(unique(km_cuts$centers))

    if (
      km_cuts[length(km_cuts)] >= max(data[, x],na.rm = T)
    ){
      km_cuts <- km_cuts[-(length(km_cuts))]
    }

    if (
      km_cuts[1] <= min(data[, x],na.rm = T)
    ){
      km_cuts <- km_cuts[-1]
    }

    return(
      unique(
        sort(
          c(-Inf,km_cuts,Inf)
        )
      )
    )

  }else if (method == 'K-Medoids'){

    pam_cuts <- pam(as.matrix(data),nbins - 1)

    pam_cuts <- unique(sort(pam_cuts$medoids))

    if (
      pam_cuts[length(pam_cuts)] >= max(data[, x],na.rm = T)
    ){
      pam_cuts <- pam_cuts[-(length(pam_cuts))]
    }

    if (
      pam_cuts[1] <= min(data[, x],na.rm = T)
    ){
      pam_cuts <- pam_cuts[-1]
    }

    return(
      unique(
        sort(c(-Inf,pam_cuts,Inf))
      )
    )

  }else if (method == 'EM'){

    em_cuts <- Mclust(as.matrix(data),G = nbins - 1,verbose = F)

    if (is.null(em_cuts)){

      em_cuts <- c(-Inf,Inf)

    } else{

      em_cuts <- em_cuts$parameters$mean[unique(em_cuts$classification)]

      if (
        em_cuts[length(em_cuts)] >= max(data[, x],na.rm = T)
      ){
        em_cuts <- em_cuts[-(length(em_cuts))]
      }

      if (
        em_cuts[1] <= min(data[, x],na.rm = T)
      ){
        em_cuts <- em_cuts[-1]
      }

    }

    return(
      unique(
        sort(
          c(-Inf,em_cuts,Inf)
        )
      )
    )

  }

}
