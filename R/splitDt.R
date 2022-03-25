#' Title
#'
#' @param data A data frame.
#' @param ratio The ratio of the two datasets before and after.
#' @param dataNames The names of the two datasets before and after.
#' @param ifKeepI Whether to keep the row count index of the original row.
#' @param seed Random seed.
#' @param kFold K-fold cross split, the priority is the highest, if there is a value, the previous parameters are ignored.
#' @param kFoldRate K-fold cross ratio.
#'
#' @return A list containing the training set, test set, or k-fold crossover data.
#'
#' @import data.table
#'
#' @export
#'
#' @examples
#'
#' library(data.table)
#'
#' data("germancredit", package="scorecard")
#'
#' data <- germancredit
#'
#' useData <- splitDt(data)
#'
#' useDataKfold <- splitDt(data,kFold = 5,kFoldRate = 0.75)
#'
splitDt <- function(
  data
  , ratio = c(0.7,0.3)
  , dataNames = c('train','test')
  , ifKeepI = FALSE
  , seed = 520
  , kFold = NULL    ##  优先级最高
  , kFoldRate = 0.8
){

  if (
    length(
      intersect(class(data),c('data.frame','data.table','tbl_df'))
    ) == 0
  ){

    stop("The data type must be a data frame class.")

  }else{

    data <- copy(setDT(data))

  }

  set.seed(seed)

  data[
    , `:=`(
      rowI = .I
    )
  ]

  if (is.null(kFold)){

    modelData <- list()

    sam <- sample(nrow(data),nrow(data) * ratio[1])

    if (ifKeepI == F){

      data[,rowI := NULL]

      modelData[[dataNames[1]]] <- data[sam,]

      modelData[[dataNames[2]]] <- data[-sam,]
    }else{
      modelData[[dataNames[1]]] <- data[sam,]

      modelData[[dataNames[2]]] <- data[-sam,]
    }



  } else {


    modelDataK <- list()
    modelData <- list()

    if (ifKeepI == F){
      data[,rowI := NULL]
    }

    for (i in 1:kFold){

      modelDataK[[i]] <- sample(nrow(data),nrow(data) * kFoldRate)
      modelData[[shQuote(i)]] <- data[modelDataK[[i]]]
    }

  }

  return(modelData)

}
