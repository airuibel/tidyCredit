#' Title transformResponse
#'
#' Y label replacement on raw data.
#'
#' @param data A data frame.
#' @param y Response value label column name.
#' @param responseSign Response value label name.Default is c('bad','1','no','response').
#' @param ifKeepOriY Whether the original Y label is saved, the default is not to save.
#'
#' @return Raw data add a column of Y label.
#'
#' @import data.table
#'
#' @export
#'
#' @examples
#' data("germancredit", package="scorecard")
#'
#' data <- germancredit
#'
#' newData <- transformResponse(data,'creditability')
#'
#'
#'
transformResponse <- function(
  data
  , y
  , responseSign = c('bad','1','no','response')
  , ifKeepOriY = FALSE
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

  if (length(unique(data[, get(y)])) != 2){

    stop('The Y value must be binary.')

  }

  y_ <- y

  # setdiff(names(data),y_)

  if (ifKeepOriY == F){
    data <- cbind(
      data
      , data[
        , .(y = get(y_))
      ][
        , y := fifelse(y %in% responseSign,1,0)
      ]
    )

    data <- data[,.SD,.SDcols = setdiff(names(data),y_)]

  } else {
    data <- cbind(
      data
      , data[
        , .(y = get(y_))
      ][
        , y := fifelse(y %in% responseSign,1,0)
      ]
    )
  }

  return(data)

}
