#' Title describeData
#'
#' Basic data statistics.
#'
#' @param data A data frame.
#' @param nBins Number of bins for small graphics.
#'
#' @return Basic data statistics
#'
#'
#' @import data.table
#' @importFrom stats setNames cor qchisq kmeans quantile sd median
#'
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
#' baseSta <- describeData(data)
#' baseSta
#'

describeData <- function(data,nBins = 8){

  spark_bar <- function(x, safe = TRUE) {
    stopifnot(is.numeric(x))

    bars <- vapply(0x2581:0x2588, intToUtf8, character(1))
    if (safe) {
      bars <- bars[-c(4, 8)]
    }

    factor <- cut(
      x,
      breaks = seq(0, 1, length.out = length(bars) + 1),
      labels = bars,
      include.lowest = TRUE
    )
    chars <- as.character(factor)
    chars[is.na(chars)] <- bars[length(bars)]
    paste0(chars, collapse = "")
  }

  inline_hist <- function(x, nBins) {
    # For the purposes of the histogram, treat infinite as NA
    # (before the test for all NA)
    if (any(is.infinite(x))) {
      x[is.infinite(x)] <- NA
      warning(
        "Variable contains Inf or -Inf value(s) that were converted to NA."
      )
    }

    # Handle empty and NA vectors (is.na is TRUE for NaN)
    if (length(x) < 1 || all(is.na(x))) {
      return(" ")
    }

    # Addresses a known bug in cut()
    if (all(x == 0, na.rm = TRUE)) x <- x + 1
    hist_dt <- table(cut(x, nBins))
    hist_dt <- hist_dt / max(hist_dt)
    spark_bar(hist_dt)
  }

  setDT(data)

  t1 <- data.table(t(data[
    ,.SD,.SDcols = is.numeric
  ][
    ,lapply(
      .SD
      , function(x) list(
        dataClass = class(x)
        , dataCount = .N
        , dataMissC = sum(is.na(x))
        , dataMissR = sum(is.na(x))/.N
        , dataIdenticalC = length(unique(x))
        , dataIdenticalR = length(unique(x)) / .N
        , dataMean = mean(x,na.rm = T)
        , dataMedian = median(x,na.rm = T)
        , dataSd = sd(x,na.rm = T)
        , dataMin = min(x,na.rm = T)
        , dataMax = max(x,na.rm = T)
        , dataP25 = as.numeric(quantile(x,prob = 0.25,na.rm = T))
        , dataP50 = as.numeric(quantile(x,prob = 0.50,na.rm = T))
        , dataP75 = as.numeric(quantile(x,prob = 0.75,na.rm = T))
        # , dataIfnorm = fifelse(shapiro.test(x)$p.value > 0.05,1,0)
        , dataHist = inline_hist(x,nBins = nBins)
      )
    )
    # ,by = .SD
  ]),keep.rownames = T)

  if (nrow(t1) > 0){

    names(t1) <- c(
      'varsNames','dataClass'
      , 'dataCount','dataMissC','dataMissR'
      , 'dataIdenticalC','dataIdenticalR'
      , 'dataMean','dataMedian','dataSd','dataMin','dataMax'
      , 'dataP25','dataP50','dataP75'
      , 'dataHist'
    )
  }

  t2 <- data.table(t(data[
    ,.SD,.SDcols = is.character
  ][
    ,lapply(
      .SD
      , function(x) list(
        dataClass = class(x)
        , dataCount = .N
        , dataMissC = sum(is.na(x))
        , dataMissR = sum(is.na(x))/.N
        , dataIdenticalC = length(unique(x))
        , dataIdenticalR = length(unique(x)) / .N
      )
    )
    # ,by = .SD
  ]),keep.rownames = T)

  if (nrow(t2) > 0){

    names(t2) <- c(
      'varsNames','dataClass','dataCount','dataMissC','dataMissR'
      ,'dataIdenticalC','dataIdenticalR'
    )

    # t2 <- data.table(apply(t2,2,function(x) as.character(x)))

    # t2 <- t2[,lapply(.SD, as.character)]

  }

  t3 <- data.table(t(data[
    ,.SD,.SDcols = is.factor
  ][
    ,lapply(
      .SD
      , function(x) list(
        dataClass = class(x)
        , dataCount = .N
        , dataMissC = sum(is.na(x))
        , dataMissR = sum(is.na(x))/.N
        , dataIdenticalC = length(unique(x))
        , dataIdenticalR = length(unique(x)) / .N
        , dataLevels = paste(levels(x),collapse="<,>")
      )
    )
    # ,by = .SD
  ]),keep.rownames = T)

  if (nrow(t3) > 0){

    names(t3) <- c(
      'varsNames','dataClass','dataCount','dataMissC','dataMissR'
      ,'dataIdenticalC','dataIdenticalR','dataLevels'
    )

    # t3 <- t3[,lapply(.SD, as.character)]

  }

  return(
    list(
      dataNumeric = t1,
      dataCharacter = t2,
      dataFactor = t3
    )
  )

}
