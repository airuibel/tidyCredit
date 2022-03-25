#' Title
#'
#' @param data A data frame.
#' @param y Response value label column name.
#' @param x Variable value label column name.
#' @param bins Variable binning, which can be a list or a data frame.
#' @param repClass It can only be converted to woe for the time being, and bins will be added later.
#' @param supplyCl The number of cores in the parallel backend, considering that most of the analysis will be performed on the server, in order to avoid affecting others, half of the core number is used by default.
#'
#' @return The data frame converted by woe encoding.
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
#'   data = data
#'   , y = 'creditability'
#' )
#'
#' woeData <- woeTrans(data = germancredit,y = 'creditability',bins = binsChi)
#'
#'
woeTrans <- function(
  data
  , y = NULL
  , x = NULL
  , bins
  , repClass = 'woe'
  , supplyCl = NULL #  多线程的核心数,默认使用电脑一般核心数
){

  handlers(global = TRUE)
  handlers("progress")

  on.exit(stopCluster(cl))

  ##  数据必须是数据框形式

  if (
    length(
      intersect(class(data),c('data.frame','data.table','tbl_df'))
    ) == 0
  ){

    stop("The data type must be a data frame class.")

  }else{

    setDT(data)

  }

  if (is.null(y) == F){

    y_ <- y

    ##  利用bins列表名唯一值判断需要转换的列

    data <- data[
      , c(names(bins),y_)
      , with = F
    ]

    dataNames <- names(data)[!(names(data) %in% y_)]

  }else{

    ##  利用bins列表名唯一值判断需要转换的列

    data <- data[
      , c(names(bins))
      , with = F
    ]

    dataNames <- names(data)

  }

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

    p <- progressor(along = 1:length(dataNames))

    resultDataTrans <- foreach(
      i = dataNames
      , .packages = c("data.table")
      , .final = function(x) setNames(x, dataNames)
      , .combine=cbind
    ) %dopar%{

      p()

      x_ <- i

      binsi <- bins[[x_]]

      # datai <- data[,x_,with=FALSE][,lapply(.SD,as.numeric)][, rowid := .I]

      if (is.numeric(data[,get(x_)])){

        datai <- data[,x_,with = F][,lapply(.SD,as.numeric)][, rowid := .I]

        specialValue <- binsi[isSpecial == 'TRUE' & bins != 'Missing',splits]

        missValue <- binsi[isSpecial == 'TRUE' & bins == 'Missing',splits]

        woemissing <- function(data,bins){

          missData <- data[is.na(get(x_)) == T,]

          # missData[,get(x_) := bins[splits == "Missing",woe]]

          missData[,(x_) := bins[splits == 'Missing',woe]]

          # missData <- bins[splits == "Missing",woe]

          return(missData)

        }

        missingDataTrans <- woemissing(datai,binsi)

        woespecial <- function(data,bins){

          specialData <- data[get(x_) %in% specialValue,]

          specialBins <- bins[isSpecial == 'TRUE' & bins != 'Missing',]

          for (specialI in 1:length(specialValue)){

            specialData[get(x_) == specialValue[specialI],(x_) := specialBins[splits == specialValue[specialI],woe]]

          }

          return(specialData)

        }

        specialDataTrans <- woespecial(datai,binsi)

        woenormal <- function(data,bins){

          normalData <- data[is.na(get(x_)) == F & !(get(x_) %in% specialValue)]

          normalBins <- bins[isSpecial == 'FALSE',]

          normalBins[,splitsShift := shift(normalBins[,splits],n=1L,fill = -Inf,type = 'shift')]

          normalDataRes <- data.table()

          for (normalI in 1:nrow(normalBins)){

            normalDataRes <- rbind(
              normalDataRes
              , normalData[
                get(x_) >= as.numeric(normalBins[normalI,splitsShift])
                & get(x_) < as.numeric(normalBins[normalI,splits])
                , .(temName = normalBins[normalI,woe],rowid = rowid)
              ]
            )

          }

          setnames(normalDataRes,'temName',x_)

          return(normalDataRes)

        }

        normalDataTrans <- woenormal(datai,binsi)

      } else if (is.character(data[,get(x_)]) | is.factor(data[,get(x_)])) {

        # if (sapply(datai,class)[[1]] == 'factor'){
        #
        #   datai <- datai[,x_,with = F][,lapply(.SD,as.character)][, rowid := .I]
        #
        # }

        datai <- data[,x_,with = F][,lapply(.SD,as.character)][, rowid := .I]

        specialValue <- binsi[isSpecial == 'TRUE' & bins != 'Missing',splits]

        missValue <- binsi[isSpecial == 'TRUE' & bins == 'Missing',splits]

        woemissing <- function(data,bins){

          missData <- data[is.na(get(x_)) == T | get(x_) == '',]

          # missData[,get(x_) := bins[splits == "Missing",woe]]

          missData[,(x_) := bins[splits == 'Missing',woe]]

          # missData <- bins[splits == "Missing",woe]

          return(missData)

        }

        missingDataTrans <- woemissing(datai,binsi)

        woespecial <- function(data,bins){

          specialData <- data[get(x_) %in% specialValue,]

          specialBins <- bins[isSpecial == 'TRUE' & bins != 'Missing',]

          if (nrow(specialData) != 0){

            for (specialI in 1:length(specialValue)){

              specialData[
                get(x_) == specialValue[specialI]
                ,(x_) := specialBins[splits == specialValue[specialI],woe]
              ]

            }

          }

          return(specialData)

        }

        specialDataTrans <- woespecial(datai,binsi)

        woenormal <- function(data,bins){

          normalData <- data[is.na(get(x_)) == F & !(get(x_) %in% specialValue)]

          normalBins <- bins[isSpecial == 'FALSE',]

          splitTable <- data.table(
            splitValue = as.character(bins[isSpecial == 'FALSE',bins])
            , splitValueVec = strsplit(as.character(bins[isSpecial == 'FALSE',bins]),"<,>")
            , woe = as.numeric(bins[isSpecial == 'FALSE',woe])
          )

          normalDataRes <- data.table()

          for (normalI in 1:nrow(normalBins)){

            normalDataRes <- rbind(
              normalDataRes
              , normalData[
                get(x_) %in% splitTable[normalI,splitValueVec][[1]]
                , .(temName = splitTable[normalI,woe][[1]],rowid = rowid)
              ]
            )

          }

          setnames(normalDataRes,'temName',x_)

          return(normalDataRes)

        }

        normalDataTrans <- woenormal(datai,binsi)

      }

      if (length(missValue) != 0 & length(specialValue) != 0){

        resultDataTrans <- rbind(
          missingDataTrans
          , specialDataTrans
          , normalDataTrans
        )

        resultDataTrans <- resultDataTrans[order(rowid)][,rowid := NULL][]

      }else if (length(missValue) != 0 & length(specialValue) == 0){

        resultDataTrans <- rbind(
          missingDataTrans
          , normalDataTrans
        )

        resultDataTrans <- resultDataTrans[order(rowid)][,rowid := NULL][]

      }else if (length(missValue) == 0 & length(specialValue) != 0){

        resultDataTrans <- rbind(
          specialDataTrans
          , normalDataTrans
        )

        resultDataTrans <- resultDataTrans[order(rowid)][,rowid := NULL][]

      }else if (length(missValue) == 0 & length(specialValue) == 0){

        resultDataTrans <- normalDataTrans

        resultDataTrans <- resultDataTrans[order(rowid)][,rowid := NULL][]

      }else{

        stop("The data conversion is abnormal, there is a need to deal with the BUG.")

      }
    }

  }else{

    cl <- makeCluster(1)

    plan(cluster, workers = cl,earlySignal = TRUE)

    x_ <- x

    binsi <- bins[[x_]]

    # datai <- data[,x_,with=FALSE][,lapply(.SD,as.numeric)][, rowid := .I]

    ## missingdata

    if (is.numeric(data[,get(x_)])){

      datai <- data[,x_,with = F][,lapply(.SD,as.numeric)][, rowid := .I]

      specialValue <- binsi[isSpecial == 'TRUE' & bins != 'Missing',splits]

      missValue <- binsi[isSpecial == 'TRUE' & bins == 'Missing',splits]

      woemissing <- function(data,bins){

        missData <- data[is.na(get(x_)) == T,]

        # missData[,get(x_) := bins[splits == "Missing",woe]]

        missData[,(x_) := bins[splits == 'Missing',woe]]

        # missData <- bins[splits == "Missing",woe]

        return(missData)

      }

      missingDataTrans <- woemissing(datai,binsi)

      woespecial <- function(data,bins){

        specialData <- data[get(x_) %in% specialValue,]

        specialBins <- bins[isSpecial == 'TRUE' & bins != 'Missing',]

        for (specialI in 1:length(specialValue)){

          specialData[get(x_) == specialValue[specialI],(x_) := specialBins[splits == specialValue[specialI],woe]]

        }

        return(specialData)

      }

      specialDataTrans <- woespecial(datai,binsi)

      woenormal <- function(data,bins){

        normalData <- data[is.na(get(x_)) == F & !(get(x_) %in% specialValue)]

        normalBins <- bins[isSpecial == 'FALSE',]

        normalBins[,splitsShift := shift(normalBins[,splits],n=1L,fill = -Inf,type = 'shift')]

        normalDataRes <- data.table()

        for (normalI in 1:nrow(normalBins)){

          normalDataRes <- rbind(
            normalDataRes
            , normalData[
              get(x_) >= as.numeric(normalBins[normalI,splitsShift])
              & get(x_) < as.numeric(normalBins[normalI,splits])
              , .(temName = normalBins[normalI,woe],rowid = rowid)
            ]
          )

        }

        setnames(normalDataRes,'temName',x_)

        return(normalDataRes)
      }

      normalDataTrans <- woenormal(datai,binsi)

    } else if (is.character(data[,get(x_)]) | is.factor(data[,get(x_)])){

      # if (sapply(datai,class)[[1]] == 'factor'){
      #
      #   datai <- datai[,x_,with = F][,lapply(.SD,as.character)][, rowid := .I]
      #
      # }

      datai <- data[,x_,with = F][,lapply(.SD,as.character)][, rowid := .I]

      specialValue <- binsi[isSpecial == 'TRUE' & bins != 'Missing',splits]

      missValue <- binsi[isSpecial == 'TRUE' & bins == 'Missing',splits]

      woemissing <- function(data,bins){

        missData <- data[is.na(get(x_)) == T | get(x_) == '',]

        # missData[,get(x_) := bins[splits == "Missing",woe]]

        missData[,(x_) := bins[splits == 'Missing',woe]]

        # missData <- bins[splits == "Missing",woe]

        return(missData)

      }

      missingDataTrans <- woemissing(datai,binsi)

      woespecial <- function(data,bins){

        specialData <- data[get(x_) %in% specialValue,]

        specialBins <- bins[isSpecial == 'TRUE' & bins != 'Missing',]

        if (nrow(specialData) != 0){

          for (specialI in 1:length(specialValue)){

            specialData[
              get(x_) == specialValue[specialI]
              ,(x_) := specialBins[splits == specialValue[specialI],woe]
            ]

          }

        }

        return(specialData)

      }

      specialDataTrans <- woespecial(datai,binsi)

      woenormal <- function(data,bins){

        normalData <- data[is.na(get(x_)) == F & !(get(x_) %in% specialValue)]

        normalBins <- bins[isSpecial == 'FALSE',]

        splitTable <- data.table(
          splitValue = as.character(bins[isSpecial == 'FALSE',bins])
          , splitValueVec = strsplit(as.character(bins[isSpecial == 'FALSE',bins]),"<,>")
          , woe = as.numeric(bins[isSpecial == 'FALSE',woe])
        )

        normalDataRes <- data.table()

        for (normalI in 1:nrow(normalBins)){

          normalDataRes <- rbind(
            normalDataRes
            , normalData[
              get(x_) %in% splitTable[normalI,splitValueVec][[1]]
              , .(temName = splitTable[normalI,woe][[1]],rowid = rowid)
            ]
          )

        }

        setnames(normalDataRes,'temName',x_)

        return(normalDataRes)

      }

      normalDataTrans <- woenormal(datai,binsi)

    }

    if (length(missValue) != 0 & length(specialValue) != 0){

      # print(1)

      resultDataTrans <- rbind(
        missingDataTrans
        , specialDataTrans
        , normalDataTrans
      )

      resultDataTrans <- resultDataTrans[order(rowid)][,rowid := NULL][]

    }else if (length(missValue) != 0 & length(specialValue) == 0){

      # print(2)

      resultDataTrans <- rbind(
        missingDataTrans
        , normalDataTrans
      )

      resultDataTrans <- resultDataTrans[order(rowid)][,rowid := NULL][]

    }else if (length(missValue) == 0 & length(specialValue) != 0){

      # print(3)

      resultDataTrans <- rbind(
        specialDataTrans
        , normalDataTrans
      )

      resultDataTrans <- resultDataTrans[order(rowid)][,rowid := NULL][]

    }else if (length(missValue) == 0 & length(specialValue) == 0){

      # print(4)

      resultDataTrans <- normalDataTrans

      resultDataTrans <- resultDataTrans[order(rowid)][,rowid := NULL][]

    }else{

      stop("The data conversion is abnormal, there is a need to deal with the BUG.")

    }

  }

  if (is.null(y) == F){

    resultDataTrans <- cbind(
      resultDataTrans[,lapply(.SD,as.numeric)]
      , data[,y,with = FALSE]
    )

  }else{

    resultDataTrans <- resultDataTrans[,lapply(.SD,as.numeric)]

  }

  return(resultDataTrans)

}

