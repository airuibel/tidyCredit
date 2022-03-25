##### naZero #####

naZero <- function(x){

  x[is.na(x)] <- 0

  return(x)

}

##### data.frame is.nan #####

is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))

##### data.frame is.infinite #####

is.infinite.data.frame <- function(x) do.call(cbind, lapply(x, is.infinite))

##### reciprocal #####

reciprocal <- function(x){
  # calculate the reciprocal
  y <- 1/x
  return(y)
}

##### ConstantCheck #####

ConstantCheck <- function(data){

  setDT(data)

  ConstantNum <- data[,lapply(.SD, function(x) length(unique(x)))]

  ConstantNumTab <-
    data.table(
      cnm = names(ConstantNum),
      ConstantNum = as.numeric(ConstantNum)
    )

  ConstantNumT <- ConstantNumTab[ConstantNum == 1, cnm]

  return(ConstantNumT)

}

##### NasCheck #####

NasCheck <- function(data){

  setDT(data)

  NasSum <-
    data[,lapply(.SD, function(x) sum(is.na(x)))]


  NasSumTab <-
    data.table(
      cnm = names(NasSum),
      Nassum = as.numeric(NasSum)
    )

  NasAllx <- NasSumTab[Nassum == nrow(data) ,cnm]

  return(NasAllx)

}

##### ChaCheck #####

ChaCheck <- function(data){

  setDT(data)

  ChaIf <- data[,lapply(.SD, function(x) is.numeric(x))]

  ChaNumTab <-
    data.table(
      cnm = names(ChaIf),
      ChaIf = as.character(ChaIf)
    )

  ChaNumT <- ChaNumTab[ChaIf == 'FALSE', cnm]

  return(ChaNumT)

}
