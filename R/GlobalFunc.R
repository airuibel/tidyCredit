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

#####  mdlp  #####

ent <- function(y){
    p <- table(y)/length(y)
    e <- -sum(p*mylog(p))
    return(e)
  }

mylog <- function(x){
    x[which(x<=1.0e-10)] <- 1
    return (log(x))
  }

cutIndex <- function(x,y){
    n <- length(y)
    # print(n)
    initEnt <- 9999
    entropy <- initEnt; ci <- NULL;
    for (i in 1:(n-1)){
      if(n == 1){
        break
      }else{
        if(is.na(x[i+1]) == F){
          # if(n == 1){
          # break
          if(x[i+1]!=x[i]) {
            ct <- (x[i]+x[i+1])/2
            wx <- which(x<=ct)
            wn <- length(wx)/n
            e1 <- wn*ent(y[wx])
            e2 <- (1-wn)*ent(y[-wx])
            val <- e1+e2
            if(val<entropy) {
              entropy <- val
              ci <- i
            }
          }
          # }

        }
      }


    }
    if(is.null(ci)) return(NULL)
    return (c(ci, entropy))
  }

cutPoints <- function(x,y,gainLimit){
    od <- order(x)
    xo <- x[od]
    yo <- y[od]
    depth <- 1

    gr <- function(low,upp,depth=depth){
      x <- xo[low:upp]
      y <- yo[low:upp]
      n <- length(y)
      ct <- cutIndex(x,y)
      if(is.null(ct)) return (NULL) ## when cut index=NULL
      ci <- ct[1]; entropy <- ct[2]
      ret <- mdlStop(ci,y,entropy,gainLimit) # MDL Stop
      if(is.null(ret)) return(NULL)
      return(c(ci,depth+1))
    }

    ## xo: original x in ascending order of x;
    ## yo: original y reordered in ascending order of x
    part <- function(low=1, upp=length(xo), cutPoints=NULL,depth=depth){
      x <- xo[low:upp]
      y <- yo[low:upp]
      n <- length(x)
      # if(n<2) return (cutPoints)
      cc <- gr(low, upp, depth=depth)
      ci <- cc[1]
      depth <- cc[2]
      if(is.null(ci)) return(cutPoints)
      cutPoints <- c(cutPoints,low+ci-1)
      cutPoints <- as.integer(sort(cutPoints))
      return(c(part(low, low+ci-1,cutPoints,depth=depth),
               part(low+ci,upp,cutPoints,depth=depth)))
    }

    res <- part(depth=depth)
    ci <- NULL ;cv <- numeric()
    if(!is.null(res)) {
      ci <- as.integer(res)
      cv <- (xo[ci]+xo[ci+1])/2
    }
    res <- unique(cv)## returns cutIndex and cutValues
    return(res)
  }


mdlStop <- function(ci,y,entropy,gainLimit){
    n <- length(y)
    es <- ent(y)
    left <- 1:ci; right <- (ci+1):n
    gain <- es-entropy
    l0 <- levels(factor(y))
    l1 <- levels(factor(y[left])); l2 <- levels(factor(y[right]))
    k <- length(l0)
    k1 <- length(l1); k2 <- length(l2)
    delta <- mylog(3^k-2)-(k*es-k1*ent(y[left])-k2*ent(y[right]))
    cond <- mylog(n-1)/n+delta/n
    # print(gain)
    # print(cond)
    # print(gainLimit)
    # if(gain<cond) return (NULL) # 否则会暂停较快
    if(gain<gainLimit) return (NULL)
    return(gain)
  }

mdlp <- function(data,y,x,gainLimit){

    data <- copy(data)

    setDT(data)

    y_ <- y

    x_ <- x

    ## 选取要计算的变量值和响应值

    data <- data[
      , .(y = get(y_),x = get(x_))
    ]

    cutp <- list()

    cuts1 <- cutPoints(data$x,data$y,gainLimit = gainLimit)

    cuts <- c(min(data$x,na.rm = T),cuts1,max(data$x,na.rm = T))

    if(length(cuts1)==0)cuts1 <- NULL

    # data[,x] <- as.integer(cut(data$x,cuts,include.lowest = TRUE))

    return (cuts1)
  }

