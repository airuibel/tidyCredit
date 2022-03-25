riskFormFunc <- function(data,ord = 'desc'){

  if (ord == 'desc'){

    naZero(
      dcast(
        data[
          , .N
          , by = .(y,predCut)
        ]
        , predCut ~ y
        , value.var = 'N'
      )
    )[
      order(-predCut)
    ][
      , `:=`(
        count = bad + good
        , nmlCount = good
        , rpsCount = bad
      )
    ][
      ,`:=`(
        countRate = round(count / sum(count),4)
        , nmlCountCum = cumsum(nmlCount)
        , rpsCountCum = cumsum(rpsCount)
      )
    ][
      ,`:=` (
        countRateCum = cumsum(countRate)
        , nmlCountCumRate = round(nmlCountCum / max(nmlCountCum),4)
        , rpsCountCumRate = round(rpsCountCum / max(rpsCountCum),4)
      )
    ][
      ,`:=`(
        rpsRate = rpsCount / (rpsCount + nmlCount)
        , woe =
          round(
            log(
              (rpsCount / sum(rpsCount) + 1e-10) /
                (nmlCount / sum(nmlCount) + 1e-10)
            ), 4
          )
        , iv = round(
          (rpsCount / sum(rpsCount) - nmlCount / sum(nmlCount)) *
            log(
              (rpsCount / sum(rpsCount) + 1e-10) /
                (nmlCount / sum(nmlCount) + 1e-10)
            )
          , 4
        )
        , totalIv = round(
          sum(
            (rpsCount / sum(rpsCount) - nmlCount / sum(nmlCount)) *
              log((rpsCount / sum(rpsCount) + 1e-10) / (nmlCount / sum(nmlCount) + 1e-10))
          )
          , 4
        )
        , binsLift = round(
          (rpsCount / (rpsCount + nmlCount)) / (
            (max(rpsCountCum) / (max(nmlCountCum) + max(rpsCountCum)))  + 1e-10
          )
          , 4
        )
        , accumLift = round(
          rpsCountCumRate / (countRateCum)
          , 4
        )
      )
    ][]

  } else if(ord == 'asc'){

    naZero(
      dcast(
        data[
          , .N
          , by = .(y,predCut)
        ]
        , predCut ~ y
        , value.var = 'N'
      )
    )[
      order(predCut)
    ][
      , `:=`(
        count = bad + good
        , nmlCount = good
        , rpsCount = bad
      )
    ][
      ,`:=`(
        countRate = round(count / sum(count),4)
        , nmlCountCum = cumsum(nmlCount)
        , rpsCountCum = cumsum(rpsCount)
      )
    ][
      ,`:=` (
        countRateCum = cumsum(countRate)
        , nmlCountCumRate = round(nmlCountCum / max(nmlCountCum),4)
        , rpsCountCumRate = round(rpsCountCum / max(rpsCountCum),4)
      )
    ][
      ,`:=`(
        rpsRate = rpsCount / (rpsCount + nmlCount)
        , woe =
          round(
            log(
              (rpsCount / sum(rpsCount) + 1e-10) /
                (nmlCount / sum(nmlCount) + 1e-10)
            ), 4
          )
        , iv = round(
          (rpsCount / sum(rpsCount) - nmlCount / sum(nmlCount)) *
            log(
              (rpsCount / sum(rpsCount) + 1e-10) /
                (nmlCount / sum(nmlCount) + 1e-10)
            )
          , 4
        )
        , totalIv = round(
          sum(
            (rpsCount / sum(rpsCount) - nmlCount / sum(nmlCount)) *
              log((rpsCount / sum(rpsCount) + 1e-10) / (nmlCount / sum(nmlCount) + 1e-10))
          )
          , 4
        )
        , binsLift = round(
          (rpsCount / (rpsCount + nmlCount)) / (
            (max(rpsCountCum) / (max(nmlCountCum) + max(rpsCountCum)))  + 1e-10
          )
          , 4
        )
        , accumLift = round(
          rpsCountCumRate / (countRateCum)
          , 4
        )
      )
    ][]

  }



}
