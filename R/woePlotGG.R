#' Title woePlotGG
#'
#' Visualize variable binning via ggplot2
#'
#' @param bins
#'
#' @return ggplot2 image for binning of variables
#' @export
#' @import data.table ggplot2 ggthemes
#'
#' @examples
#' library(data.table)
#'
#' data("germancredit", package="scorecard")
#'
#' data <- germancredit
#'
#' binsChi <- binningsChimerge(
#'   data = data
#'   , y = 'creditability'
#' )
#'
#'
#' woePlotGG(binsChi)
#'
#' woePlotGG(binsChi$duration.in.month)
#'
#'
woePlotGG <- function(
  bins
){

  if (class(bins)[1] == 'list'){
    for (i in 1:length(names(bins))){

      binsi <- bins[[i]]

      dataMelt <- melt(
        binsi
        , id=c("vars","bins")
        , measure=c("nmlCount", "rpsCount")
      )[
        , `:=`(
          valueRate = round(value / sum(value),4)
        )
      ][]

      # print(dataMelt)

      dataMelt$variable <- factor(
        dataMelt$variable
        , levels = c('rpsCount','nmlCount')
      )

      print(dataMelt)

      binsiP <- ggplot() +
        scale_y_continuous(
          limits = c(
            0
            # , max(
            #   binsi$countRate
            #   , binsi$rpsRate
            # ) + 0.2
            , 1
          )
          , sec.axis = sec_axis(
            # ~./(
            #   (max(binsi$rpsRate) + 0.2) /
            #     (max(binsi$countRate) + 0.2)
            # )
            ~./ 1
            , name = 'Response Hit Rate'
          )
        ) +
        geom_bar(
          data = dataMelt
          ,aes(x = bins,y = valueRate,fill = variable)
          ,stat="identity"
        ) +
        geom_line(
          data = binsi
          ,aes(x = bins,y = rpsRate,group = 1)
          ,colour = '#0035BF'
        ) +
        geom_point(
          data = binsi
          ,aes(x = bins,y = rpsRate)
          , colour = '#0035BF'
          , shape=21
          , fill="white"
        ) +
        geom_text(
          data=binsi
          , aes(
            x = bins
            , y = countRate
            , label =
              paste0(
                countRate*100,'%',',',count
              )
          )
          , vjust = -0.5
        ) +
        geom_text(
          data=binsi
          , aes(
            x = bins
            , y = rpsRate
            , label =
              # round(rpsRate,4)
              paste0(
                round(rpsRate,4) * 100,'%'
              )
          )
          , colour = '#0035BF'
          , vjust = -0.5
        ) +
        labs(
          title =
            paste0(
              binsi$vars[1]
              , ' IV:'
              , binsi$totalIv[1]
            )
          , x=NULL
          , y = "Count distribution"
          , fill=NULL
        ) +
        theme_bw() +
        theme(
          legend.position="bottom", legend.direction="horizontal",
          axis.title.y.right = element_text(colour = '#0035BF'),
          axis.text.y.right  = element_text(colour = '#0035BF', angle = 90, hjust = 0.5),
          axis.text.y = element_text(angle = 90, hjust = 0.5) )

      print(binsiP)
    }
  }else{

    dataMelt <- melt(
      bins
      , id=c("vars","bins")
      , measure=c("nmlCount", "rpsCount")
    )[
      , `:=`(
        valueRate = round(value / sum(value),4)
      )
    ][]

    dataMelt$variable <- factor(
      dataMelt$variable
      , levels = c('rpsCount','nmlCount')
    )

    binsiP <- ggplot() +
      scale_y_continuous(
        limits = c(
          0
          # , max(
          #   bins$countRate
          #   , bins$rpsRate
          # ) + 0.2
          , 1
        )
        , sec.axis = sec_axis(
          # ~./(
          #   (max(bins$rpsRate) + 0.2) /
          #     (max(bins$countRate) + 0.2)
          # )
          ~./ 1
          , name = 'Response Hit Rate'
        )
      ) +
      geom_bar(
        data = dataMelt
        ,aes(x = bins,y = valueRate,fill = variable)
        ,stat="identity"
      ) +
      geom_line(
        data = bins
        ,aes(x = bins,y = rpsRate,group = 1)
        ,colour = '#0035BF'
      ) +
      geom_point(
        data = bins
        ,aes(x = bins,y = rpsRate)
        , colour = '#0035BF'
        , shape=21
        , fill="white"
      ) +
      geom_text(
        data=bins
        , aes(
          x = bins
          , y = countRate
          , label =
            paste0(
              countRate*100,'%',',',count
            )
        )
        , vjust = -0.5
      ) +
      geom_text(
        data=bins
        , aes(
          x = bins
          , y = rpsRate
          , label =
            # round(rpsRate,4)
            paste0(
              round(rpsRate,4) * 100,'%'
            )
        )
        , colour = '#0035BF'
        , vjust = -0.5
      ) +
      labs(
        title =
          paste0(
            bins$vars[1]
            , ' IV:'
            , bins$totalIv[1]
          )
        , x=NULL
        , y = "Count distribution"
        , fill=NULL
      ) +
      theme_bw() +
      theme(
        legend.position="bottom", legend.direction="horizontal",
        axis.title.y.right = element_text(colour = '#0035BF'),
        axis.text.y.right  = element_text(colour = '#0035BF', angle = 90, hjust = 0.5),
        axis.text.y = element_text(angle = 90, hjust = 0.5) )

    print(binsiP)

  }

}
