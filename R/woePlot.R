#' Title woePlot
#'
#' Visualize variable binning via echarts4r
#'
#' @param bins Variable binning, which can be a list or a data frame, see the example for details.
#' @param etheme Please refer to ?echarts4r::e_theme().
#' @param elabels Please refer to ?echarts4r::e_labels
#' @param elabelspos Please refer to ?echarts4r::e_labels position
#' @param stacking Whether stacking, the default is FALSE
#'
#' @return Interactive graphics for binning of variables.
#'
#' @import data.table echarts4r
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
#' binsChi <- binningsChimerge(
#'   data = data
#'   , y = 'creditability'
#' )
#'
#'
#' woePlot(binsChi)
#'
#' woePlot(binsChi$duration.in.month)
#'
woePlot <- function(bins,etheme = 'westeros',elabels = TRUE,elabelspos = 'top',stacking = FALSE){

  if (stacking == FALSE){

    if (class(bins)[1] == 'list'){

      for (i in 1:length(names(bins))){

        binsi <- bins[[i]]

        binsiP <- binsi |>
          e_charts(bins) |>
          e_bar(nmlCount) |>
          e_bar(rpsCount) |>
          e_line(rpsRate,x_index = 1, y_index = 1) |>
          e_labels(show = elabels,position = elabelspos) |>
          e_datazoom(x_index = c(0, 1)) |>
          e_title(binsi[,vars][1],paste0("IV: ",binsi[,totalIv][1])) |>
          e_tooltip(trigger = "axis") |>
          e_theme(etheme) |>
          e_toolbox_feature(feature = "saveAsImage") |>
          e_draft(text = "riskerTools") |>
          e_toolbox_feature(feature = "dataView") |>
          e_axis_labels(x = "Binning",y = "Count Distribution")  # axis labels

        print(binsiP)

      }

    } else{

      binsiP <- bins |>
        e_charts(bins) |>
        e_bar(nmlCount) |>
        e_bar(rpsCount) |>
        e_line(rpsRate,x_index = 1, y_index = 1) |>
        e_labels(show = elabels,position = elabelspos) |>
        e_datazoom(x_index = c(0, 1)) |>
        e_title(bins[,vars][1],paste0("IV: ",bins[,totalIv][1])) |>
        e_tooltip(trigger = "axis") |>
        e_theme(etheme) |>
        e_toolbox_feature(feature = "saveAsImage") |>
        e_draft(text = "riskerTools") |>
        e_toolbox_feature(feature = "dataView") |>
        e_axis_labels(x = "Binning",y = "Count Distribution")  # axis labels

      print(binsiP)

    }

  } else if (stacking == TRUE) {

    if (class(bins)[1] == 'list'){

      for (i in 1:length(names(bins))){

        binsi <- bins[[i]]

        binsiP <- binsi |>
          e_charts(bins) |>
          e_bar(nmlCount,stack = 1) |>
          e_bar(rpsCount,stack = 1) |>
          e_line(rpsRate,x_index = 1, y_index = 1) |>
          e_labels(show = elabels,position = elabelspos) |>
          e_datazoom(x_index = c(0, 1)) |>
          e_title(binsi[,vars][1],paste0("IV: ",binsi[,totalIv][1])) |>
          e_tooltip(trigger = "axis") |>
          e_theme(etheme) |>
          e_toolbox_feature(feature = "saveAsImage") |>
          e_draft(text = "riskerTools") |>
          e_toolbox_feature(feature = "dataView") |>
          e_axis_labels(x = "Binning",y = "Count Distribution")  # axis labels

        print(binsiP)

      }

    } else{

      binsiP <- bins |>
        e_charts(bins) |>
        e_bar(nmlCount,stack = 1) |>
        e_bar(rpsCount,stack = 1) |>
        e_line(rpsRate,x_index = 1, y_index = 1) |>
        e_labels(show = elabels,position = elabelspos) |>
        e_datazoom(x_index = c(0, 1)) |>
        e_title(bins[,vars][1],paste0("IV: ",bins[,totalIv][1])) |>
        e_tooltip(trigger = "axis") |>
        e_theme(etheme) |>
        e_toolbox_feature(feature = "saveAsImage") |>
        e_draft(text = "riskerTools") |>
        e_toolbox_feature(feature = "dataView") |>
        e_axis_labels(x = "Binning",y = "Count Distribution")  # axis labels

      print(binsiP)

    }

  }else{
    # stop("未知的参数，stacking参数仅为TRUE or FALSE!")
    stop("Unknown parameter, stacking parameter is only TRUE or FALSE!")
  }


}

# Theme names:
# default
# dark
# vintage
# westeros
# essos
# wonderland
# walden
# chalk
# infographic
# macarons
# roma
# shine
# purple-passion
# halloween
# auritus
# azul
# bee-insipired
# blue
# caravan
# carp
# cool
# dark-blue
# dark-bold
# dark-digerati
# dark-fresh-cut
# dark-mushroom
# eduardo
# forest
# fresh-cut
# fruit
# gray
# green
# helianthus
# inspired
# jazz
# london
# macarons
# macarons2
# mint
# red
# red-velvet
# royal
# sakura
# tech-blue
