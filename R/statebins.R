
#' Create ggplot-based "statebin" charts in the style of \url{http://www.washingtonpost.com/wp-srv/special/business/states-most-threatened-by-trade/}
#'
#' This version uses a discrete scale, binned by the "breaks" parameter
#'
#' @param state_data data frame of states and values to plot
#' @param state_col column name in \code{state_data} that has the states. no duplicates and can be names (e.g. "Maine") or abbreviatons (e.g. "ME")
#' @param value_col column name in \code{state_data} that holds the values to be plotted
#' @param text_color default "white"
#' @param font_size font size (default = 2)
#' @param state_border_col default "white" - this creates the "spaces" between boxes
#' @param breaks a single number (greater than or equal to 2) giving the number of intervals into which data values are to be cut.
#' @param labels labels for the levels \code{breaks}
#' @param legend_title title for the legend
#' @param legend_position "none", "top" or "bottom" (defaults to "top")
#' @param brewer_pal which named RColorBrewer palette to use
#' @param plot_title title for the plot
#' @param title_position where to put the title ("bottom" or "top" or " " for none); if "bottom", you get back a grob vs a ggplot object
#' @return ggplot2 object or grob
#' @export
statebins <- function(state_data, state_col="state", value_col="value",
                     text_color="white", font_size=2,
                     state_border_col="white", breaks=5, labels=1:4,
                     legend_title="Legend", legend_position="top",
                     brewer_pal="PuBu", plot_title="", title_position="bottom") {

  stopifnot(breaks > 0 && breaks < 10)
  stopifnot(title_position %in% c("", "top", "bottom"))
  stopifnot(legend_position %in% c("", "none", "top", "bottom"))

  if (max(nchar(state_data[,state_col])) == 2) {
    merge.x <- "abbrev"
  } else {
    merge.x <- "state"
  }

  stopifnot(state_data[,state_col] %in% state_coords[,merge.x])
  stopifnot(!any(duplicated(state_data[,state_col])))

  st.dat <- merge(state_coords, state_data, by.x=merge.x, by.y=state_col)

  st.dat$fill_color <- cut(st.dat[, value_col], breaks=breaks, labels=labels)

  gg <- ggplot(st.dat, aes_string(x="col", y="row", label="abbrev"))
  gg <- gg + geom_tile(aes_string(fill="fill_color"))
  gg <- gg + geom_tile(color=state_border_col, aes_string(fill="fill_color"), size=3, show_guide=FALSE)
  gg <- gg + geom_text(color=text_color, size=font_size)
  gg <- gg + scale_y_reverse()
  gg <- gg + scale_fill_brewer(palette=brewer_pal, name=legend_title)
  gg <- gg + coord_equal()
  gg <- gg + labs(x="", y="", title="")
  gg <- gg + theme_bw()
  gg <- gg + theme(legend.position=legend_position)
  gg <- gg + theme(plot.margin=unit(c(0,0,0,0), "lines"))
  gg <- gg + theme(panel.margin=unit(0, "lines"))
  gg <- gg + theme(panel.border=element_blank())
  gg <- gg + theme(panel.grid=element_blank())
  gg <- gg + theme(panel.background=element_blank())
  gg <- gg + theme(axis.ticks=element_blank())
  gg <- gg + theme(axis.text=element_blank())

  if (plot_title != "") {

    if (title_position == "bottom") {
      gg <- arrangeGrob(gg, sub=textGrob(plot_title, gp=gpar(cex=1)))
    } else {
      gg <- gg + ggtitle(plot_title)
    }

  }

  return(gg)

}



#' Create ggplot-based "statebin" charts in the style of \url{http://www.washingtonpost.com/wp-srv/special/business/states-most-threatened-by-trade/}
#'
#' This version uses a continuous scale akin to \link{scale_fill_distiller}
#'
#' @param state_data data frame of states and values to plot
#' @param state_col column name in \code{state_data} that has the states. no duplicates and can be names (e.g. "Maine") or abbreviatons (e.g. "ME")
#' @param value_col column name in \code{state_data} that holds the values to be plotted
#' @param text_color default "white"
#' @param font_size font size (default = 2)
#' @param state_border_col default "white" - this creates the "spaces" between boxes
#' @param legend_title title for the legend
#' @param legend_position "none", "top" or "bottom" (defaults to "top")
#' @param brewer_pal which named RColorBrewer palette to use
#' @param plot_title title for the plot
#' @param title_position where to put the title ("bottom" or "top" or " " for none); if "bottom", you get back a grob vs a ggplot object
#' @return ggplot2 object or grob
#' @export
statebins_continuous <- function(state_data, state_col="state", value_col="value",
                      text_color="white", font_size=2,
                      state_border_col="white",
                      legend_title="Legend", legend_position="top",
                      brewer_pal="PuBu", plot_title="", title_position="bottom") {

  stopifnot(title_position %in% c("", "top", "bottom"))
  stopifnot(legend_position %in% c("", "none", "top", "bottom"))

  if (max(nchar(state_data[,state_col])) == 2) {
    merge.x <- "abbrev"
  } else {
    merge.x <- "state"
  }

  stopifnot(state_data[,state_col] %in% state_coords[,merge.x])
  stopifnot(!any(duplicated(state_data[,state_col])))

  st.dat <- merge(state_coords, state_data, by.x=merge.x, by.y=state_col)

  gg <- ggplot(st.dat, aes_string(x="col", y="row", label="abbrev"))
  gg <- gg + geom_tile(aes_string(fill=value_col))
  gg <- gg + geom_tile(color=state_border_col,
                       aes_string(fill=value_col), size=3, show_guide=FALSE)
  gg <- gg + geom_text(color=text_color, size=font_size)
  gg <- gg + scale_y_reverse()
  gg <- gg + continuous_scale("fill", "distiller",
                   gradient_n_pal(brewer_pal("seq", brewer_pal)(6), NULL, "Lab"), na.value = "grey50", name=legend_title)
  gg <- gg + coord_equal()
  gg <- gg + labs(x="", y="", title="")
  gg <- gg + theme_bw()
  gg <- gg + theme(legend.position=legend_position)
  gg <- gg + theme(plot.margin=unit(c(0,0,0,0), "lines"))
  gg <- gg + theme(panel.margin=unit(0, "lines"))
  gg <- gg + theme(panel.border=element_blank())
  gg <- gg + theme(panel.grid=element_blank())
  gg <- gg + theme(panel.background=element_blank())
  gg <- gg + theme(axis.ticks=element_blank())
  gg <- gg + theme(axis.text=element_blank())

  if (plot_title != "") {

    if (title_position == "bottom") {
      gg <- arrangeGrob(gg, sub=textGrob(plot_title, gp=gpar(cex=1)))
    } else {
      gg <- gg + ggtitle(plot_title)
    }

  }

  return(gg)

}