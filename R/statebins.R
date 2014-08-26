
#' Create ggplot-based "statebin" charts in the style of \url{http://www.washingtonpost.com/wp-srv/special/business/states-most-threatened-by-trade/}
#'
#' This version uses a discrete scale, binned by the "breaks" parameter
#' @param state_data
#' @param state_col
#' @param value_col
#' @param text_color
#' @param state_border_col
#' @param breaks
#' @param labels
#' @param legend_title
#' @param legend_position
#' @param brewer_pal
#' @param plot_title
#' @param title_position
#' @return ggplot2 object or grob
#' @export
statebins <- function(state_data, state_col="state", value_col="value",
                     text_color="white",
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

  gg <- ggplot(st.dat, aes(x=col, y=row, label=abbrev))
  gg <- gg + geom_tile(aes(fill=fill_color))
  gg <- gg + geom_tile(color=state_border_col, aes(fill=fill_color), size=3, show_guide=FALSE)
  gg <- gg + geom_text(color=text_color, size=1)
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
#' This version uses a discrete scale, binned by the "breaks" parameter
#' @param state_data
#' @param state_col
#' @param value_col
#' @param text_color
#' @param state_border_col
#' @param legend_title
#' @param legend_position
#' @param brewer_pal
#' @param plot_title
#' @param title_position
#' @return ggplot2 object or grob
#' @export
statebins_continuous <- function(state_data, state_col="state", value_col="value",
                      text_color="white",
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

  gg <- ggplot(st.dat, aes(x=col, y=row, label=abbrev))
  gg <- gg + geom_tile(aes_string(fill=value_col))
  gg <- gg + geom_tile(color=state_border_col,
                       aes_string(fill=value_col), size=3, show_guide=FALSE)
  gg <- gg + geom_text(color=text_color, size=1)
  gg <- gg + scale_y_reverse()
  gg <- gg + continuous_scale("fill", "distiller",
                   gradient_n_pal(brewer_pal(type, brewer_pal)(6), NULL, "Lab"), na.value = "grey50", name=legend_title)
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