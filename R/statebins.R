invert <- function(hexColor, darkColor="black", lightColor="white") {

  hexColor <- gsub("#", "", hexColor)

  R <- as.integer(paste("0x", substr(hexColor,1,2), sep=""))
  G <- as.integer(paste("0x", substr(hexColor,3,4), sep=""))
  B <- as.integer(paste("0x", substr(hexColor,5,6), sep=""))

  cat("R ") ; print(R)
  cat("G ") ; print(G)
  cat("B ") ; print(B)

  YIQ <- ((R*299) + (G*587) + (B*114)) / 1000

  cat("YIQ ") ; print(YIQ)

  return(ifelse(YIQ >= 128, darkColor, lightColor))

}

#' Create a new ggplot-based "statebin" chart
#'
#' \code{statebins()} creates "statebin" charts in the style of \url{http://bit.ly/statebins}
#'
#' This version uses discrete \link{RColorBrewer} scales, binned by the "breaks" parameter.
#'
#' The function minimally expects the caller to pass in a data frame that:
#'
#' \itemize{
#'   \item has one column of all state abbreviationis (all caps, including \code{DC} or a column of state names (standard capitalization) named \code{state}
#'   \item has another column of values named \code{value}
#' }
#'
#' Doing so will create a "statebin" chart with 5 breaks and return a \link{ggplot2} object.
#'
#' You can use a different column for the state names and values by changing \code{state_col}
#' and \code{value_col} accordingly.
#'
#' To add a title, change \code{plot_title} to anything but an empty atomic string vector (i.e. \code{""})
#' and set \code{title_positioin} to "\code{top}" or "\code{bottom}". Choosing "\code{bottom}"
#' will cause \code{statebins} to use \link{arrangeGrob} to position the title via \code{sub} and
#' return a frame grob instead of a ggplot2 object.
#'
#' @param state_data data frame of states and values to plot
#' @param state_col column name in \code{state_data} that has the states. no duplicates and can be names (e.g. "\code{Maine}") or abbreviatons (e.g. "\code{ME}")
#' @param value_col column name in \code{state_data} that holds the values to be plotted
#' @param text_color default "\code{white}"
#' @param font_size font size (default = \code{2})
#' @param state_border_col default "\code{white}" - this creates the "spaces" between boxes
#' @param breaks a single number (greater than or equal to 2) giving the number of intervals into which data values are to be cut.
#' @param labels labels for the levels \code{breaks}
#' @param legend_title title for the legend
#' @param legend_position "\code{none}", "\code{top}", "\code{left}", "\code{right}" or "\code{bottom}" (defaults to "\code{top}")
#' @param brewer_pal which named \link{RColorBrewer} palette to use (defaults to "PuBu")
#' @param plot_title title for the plot
#' @param title_position where to put the title ("\code{bottom}" or "\code{top}" or "" for none); if "\code{bottom}", you get back a grob vs a ggplot object
#' @return ggplot2 object or grob
#' @export
statebins <- function(state_data, state_col="state", value_col="value",
                     text_color="white", font_size=2,
                     state_border_col="white", breaks=5, labels=1:5,
                     legend_title="Legend", legend_position="top",
                     brewer_pal="PuBu", plot_title="", title_position="bottom") {

  stopifnot(breaks > 0 && breaks < 10)
  stopifnot(title_position %in% c("", "top", "bottom"))
  stopifnot(legend_position %in% c("", "none", "left", "right", "top", "bottom"))

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
  gg <- gg + geom_tile(color=state_border_col, aes_string(fill="fill_color"), size=2, show_guide=FALSE)
  gg <- gg + geom_text(color=text_color, size=font_size)
  gg <- gg + scale_y_reverse()
  gg <- gg + scale_fill_brewer(palette=brewer_pal, name=legend_title)
  gg <- gg + coord_equal()
  gg <- gg + labs(x=NULL, y=NULL, title="")
  gg <- gg + theme_bw()
#   gg <- gg + theme(plot.margin=unit(c(-0.5,-0.5,-0.5,-0.5), "cm"))
  gg <- gg + theme(legend.position=legend_position)
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


#' Create ggplot-based "statebin" charts in the style of \url{http://bit.ly/statebins}
#'
#' \code{statebins()} creates "statebin" charts in the style of \url{http://bit.ly/statebins}
#'
#' This version uses a continuous scale based on \link{RColorBrewer} scales
#' (passing in a 6 element \code{RColorBrewer} palette to \link{scale_fill_gradientn}).
#'
#' The function minimally expects the caller to pass in a data frame that:
#'
#' \itemize{
#'   \item has one column of all state abbreviationis (all caps, including \code{DC}) or a column of state names (standard capitalization) named \code{state}
#'   \item has another column of values named \code{value}
#' }
#'
#' Doing so will create a "statebin" chart with 5 breaks and return a \link{ggplot2} object.
#'
#' You can use a different column for the state names and values by changing \code{state_col}
#' and \code{value_col} accordingly.
#'
#' To add a title, change \code{plot_title} to anything but an empty atomic string vector (i.e. \code{""})
#' and set \code{title_positioin} to "\code{top}" or "\code{bottom}". Choosing "\code{bottom}"
#' will cause \code{statebins} to use \link{arrangeGrob} to position the title via \code{sub} and
#' return a frame grob instead of a ggplot2 object.
#'
#' @param state_data data frame of states and values to plot
#' @param state_col column name in \code{state_data} that has the states. no duplicates and can be names (e.g. "\code{Maine}") or abbreviatons (e.g. "\code{ME}")
#' @param value_col column name in \code{state_data} that holds the values to be plotted
#' @param text_color default "\code{white}"
#' @param font_size font size (default = \code{2})
#' @param state_border_col default "\code{white}" - this creates the "spaces" between boxes
#' @param legend_title title for the legend
#' @param legend_position "\code{none}", "\code{top}", "\code{left}", "\code{right}" or "\code{bottom}" (defaults to "\code{top}")
#' @param brewer_pal which named \link{RColorBrewer} palette to use (defaults to "PuBu")
#' @param plot_title title for the plot
#' @param title_position where to put the title ("\code{bottom}" or "\code{top}" or "" for none); if "\code{bottom}", you get back a grob vs a ggplot object
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
  gg <- gg + scale_fill_gradientn(colours = brewer.pal(6, brewer_pal), name=legend_title)
  gg <- gg + coord_equal()
  gg <- gg + labs(x=NULL, y=NULL, title="")
  gg <- gg + theme_bw()
  gg <- gg + theme(legend.position=legend_position)
#   gg <- gg + theme(plot.margin=unit(c(-0.5,-0.5,-0.5,-0.5), "cm"))
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