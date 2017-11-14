#' Create a new ggplot-based "statebin" chart for USA states (manually colored)
#'
#' \code{statebins()} creates "statebin" charts in the style of \url{http://bit.ly/statebins}
#'
#' This version uses manual colors (i.e. pass in a column that defines the color per-state)
#'
#' The function minimally expects the caller to pass in a data frame that:
#'
#' \itemize{
#'   \item has one column of all state abbreviationis (all caps, including \code{DC} &
#'     \code{PR}  or a column of state names (standard capitalization) named \code{state}
#'   \item has another column of colors named \code{color}
#' }
#'
#' Doing so will create a "statebin" chart with the colors specified as a ggplot2 object.
#'
#' You can use a different column for the state names and colors by changing \code{state_col}
#' and \code{color_col} accordingly.
#'
#' To add a title, change \code{plot_title} to anything but an empty atomic string vector (i.e. \code{""})
#' and set \code{title_position} to "\code{top}" or "\code{bottom}". Choosing "\code{bottom}"
#' will cause \code{statebins} to use the X axis title placeholder.
#'
#' @param state_data data frame of states and values to plot
#' @param state_col column name in \code{state_data} that has the states. no duplicates
#'        and can be names (e.g. "\code{Maine}") or abbreviatons (e.g. "\code{ME}")
#' @param color_col column name in \code{state_data} that holds the colors to be used
#' @param text_color default "\code{black}". Size 1 for global color across all tiles or
#'        a vector of colors the same length as the number of states you passed in.
#'        Use the sort order for the states as they are sorted before being plotted.
#' @param font_size font size (default = \code{3})
#' @param state_border_col default "\code{white}" - this creates the "spaces" between boxes
#' @param labels labels for the legend (should be the same number as distinct colors in
#'        \code{color_col}); \code{NULL} == no labels/legend
#' @param legend_title title for the legend
#' @param legend_position "\code{none}", "\code{top}", "\code{left}", "\code{right}" or
#'        "\code{bottom}" (defaults to "\code{top}")
#' @param plot_title title for the plot
#' @param title_position where to put the title ("\code{bottom}" or "\code{top}" or ""
#'        for none); if "\code{bottom}", you get back a grob vs a ggplot object
#' @return ggplot2 object or grob
#' @export
#' @examples
#' \dontrun{
#' library(httr)
#' library(dplyr)
#' election_2012 <-
#'   GET("https://raw.githubusercontent.com/hrbrmstr/statebins/master/tmp/election2012.csv")
#' results <- read.csv(textConnection(content(election_2012, as="text")),
#'                     header=TRUE, stringsAsFactors=FALSE)
#' results <- results %>%
#'   mutate(color=ifelse(is.na(Obama), "#2166ac", "#b2182b")) %>%
#'   select(state, color)
#' results %>%
#'   statebins_manual(font_size=4,
#'       text_color = "white", labels=c("Romney", "Obama"),
#'       legend_position="right", legend_title="Winner")
#' }
statebins_manual <- function(state_data, state_col="state", color_col="color",
                             text_color="black", font_size=3,
                             state_border_col="white", labels=NULL,
                             legend_title="Legend", legend_position="top",
                             plot_title="", title_position="bottom") {

  if (!title_position %in% c("", "top", "bottom")) {
    stop("'title_position' must be either blank, 'top' or 'bottom'")
  }

  state_data <- data.frame(state_data, stringsAsFactors=FALSE)

  if (max(nchar(state_data[,state_col])) == 2) {
    merge.x <- "abbrev"
  } else {
    merge.x <- "state"
  }

  state_data <- validate_states(state_data, state_col, merge.x)

  st.dat <- merge(state_coords, state_data, by.x=merge.x, by.y=state_col, all.y=TRUE)

  gg <- ggplot(st.dat, aes_string(x="col", y="row", label="abbrev"))
  gg <- gg + geom_tile(aes_string(fill="color"))
  gg <- gg + geom_tile(color=state_border_col, aes_string(fill="color"),
                       size=2, show.legend=FALSE)
  gg <- gg + geom_text(color=text_color, size=font_size)
  gg <- gg + scale_y_reverse()
  if (is.null(labels)) {
    gg <- gg + scale_fill_manual(values=unique(st.dat[,color_col]))
    legend_position = "none"
  } else {
    gg <- gg + scale_fill_manual(values=unique(st.dat[,color_col]),
                                 labels=labels, name=legend_title)
  }
  gg <- gg + coord_equal()
  gg <- gg + labs(x=NULL, y=NULL, title=NULL)
  gg <- gg + theme_bw()
  gg <- gg + theme(legend.position=legend_position)
  gg <- gg + theme(panel.border=element_blank())
  gg <- gg + theme(panel.grid=element_blank())
  gg <- gg + theme(panel.background=element_blank())
  gg <- gg + theme(axis.ticks=element_blank())
  gg <- gg + theme(axis.text=element_blank())

  if (plot_title != "") {

    if (title_position == "bottom") {
      gg <-  gg + labs(x=plot_title)
      gg <-  gg + theme(axis.text.x=element_text(hjust=0.5, size=ggplot2::rel(1.2), angle=0))
    } else {
      gg <- gg + ggtitle(plot_title)
    }

  }

  return(gg)

}
