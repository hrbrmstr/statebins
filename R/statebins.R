state_coords <- structure(list(abbrev = c("AL", "AK", "AZ", "AR", "CA", "CO",
                                          "CT", "DC", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS",
                                          "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE",
                                          "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA",
                                          "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
                                          "PR"),
                               state = c("Alabama", "Alaska", "Arizona", "Arkansas",
                                         "California", "Colorado", "Connecticut", "District of Columbia",
                                         "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois",
                                         "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
                                         "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
                                         "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                                         "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                                         "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
                                         "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah",
                                         "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming",
                                         "Puerto Rico"),
                               col = c(8L, 1L, 3L, 6L, 2L, 4L, 11L, 10L, 11L, 10L,
                                       9L, 1L, 3L, 7L, 7L, 6L, 5L, 7L, 6L, 12L, 10L, 11L, 8L, 6L, 7L,
                                       6L, 4L, 5L, 3L, 12L, 10L, 4L, 10L, 8L, 5L, 8L, 5L, 2L, 9L, 12L,
                                       9L, 5L, 7L, 5L, 3L, 11L, 9L, 2L, 8L, 7L, 4L, 12L),
                               row = c(7L, 7L,
                                       6L, 6L, 5L, 5L, 4L, 6L, 5L, 8L, 7L, 8L, 3L, 3L, 4L, 4L, 6L, 5L,
                                       7L, 1L, 5L, 3L, 3L, 3L, 7L, 5L, 3L, 5L, 4L, 2L, 4L, 6L, 3L, 6L,
                                       3L, 4L, 7L, 4L, 4L, 4L, 6L, 4L, 6L, 8L, 5L, 2L, 5L, 3L, 5L, 2L, 4L, 8L)),
                          .Names = c("abbrev", "state", "col", "row"), class = "data.frame", row.names = c(NA, -52L))


invert <- function(hexColor, darkColor="black", lightColor="white") {

  hexColor <- gsub("#", "", hexColor)

  R <- as.integer(paste("0x", substr(hexColor,1,2), sep=""))
  G <- as.integer(paste("0x", substr(hexColor,3,4), sep=""))
  B <- as.integer(paste("0x", substr(hexColor,5,6), sep=""))

  YIQ <- ((R*299) + (G*587) + (B*114)) / 1000

  return(ifelse(YIQ >= 128, darkColor, lightColor))

}

#' Create a new ggplot-based "statebin" chart for USA states (discrete scale)
#'
#' \code{statebins()} creates "statebin" charts in the style of \url{http://bit.ly/statebins}
#'
#' This version uses discrete \code{RColorBrewer} scales, binned by the "breaks" parameter.
#'
#' The function minimally expects the caller to pass in a data frame that:
#'
#' \itemize{
#'   \item has one column of all state abbreviationis (all caps, including \code{DC} &
#'     \code{PR} or a column of state names (standard capitalization) named \code{state}
#'   \item has another column of values named \code{value}
#' }
#'
#' Doing so will create a "statebin" chart with 5 breaks and return a ggplot2 object.
#'
#' You can use a different column for the state names and values by changing \code{state_col}
#' and \code{value_col} accordingly.
#'
#' To add a title, change \code{plot_title} to anything but an empty atomic string vector (i.e. \code{""})
#' and set \code{title_position} to "\code{top}" or "\code{bottom}". Choosing "\code{bottom}"
#' will cause \code{statebins} to use \code{arrangeGrob} to position the title via \code{sub} and
#' return a frame grob instead of a ggplot2 object.
#'
#' @param state_data data frame of states and values to plot
#' @param state_col column name in \code{state_data} that has the states. no duplicates
#'        and can be names (e.g. "\code{Maine}") or abbreviatons (e.g. "\code{ME}")
#' @param value_col column name in \code{state_data} that holds the values to be plotted
#' @param text_color default "\code{black}"
#' @param font_size font size (default = \code{3})
#' @param state_border_col default "\code{white}" - this creates the "spaces" between boxes
#' @param breaks a single number (greater than or equal to 2) giving the number of intervals
#'        into which data values are to be cut.
#' @param labels labels for the levels \code{breaks}
#' @param legend_title title for the legend
#' @param legend_position "\code{none}", "\code{top}", "\code{left}", "\code{right}" or
#'        "\code{bottom}" (defaults to "\code{top}")
#' @param brewer_pal which named \code{RColorBrewer} palette to use (defaults to "PuBu")
#' @param plot_title title for the plot
#' @param title_position where to put the title ("\code{bottom}" or "\code{top}" or ""
#'        for none); if "\code{bottom}", you get back a grob vs a ggplot object
#' @return ggplot2 object or grob
#' @export
#' @examples
#' \dontrun{
#' data(USArrests)
#' USArrests$state <- rownames(USArrests)
#' statebins(USArrests, value_col="Assault", text_color="black", font_size=3,
#'           legend_title = "Assault", legend_position="bottom")
#' }
statebins <- function(state_data, state_col="state", value_col="value",
                     text_color="black", font_size=3,
                     state_border_col="white", breaks=5, labels=1:5,
                     legend_title="Legend", legend_position="top",
                     brewer_pal="PuBu", plot_title="", title_position="bottom") {

  if (breaks <= 0 | breaks >= 10) {
    stop("'breaks' must be between 0 & 10")
  }

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

  st.dat$fill_color <- cut(st.dat[, value_col], breaks=breaks, labels=labels)

  gg <- ggplot(st.dat, aes_string(x="col", y="row", label="abbrev"))
  gg <- gg + geom_tile(aes_string(fill="fill_color"))
  gg <- gg + geom_tile(color=state_border_col, aes_string(fill="fill_color"), size=2, show_guide=FALSE)
  gg <- gg + geom_text(color=text_color, size=font_size)
  gg <- gg + scale_y_reverse()
  gg <- gg + scale_fill_brewer(palette=brewer_pal, name=legend_title)
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
      gg <- arrangeGrob(gg, sub=textGrob(plot_title, gp=gpar(cex=1)))
    } else {
      gg <- gg + ggtitle(plot_title)
    }

  }

  return(gg)

}


#' Create a new ggplot-based "statebin" chart for USA states (continuous scale)
#'
#' \code{statebins()} creates "statebin" charts in the style of \url{http://bit.ly/statebins}
#'
#' This version uses a continuous scale based on \code{RColorBrewer} scales
#' (passing in a 6 element \code{RColorBrewer} palette to \code{scale_fill_gradientn}).
#'
#' The function minimally expects the caller to pass in a data frame that:
#'
#' \itemize{
#'   \item has one column of all state abbreviationis (all caps, including \code{DC} &
#'     \code{PR} ) or a column of state names (standard capitalization) named \code{state}
#'   \item has another column of values named \code{value}
#' }
#'
#' Doing so will create a "statebin" chart with 5 breaks and return a \code{ggplot2} object.
#'
#' You can use a different column for the state names and values by changing \code{state_col}
#' and \code{value_col} accordingly.
#'
#' To add a title, change \code{plot_title} to anything but an empty atomic string vector (i.e. \code{""})
#' and set \code{title_position} to "\code{top}" or "\code{bottom}". Choosing "\code{bottom}"
#' will cause \code{statebins} to use \code{arrangeGrob} to position the title via \code{sub} and
#' return a frame grob instead of a ggplot2 object.
#'
#' @param state_data data frame of states and values to plot
#' @param state_col column name in \code{state_data} that has the states. no duplicates
#'        and can be names (e.g. "\code{Maine}") or abbreviatons (e.g. "\code{ME}")
#' @param value_col column name in \code{state_data} that holds the values to be plotted
#' @param text_color default "\code{black}"
#' @param font_size font size (default = \code{3})
#' @param state_border_col default "\code{white}" - this creates the "spaces" between boxes
#' @param legend_title title for the legend
#' @param legend_position "\code{none}", "\code{top}", "\code{left}", "\code{right}" or
#'        "\code{bottom}" (defaults to "\code{top}")
#' @param brewer_pal which named \code{RColorBrewer} palette to use (defaults to "PuBu")
#' @param plot_title title for the plot
#' @param title_position where to put the title ("\code{bottom}" or "\code{top}" or ""
#'        for none); if "\code{bottom}", you get back a grob vs a ggplot object
#' @return ggplot2 object or grob
#' @export
#' @examples
#' \dontrun{
#' data(USArrests)
#' USArrests$state <- rownames(USArrests)
#' statebins_continuous(USArrests, value_col="Murder", text_color="black", font_size=3,
#'                      legend_title = "Murder", legend_position="bottom")
#' }
statebins_continuous <- function(state_data, state_col="state", value_col="value",
                      text_color="black", font_size=3,
                      state_border_col="white",
                      legend_title="Legend", legend_position="top",
                      brewer_pal="PuBu", plot_title="", title_position="bottom") {

  if (!title_position %in% c("", "top", "bottom")) {
    stop("'title_position' must be either blank, 'top' or 'bottom'")
  }

  if (!legend_position %in% c("", "none", "top", "bottom")) {
    stop("'legend_position' must be either blank, 'none', 'top' or 'bottom'")
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
  gg <- gg + geom_tile(aes_string(fill=value_col))
  gg <- gg + geom_tile(color=state_border_col,
                       aes_string(fill=value_col), size=3, show_guide=FALSE)
  gg <- gg + geom_text(color=text_color, size=font_size)
  gg <- gg + scale_y_reverse()
  gg <- gg + scale_fill_gradientn(colours = brewer.pal(6, brewer_pal), name=legend_title)
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
      gg <- arrangeGrob(gg, sub=textGrob(plot_title, gp=gpar(cex=1)))
    } else {
      gg <- gg + ggtitle(plot_title)
    }

  }

  return(gg)

}


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
#' will cause \code{statebins} to use \code{arrangeGrob} to position the title via \code{sub} and
#' return a frame grob instead of a ggplot2 object.
#'
#' @param state_data data frame of states and values to plot
#' @param state_col column name in \code{state_data} that has the states. no duplicates
#'        and can be names (e.g. "\code{Maine}") or abbreviatons (e.g. "\code{ME}")
#' @param color_col column name in \code{state_data} that holds the colors to be used
#' @param text_color default "\code{black}"
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
  gg <- gg + geom_tile(color=state_border_col, aes_string(fill="color"), size=2, show_guide=FALSE)
  gg <- gg + geom_text(color=text_color, size=font_size)
  gg <- gg + scale_y_reverse()
  if (is.null(labels)) {
    gg <- gg + scale_fill_manual(values=unique(st.dat[,color_col]))
    legend_position = "none"
  } else {
    gg <- gg + scale_fill_manual(values=unique(st.dat[,color_col]), labels=labels, name=legend_title)
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
      gg <- arrangeGrob(gg, sub=textGrob(plot_title, gp=gpar(cex=1)))
    } else {
      gg <- gg + ggtitle(plot_title)
    }

  }

  return(gg)

}

# sanity checks for state values
validate_states <- function(state_data, state_col, merge.x) {

  good_states <- state_data[,state_col] %in% state_coords[,merge.x]
  if (any(!good_states)) {
    invalid <- state_data[,state_col][which(!good_states)]
    state_data <- state_data[which(good_states),]
    warning("Found invalid state values: ", invalid)
  }

  dups <- duplicated(state_data[,state_col])
  if (any(dups)) {
    state_data <- state_data[which(!dups),]
    warning("Removing duplicate state rows")
  }

  return(state_data)

}




