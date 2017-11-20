#' Base statebins theme
#'
#' Clears out most of the cruft. Builds off of `theme_bw()`
#'
#' @md
#' @param legend_position fills in `legend.position`
#' @param base_family,base_size,base_line_size,base_rect_size same as `theme_bw()`
#' @export
theme_statebins <- function(legend_position="bottom",
                            base_size = 11, base_family = "",
                            base_line_size = base_size/22,
                            base_rect_size = base_size/22) {

  if ("base_line_size" %in% names(formals(ggplot2::theme_bw))) {
    gg <- theme_bw(base_family = base_family, base_size = base_size,
                 base_line_size = base_line_size, base_rect_size = base_rect_size)
  } else {
    gg <- theme_bw(base_family = base_family, base_size = base_size)
    gg <- gg + theme(size = base_line_size)
    gg <- gg + theme(rect = element_rect(fill = "white", colour = "black",
                                         size = base_rect_size, linetype = 1))
  }

  gg <- gg + theme(panel.border=element_blank())
  gg <- gg + theme(strip.background = element_rect(color="#2b2b2b", fill="white"))
  gg <- gg + theme(panel.grid=element_blank())
  gg <- gg + theme(panel.background=element_blank())
  gg <- gg + theme(axis.ticks=element_blank())
  gg <- gg + theme(axis.text=element_blank())
  gg <- gg + theme(plot.title=element_text(hjust=0.5))
  gg <- gg + theme(axis.title.x=element_text(hjust=0.5))
  gg <- gg + theme(legend.position=legend_position)
  gg <- gg + theme(plot.title=element_text(size=16, hjust=0))
  gg <- gg + theme(plot.margin = margin(30,30,30,30))
  gg

}
