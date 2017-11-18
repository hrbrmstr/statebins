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
  gg <- theme_bw(base_family = base_family, base_size = base_size,
                 base_line_size = base_line_size, base_rect_size = base_rect_size)
  gg <- gg + theme(panel.border=element_blank())
  gg <- gg + theme(panel.grid=element_blank())
  gg <- gg + theme(panel.background=element_blank())
  gg <- gg + theme(axis.ticks=element_blank())
  gg <- gg + theme(axis.text=element_blank())
  gg <- gg + theme(plot.title=element_text(hjust=0.5))
  gg <- gg + theme(axis.title.x=element_text(hjust=0.5))
  gg <- gg + theme(legend.position=legend_position)
  gg
}
