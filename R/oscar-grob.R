
oscarGrob <- function(x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                      width=unit(1, "npc"), height=unit(1, "npc"),
                      default.units="npc",
                      name=NULL, gpbl=gpar(), gptr=gpar(), vp=NULL) {
  if (!is.unit(x))
    x <- unit(x, default.units)
  if (!is.unit(y))
    y <- unit(y, default.units)
  if (!is.unit(width))
    width <- unit(width, default.units)
  if (!is.unit(height))
    height <- unit(height, default.units)

  if (length(name) == 0) name <- "oscar"

  ggname(

    name,

    grid::grobTree(

      grob(x=unit.c(x, x+width, x, x),
           y=unit.c(y, y, y+height, y),
           name=sprintf("%s_bl", name), gp=gpbl, vp=vp, cl="polygon"),

      grob(x=unit.c(x+width, x, x+width, x+width),
           y=unit.c(y+height, y+height, y, y+height),
           name=sprintf("%s_tr", name), gp=gptr, vp=vp, cl="polygon")
    )

  )

}

grid.oscar <- function(x=unit(0.5, "npc"), y=unit(0.5, "npc"),
                       width=unit(1, "npc"), height=unit(1, "npc"),
                       default.units="npc",
                       name=NULL, gpbl=gpar(), gptr=gpar(),
                       draw=TRUE, vp=NULL) {

  rg <- oscarGrob(x=x, y=y, width=width, height=height,
                  default.units=default.units,
                  name=name, gpbl=gpbl, gptr=gptr, vp=vp)
  if (draw)
    grid.draw(rg)
  invisible(rg)
}
