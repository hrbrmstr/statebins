.sb_invert <- function(hex_color, dark_color="black", light_color="white",
                       na_color="white") {

  hex_color <- gsub("#", "", hex_color)

  R <- as.integer(paste("0x", substr(hex_color,1,2), sep=""))
  G <- as.integer(paste("0x", substr(hex_color,3,4), sep=""))
  B <- as.integer(paste("0x", substr(hex_color,5,6), sep=""))

  YIQ <- ((R*299) + (G*587) + (B*114)) / 1000

  return(
    ifelse(is.na(YIQ), na_color,
      ifelse(
        YIQ >= 128, dark_color, light_color)
      )
    )
}

# sanity checks for state values
validate_states <- function(state_data, state_col, merge.x, ignore_dups=FALSE) {

  good_states <- state_data[,state_col] %in% state_coords[,merge.x]
  if (any(!good_states)) {
    invalid <- state_data[,state_col][which(!good_states)]
    state_data <- state_data[which(good_states),]
    warning("Found invalid state values: ", invalid)
  }

  if (!ignore_dups) {
    dups <- duplicated(state_data[,state_col])
    if (any(dups)) {
      state_data <- state_data[which(!dups),]
      warning("Removing duplicate state rows")
    }
  }

  return(state_data)

}

"%||%" <- function(a, b) { if (!is.null(a)) a else b }

.pt <- 2.84527559055118