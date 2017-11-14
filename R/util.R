invert <- function(hexColor, darkColor="black", lightColor="white") {

  hexColor <- gsub("#", "", hexColor)

  R <- as.integer(paste("0x", substr(hexColor,1,2), sep=""))
  G <- as.integer(paste("0x", substr(hexColor,3,4), sep=""))
  B <- as.integer(paste("0x", substr(hexColor,5,6), sep=""))

  YIQ <- ((R*299) + (G*587) + (B*114)) / 1000

  return(ifelse(YIQ >= 128, darkColor, lightColor))

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
