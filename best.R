best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  data <- data[ ,c(2, 7, 11, 17, 23)]
  names(data)[1] <- "name"
  names(data)[2] <- "state"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  
  outcomes = c("heart attack", "heart failure", "pneumonia")
  if( !(outcome %in%  outcomes ) ) {
    stop("invalid outcome")
  }
  if( !(state %in% data$state) ) {
    stop("invalid state")
  }
  
  data <- data[data$state==state & data[outcome] != 'Not Available', ]
  val <- data[ ,outcome]
  r <- which.min(val)
  
  data[r, ]$name
}