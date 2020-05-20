rankall <- function(outcome, num = "best") {
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- data[c(2, 7, 11, 17, 23)]
  names(data)[1] <- "name"
  names(data)[2] <- "state"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  
  outcomes = c("heart attack", "heart failure", "pneumonia")
  if( outcome %in% outcomes == FALSE ) stop("invalid outcome")
  
  if( num != "best" && num != "worst" && num%%1 != 0 ) stop("invalid num")
  
  data <- data[data[outcome] != 'Not Available', ]

  statelist <- split(data, data$state)

  newdata <- data.frame("hospital"=character(), "state"=character())
  
  for(i in 1:length(statelist)){
    d <- (statelist[[i]])
   
    d[outcome] <- as.data.frame(sapply(d[outcome], as.numeric))
    
    d <- d[order(d$name, decreasing = FALSE), ]
    d <- d[order(d[outcome], decreasing = FALSE), ]
    
    vals <- d[, outcome]
    if( num == "best" ) {
      rowNum <- which.min(vals)
    } else if( num == "worst" ) {
      rowNum <- which.max(vals)
    } else {
      rowNum <- num
    }
    
    h <- d[rowNum, ]$name
      newdata <- rbind(newdata, data.frame(hospital=h, state=names(statelist)[i], row.names = names(statelist)[i]))
  }
  
  newdata
  }