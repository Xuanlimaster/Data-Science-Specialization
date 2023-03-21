library(dplyr)

#1. Plot the 30-day mortality rates for heart attack
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[,11])

#2. Finding the best hospital in a state
best <- function(state, outcome) {
  ## 1.Read outcome data
  ## 2.Check that state and outcome are valid
  ## 3.Return hospital name in that state with lowest 30-day death rate
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  fd <- as.data.frame(cbind(data[, 2],   # hospital
                            data[, 7],   # state
                            data[, 11],  # heart attack
                            data[, 17],  # heart failure
                            data[, 23]), # pneumonia
                      stringsAsFactors = TRUE)
  colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  if(!state %in% fd[,'state']){
    stop('invalid state')
  } else if(!outcome %in% c('heart attack', 'heart failure', 'pneumonia')){
    stop('invalid outcome')
  } else{
    state_filt <- fd[which(fd[, "state"] == "SC"), ]    # extracting data for the called state
    numout <- as.numeric(state_filt[,eval(outcome)])
    result <- state_filt[,'hospital'][which(numout == min(numout,na.rm = FALSE))]
    output <- result[order(result)]
  }
  return(output)
}

#3. Ranking hospitals by outcome in a state
rankhospital <- function(state, outcome, num = "best") {
  ## 1.Read outcome data
  ## 2.Check that state and outcome are valid
  ## 3.Return hospital name in that state with the given rank 30-day death rate
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  fd   <- as.data.frame(cbind(data[, 2],  # hospital
                              data[, 7],  # state
                              data[, 11],  # heart attack
                              data[, 17],  # heart failure
                              data[, 23]), # pneumonia
                        stringsAsFactors = FALSE)
  colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  if (!state %in% fd[, "state"]) {
    stop('invalid state')
  } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else if (is.numeric(rank)) {
    state_filt <- fd[which(fd[, "state"] == state), ]                     # extracting dataframe for the called state
    state_filt[, eval(outcome)] <- as.numeric(state_filt[, eval(outcome)])
    state_filt <- state_filt[order(state_filt[, eval(outcome)], state_filt[, "hospital"]), ]
    output <- state_filt[, "hospital"][rank]
  } else if (!is.numeric(rank)){
    if (rank == "best") {
      output <- best(state, outcome)
    } else if (rank == "worst") {
      state_filt <- fd[which(fd[, "state"] == state), ]    
      state_filt[, eval(outcome)] <- as.numeric(state_filt[, eval(outcome)])
      state_filt <- state_filt[order(state_filt[, eval(outcome)], state_filt[, "hospital"], decreasing = TRUE), ]
      output <- state_filt[, "hospital"][1]
    } else {
      stop('invalid rank')
    }
  }
  return(output)
}

#4. Ranking hospitals in all states
rankall <- function(outcome, num = "best") {
  ## 1.Read outcome data
  ## 2.Check that state and outcome are valid
  ## 3.For each state, find the hospital of the given rank
  ## 4.Return a data frame with the hospital names and the (abbreviated) state name
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  fd   <- as.data.frame(cbind(data[, 2],  # hospital
                              data[, 7],  # state
                              data[, 11],  # heart attack
                              data[, 17],  # heart failure
                              data[, 23]), # pneumonia
                        stringsAsFactors = FALSE)
  colnames(fd) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  fd[, eval(outcome)] <- as.numeric(fd[, eval(outcome)])
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else if (is.numeric(num)) {
    by_state <- with(fd, split(fd, state))
    ordered  <- list()
    for (i in seq_along(by_state)){
      by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                           by_state[[i]][, "hospital"]), ]
      ordered[[i]]  <- c(by_state[[i]][num, "hospital"], by_state[[i]][, "state"][1])
    }
    result <- do.call(rbind, ordered)
    output <- as.data.frame(result, row.names = result[, 2], stringsAsFactors = FALSE)
    names(output) <- c("hospital", "state")
  } else if (!is.numeric(num)) {
    if (num == "best") {
      by_state <- with(fd, split(fd, state))
      ordered  <- list()
      for (i in seq_along(by_state)){
        by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                             by_state[[i]][, "hospital"]), ]
        ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
      }
      result <- do.call(rbind, ordered)
      output <- as.data.frame(result, stringsAsFactors = FALSE)
      rownames(output) <- output[, 2]
    } else if (num == "worst") {
      by_state <- with(fd, split(fd, state))
      ordered  <- list()
      for (i in seq_along(by_state)){
        by_state[[i]] <- by_state[[i]][order(by_state[[i]][, eval(outcome)], 
                                             by_state[[i]][, "hospital"], 
                                             decreasing = TRUE), ]
        ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
      }
      result <- do.call(rbind, ordered)
      output <- as.data.frame(result, stringsAsFactors = FALSE)
      rownames(output) <- output[, 2]
    } else {
      stop('invalid num')
    }
  }
  return(output)
}