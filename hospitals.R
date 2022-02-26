outcome <- read.csv('outcome-of-care-measures.csv', colClasses =  'character')
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])


best <- function(state, outcome){
  data <- read.csv('outcome-of-care-measures.csv', colClasses =  'character')
  
  
  '%notin%' <- Negate('%in%') # user defined operator for 'NOt IN'
  States <- data$State # List of States
  diseases <- c('heart attack', 'heart failure', 'pneumonia') # List of potential diseases

  
  
  # invalid arguments
  
  if (state  %notin%  States){
    stop('invalid state')
  }
  
  if (outcome %notin% diseases){
    stop('invalid outcome')
  }
  
  # search for lowest death rate
  
  data_state_level <- data[data$State==state,] #State-level subset
  
  
  if (outcome == 'heart attack'){
    
    colnum <- 11 # col number in original data set for mortality of heart attack
    data_state_level[,colnum]<-as.numeric(data_state_level[,colnum]) # necessary since we defined as character
    data_filter <- complete.cases(data_state_level[, colnum]) # excluding NA from respective column
    data_clean <- data_state_level[data_filter,] # apply cleansed data to the original data frame
    data_sorted <- data_clean[order(data_clean[,colnum],data_clean$Hospital.Name),] # alphabetical order of hospitals
    data_sorted$Hospital.Name[1] # return the first entry of the sorted data frame
    
  }
  
  
  else if (outcome == 'heart failure'){
    
    colnum <- 17
    data_state_level[,colnum]<-as.numeric(data_state_level[,colnum])
    data_filter <- complete.cases(data_state_level[, colnum])
    data_clean <- data_state_level[data_filter,]
    data_sorted <- data_clean[order(data_clean[,colnum],data_clean$Hospital.Name),]
    data_sorted$Hospital.Name[1]
  }
  
  
  else if (outcome == 'pneumonia'){
    
    colnum <- 23
    data_state_level[,colnum]<-as.numeric(data_state_level[,colnum])
    data_filter <- complete.cases(data_state_level[, colnum])
    data_clean <- data_state_level[data_filter,]
    data_sorted <- data_clean[order(data_clean[,colnum],data_clean$Hospital.Name),]
    data_sorted$Hospital.Name[1]
  }
  
}