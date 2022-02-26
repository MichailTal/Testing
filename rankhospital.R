rankhospital <- function(state, outcome, num = 'best'){
  
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
  
  data_state_level <- data[data$State==state,]
  
  
  
  if (outcome == 'heart attack') {
    colnum <- 11
    data_state_level[,colnum]<-as.numeric(data_state_level[,colnum])
    data_filter <- complete.cases(data_state_level[, colnum])
    data_clean <- data_state_level[data_filter,]
    data_sorted_rates <- data_clean[order(data_clean[,colnum],data_clean$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
    data_sorted_names <- data_sorted_rates[order(data_sorted_rates[,colnum],data_sorted_rates$Hospital.Name),]
    
    if (num == 'best'){num <- 1}
    else if (num == 'worst') {num <- nrow(data_sorted_names)}
    else if (num > nrow(data_sorted_names)) {stop('NA')}
    
    data_sorted_names$Hospital.Name[num]
    
    
    
    }
  else if (outcome == 'heart failure') {
    colnum <- 17
    data_state_level[,colnum]<-as.numeric(data_state_level[,colnum])
    data_filter <- complete.cases(data_state_level[, colnum])
    data_clean <- data_state_level[data_filter,]
    data_sorted_rates <- data_clean[order(data_clean[,colnum],data_clean$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
    data_sorted_names <- data_sorted_rates[order(data_sorted_rates[,colnum],data_sorted_rates$Hospital.Name),]
    
    if (num == 'best'){num <- 1}
    else if (num == 'worst') {num <- nrow(data_sorted_names)}
    else if (num > nrow(data_sorted_names)) {stop('NA')}
    
    data_sorted_names$Hospital.Name[num]
    
    
    }
  else if (outcome == 'pneumonia') {
    colnum <- 23
    data_state_level[,colnum]<-as.numeric(data_state_level[,colnum])
    data_filter <- complete.cases(data_state_level[, colnum])
    data_clean <- data_state_level[data_filter,]
    data_sorted_rates <- data_clean[order(data_clean[,colnum],data_clean$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
    data_sorted_names <- data_sorted_rates[order(data_sorted_rates[,colnum],data_sorted_rates$Hospital.Name),]
    
    if (num == 'best'){num <- 1}
    else if (num == 'worst') {num <- nrow(data_sorted_names)}
    else if (num > nrow(data_sorted_names)) {stop('NA')}
    
    data_sorted_names$Hospital.Name[num]
    
    }
  
  
  
}