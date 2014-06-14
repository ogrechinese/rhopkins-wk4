# This is Week 4, Programming Assignment 3
# Question #4: Ranking hospitals in all states
# This function takes two arguments: an outcome and the outcome ranking.
# The function reads the outcomes csv file and returns a character vector with the
# name of the hospital from each state that has the ranking for that state.

# Column notes for the original file
# state column is column 7
# 30 day mortality rate column info
# heart attack: column 11
# heart failure: column 17
# pneumonia: column 23

# Requires plyr library
# Utilizes the arrange() function from the plyr library

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data = read.csv('outcome-of-care-measures.csv', colClasses = 'character')
        data[,11] <- as.numeric(data[,11])
        data[,17] <- as.numeric(data[,17])
        data[,23] <- as.numeric(data[,23])
        
        #------------------------------------------------     
                
        ## Check that state and outcome are valid
        # Check that the outcome is valid
        # Create a valid outcomes vector
        # Check the "outcome" input with this vector
        # If there are no matches with the valid outcomes vector, then the input was invalid  
        valid_outcomes <- c('heart attack', 'heart failure', 'pneumonia')
        check_outcome <- outcome == valid_outcomes
        if (sum(check_outcome) == 0) {
                stop("invalid outcome", call. = TRUE)
        }
        
        
        
        #------------------------------------------------
        
        # Find all distinct states
        state_vector = data[,7]
        state_vector = unique(state_vector)
        state_index = complete.cases(state_vector)
        state_vector = state_vector[state_index]
        state_vector = state_vector[sort.list(state_vector)]
        
        # Initialize the data frame to store all this data; set the row names to be 
        # the state names, and set the column names to be 'hospital' and 'state'
        df <- data.frame(matrix(ncol = 2, nrow = length(state_vector)))
        rownames(df) = state_vector
        colnames(df) = c('hospital', 'state')
        
        # identify the column number, which is cnum
        if (outcome == 'heart attack') {
                cnum = 11
        }
        else if (outcome == 'heart failure'){
                cnum = 17
        }
        else if (outcome == 'pneumonia') {
                cnum = 23
        }
        
        ## For each state, find the hospital of the given rank
        # then write that data into the df data frame.
        for (state in state_vector) {
                
                # Return hosital with the given rank
                # subset the data to pull all rows matching the state selection value
                state_subset = subset(data,data[,7] == state)
                
                # then filter for the hospital name column and the mortality rate
                # for the particular condition
                state_subset2 = state_subset[, c(2,cnum)]
                
                # now find the rows where there are no NA's
                complete = complete.cases(state_subset2)
                
                # now filter state_subset2 for rows where there are no NA's for the
                # specific mortality rate
                state_subset3 = state_subset2[complete, ]
                
                # Originally, you did not rename the column names, which caused problems
                colnames(state_subset3) = c('hospital','mortality_rate')
                # This if statement will sort ascending if 
                if(is.numeric(num)){
                        if(num > nrow(state_subset3)) {
                                hospital_name = NA
                        }
                        
                        else {
                                # Sort the values in ascending order
                                # And find the hospital name with the i-th outcome
                                sorted = arrange(state_subset3, mortality_rate, hospital)
                                hospital_name = sorted[num,1] 
                        }
                }
                
                else if (num == 'best') {
                        # Sort the values in ascending order
                        # And find the hospital name with the i-th outcome
                        sorted = arrange(state_subset3, mortality_rate, hospital)
                        hospital_name = sorted[1,1]        
                }
                
                else if (num == 'worst') {
                        # Sort the values in descending order
                        # And find the hospital name with the i-th outcome
                        sorted = arrange(state_subset3, desc(mortality_rate), hospital)
                        hospital_name = sorted[1,1]      
                }
                
                else
                        print('Error in num input')
                
                # Write the data for the current state to the final dataframe
                #print('the state is')
                #print(state)
                #print('')
                
                #print ('the hospital name is')
                #print (hospital_name)
                #print('')
                
                #print('cof hospital name and state is')
                #print(c(hospital_name, state))
                
                #print('the state is')
                #print(df[rownames(df) == state])
                #print(' ')
                
                df[rownames(df) == state,] = c(hospital_name,state)
                
        }
        
        #------------------------------------------------
        
        ## Return a data frame with the hospital names and the        
        ## (abbreviated) state name
        
        df

       
        
}