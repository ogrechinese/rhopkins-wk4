# This is Week 4, Programming Assignment 3
# Question #3: Ranking hospitals by outcome by state
# This function takes three arguments: 2-character abbreivated state, an outcome,
# and the ranking for the hosptial in that state.
# The function reads the outcomes csv file and returns a character vector with the
# name of the hospital that has the ranking specified by the main argument.

# Column notes for the original file
# state column is column 7
# 30 day mortality rate column info
        # heart attack: column 11
        # heart failure: column 17
        # pneumonia: column 23

# Requires plyr library
# Utilizes the arrange() function from the plyr library

rankhospital <- function(state, outcome, num = "best"){
        
        # Read the data
                data = read.csv('outcome-of-care-measures.csv', colClasses = 'character')
                data[,11] <- as.numeric(data[,11])
                data[,17] <- as.numeric(data[,17])
                data[,23] <- as.numeric(data[,23])
        
        #------------------------------------------------        
                
        # Check that state and outcome are valid
        # Create an index vector that identifies all rows from column 7
        # of the outcomes matrix where the value of the state in that row
        # equals what the user inputted for the function
        # If the sum of this index vector is 0, then the input did not match
        # any of the states in the file, and we have an invalid state
                state_test <- data[,7] == state
                if (sum(state_test) == 0) {
                        stop("invalid state", call. = TRUE)
                }
        
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
        
        # Check the value of num. It needs to accept best, worse, or an integer
        # It must also return NA if num is higher than total number of hospitals
        
        #------------------------------------------------
        
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
                        NA
                }
                
                else {
                # Sort the values in ascending order
                # And find the hospital name with the i-th outcome
                sorted = arrange(state_subset3, mortality_rate, hospital)
                sorted[num,1] 
                }
        }
        
        else if (num == 'best') {
                # Sort the values in ascending order
                # And find the hospital name with the i-th outcome
                sorted = arrange(state_subset3, mortality_rate, hospital)
                sorted[1,1]        
        }
        
        else if (num == 'worst') {
                # Sort the values in descending order
                # And find the hospital name with the i-th outcome
                sorted = arrange(state_subset3, desc(mortality_rate), hospital)
                sorted[1,1]      
        }
        
        else
                print('Error')
}
