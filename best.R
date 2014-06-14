# Part 2. Finding the best hospital in a state

# state column is column 7

# 30 day mortality rate column info
# heart attack: column 11
# heart failure: column 17
# pneumonia: column 23


best <- function(state, outcome) {
        ## Read outcome data
        # and convert the outcomes columns into numeric values
                data = read.csv('outcome-of-care-measures.csv', colClasses = 'character')
                data[,11] <- as.numeric(data[,11])
                data[,17] <- as.numeric(data[,17])
                data[,23] <- as.numeric(data[,23])
                
        ## Check that state and outcome are valid
                
        # Check that state is valid
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
        
        ## Return hospital name in that state with lowest 30-day death rate
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
        
                # Now find the row with the lowest mortality rate for the particular condition
                # BE SURE TO ADD na.rm = T TO ADDRESS THE NA VALUES
                lowest = min(state_subset3[,2], na.rm = T)
                                
                # next find the index from the subset value where mortality rate matches lowest mortality rate
                index = state_subset3[,2] == lowest
        
                # then find hospitals corresponding to the ibdex. Hospitals is column 2.
                state_subset3[index, 1]
}               
