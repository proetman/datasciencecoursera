# The program runs a function called "best" that will calculate the
# hospital with the best (ie lowest) 30 day mortality rate based on the data
# from "outcome-of-care-measures.csv". This file originated in the
# Hospital Compare website: http://hospitalcompare.hhs.gov. The purpose of this
# site is to provide data and infomation about the quality of care at over
# 4,000 Medicare-certified hospitals in the U.S.

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                        Global Variables
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

dir_proj_working <- 'c:/work/R/datasciencecoursera/week4'

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Set Default Working Directory
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Change directory to the global working directory defined for this project.

dir_set_default_working <- function(){
        # Change to the default working directory for
        # this weeks project/assignment.
        #
        setwd(dir_proj_working)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    change directory
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Change directory to directory name supplied as a parameter.
# On succes, return True
# On error, return False.

dir_change <- function(p_dir){
        # Change to directory
        # Parameters:
        #       p_dir:  character vector of len 1
        #               target directory to change into
        #
        # Returns:      TRUE on successful directory change
        #               FALSE on failed directory change
        #
        # this weeks assignment.
        #

        if (nchar(p_dir) < 1){
                print('ERROR: Parameter length zero, p_dir is mandatory')
                return(FALSE)
        }

        if ( ! dir.exists(p_dir)){
                print('ERROR: target directory does not exist')
                print(paste('       cannot change dir to [', p_dir, ']',sep=''))
                return(FALSE)
        }
        setwd(p_dir)
        return(TRUE)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Validate Parmaeter String
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Validate parameter string
# Ensure that the first parameter supplied falls within the range of values
# supplied in the second parameter.
# On success, return True
# On failure, return False

validate_parameter_string <- function(p_param, p_all_values){
        # Validate parameter string p_param exists within the
        # vector of all possible values.
        # The comparison is case insensitive.
        #
        # parameters
        #   p_param     : character vector of length 1
        #                 validate the parameteris one of set from
        #                 the all possible values (p_all_values)
        #   p_all_values: character vector of all possible values
        #                 possible values are derived from the data source.
        #
        # returns
        #       boolean: true: valid outcome
        #                false: invalid outcome
        #
        if (is.na(p_param))
                return(FALSE)

        if (! is.vector(p_param))
                return(FALSE)

        if (length(p_param) != 1)
                return(FALSE)

        if (class(p_param) != 'character')
                return(FALSE)

        # Only value to return success
        if (toupper(p_param) %in% toupper(p_all_values))
                return(TRUE)

        return(FALSE)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    csv data load
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Load a csv file (second parameter) from the directory supplied as the first
# first parameter.
# On success, return the data as a data frame.
# On failure, return an empty data frame.

csv_data_load <- function(p_directory, p_csv_filename){
        # csv data load
        # Parameters
        #   p_directory: character vector of length 1
        #                directory where CSV file is located
        #                relative to global project directory.
        #   p_csv_filename: character vector of length 1
        #                   filename containing CSV data
        #
        # returns
        #       dataframe of data from the file
        #       on any error, returns empty dataframe.


        empty_dataframe = data.frame()

        # Ensure we are in the project working directory
        dir_set_default_working()

        if ( ! dir_change(p_directory)){
                err_txt <- 'ERRORS raised, please correct and retry. Aborting'
                print(noquote(err_txt))
                return(empty_dataframe)
        }
        outcome_data <- read.csv(p_csv_filename,
                                 colClasses = "character")

        # Ensure we are in the correct working directory
        dir_set_default_working()

        return(outcome_data)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    fetch state data
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Fetch State Data
# Take the data supplied in first parameter, and extract the data into
# a single data frame containing data for just a single state, supplied as the
# second parameter. All other data is discarded.
# As the program parameters have been validated, it is assumed that this
# function will always return valid data.

fetch_state_data <- function(p_outcome_data, p_state){
        # fetch_state_data: Fetch all the data from p_outcome_data
        #                   that relates to an individual state
        #
        # Parameters
        #   p_outcome_data: data.frame of all outcome data
        #
        #   p_state: character vector of length 1
        #            Data relates to column in Data.Frame called State
        #            This value has been pre-validated.
        #
        # returns
        #       dataframe of data from the file
        #       where the column State = p_state

        # First, split the data is a list of DF, one per state
        outcome_by_state <- split(p_outcome_data, p_outcome_data$State)

        # Then fetch the data frame for this state only
        ret_outcome <- outcome_by_state[[p_state]]

        # return the subset of data for just this state.
        return(ret_outcome)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    fetch outcome data
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Fetch Outcome Data
# Take the State data that has been supplied and extract the data into a single
# data frame containing data for just a single outcome, supplied as the second
# parameter. All other data is discarded.
# As the program parameters have been validated, it is assumed that this
# function will always return valid data.

fetch_outcome_data <- function(p_state_data, p_outcome){
        # fetch state: Fetch all the data from p_outcome_data
        #              that relates to an individual state
        # Parameters
        #   p_state_data: data.frame of all outcome data for a state

        #   p_outcome: character vector of length 1
        #              determine which column of data will be used to
        #              order by
        #
        # returns
        #       dataframe of data from the file
        #       where the column State = p_state

        # Identify the potential columns by outcome type
        l_ha <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
        l_hf <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
        l_pn <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'

        # isolate which column will be used for first sort
        if (toupper(p_outcome) == 'HEART ATTACK')
                p_col <- l_ha
        else if (toupper(p_outcome) == 'HEART FAILURE')
                p_col <- l_hf
        else
                p_col <- l_pn

        # Create a vector of the columns to extract
        result_cols <- c('Hospital.Name', p_col)

        # extract the columns to return. These are the only columns
        # that are required for determining the "best" hospital.
        result_data <- p_state_data[, result_cols]

        # Rename the column headings. This will ensure all future references
        # to this data frame will "know" absolutely what the column headings
        # are

        col_headings <- c('Hospital.Name', 'Sort.Order')
        names(result_data) <- col_headings

        # Remove hospitals with no data
        result_data_clean <- result_data[result_data[['Sort.Order']] !=
                                                 'Not Available', ]

        result_data_clean[,'Sort.Order'] <- as.numeric(
                result_data_clean[,'Sort.Order'])

        # return the result
        return(result_data_clean)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                        BEST
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Best
# Please see top of program for an outline of this function.
# Essentially, find the "best" hospital by state for a particular outcome.

best <- function(state, outcome) {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        ##
        # Parameters
        #   state: character vector of length 1
        #          indicates which state the data fetched should be from.
        #          Possible values are all the states of USA, as a 2 letter
        #          abbreviation.
        #
        #   outcome: character vector of length 1
        #            indicates which outcome will be used to determine
        #            which state is "best".
        #            Possible values are: 'Heart Attack', 'Heart Failure',
        #            and 'Pneumonia'
        #
        # returns
        #       character vector of length 1
        #       This vector contains the name of the hospital that has the
        #       best results for a particular outcome for a particular state.

        # Load data
        outcome_data <- csv_data_load('ProgAss3_data',
                                      'outcome-of-care-measures.csv')

        # Create a list of unique states from the complete data set.
        all_states <- unique(outcome_data$State)

        # create a list of unique outcomes.
        all_outcomes <- c('Heart Attack', 'Heart Failure', 'Pneumonia')

        # Validate the state parameter exists in the dataset.
        if ( ! validate_parameter_string(state, all_states)) {
                stop('invalid state')
        }

        # validate the outcome parameter exists in the pre-determined vector.
        if ( ! validate_parameter_string(outcome, all_outcomes)) {
                stop('invalid outcome')
        }

        # Narrow the data down to a single state
        state_data <- fetch_state_data(outcome_data, state)

        # Now extract the Hospital and sort order column by outcome
        outcome_data <- fetch_outcome_data(state_data, outcome)

        # Sort the data into outcome/hospital order
        result <- outcome_data[with(outcome_data,
                                    order(Sort.Order, Hospital.Name)),]

        # Print Top Hospital name
        head(result$Hospital.Name,1)

}

# --- eof ---
