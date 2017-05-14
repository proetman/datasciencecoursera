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

dir_set_default_working <- function()
{
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

dir_change <- function(p_dir)
{
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

        if (nchar(p_dir) < 1)
        {
                print('ERROR: Parameter length zero, p_dir is mandatory')
                return(FALSE)
        }

        if ( ! dir.exists(p_dir) )
        {
                print('ERROR: target directory does not exist')
                print(paste('       cannot change dir to [', p_dir, ']',sep=''))
                return(FALSE)
        }
        setwd(p_dir)
        return(TRUE)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Validate Parameter String
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Validate parameter string
# Ensure that the first parameter supplied falls within the range of values
# supplied in the second parameter.
# On success, return True
# On failure, return False

validate_parameter_string <- function(p_param, p_all_values)
{
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

        if (toupper(p_param) %in% toupper(p_all_values))
        {
                return(TRUE)
        }
        else
        {
                return(FALSE)
        }
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Validate parameter num
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Validate parameter num: ensure that the num parameter supplied is valid.
#
# Ensure that the parameter supplied is one of the following
# On success, return integer
# On failure, return empty vector

validate_parameter_num <- function(p_num)
{
        # Validate parameter string p_param exists within the
        # vector of all possible values.
        # The comparison is case insensitive.
        #
        # parameters
        #   p_param : character or numeric vector of length 1
        #             best
        #             worst
        #             a positive integer
        #
        # returns
        #       integer

        na_vector <- c(NA)
        retval <- na_vector    # default return value.

        if (is.na(p_num))
                retval <- na_vector

        else if (! is.vector(p_num))
                retval <- na_vector

        else if (length(p_num) != 1)
                retval <- na_vector

        else if (class(p_num) == 'character'){
                if (toupper(p_num) == 'BEST')
                        retval <- 1
                else if (toupper(p_num) == 'WORST')
                        retval <- -1
                else
                        retval <- na_vector
        }

        else if  (class(p_num) == 'numeric') {
                if (floor(p_num)==p_num & p_num > 0 )
                        retval <- p_num
                else
                        retval <- na_vector
        }
        else
                retval <- na_vector

        return(retval)

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

csv_data_load <- function(p_directory, p_csv_filename)
{
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

        if ( ! dir_change(p_directory))
        {
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
#                    determine hospital name
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# determine which hospital to return from the list by the number value

determine_hospital_name <- function(p_list, p_num) {
        # determine hospital name from the list
        # parameters
        #   p_list: character vector of all hospitals
        #   p_num: which hospital to return
        #          -1 : last
        #          1..n : (n = length of list), which hospital by number
        #          >n: return c(NA)

        na_vector <- c(NA)
        list_length <- length(p_list)

        if (p_num == -1)
                retval <- tail(p_list,1)
        else if (p_num > list_length)
                retval <- na_vector
        else {
                # find last value from the head of the list.
                #
                retval <- tail(head(p_list,p_num),1)
        }
        return(retval)
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

fetch_outcome_data <- function(p_state_data, p_outcome, p_num)
{
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

        result_data_clean <- result_data[result_data[['Sort.Order']] !=
                                                 'Not Available', ]

        result_data_clean[,'Sort.Order'] <- as.numeric(
                result_data_clean[,'Sort.Order'])

        # Sort the data into outcome/hospital order
        result_data_sort <- result_data_clean[with(result_data_clean,
                                    order(Sort.Order, Hospital.Name)),]

        # determine Top Hospital name
        numth_hosp <- determine_hospital_name(result_data_sort$Hospital.Name,
                                              p_num)

        # return the result
        return(numth_hosp)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                        RANK HOSPITAL
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Best
# Please see top of program for an outline of this function.
# Essentially, find the "best" hospital by state for a particular outcome.

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name

        ##
        # Parameters
        #   outcome: character vector of length 1
        #            indicates which outcome will be used to determine
        #            which state is "best".
        #            Possible values are: 'Heart Attack', 'Heart Failure',
        #            and 'Pneumonia'
        #
        #   best: return the "num" ranked position, eg for num = 5, return the
        #         5th best hospital. Other possible value are
        #         "best" (also default value), returns the 1 rank
        #         "worst": returns the lowest ranked hospital
        #         If num > hospitals available, NA is returned.
        #
        # returns
        #       dataframe with each states "num" ranked hospital.
        #       eg if num=best, each states best hospital,
        #          if num=5, each states fifth best hospital.


        # Load data
        outcome_data <- csv_data_load('ProgAss3_data',
                                      'outcome-of-care-measures.csv')

        # create a list of unique outcomes.
        all_outcomes <- c('Heart Attack', 'Heart Failure', 'Pneumonia')

        # validate the outcome parameter exists in the pre-determined vector.
        if ( ! validate_parameter_string(outcome, all_outcomes)) {
                stop('invalid outcome')
        }

        # validate the num parameter
        p_num <- validate_parameter_num(num)
        if (is.na(p_num)) {
                stop('invalid num')
        }


        # Convert the data into a list of data frames by state
        outcome_by_state <- outcome_by_state <- split(outcome_data,
                                                      outcome_data$State)

        # find numth hospital for each dataframe (ie by state)
        outcome_by_state <- lapply(outcome_by_state, function(s_df)
                                   fetch_outcome_data(s_df, outcome, p_num))


        # convert the result into a dataframe
        result_df <- data.frame(unlist(outcome_by_state),
                                stringsAsFactors = FALSE)

        # name the column of the dataframe, and add a column for the state
        names(result_df) <- c('hospital')
        result_df$state <- rownames(result_df)

        # Display the result on the output
        result_df
}

# --- eof ---
