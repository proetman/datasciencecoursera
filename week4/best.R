

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
#                    Set Default Working Directory
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

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
#                    csv data load
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

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
#                    fetch state data
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

fetch_state_data <- function(p_outcome_data, p_state)
{
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
#                    fetch state data
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

fetch_outcome_data <- function(p_state_data, p_outcome)
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

        # Remove hospitals with no data
        f <- result_data[result_data$Sort.Order != 'Not Available']

        # return the result
        return(result_data)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                        best
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

best <- function(state, outcome) {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with lowest 30-day death
        ## rate

        # Load data
        outcome_data <- csv_data_load('ProgAss3_data',
                                      'outcome-of-care-measures.csv')

        all_states <- unique(outcome_data$State)
        all_outcomes <- c('Heart Attack', 'Heart Failure', 'Pneumonia')

        if ( ! validate_parameter_string(state, all_states)) {
                stop('invalid state')
        }

        if ( ! validate_parameter_string(outcome, all_outcomes)) {
                stop('invalid outcome')
        }

        # Narrow the data down to a single state
        state_data <- fetch_state_data(outcome_data, state)

        # Now extract the Hospital and sort order column by outcome
        outcome_data <- fetch_outcome_data(state_data, outcome)

        head(outcome_data)
        # Sort the data
        # result <- outcome_data[with(outcome_data,
          #                           order('Sort.Order', 'Hospital.Name'))]

        # head(result)

}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                        Validate ID Parameter
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

validate_id_parameter <- function(p_id)
{
        # parameters
        #       p_id: id of files to load
        #             may be a single integer         23
        #             may be a vector of integers     c(1,4,5)
        #             may be a sequence of integers   1:20
        #
        # returns
        #       vector of integers
        #       if any issues, vector length will be zero.
        #
        if (! is.vector(p_id) && is.na(p_id))
        {
                # NA parameter passed in as p_id, return empty vector
                print('ERROR: ID Parameter is NA')
                retval <- vector('integer')
        }
        else if (length(p_id) == 0)
        {
                # empty parameter passed in as p_id, return empty vector
                print('ERROR: ID Parameter length is zero')
                retval <- vector('integer')
        }
        else if ( length(p_id) == 1)
        {
                # parameter is a single digit.
                retval <- c(p_id:p_id)
        }
        else if ( class(p_id) == 'numeric' & is.vector(p_id))
        {
                retval <- p_id
        }
        else
        {
                retval <- p_id
        }

        return (retval)
}



# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Set Default Working Directory
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

validate_pollutant_parameter <- function(p_pollutant)
{
        # parameters
        #       p_pollutant: what type of pollutant to test for
        #
        # returns
        #       boolean: true: valid pollutant
        #                false: not valid pollutant
        #
        if (p_pollutant == 'sulfate' | p_pollutant == 'nitrate')
        {
                return(TRUE)
        }
        else
        {
                print(noquote(paste('ERROR: parameter for pollutant must be',
                                     'sulphate or nitrate')))
                return(FALSE)
        }


}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    csv file name fetch
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

csv_file_name_fetch <- function(p_id)
{
        # csv file name fetch
        # Parameters:
        #       p_id:   integer that corrosponds to a csv file
        #               each id corrosponds to a file of that name
        #               file name example: 010.csv for ID 10.
        #
        # Returns:      string that will be the complete file name
        #               Note: this does not test if the file exists
        #

        p_str_id <- str_pad(p_id, 3, pad = '0')
        paste(p_str_id, ".csv", sep='')
}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    load data
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

load_data_files <- function(p_id_seq)
{
        # load data files
        # Parameters:
        #       p_id_seq: integer vector of id's to load.
        #                 each id corrosponds to a file of that name
        #                 file name example: 010.csv for ID 10.
        #
        # Returns:      list of dataframes, each containing a days worth of
        #               data.
        #               On error, an empty list is returned


        df_list = list()

        # Create an empty list for the CSV data


        for (i in p_id_seq)
        {
                csv_file_name <- csv_file_name_fetch(i)
                if ( ! file.exists(csv_file_name))
                {
                        print(paste('Skip file',
                                    csv_file_name,
                                    'does not exist'))
                        next
                }
                id_data <- csv_data_load(csv_file_name)
                # Append data dataframe to the list.
                key <- str_pad(i, 3, pad = '0')
                df_list[[key]] <- id_data
        }
        df_list

}
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Read Valid Values
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

read_valid_values  <- function(p_list_df_data, p_pollutant)
{
        # Parameters
        #       p_list_df_data - list of all the data frames with the
        #                        pollution data
        #       p_pollutant - char vector of length 1 indicating the name of the
        #                   pollutant for which we will calculate the mean;
        #                   either "sulphate" or "nitrate"


        # Returns: vector of double values containing all data exclude NA
        df_data_v = double(0)
        for (df in p_list_df_data)
        {
                # browser()
                poll_data = df[[p_pollutant]]
                df_data_v <- c(df_data_v, poll_data)

        }
        return(df_data_v)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Pollutant Mean - Part 1
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---


remove_na_values <- function(p_list_df_data)
{
        # Parameters
        #       p_list_df_data - list of dataframes with data loaded
        #                        key is the file name
        #                        values is a DF with the raw data
        #
        #
        # Returns: list of dataframes with the NA values removed
        #          no other change.

        df_list = list()

        # print(names(p_list_df_data))
        for (key in names(p_list_df_data))
        {
                curr_df <- p_list_df_data[[key]]
                new_df_logical <- complete.cases(curr_df)
                new_df <- curr_df[new_df_logical,]

                df_list[[key]] <- new_df
        }

        return(df_list)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Complete - Part 2
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---


count_results <- function(p_list_df_data)
{
        # Parameters
        #       p_list_df_data - list of dataframes with data loaded
        #                        key is the file name
        #                        values is a DF with the raw data
        #
        # returns data frame of ID, count values in DF

        keys = as.integer(names(p_list_df_data))
        cnt_nobs = lapply(p_list_df_data, nrow)

        df <- do.call(rbind, Map(data.frame, id=keys, nobs=cnt_nobs))

        return (df)
}
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    corr calc result
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---


corr_calc_result <- function(p_list_df_data)
{
        # Parameters
        #       p_list_df_data - list of dataframes with data loaded
        #                        key is the file name
        #                        values is a DF with the raw data
        #
        # Returns: vector of numbers of the correlations between s and n
        #          0 if none

        retval <- c()
        i <- 1
        for (key in names(p_list_df_data))
        {
                curr_df <- p_list_df_data[[key]]
                x <- curr_df$sulfate
                y <- curr_df$nitrate
                res = cor(x,y)
                retval[i] <- res
                i <- i + 1
        }
        return(retval)

}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Corr Remove Threshold
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---


corr_remove_threshold <- function(p_list_df_data, p_threshold)
{
        # Parameters
        #       p_list_df_data - list of dataframes with data loaded
        #                        key is the file name
        #                        values is a DF with the raw data
        #       p_threshold    - min value of complete cases to be included.
        #
        #
        # Returns: list of dataframes with the NA values removed
        #          no other change.

        df_list = list()

        # print(names(p_list_df_data))
        for (key in names(p_list_df_data))
        {
                curr_df <- p_list_df_data[[key]]
                new_df_logical <- complete.cases(curr_df)
                new_df <- curr_df[new_df_logical,]

                if(nrow(new_df) >= p_threshold)
                        df_list[[key]] <- new_df
        }

        return(df_list)
}



# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Pollutant Mean - Part 1
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---


pollutantmean <- function(directory, pollutant, id = 1:332)
{
        # Parameters
        #       directory - char vector of length 1 indicating location of data.
        #       pollutant - char vector of length 1 indicating the name of the
        #                   pollutant for which we will calculate the mean;
        #                   either "sulphate" or "nitrate"
        #       id        - integer vector indicating the monitor ID numbers to
        #                   use

        # Returns: mean of the pollutant across all monitors list in the 'id'
        #          vector (ignoring NA values)
        #          Results are not rounded.


        # Ensure we are in the correct working directory
        dir_set_default_working()

        if ( ! validate_pollutant_parameter(pollutant))
        {
                print(noquote('ERRORS raised, please correct and retry. Aborting'))
                return(FALSE)
        }

        if ( ! dir_change(directory))
        {
                print(noquote('ERRORS raised, please correct and retry. Aborting'))
                return(FALSE)
        }

        l_id_v = validate_id_parameter(id)
        if (length(l_id_v) == 0)
        {
                print(noquote('ERROR: ID parameter invalid. Aborting'))
                return(FALSE)
        }

        pol_data <- load_data_files(l_id_v)
        if (is.list(pol_data) & length(pol_data) == 0)
        {
                print(noquote('ERROR: no data files found. Aborting'))
                return(FALSE)
        }

        test_data <- read_valid_values(pol_data, pollutant)
        result <- mean(test_data, na.rm = TRUE)

        # Return back to default directory.
        dir_set_default_working()
        result

}


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Complete - Part 2
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---


complete <- function(directory, id = 1:332)
{
        # Parameters
        #       directory - char vector of length 1 indicating location of data.
        #       id        - integer vector indicating the monitor ID numbers to
        #                   use

        # Returns: The count of complete cases by ID
        #          Results are not rounded.


        # Ensure we are in the correct working directory
        dir_set_default_working()

        if ( ! dir_change(directory))
        {
                print(noquote('ERRORS raised, please correct and retry. Aborting'))
                return(FALSE)
        }

        p_id_values <- validate_id_parameter(id)

        pol_data <- load_data_files(p_id_values)
        if (is.list(pol_data) & length(pol_data) == 0)
        {
                print(noquote('ERROR: no data files found. Aborting'))
                return(FALSE)
        }

        complete_cases <- remove_na_values(pol_data)
        results <- count_results(complete_cases)
        # test_data <- read_valid_values(pol_data)
        # use complete.cases

        # Return back to default directory.
        dir_set_default_working()

        print.data.frame(results, row.names = TRUE)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Corr - Part 3
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---


corr <- function(directory, threshold = 0)
{
        # Parameters
        #       directory - char vector of length 1 indicating location of data.
        #       threshold - numeric vector of length 1 indicating the number of
        #           completely observced observations (on all variables)
        #           required to compute the correlation between nitrate and
        #           sulfate; the default is 0
        #

        # Returns: numeric vector or correlations
        #          Results are not rounded.


        # Ensure we are in the correct working directory
        dir_set_default_working()


        if ( ! dir_change(directory))
        {
                print(noquote('ERRORS raised, please correct and retry. Aborting'))
                return(FALSE)
        }

        # load all data files
        pol_data <- load_data_files(1:332)
        if (is.list(pol_data) & length(pol_data) == 0)
        {
                print(noquote('ERROR: no data files found. Aborting'))
                return(FALSE)
        }

        above_threshold <- corr_remove_threshold(pol_data, threshold)

        result <- corr_calc_result(above_threshold)

        dir_set_default_working()
        return(result)
}
