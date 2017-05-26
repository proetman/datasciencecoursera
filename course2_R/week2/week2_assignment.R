library(readr)
library(stringr)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                        Global Variables
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

proj_working_dir <- 'c:/work/R/week2'

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

set_default_working_dir <- function()
{
        # Change to the default working directory for
        # this weeks assignment.
        #
        setwd(proj_working_dir)
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
#                    change directory
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

change_dir <- function(p_dir)
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

csv_data_load <- function(csv_file_name)
{
        # csv data load
        # Parameters
        #       csv_file_name: file to load
        #
        # returns
        #       dataframe of data from the file
        # print(csv_file_name)

        data <- read_csv(csv_file_name, col_types="Dddi")
        return(data)
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
        set_default_working_dir()

        if ( ! validate_pollutant_parameter(pollutant))
        {
                print(noquote('ERRORS raised, please correct and retry. Aborting'))
                return(FALSE)
        }

        if ( ! change_dir(directory))
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
        set_default_working_dir()
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
        set_default_working_dir()

        if ( ! change_dir(directory))
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
        set_default_working_dir()

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
        set_default_working_dir()


        if ( ! change_dir(directory))
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

        set_default_working_dir()
        return(result)
}
