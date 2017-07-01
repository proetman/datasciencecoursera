# Program: Run Analyse
#    This program will load the training and test data sets,
#    merge the data into a single set,
#    then print the average for each subject/activity for each measure.

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                        Load Libraries
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

library(dplyr)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                        Global Variables
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

dir_proj_data <- 'C:/temp/UCI HAR Dataset'

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Tidy Column Names
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Tidy up column names by
#    removing abbreviations
#    init cap words to make it readable
#    remove column numbers from headings.

tidy_column_names <- function(p_data){

        # Note: the order of these substitutions is important, for example
        #    Freq -> Frequency must go before
        #    f -> Frequency; otherwise the substituion would occur twice.
        names(p_data) <- gsub('activity_full','Activity',names(p_data))
        names(p_data) <- gsub('Acc','Acceleration',names(p_data))
        names(p_data) <- gsub('Gyro','Gyroscope',names(p_data))
        names(p_data) <- gsub('_t','_Time',names(p_data))
        names(p_data) <- gsub('Freq','Frequency',names(p_data))
        names(p_data) <- gsub('_f','_Frequency',names(p_data))
        names(p_data) <- gsub('tBody','TimeBody',names(p_data))
        names(p_data) <- gsub('mean','Mean',names(p_data))
        names(p_data) <- gsub('std','STD',names(p_data))
        names(p_data) <- gsub('angle','Angle',names(p_data))
        names(p_data) <- gsub('BodyBody','body',names(p_data))
        names(p_data) <- gsub('Mag','magnitude',names(p_data))
        names(p_data) <- gsub('gravity','Gravity',names(p_data))

        # The column numbers are no longer requred as duplicate column
        # names have been remvoed.
        names(p_data) <- gsub('^[0-9]*_','', names(p_data))

        # print(names(p_data))    # debug only

        # Return the data with clean and tidy column names.
        return(p_data)

}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Load data
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Load the measurement data
#    Assign the subject and activity to each line of data
#    Assign column headings

load_data <- function(p_dir, p_activity_labels, p_col_all, p_col_required){

        mode <- basename(p_dir)
        print(paste0('load data from: ', mode))

        subject_file <- paste(paste("subject", mode, sep="_"), 'txt', sep='.')
        x_file <- paste(paste("x", mode, sep="_"), 'txt', sep='.')
        y_file <- paste(paste("y", mode, sep="_"), 'txt', sep='.')

        #  load subject test (who)
        subjects_file <- paste(p_dir, subject_file, sep='/')
        subjects <- read.delim(subjects_file, header=FALSE)
        names(subjects) <- c('Subject')

        #  load Y (what activity)

        activities_file <- paste(p_dir, y_file, sep='/')
        activities <- read.delim(activities_file, header=FALSE)

        #  load data (results)
        field_widths <- rep(16, 561)
        results_file <- paste(p_dir, x_file, sep='/')
        results <- read.fwf(results_file, field_widths)

        # Give proper names to columns
        names(results) <- p_col_all

        # Remove columns that are not required
        results <- select(results, one_of(p_col_required))

        # Add who did what
        results <- cbind(subjects, results)

        # Add there activity
        results$activity <- activities

        # Add full name of there activity (not the code)
        map <- setNames(p_activity_labels$activityname,
                        p_activity_labels$activityid)

        results$activity_full <- apply(results$activity,
                                       2,
                                       function(x) map[as.character(x)])

        # remove the acvitiy code, only require activity name!
        results <- select(results, -activity)

        # Return the dataset.
        return(results)


}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Load Activity lookup
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Load the activity lookup
#    Has 2 columns
#    ID:   Activity ID, data range 1-6
#    Name: Activity Name

load_activity_lookup <- function(p_dir, p_file) {

        activity_labels_file <- paste(p_dir, p_file, sep='/')
        activity_labels <- read.fwf(activity_labels_file, c(1,20))
        names(activity_labels) <- c('activityid','activityname')

        # remove leading and trailing spaces
        activity_labels$activityname <- gsub(" ",
                                             "",
                                             activity_labels$activityname)

        return(activity_labels)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Load Column Headings
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Load the column headings from the features file.
#   Pre-pend the column number to the name so when non-alpha characters
#   are removed, duplicate columns are not created.

load_column_headings <- function(p_dir, p_file) {
        data_file <- paste(p_dir, p_file, sep='/')
        features <- read.delim(data_file, header=FALSE, sep=" ")
        features$V2 <- gsub("[^[:alnum:]]", "", features$V2)

        # Ensure every column will be unique by merging
        # the column number with the column name

        features$col <- paste(sep='_', features$V1, features$V2)

        return(features$col)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Run Analysis
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Run Analysis
#    Load the column headings and activity labels,
#    then load the train and test data.
#    Merge the training and test data to a single data frame, and
#    remove columns that are not required.
#    Then generate the mean for each variable per subject per activity.
#    Save this data to result.txt file.

run_analysis <- function() {

        test_dir <- paste(dir_proj_data, 'test', sep='/')
        train_dir <- paste(dir_proj_data, 'train', sep='/')

        # Load column headings from features.txt file
        column_headings <- load_column_headings(dir_proj_data, "features.txt")

        # We only require columns with mean and standard deviation.
        col_to_keep <- grep('mean|std',
                            column_headings,
                            ignore.case = TRUE,
                            value=TRUE)

        # activity labels
        activity_labels <- load_activity_lookup(dir_proj_data,
                                                "activity_labels.txt")


        test_data <- load_data(test_dir,
                               activity_labels,
                               column_headings,
                               col_to_keep)

        train_data <- load_data(train_dir,
                                activity_labels,
                                column_headings,
                                col_to_keep)

        merged_data <- rbind(train_data, test_data)

        merged_data <- tidy_column_names(merged_data)

        result_data <- merged_data %>%
                       group_by(Subject, Activity) %>%
                       summarise_each(funs(mean(.)))

        names(result_data)[-c(1:2)] <- paste0(names(result_data)[-c(1:2)],
                                              '_Mean')

        print('write data to results.txt')
        write.table(result_data,
                    file=paste(dir_proj_data,
                               'results.txt',
                               sep='/'),
                    row.names = FALSE)

        return(TRUE)
}
