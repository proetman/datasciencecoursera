

library(dplyr)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                        Global Variables
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

dir_proj_data <- 'C:/temp/UCI HAR Dataset'
tmp_dir <- 'c:/temp'

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
#                    Set Default Working Directory
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Change directory to the global working directory defined for this project.

load_data <- function(p_dir, p_activity_labels, p_col_all, p_col_required){

        mode <- basename(p_dir)

        subject_file <- paste(paste("subject", mode, sep="_"), 'txt', sep='.')
        x_file <- paste(paste("x", mode, sep="_"), 'txt', sep='.')
        y_file <- paste(paste("y", mode, sep="_"), 'txt', sep='.')

        #  load subject test (who)
        print(paste0('load subject file: ', subject_file))
        subjects_file <- paste(p_dir, subject_file, sep='/')
        subjects <- read.delim(subjects_file, header=FALSE)
        names(subjects) <- c('Subject')

        #  load Y (what activity)
        print(paste0('load Y file      : ', y_file))
        activities_file <- paste(p_dir, y_file, sep='/')
        activities <- read.delim(activities_file, header=FALSE)

        #  load data (results)
        print(paste0('load X file      : ', x_file))
        field_widths <- rep(16, 561)
        results_file <- paste(p_dir, x_file, sep='/')
        results <- read.fwf(results_file, field_widths)

        # Give proper names to columns
        names(results) <- p_col_all

        # Remove columns that are not required
        results <- select(results, one_of(p_col_required))

        # Add who did what
        results <- cbind(subjects, results)
        print(names(results))
        # results$who <- subjects

        # Add there activity
        results$activity <- activities

        # Add full name of there activity (not the code)
        map <- setNames(p_activity_labels$activityname,
                        p_activity_labels$activityid)

        results$activity_full <- apply(results$activity,
                                       2,
                                       function(x) map[as.character(x)])

        names(results)
        results <- select(results, -activity)
        names(results)
        return(results)


}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Load Activity lookup
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Load the activity lookup

load_activity_lookup <- function(p_dir, p_file) {

        activity_labels_file <- paste(p_dir, p_file, sep='/')
        activity_labels <- read.fwf(activity_labels_file, c(1,20))
        names(activity_labels) <- c('activityid','activityname')

        # remove leading and trailing spaces
        activity_labels$activityname <- gsub(" ","", activity_labels$activityname)

        return(activity_labels)
}

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                    Load Column Headings
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Load the column headings

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

        myresult <- merged_data %>%
                    group_by(Subject, activity_full) %>%
                    summarise_each(funs(mean(.)))

        print(dim(myresult))


}
