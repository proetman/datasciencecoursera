

library(dplyr)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
#                        Global Variables
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

dir_proj_working <- 'C:/work/R/datasciencecoursera/course3_GetData'
dir_proj_data <- paste(dir_proj_working,'week4_data/UCI HAR Dataset', sep='/')
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
#                    Run Analysis
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Run Analysis

run_analysis <- function() {

        test_raw_dir <- paste(dir_proj_data, 'test', 'Inertial Signals', sep='/')
        train_raw_dir <- paste(dir_proj_data, 'train', 'Inertial Signals', sep='/')

        test_dir <- paste(dir_proj_data, 'test', sep='/')
        train_dir <- paste(dir_proj_data, 'train', sep='/')

        #  load 561 headings
        features_file <- paste(dir_proj_data, "features.txt", sep='/')
        features <- read.delim(features_file, header=FALSE, sep=" ")
        features$V2 <- gsub("[^[:alnum:]]", "", features$V2)
        features$col <- paste(sep='_', features$V1, features$V2)

        col_to_keep <- grep('mean|std', features$col, ignore.case = TRUE, value=TRUE)

        #  load subject test (who)
        subjects_file <- paste(test_dir, "subject_test.txt", sep='/')
        subjects <- read.delim(subjects_file, header=FALSE)

        #  load Y (what activity)
        activities_file <- paste(test_dir, "y_test.txt", sep='/')
        activities <- read.delim(activities_file, header=FALSE)

        #  load data (results)
        field_widths <- rep(16, 561)
        results_file <- paste(test_dir, "x_test.txt", sep='/')
        results <- read.fwf(results_file, field_widths)

        # activity labels
        activity_labels_file <- paste(dir_proj_data, "activity_labels.txt", sep='/')
        activity_labels <- read.fwf(activity_labels_file, c(1,20))
        names(activity_labels) <- c('activityid','activityname')
        activity_labels$activityname <- gsub(" ","", activity_labels$activityname)


        # Give proper names to columns
        names(results) <- features$col

        # Remove columns that are not required
        results <- select(results, one_of(col_to_keep))

        # Add who did what
        results$who <- subjects

        # Add there activity
        results$activity <- activities

        # Add full name of there activity (not the code)
        map <- setNames(activity_labels$activityname, activity_labels$activityid)
        results$activity_full <- apply(results$activity, 2, function(x) map[as.character(x)])


        print('dim')
        print(dim(results))
        print(head(results,1))
        # ames(results) <- list(features)
        print('summarize')
        print(summarize(results))
        print('the end')
        if ( 1 == 0 ) {
                files <- list.files(path = train_dir,
                                    pattern = '*.txt',
                                    recursive = FALSE)

                field_widths <- rep(16, 128)
                for (f in files) {
                        curr_file <- paste(train_dir, f, sep='/')
                        print_val <- gsub("/","\\\\", curr_file)
                        print(print_val)
                        f_data <- read.fwf(curr_file, field_widths)
                        # names(f_data) <- features
                        print(dim(f_data))
                        break
                }

        }



}
