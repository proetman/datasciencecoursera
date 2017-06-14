


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

        train_dir <- paste(dir_proj_data, 'train', 'Inertial Signals', sep='/')
        test_dir <- paste(dir_proj_data, 'test', 'Inertial Signals', sep='/')

        files <- list.files(path = train_dir,
                            pattern = '*.txt',
                            recursive = FALSE)

        field_widths <- rep(16, 128)
        for (f in files) {
                curr_file <- paste(train_dir, f, sep='/')
                print(gsub("/","\\\\", curr_file))
                f_data <- read.fwf(curr_file,
                                   field_widths)

        }


}
