install.packages("swirl")
packageVersion("swirl")                      # verify greater than 2.2
install_from_swirl("R Programming")          # install R Programming from Swirl
swirl()

To load:
rm(list=ls())
library(swirl)
swirl()

install_from_swirl("Getting and Cleaning Data")



# Note: to exit
#     press escape
#     type  bye()
#
# other notes:
#   skip()   to skip current question
#   play()   play in R on your own
#   nxt()    return back to swirl

#   main()   return to main menu
#   info()   display these options

#   help.start()    loads help as either web page or within R studio
'
| When you are at the R prompt (>):
| -- Typing skip() allows you to skip the current question.
| -- Typing play() lets you experiment with R on your own; swirl will ignore what you do...
| -- UNTIL you type nxt() which will regain swirls attention.
| -- Typing bye() causes swirl to exit. Your progress will be saved.
| -- Typing main() returns you to swirls main menu.
| -- Typing info() displays these options again.

'
