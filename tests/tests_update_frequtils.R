library(frequtils)
update_frequtils()

detach("package:frequtils", unload=TRUE)

frequtils::update_frequtils()

update_frequtils()

# Check to see if it worked:
get.script("dice6.R")
data(nba)
