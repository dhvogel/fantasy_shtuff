this.dir <- dirname(sys.frame(1)$ofile) 
source(paste0(this.dir,"/NFLIndividualPlots.R"))
source(paste0(this.dir,"/DefensiveStats.R"))

getPassingDefense <- function(team) {
	oppStats <- getOpponentStats(team, 5)
	a <- mean(as.numeric(oppStats[,"PYDSG"]), na.rm=TRUE)
	return(a)
}

getPassingYards <- function(playerName, playerPos) {

}