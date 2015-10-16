this.dir <- dirname(sys.frame(1)$ofile) 


getMatchups <- function() {
	matchups <- read.csv(paste0(this.dir,"/NFLSchedule.csv"), header = TRUE, sep = ",", stringsAsFactors=FALSE)
	return (matchups)
}

getDefensiveStats <- function() {
	defStats <- read.csv(paste0(this.dir,"/DefensiveStatsWeek5.csv"), header = TRUE, sep = ",", stringsAsFactors=FALSE)
	return (defStats)
}


getOpponent <- function(team, week) {
	matchups <- getMatchups()
	if (substr(matchups[[team]][week],1,1) == "@") {
		return(substr(matchups[[team]][week],2,nchar(matchups[[team]][week])))
	}
	else {
		return(matchups[[team]][week])
	}
}



getDefensiveStatsfromTeam <- function(team) {
	defStats <- getDefensiveStats()
	teamStats <- defStats[[team]]
	return(teamStats)
}

getOppositionDefStat <- function(team, week) {
	opponent <- getOpponent(team, week)
	if (opponent == "BYE") {
		oppStats <- as.list(rep(NA, 8))
	}
	else {
		oppStats <- getDefensiveStatsfromTeam(opponent)
	}
	oppStats[length(oppStats) + 1] <- week
	oppStats[length(oppStats) + 1] <- opponent
	return (oppStats)
}

getOpponentStats <- function(team, curWeek) {
	oppFrame <- getOppFrame()
	for (i in 1:curWeek) {
		oppStats <- getOppositionDefStat(team, i)
		if (length(oppStats) > 0) {
			oppStats <- getOppositionDefStat(team, i)
			oppFrame[nrow(oppFrame) + 1,] = oppStats
		}
	}
	return (oppFrame)
}

getOppFrame <- function() {
	df = data.frame(YDS=numeric(),
					YDSG=numeric(),	
					PASS=numeric(),	
					PYDSG=numeric(),
					RUSH=numeric(),
					RYDSG=numeric(),
					PTS=numeric(),
					PTSG=numeric(),
					WEEK=numeric(),
					TEAM=character(),
					stringsAsFactors=F)
	return (df)

}
