this.dir <- dirname(sys.frame(1)$ofile) 
source(paste0(this.dir,"/DefensiveStats.R"))

loadWeeklyData <- function() {
	print(this.dir)
	week1 <- read.csv(paste0(this.dir, "/Week1Stats.csv"), header = TRUE, sep = ",", stringsAsFactors=FALSE)
	week2 <- read.csv(paste0(this.dir, "/Week2Stats.csv"), header = TRUE, sep = ",", stringsAsFactors=FALSE)
	week3 <- read.csv(paste0(this.dir, "/Week3Stats.csv"), header = TRUE, sep = ",", stringsAsFactors=FALSE)
	week4 <- read.csv(paste0(this.dir, "/Week4Stats.csv"), header = TRUE, sep = ",", stringsAsFactors=FALSE)
	week5 <- read.csv(paste0(this.dir, "/Week5Stats.csv"), header = TRUE, sep = ",", stringsAsFactors=FALSE)

	return (list(week1, week2, week3, week4, week5))
}


getWeeklyDataforPlayer <- function(playerName, playerPos) {
	frame <- getDataFrame()
	weekData <- loadWeeklyData()
	for (i in 1:length(weekData)) {
		if (dim(subset(weekData[[i]], name==playerName & pos==playerPos))[1] > 0) {
			frame[nrow(frame) + 1,] = subset(weekData[[i]], name==playerName & pos==playerPos)
		}
	}
	return(frame)
}

getDataFrame <- function() {

	df = data.frame(week=numeric(),
					name=character(),	
					id=character(),	
					home=character(),
					team=character(),	
					pos=character(),	
					defense_ast=numeric(),	
					defense_ffum=numeric(),	
					defense_int=numeric(),	
					defense_sk=numeric(),	
					defense_tkl=numeric(),	
					fumbles_lost=numeric(),	
					fumbles_rcv=numeric(),	
					fumbles_tot=numeric(),	
					fumbles_trcv=numeric(),	
					fumbles_yds=numeric(),	
					kicking_fga=numeric(),	
					kicking_fgm=numeric(),	
					kicking_fgyds=numeric(),	
					kicking_totpfg=numeric(),	
					kicking_xpa=numeric(),
					kicking_xpb=numeric(),	
					kicking_xpmade=numeric(),	
					kicking_xpmissed=numeric(),	
					kicking_xptot=numeric(),	
					kickret_avg=numeric(),	
					kickret_lng=numeric(),	
					kickret_lngtd=numeric(),	
					kickret_ret=numeric(),	
					kickret_tds=numeric(),	
					passing_att=numeric(),	
					passing_cmp=numeric(),	
					passing_ints=numeric(),	
					passing_tds=numeric(),	
					passing_twopta=numeric(),	
					passing_twoptm=numeric(),	
					passing_yds=numeric(),	
					punting_avg=numeric(),	
					punting_i20=numeric(),	
					punting_lng=numeric(),	
					punting_pts=numeric(),	
					punting_yds=numeric(),	
					puntret_avg=numeric(),	
					puntret_lng=numeric(),	
					puntret_lngtd=numeric(),	
					puntret_ret=numeric(),	
					puntret_tds=numeric(),	
					receiving_lng=numeric(),	
					receiving_lngtd=numeric(),	
					receiving_rec=numeric(),	
					receiving_tds=numeric(),	
					receiving_twopta=numeric(),	
					receiving_twoptm=numeric(),	
					receiving_yds=numeric(),	
					rushing_att=numeric(),
					rushing_lng=numeric(),	
					rushing_lngtd=numeric(),	
					rushing_tds=numeric(),	
					rushing_twopta=numeric(),	
					rushing_twoptm=numeric(),	
					rushing_yds=numeric(),
					stringsAsFactors=F)
	return(df)
}

plotPassingStats <- function(playerData) {
	#setWorkingDirectory(playerData)
	playerName = playerData[1,]$name
	par(mfrow=c(2,3))
	plot(playerData$week, playerData$passing_att, main=paste0(playerName, " passing attempts"), xlim=c(1,5), ylim=c(0,70))
	text(playerData$week, playerData$passing_att, playerData$passing_att, pos=1, offset=-1, cex=0.7)
	plot(playerData$week, playerData$passing_cmp, main=paste0(playerName, " passing completions"), xlim=c(1,5), ylim=c(0,70))
	text(playerData$week, playerData$passing_cmp, playerData$passing_cmp, pos=1, offset=-1, cex=0.7)
	plot(playerData$week, playerData$passing_yds, main=paste0(playerName, " passing yards"), xlim=c(1,5), ylim=c(0,500))
	text(playerData$week, playerData$passing_yds, playerData$passing_yds, pos=1, offset=-1, cex=0.7)
	plot(playerData$week, playerData$passing_tds, main=paste0(playerName, " passing TDs"), xlim=c(1,5), ylim=c(0,5))
	text(playerData$week, playerData$passing_tds, playerData$passing_tds, pos=1, offset=-1, cex=0.7)
	plot(playerData$week, playerData$passing_ints, main=paste0(playerName, " passing INTs"), xlim=c(1,5), ylim=c(0,5))
	text(playerData$week, playerData$passing_ints, playerData$passing_ints, pos=1, offset=-1, cex=0.7)
	plot(playerData$week, playerData$rushing_yds, main=paste0(playerName, " rushing yards"), xlim=c(1,5), ylim=c(0,100))
	text(playerData$week, playerData$rushing_yds, playerData$rushing_yds, pos=1, offset=-1, cex=0.7)
	#dev.off()
}


plotRushingStats <- function(playerData, oppData) {
	#setWorkingDirectory(playerData)
	playerName = playerData[1,]$name
	par(mfrow=c(2,3))
	plot(playerData$week, playerData$rushing_att, main=paste0(playerName, " rushing attempts"), xlim=c(1,5), ylim=c(0,30))
	text(playerData$week, playerData$rushing_att, playerData$rushing_att, pos=1, offset=-1, cex=0.7)
	plot(playerData$week, playerData$rushing_yds, main=paste0(playerName, " rushing yards"), xlim=c(1,5), ylim=c(0,250))
	text(playerData$week, playerData$rushing_yds, playerData$rushing_yds, pos=1, offset=-1, cex=0.7)
	points(oppData$WEEK, oppData$RYDSG, col="red")
	text(oppData$RYDSG, paste0(oppData$RYDSG,"-",oppData$TEAM), as.character(oppData$TEAM), pos=1, offset=-1, cex=0.7, col="red")
	plot(playerData$week, playerData$rushing_tds, main=paste0(playerName, " rushing TDs"), xlim=c(1,5), ylim=c(0,5))
	text(playerData$week, playerData$rushing_tds, playerData$rushing_tds, pos=1, offset=-1, cex=0.7)
	plot(playerData$week, playerData$receiving_rec, main=paste0(playerName, " receptions"), xlim=c(1,5), ylim=c(0,20))
	text(playerData$week, playerData$receiving_rec, playerData$receiving_rec, pos=1, offset=1, cex=0.7)
	plot(playerData$week, playerData$receiving_yds, main=paste0(playerName, " receiving yards"), xlim=c(1,5), ylim=c(0,200))
	text(playerData$week, playerData$receiving_yds, playerData$receiving_yds, pos=1, offset=-1, cex=0.7)
	plot(playerData$week, playerData$receiving_tds, main=paste0(playerName, " receiving TDs"), xlim=c(1,5), ylim=c(0,5))
	text(playerData$week, playerData$receiving_tds, playerData$receiving_tds, pos=1, offset=-1, cex=0.7)
	#dev.off()
}

plotReceivingStats <- function(playerData) {
	#setWorkingDirectory(playerData)
	playerName = playerData[1,]$name
	par(mfrow=c(1,3))
	plot(playerData$week, playerData$receiving_rec, main=paste0(playerName, " receptions"), xlim=c(1,5), ylim=c(0,20))
	text(playerData$week, playerData$receiving_rec, playerData$receiving_rec, pos=1, offset=-1, cex=0.7)
	plot(playerData$week, playerData$receiving_yds, main=paste0(playerName, " receiving yards"), xlim=c(1,5), ylim=c(0,200))
	text(playerData$week, playerData$receiving_yds, playerData$receiving_yds, pos=1, offset=-1, cex=0.7)
	plot(playerData$week, playerData$receiving_tds, main=paste0(playerName, " receiving TDs"), xlim=c(1,5), ylim=c(0,5))
	text(playerData$week, playerData$receiving_tds, playerData$receiving_tds, pos=1, offset=-1, cex=0.7)
	#dev.off()
}

plotKickingStats <- function(playerData) {
	#setWorkingDirectory(playerData)
	playerName = playerData[1,]$name
	par(mfrow=c(1,3))
	plot(playerData$week, playerData$kicking_fga, main=paste0(playerName, " FG Attempts"), xlim=c(1,5), ylim=c(0,8))
	text(playerData$week, playerData$kicking_fga, playerData$kicking_fga, pos=1, offset=-1, cex=0.7)
	plot(playerData$week, playerData$kicking_fgm, main=paste0(playerName, " FG Made"), xlim=c(1,5), ylim=c(0,8))
	text(playerData$week, playerData$kicking_fgm, playerData$kicking_fgm, pos=1, offset=-1, cex=0.7)
	plot(playerData$week, playerData$kicking_xpa, main=paste0(playerName, " Extra Point Attempts"), xlim=c(1,5), ylim=c(0,8))
	text(playerData$week, playerData$kicking_xpa, playerData$kicking_xpa, pos=1, offset=-1, cex=0.7)
	#dev.off()
}

plotPlayer <- function(playerName, team, position) {
	
	frame <- getWeeklyDataforPlayer(playerName, position)
	opponents <- getOpponentStats(team, 5)
	pos <- frame[1,]$pos
	if (pos == "QB") {
		plotPassingStats(frame)
	} else if (pos == "RB") {
		plotRushingStats(frame, opponents)
	} else if (pos == "WR" || pos=="TE") {
		plotReceivingStats(frame)
	} else if (pos == "K") {
		plotKickingStats(frame)
	} else {
		print(paste0("Position unrecognized: ", pos))
	}
}


plotPosition <- function(position) {
	data <- loadWeeklyData()
	for (j in 1:length(data)) {
		players <- subset(data[[j]], pos==position)
		for (i in 1:length(players)) {
			plotPlayer(players[i,]$name, players[i,]$team, position)
		}
	}
}



setWorkingDirectory <- function(playerData) {
	playerName = playerData[1,]$name
	playerTeam = playerData[1,]$team
	playerPos = playerData[1,]$pos
	teamFilePath <- file.path("/Users","dhvogel","Documents","NFL","TeamData",playerTeam)
	posFilePath <- file.path("/Users","dhvogel","Documents","NFL","TeamData",playerTeam, playerPos)
	print(posFilePath)
	dir.create(teamFilePath)
	dir.create(posFilePath)
	setwd(posFilePath)
	pdf(paste0(playerName,".pdf"))
}

