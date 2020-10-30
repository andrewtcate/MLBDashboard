# install.packages('rsconnect')
library(rsconnect)
library(shiny)
library(regclass)

# Reading in Data
BATTING <- read.csv("http://andrewtcate.com/wp-content/uploads/2020/10/2020TeamStandardBatting.csv")
FIELDING <- read.csv("http://andrewtcate.com/wp-content/uploads/2020/10/2020TeamFielding.csv")
PITCHING <- read.csv("http://andrewtcate.com/wp-content/uploads/2020/10/2020TeamStandardPitching.csv")
STANDINGS <- read.csv("http://andrewtcate.com/wp-content/uploads/2020/10/2020TeamStandings.csv")
BETS <- read.csv("http://andrewtcate.com/wp-content/uploads/2020/10/mlb-odds-2020.csv")

# Cleaning up MLB Stats Data
BATTING <- BATTING[1:30,]
FIELDING <- FIELDING[1:30,]
PITCHING <- PITCHING[1:30,]
STANDINGS <- STANDINGS[1:30,2:ncol(STANDINGS)]


# Renaming Teams who have "clinched" already
STANDINGS[1,1] <- "LAD"
STANDINGS[2,1] <- "TBR"
STANDINGS[3,1] <- "CHW"
STANDINGS[5,1] <- "OAK"

# Suppress warning message that says a column is duplicated in merge
defaultW <- getOption("warn") 
options(warn = -1) 
TEAMS <- merge(STANDINGS,BATTING, by = "Tm")
TEAMS <- merge(TEAMS,FIELDING, by = "Tm")
TEAMS <- merge(TEAMS,PITCHING, by = "Tm")
options(warn = defaultW)

# Declaring a variable numericStats that will hold the names of stats that are numeric
numericStats <- c()

MLBcolnames <- c("Team","League","Games","Wins","Losses","WinLoss","Streak","RunsScoredAvg","RunsAllowedAvg","RunDifferential","StrengthOfSchedule","SimpleRatingSystem","PythagoreanWinLoss","Luck","vsEast","vsCent","vsWest","Inter","HomeWL","RoadWL","ExInWL","1Run","vsRHP","vsLHP","vs501","vs499","Last10","Last20","Last30","TotalBatters","AvgAge","RunsPerGame","GamesPlayed","PlateAppearances","AtBats","Runs","Hits","2Bs","3Bs","HRs","RBIs","StolenBases","CaughtStealing","BaseOnBalls","StrikeOuts","BattingAverage","OnBasePercentage","SluggingPercentage","OB+SluggingPercent","OPS+","TotalBases","DoublePlaysGroundedInto","HitByPitch","SacrificeHits","SacrificeFlies","IntentionalBaseOnBalls","RunnersLeftOnBase","TotalFielders","RunsAgainstPerGame","DefensiveEfficiency","GamesPlayed1","GamesStarted","CompleteGames","Innings","DefensiveChances","Putouts","Assists","Errors","DoublePlays","FieldingPercentage","TotalZoneTotalFieldingRunsAboveAvg","TotalZoneTotalFieldingRunsAboveAvgPer1200In","BISDefensiveRunsSavedAboveAvg","BISDefensiveRunsSavedAboveAvgPer1200In","BISGoodPlaysMisplaysRunsAboveAvg","TotalPitchers","PitchingAge","RunsAllowedPerGame","Wins","Losses","WinLoss","EarnedRunsAgainst","GamesPlayed3","GamesStarted","GamesFinished","CompleteGame","TeamShutouts","SingleShutouts","Saves","InningsPitched","HitsAllowed","RunsAllowed","EarnedRunsAllowed","HomeRunsAllowed","Walks","IntentionalWalks","PitcherStrikeOuts","BattersHit","Balks","WildPitches","BattersFaced","ERA+","FieldingIndependentPitching","WHIP","H9","HR9","BB9","SO9","SO/W","RunnersStranded")

# Renaming columns in MLB Stats data
for (i in 1:ncol(TEAMS)) {
    names(TEAMS)[i] <- MLBcolnames[i]
}

# Changing rownames from #s to Teams
rownames(TEAMS) <- TEAMS$Team
TEAMS$Team <- NULL


# Filling numericStats list
for (i in 1:ncol(TEAMS)) {
    if(sapply(TEAMS,typeof)[i] %in% c("integer","double")){
        numericStats[i] <- names(TEAMS)[i]
    }
    
}

# Remove NAs from numericStats
numericStats <- numericStats[-which(is.na(numericStats))]


# Cleaning up BETS data
BETS$X1st <- NULL
BETS$X2nd <- NULL
BETS$X3rd <- NULL
BETS$X4th <- NULL
BETS$X5th <- NULL
BETS$X6th <- NULL
BETS$X7th <- NULL
BETS$X8th <- NULL
BETS$X9th <- NULL
BETS$Rot <- NULL
names(BETS)[2] <- "HomeAway"
names(BETS)[9] <- "RunLineOdds"
names(BETS)[11] <- "OpenOUOdds"
names(BETS)[13] <- "CloseOUOdds"

BETS$HomeAway[which(BETS$HomeAway == "V")] <- "Away"
BETS$HomeAway[which(BETS$HomeAway == "H")] <- "Home"


# Creating GAME datafrane using Betting data
GAME <- data.frame(row.names = 1:(length(BETS$Date)/2))
GAME$GameID <- 0
GAME$HomeTeam <- ""
GAME$AwayTeam <- ""
GAME$HomeScore <- 0
GAME$AwayScore <- 0
GAME$Winner <- ""
GAME$HomePitcher <- ""
GAME$AwayPitcher <- ""
GAME$HomeOpenLine <- 0
GAME$AwayOpenLine <- 0
GAME$HomeRunLine <- 0
GAME$AwayRunLine <- 0
GAME$HomeRunLineOdds <- 0
GAME$AwayRunLineOdds <- 0
GAME$HomeOpenOU <- 0
GAME$HomeOpenOUOdds <- 0
GAME$AwayOpenOU <- 0
GAME$AwayOpenOUOdds <- 0
GAME$HomeCloseOU <- 0
GAME$HomeCloseOUOdds <- 0
GAME$AwayCloseOU <- 0
GAME$AwayCloseOUOdds <- 0
x <- 1:1612
x <- x[-which(x%%2 == 0)]
HomeTeam <- c()
AwayTeam <- c()
HomeScore <- c()
AwayScore <- c()
Winner <- c()
HomePitcher <- c()
AwayPitcher <- c()
HomeOpenLine <- c()
AwayOpenLine <- c()
HomeRunLine <- c()
AwayRunLine <- c()
HomeRunLineOdds <- c()
AwayRunLineOdds <- c()
HomeOpenOU <- c()
HomeOpenOUOdds <- c()
AwayOpenOU <- c()
AwayOpenOUOdds <- c()
HomeCloseOU <- c()
HomeCloseOUOdds <- c()
AwayCloseOU <- c()
AwayCloseOUOdds <- c()

for (i in x) {
    HomeTeam[i] <- BETS$Team[i]
    AwayTeam[i] <- BETS$Team[i+1]
    HomeScore[i] <- BETS$Final[i]
    AwayScore[i] <- BETS$Final[i+1]
    HomePitcher[i] <- BETS$Pitcher[i]
    AwayPitcher[i] <- BETS$Pitcher[i+1]
    HomeOpenLine[i] <- BETS$Open[i]
    AwayOpenLine[i] <- BETS$Open[i+1]
    HomeRunLine[i] <- BETS$RunLine[i]
    AwayRunLine[i] <- BETS$RunLine[i+1]
    HomeRunLineOdds[i] <- BETS$RunLineOdds[i]
    AwayRunLineOdds[i] <- BETS$RunLineOdds[i+1]
    HomeOpenOU[i] <- BETS$OpenOU[i]
    HomeOpenOUOdds[i] <- BETS$OpenOUOdds[i]
    AwayOpenOU[i] <- BETS$OpenOU[i+1]
    AwayOpenOUOdds[i] <- BETS$OpenOUOdds[i+1]
    HomeCloseOU[i] <- BETS$CloseOU[i]
    HomeCloseOUOdds[i] <- BETS$CloseOUOdds[i]
    AwayCloseOU[i] <- BETS$CloseOU[i+1]
    AwayCloseOUOdds[i] <- BETS$CloseOUOdds[i+1]
}

GAME$HomeTeam <- HomeTeam[-which(is.na(HomeTeam))]
GAME$AwayTeam <- AwayTeam[-which(is.na(AwayTeam))]
GAME$HomeScore <- HomeScore[-which(is.na(HomeScore))]
GAME$AwayScore <- AwayScore[-which(is.na(AwayScore))]
GAME$HomePitcher <- HomePitcher[-which(is.na(HomePitcher))]
GAME$AwayPitcher <- AwayPitcher[-which(is.na(AwayPitcher))]
GAME$HomeOpenLine <- HomeOpenLine[-which(is.na(HomeOpenLine))]
GAME$AwayOpenLine <- AwayOpenLine[-which(is.na(AwayOpenLine))]
GAME$HomeRunLine <- HomeRunLine[-which(is.na(HomeRunLine))]
GAME$AwayRunLine <- AwayRunLine[-which(is.na(AwayRunLine))]
GAME$HomeRunLineOdds <- HomeRunLineOdds[-which(is.na(HomeRunLineOdds))]
GAME$AwayRunLineOdds <- AwayRunLineOdds[-which(is.na(AwayRunLineOdds))]
GAME$HomeOpenOU <- HomeOpenOU[-which(is.na(HomeOpenOU))]
GAME$HomeOpenOUOdds <- HomeOpenOUOdds[-which(is.na(HomeOpenOUOdds))]
GAME$AwayOpenOU <- AwayOpenOU[-which(is.na(AwayOpenOU))]
GAME$AwayOpenOUOdds <- AwayOpenOUOdds[-which(is.na(AwayOpenOUOdds))]
GAME$HomeCloseOU <- HomeCloseOU[-which(is.na(HomeCloseOU))]
GAME$HomeCloseOUOdds <- HomeCloseOUOdds[-which(is.na(HomeCloseOUOdds))]
GAME$AwayCloseOU <- AwayCloseOU[-which(is.na(AwayCloseOU))]
GAME$AwayCloseOUOdds <- AwayCloseOUOdds[-which(is.na(AwayCloseOUOdds))]
GAME$Winner <- NULL
GAME$GameID <- 1:806


# Matching naming convention between tables
rownames(TEAMS)[5] <- "CUB"
rownames(TEAMS)[6] <- "CWS"
rownames(TEAMS)[12] <- "KAN"
rownames(TEAMS)[23] <- "SDG"
rownames(TEAMS)[25] <- "SFO"
rownames(TEAMS)[27] <- "TBR"
rownames(TEAMS)[30] <- "WAS"










#  Start Shiny App











# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("2020 MLB Team Stats Dashboard"),


    sidebarLayout(
        sidebarPanel(
            
            selectInput("yvar", label = "Choose a Y-Variable:", numericStats, selected = numericStats[1]),
            selectInput("xvar", label = "Choose a X-Variable:", numericStats, selected = numericStats[1]),
            checkboxInput("reference", "Add a reference line?", FALSE),
            sliderInput("labelsize",label = "Adjust Label Size:", 0, 2,1, 0.05),
            checkboxInput("comparison", "Comparing two teams?", FALSE),
            selectInput("team1", label = "Choose Team #1:", rownames(TEAMS), selected = rownames(TEAMS)[1]),
            selectInput("team2", label = "Choose Team #2:", rownames(TEAMS), selected = rownames(TEAMS)[2])
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput(outputId = "scatterplot"),
            verbatimTextOutput(outputId = "textbox"),
            dataTableOutput(outputId = "table")
        )
    )
)

server <- function(input, output) {

    output$scatterplot <- renderPlot({
        
        team1 <- input$team1
        team2 <- input$team2
        MATCHUP <- TEAMS[c(team1,team2),]
        x <- TEAMS[,input$xvar]
        y <- TEAMS[,input$yvar]
        title <- c(input$yvar," by ",input$xvar)
        
        labelsize <- input$labelsize
        
        if((input$labelsize == labelsize)){
            if(input$team1 == team1 & input$team2 == team2){
                plot(y~x, data = MATCHUP, ylab = input$yvar, xlab = input$xvar, main = title)
                text(y~x, data = MATCHUP, labels= rownames(TEAMS), cex=input$labelsize, font=1)
                if(input$reference == TRUE){abline(lm(y ~ x))}
            }
        }
    })
    
    
        output$textbox <- renderPrint({
            if(input$comparison == TRUE){
                TotalScored <- TEAMS[input$team1,"RunsScoredAvg"] + TEAMS[input$team2,"RunsScoredAvg"]
                TotalAllowed <- TEAMS[input$team1,"RunsAllowedAvg"] + TEAMS[input$team2,"RunsAllowedAvg"]
                cat(paste((c(input$team1," Runs Scored Average: ",TEAMS[input$team1,"RunsScoredAvg"]," Runs Allowed Average: ",TEAMS[input$team1,"RunsAllowedAvg"]))))
                cat("\n")
                cat(paste((c(input$team2," Runs Scored Average: ",TEAMS[input$team2,"RunsScoredAvg"]," Runs Allowed Average: ",TEAMS[input$team2,"RunsAllowedAvg"]))))
                cat("\n")
                cat(paste((c("Estimated Total Runs Scored: ",TotalScored))))
                cat("\n")
                cat(paste((c("Estimated Total Runs Allowed: ",TotalAllowed))))
            }
        }
        )
        
        
        output$table <- renderDataTable({
            if(input$comparison == TRUE){
                team1 <- input$team1
                team2 <- input$team2
                tablecols <- c("GameID","HomeTeam","AwayTeam","HomeScore","AwayScore","HomePitcher","AwayPitcher","HomeOpenLine","AwayOpenLine")
                GAME[which(GAME$HomeTeam %in% c(team1,team2) & GAME$AwayTeam %in% c(team1,team2)),tablecols]
            }
        })
        
    
}

# Run the application 
shinyApp(ui = ui, server = server)

