# Load necessary libraries
library( shiny )
library( plyr )

# Load data sets from csv
battingDataset = read.csv("data/Batting.csv", header = TRUE)
careerBattingDataset = read.csv("data/CareerBatting.csv", header = TRUE)
hofDataset = read.csv("data/HallOfFame.csv", header = TRUE)

shinyServer(
    function(input, output){
        
        # Reactive expression to compose a data frame containing all of
        # the values
        tableValues <- reactive({

            # Filter the hof data to only inducted
            hofInducted <- hofDataset[ which(hofDataset$inducted=='Y'), ]
            hofInductedPlayerIds <- hofInducted[ ,'playerID']
            
            # Filter batting data based to only include inductees
            battingHof <- battingDataset[ battingDataset$playerID %in% hofInductedPlayerIds, ]
            
            # Sort by playerID
            arrange( battingHof, playerID )
            sortedBattingHof <- battingHof[ order( battingHof$playerID, battingHof$yearID ), ]
            
            # Total yearly stats per player
            hofCareerStatsDF <- NULL
            allHofCareerStatsDF <- NULL
            for( inductedPlayer in hofInductedPlayerIds )
            {
                # Subset just this player's data
                hofPlayerCareer <- battingHof[ which(battingHof$playerID==inductedPlayer), ]
                
                # Total hits, hrs, stls
                hitTotal <- sum( hofPlayerCareer$H )
                if( is.na( hitTotal ) ) hitTotal <- 0
                hrTotal <- sum( hofPlayerCareer$HR )
                if( is.na( hrTotal ) ) hrTotal <- 0
                sbTotal <- sum( hofPlayerCareer$SB )
                if( is.na( sbTotal ) ) sbTotal <- 0
                
                # Add to careerStats data frame if the player meets the user input criteria
                if( hitTotal > 1160 && hitTotal > input$hits && hrTotal > input$hrs && sbTotal > input$stls )
                {
                    hofCareerStatsDF <- rbind( hofCareerStatsDF, data.frame(inductedPlayer, hitTotal, hrTotal, sbTotal) )
                }
                
                # use lowest batter hit total to filter out pitchers
                if( hitTotal > 1160 && hrTotal > 0 && sbTotal > 0 )
                {
                    allHofCareerStatsDF <- rbind( allHofCareerStatsDF, data.frame(inductedPlayer, hitTotal, hrTotal, sbTotal) )
                }
            }
            
            # Find minimum values for the hof subset
            minHitsHOF <- min( allHofCareerStatsDF[,'hitTotal'] )
            minHRsHOF <- min( allHofCareerStatsDF[,'hrTotal'])
            minSBsHOF <- min( allHofCareerStatsDF[,'sbTotal'])
            
            # Find maximum values for the hof subset
            maxHitsHOF <- max( allHofCareerStatsDF[,'hitTotal'] )
            maxHRsHOF <- max( allHofCareerStatsDF[,'hrTotal'])
            maxSBsHOF <- max( allHofCareerStatsDF[,'sbTotal'])
            
            
            # Total number of players that meet the stat minimums input
            if( is.data.frame(hofCareerStatsDF) )
            {
                inputHofStatsCount <- nrow( hofCareerStatsDF )
            }
            else
            {
                inputHofStatsCount <- 0
            }
                        
            # Find the number of all players in MLB history that match the input requirements
            careerSubset <- careerBattingDataset[ which(careerBattingDataset$hitTotal > input$hits), ]
            careerSubset <- careerSubset[ which(careerSubset$hrTotal > input$hrs), ]
            careerSubset <- careerSubset[ which(careerSubset$sbTotal > input$stls), ]
            allCareerCount <- nrow( careerSubset )
            inductedPercent <- 0
            modPercent <- 0.0
            if( inputHofStatsCount > 0 )
            {
                inductedPercent <- (inputHofStatsCount / allCareerCount) * 100
                inductedPercent <- paste( round(inductedPercent,2),"%",sep="" )
            }
            else
            {
                # No matches already in the HoF, but we could still qualify
                hitsMod <- 0.0
                hrsMod <- 0.0
                sbsMod <- 0.0
                if( input$hits > minHitsHOF )
                {
                    if( input$hits > maxHitsHOF )
                    {
                        hitsMod <- 1.0       
                    }
                    else
                    {
                        hitsMod <- ( input$hits / maxHitsHOF )
                    }
                }
                if( input$hrs > minHRsHOF )
                {
                    if( input$hrs > maxHRsHOF )
                    {
                        hrsMod <- 1.0
                    }
                    else
                    {
                        hrsMod <- ( input$hrs / maxHRsHOF )
                    }
                }
                if( input$stls > minSBsHOF )
                {
                    if( input$stls > maxSBsHOF )
                    {
                        sbsMod <- 1.0
                    }
                    else
                    {
                        sbsMod <- ( input$stls / maxSBsHOF )
                    }
                }
                
                modPercent <- (( hitsMod + hrsMod + sbsMod ) / 3.0 )
            }
            
            # Any inut value falling below the existing HOF minimum amount forces a 0% future chance
            # This should be updated to a more robust algorithm that allows some variation below existing minimums
            if( modPercent == 0.0 )
            {
                if( input$hits < 1161 ) inductedPercent <- 0
                if( input$hrs < 28 ) inductedPercent <- 0
                if( input$stls < 8 ) inductedPercent <- 0
            }
            else
            {
                modPercent <- modPercent * 100
                inductedPercent <- paste( round(modPercent,2),"%",sep="" )
            }
            
            # Compose data frame and return
            data.frame(
                Name = c("Hits", 
                         "HRs",
                         "Steals",
                         "All Players Career",
                         "HOF Players Career",
                         "Odds of Induction"),
                Value = as.character(c(input$hits, 
                                       input$hrs,
                                       input$stls,
                                       inputHofStatsCount,
                                       allCareerCount,
                                       inductedPercent)), 
                stringsAsFactors=FALSE)
        })
    
        output$tableValues <- renderTable({
            tableValues()
        })
        
    }
)