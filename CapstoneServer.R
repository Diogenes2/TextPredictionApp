   ## Capstone Project Server

library(shiny)
library(dplyr)
library(tidyr)
load(file = "~/R AND GITHUB AND EVERYTHING/R/Capstone/WeekOne/Worddfs/W2.Rda")
load(file = "~/R AND GITHUB AND EVERYTHING/R/Capstone/WeekOne/Worddfs/W3.Rda")
load(file = "~/R AND GITHUB AND EVERYTHING/R/Capstone/WeekOne/Worddfs/W4.Rda")
big <- Words2
trig <- Words3
quadg <- Words4
rm(Words2,Words3,Words4)
big$word <- as.character(big$word)
trig$word <- as.character(trig$word)
quadg$word <- as.character(quadg$word)
bigtext <- big$word
trigtext <- trig$word
quadgtext <- quadg$word
eb <- unlist( strsplit( bigtext , "\\ " ) )
et <- unlist( strsplit( trigtext , "\\ " ) )
eq <- unlist( strsplit( quadgtext , "\\ " ) )
mb <- matrix( eb , ncol = 2 , byrow = TRUE )
mt <- matrix( et , ncol = 3 , byrow = TRUE )
mq <- matrix( eq , ncol = 4 , byrow = TRUE )
dfb <- as.data.frame(mb)
dft <- as.data.frame(mt)
dfq <- as.data.frame(mq)
bigram <- cbind.data.frame(dfb,big$num)
trigram <- cbind.data.frame(dft,trig$num)
quadgram <- cbind.data.frame(dfq,quadg$num)

CapstoneServer <- function(input, output) {
        txtf <- reactive({
                 
                ifelse(input$text == "Type words here!", paste("Warning: No text has been entered yet!"),
                       ifelse(input$text == "", paste("No words have been typed in, please input one to three words."),
                                 paste("You have input:", input$text)))
        
         
        })
        
        predw <- reactive({
                lengthtest <- tolower(input$text)
                lengthtest <- strsplit(lengthtest," ")[[1]]
                strlength <- length(lengthtest)           
                paste("The number of words currently being input is:", strlength, ifelse(strlength>3, paste("Which is too many words."),
                        ifelse(strlength==0,paste("Which means I will predict the word 'the'."),paste("Which is an acceptable number of words."))))
                
        })
        
        pred <- reactive({
                lentest <- tolower(input$text)
                lentest <- strsplit(lentest," ")[[1]]
                lt <-length(lentest) 
                if (lt==0){
                       bm <- paste("Warning: No words given!","Our predicted next word is: the")

                }
                if (lt==1){
                        wordone <- lentest[1]
                        brows <- which(bigram$V1 == wordone)
                        if (length(brows)==0){
                                bm <- paste("The provided word is not common enough to enter my data and no prediction can be provided.")       
                        }
                        if (length(brows)>0){
                        bsubset <- bigram[brows,]
                        res <- bsubset[1,2]
                        bm <- paste("Our predicted next word is:",res)
                        }
                        

                }
                if (lt==2){
                        triwordone <- lentest[1]
                        triwordtwo <- lentest[2]
                        trowsone <- which(trigram$V1 == triwordone)
                        trowstwo <- which(trigram$V2 == triwordtwo)
                        if(length(trowsone)==0&length(trowstwo)==0){
                          bm <- paste("The provided words are not common enough to enter my data and no prediction can be provided.")       
                        }
                        if(length(trowsone)==0){
                           trirows <- which(bigram$V1 == triwordtwo)
                           if (length(trirows)==0){
                                   bm <- paste("The provided words are not common enough to enter my data and no prediction can be provided.")       
                           }
                           if (length(trirows)>0){
                                   bsubset <- bigram[trirows,]
                                   res <- bsubset[1,2]
                                   bm <- paste("Our predicted next word is:",res)
                           }
                           
                        }
                        if(length(trowstwo)==0){
                                trirows <- which(bigram$V1 == triwordone)
                                if (length(trirows)==0){
                                        bm <- paste("The provided words are not common enough to enter my data and no prediction can be provided.")       
                                }
                                if (length(trirows)>0){
                                        bsubset <- bigram[trirows,]
                                        res <- bsubset[1,2]
                                        bm <- paste("Our predicted next word is:",res)
                                }
                                
                        }
                        
                        
                        tribothlist <- intersect(trowsone,trowstwo)
                        if(length(tribothlist)==0&length(trowstwo)>0&length(trowsone)>0){
                                trirows <- which(bigram$V1 == triwordtwo)
                                if (length(trirows)==0){
                                        bm <- paste("The provided words are not common enough to enter my data and no prediction can be provided.")       
                                }
                                if (length(trirows)>0){
                                        bsubset <- bigram[trirows,]
                                        res <- bsubset[1,2]
                                        bm <- paste("Our predicted next word is:",res)
                                }
                                
                        }
                        if(length(tribothlist)>0){
                                trisubset <- trigram[tribothlist,]
                                res <- trisubset[1,3]
                                bm <- paste("Our predicted next word is:",res)
                        }
                        
                        
                       

                }
                if (lt==3){
                        qwordone <- lentest[1]
                        qwordtwo <- lentest[2]
                        qwordthree <- lentest[3]
                        qrowsone <- which(quadgram$V1 == qwordone)
                        qrowstwo <- which(quadgram$V2 == qwordtwo)
                        qrowsthree <- which(quadgram$V3 == qwordthree)
                        qintersectlist <- dplyr::intersect(qrowsone,qrowstwo)
                        qintersectlist <- dplyr::intersect(qintersectlist,qrowsthree)
                        if(length(qintersectlist)>0){
                                qsubset <- quadgram[qintersectlist,]
                                res <- qsubset[1,4]
                                bm <- paste("Our predicted next word is:",res)
                        }
                        if(length(qintersectlist)==0){
                                quadrows <- which(bigram$V1 == qwordthree)
                                if(length(quadrows>0)){
                                qsubset <- bigram[quadrows,]
                                res <- qsubset[1,2]
                                bm <- paste("Our predicted next word is:",res)}
                                if(length(quadrows==0)){
                                        qrs <- which(bigram$V1 == qwordtwo)
                                        if(length(qrs>0)){
                                                qqsubset <- bigram[qrs,]
                                                res <- qqsubset[1,2]
                                                bm <- paste("Our predicted next word is:",res)}
                                        if(length(qrs==0)){
                                        bm <- paste("The provided words are not common enough to enter my data and no prediction can be provided.")       
                                        }
                                        
                        }
                        

                }}
                if (lt>3){
                       bm <- paste("Warning: Too many words given!","Our predicted next word is: the")

                }


              bm  
               
        })
        
        
        output$text1 <- renderText({
                txtf()
        })
        
        output$wp <- renderText({
                predw()
        })
        
        output$p <- renderText({
                pred()
        })
        
}
        

