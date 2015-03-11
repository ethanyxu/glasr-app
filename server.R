# server.R
## source("ReadData.R")
source("./functions/autoSeg.R")
source("./functions/registration.R")
source("./functions/lasrTest.R")
source("./functions/plot.lasr.R")
source("./functions/choose_h.R")
source("./functions/densitiesToFrame.R")
source("./functions/manualRegist.R")
source("./functions/gf.R")
source("./functions/calculateM.R")
source("./functions/autoRegist.R")

list.of.packages <- c("RColorBrewer", "RNiftyReg", "bootstrap", "pbapply", "mclust", "shiny", "pracma")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(RColorBrewer)
library(RNiftyReg)
library(bootstrap)
library(pbapply)
library(mclust)
library(shiny)
library(pracma)

shinyServer(
    function(input, output, session) {

########################
### Data upload
########################
        
        frames1CSV <- reactive({
            if(input$defaultData)
                read.csv("./data/frames1.csv", header = FALSE, sep = ",")
            else{
                inFile <- input$file1
                if (is.null(inFile))
                    return(NULL)
                read.csv(inFile$datapath, header = FALSE, sep = ",")
            }
        })
        
        frames2CSV <- reactive({
            if(input$defaultData)
                read.csv("./data/frames2.csv", header = FALSE, sep = ",")
            else{
                inFile <- input$file2
                if (is.null(inFile))
                    return(NULL)
                read.csv(inFile$datapath, header = FALSE, sep = ",")
            }
        })

        frames1 <- reactive({
            input$genData
            if(is.null(frames1CSV())) return(NULL)
            lapply(frames1CSV(), function(x) matrix(x, nrow = isolate(input$nrows)))
        })

        frames2 <- reactive({
            input$genData
            if(is.null(frames1CSV())) return(NULL)            
            lapply(isolate(frames2CSV()), function(x) matrix(x, nrow = isolate(input$nrows)))
        })        
        
        nframes <- reactive({length(frames1())})

        nrows <- reactive({nrow(frames1()[[1]])})
        
        ncols <- reactive({ncol(frames1()[[1]])})

        myasp <- reactive({nrows()/ncols()})
        
########################
### Basic panel
########################
        myframeBasic <- reactive({
            mySeq <- switch(input$var1, 
                            "Sequence one" = frames1(),
                            "Sequence two" = frames2())
            mySeq[[ifelse(is.null(input$iframeBasic), 1, input$iframeBasic)]]
        })
        
        ##-------slider range 1--------##
        output$iframeUIBasic <- renderUI({
            sliderInput("iframeBasic", 
                        label = "Which frame do you want to see?",
                        min = 1, max = nframes(), value = 1)
        })
        
        output$sliderRange1 <- renderUI({
            sliderInput("range1",
                        label = "Range of pixel intensities to display",
                        min = min(myframeBasic()), max = max(myframeBasic()),
                        value = c(min(myframeBasic()), max(myframeBasic())))
        })

        ##------- Image 1--------##
        output$oneframe1 <- renderPlot({
            ## par(mar = c(2, 2, 2, 2), oma = c(0, 0, 0, 0))
            mytitle <- paste0(input$var1, "; Frame ", input$iframeBasic)
            mycols <- switch(input$cols1,
                             "heat" = heat.colors(50, alpha = 1),
                             "gray" = colorRampPalette(brewer.pal(9, "Greys"))(50),
                             "blue" = colorRampPalette(brewer.pal(9, "Blues"))(50),
                             "green" = colorRampPalette(brewer.pal(9, "Greens"))(50))
            imageArgBasic <- list(x = myframeBasic(), asp = myasp(),
                                  frame.plot = F, main = mytitle, xaxt = "n", yaxt = "n", col = mycols)
            if(is.null(input$range1)) imageArgBasic$zlim <- c(input$range1[1], input$range1[2])
            do.call(image, imageArgBasic)
        })

        ##------- Histogram 1--------##
        output$hist1 <- renderPlot({
            histArgBasic <- list(x = myframeBasic(), breaks = input$bins1,
                                 col = "skyblue", main = "", xlab = "Pixel intensities")
            if(!is.null(input$range1)){
                histArgBasic$x = myframeBasic()[(myframeBasic() >= input$range1[1]) & (myframeBasic() <= input$range1[2])]
            }
            do.call(hist, histArgBasic)
        })
        
########################
### Segmentation
########################

        colsSeg <- reactive({
            switch(input$cols2,
                   "heat" = heat.colors(50, alpha = 1),
                   "gray" = colorRampPalette(brewer.pal(9, "Greys"))(50),
                   "blue" = colorRampPalette(brewer.pal(9, "Blues"))(50),
                   "green" = colorRampPalette(brewer.pal(9, "Greens"))(50))
        })
        
        manualThreshold1 <- reactive({
            if(input$useThreshold1 == 0) return(0)
            if(is.null(isolate(input$plot_click1))) return(0)
            isolate(input$plot_click1$x)
        })

        manualThreshold2 <- reactive({
            if(input$useThreshold1 == 0) return(0)
            if(is.null(isolate(input$plot_click2))) return(0)
            isolate(input$plot_click2$x)
        })
        
        framesSegManual1 <- reactive({
            if(input$useThreshold1 == 0) return(NULL)
            lapply(frames1(), function(oneframe){
                    oneframe[oneframe < isolate(manualThreshold1())] <- 0
                    return(oneframe)
                })
        })

        framesSegManual2 <- reactive({
            if(input$useThreshold2 == 0) return(NULL)            
            lapply(frames2(), function(oneframe){
                    oneframe[oneframe < isolate(manualThreshold2())] <- 0
                    return(oneframe)
                })
        })        
        
        densities1 <- reactive({
            withProgress(message = 'Auto Segmentation', value = 0, {
                n <- length(frames1())
                ans <- vector("list", n)
                for (i in 1:n) {
                    ans[[i]] <- autoSeg(frames1()[[i]])
                    incProgress(1/n, detail = paste("Processing Seq 1, frame", i, "of", n))
                }
            })
            ans
        })
        
        framesSegAuto1 <- reactive({
            lapply(densities1(), FUN = densitiesToFrame)
        })

        densities2 <- reactive({
            withProgress(message = 'Doing Auto Segmentation', value = 0, {
                n <- length(frames2())
                ans <- vector("list", n)
                for (i in 1:n) {
                    ans[[i]] <- autoSeg(frames2()[[i]])
                    incProgress(1/n, detail = paste("Processing Seq 2, frame", i, "of", n))
                }
            })
            ans
            ## pblapply(frames2(), FUN = autoSeg)
        })
        
        framesSegAuto2 <- reactive({
            pblapply(densities2(), FUN = densitiesToFrame)
        })

        output$chooseShowUI <- renderUI({
            if(input$segAll1 > 0 | input$segAll2 > 0)
                radioButtons("chooseShow", label = NULL,
                             choices = list("Select Threshold" = 1, "Show Result" = 2), selected = 2)
            else
                radioButtons("chooseShow", label = NULL,
                             choices = list("Select Threshold" = 1, "Show Result" = 2), selected = 1)          
        })

        output$iframeAutoSeg <- renderUI({
            sliderInput("iframeSegAuto", label = "Which frame do you want to see?",
                        min = 1, max = nframes(), value = 1)
        })

        output$iframeManualSeg <- renderUI({
            sliderInput("iframeSeg", label = "Select the reference frame for manual segmentation",
                        min = 1, max = nframes(), value = 1)
        })        
        
        ##-------histogram slider range--------##
        output$sliderRange2 <- renderUI({
            mySeq <- switch(input$var2, 
                            "Sequence one" = frames1(),
                            "Sequence two" = frames2())
            myframe <- mySeq[[switch(
                input$manualAuto,
                "1" = ifelse(is.null(input$iframeSeg), 1, input$iframeSeg),
                "2" = ifelse(is.null(input$iframeSegAuto), 1, input$iframeSegAuto)
                )]]
            sliderInput("range2",
                        label = "Range of pixel intensities to display",
                        min = min(myframe), max = max(myframe),
                        value = c(min(myframe), max(myframe)))
        })

        ##------- Image before segmentation--------##
        output$oneframe_bf_seg <- renderPlot({
            mySeq <- switch(input$var2, 
                            "Sequence one" = frames1(),
                            "Sequence two" = frames2())
            myframe <- mySeq[[switch(
                input$manualAuto,
                "1" = ifelse(is.null(input$iframeSeg), 1, input$iframeSeg),
                "2" = ifelse(is.null(input$iframeSegAuto), 1, input$iframeSegAuto)                
                )]]
            par(mar = c(2, 2, 2, 2), oma = c(1, 1, 1, 1))
            mytitle <- paste0("Before Segmentation\n", input$var2, ";Frame ", input$iframeSeg)
            image.args <- list(x = myframe, asp = myasp(),
                               frame.plot = F, main = mytitle, xaxt = "n", yaxt = "n", col = colsSeg())
            if(!is.null(input$range2)) image.args$zlim <- c(input$range2[1], input$range2[2])
            do.call(image, image.args)
        })

        ##------- Image after segmentation, manual, selecting panel--------##
        output$imgAftSegSelect <- renderPlot({
            nothingToShow <- FALSE
            par(mar = c(2, 2, 2, 2), oma = c(1, 1, 1, 1))
            mytitle <- paste0("After Segmentation\n", input$var2, "; Frame ", input$iframeSeg)
            currentThreshold <- switch(input$var2,
                                        "Sequence one" = input$plot_click1$x,
                                        "Sequence two" = input$plot_click2$x)
            if(is.null(currentThreshold)){
                nothingToShow <- TRUE
                myframeAftSeg <- matrix(0, nrows(), ncols())
            }else{
                myframeAftSeg <- switch(input$var2,
                                        "Sequence one" = frames1()[[input$iframeSeg]],
                                        "Sequence two" = frames2()[[input$iframeSeg]])
                myframeAftSeg[myframeAftSeg <= currentThreshold] <- 0
            }
            image.args <- list(x = myframeAftSeg, asp = myasp(),
                               frame.plot = F, main = mytitle, xaxt = "n", yaxt = "n", col = colsSeg())
            if(!is.null(input$range2)) image.args$zlim = c(input$range2[1], input$range2[2])
            do.call(image, image.args)
            ## Write text
            if(nothingToShow) text(x = 0.5, y = 0.5, labels = "Threshold not selected yet." )            
        })

        ##------- Image after segmentation, auto--------##
        output$imgAftSegAuto <- renderPlot({
            par(mar = c(2, 2, 2, 2), oma = c(1, 1, 1, 1))
            mytitle <- paste0("After Segmentation\n", input$var2, "; Frame ", input$iframeSegAuto)
            myframeAftSeg <- switch(input$var2,
                                    "Sequence one" = framesSegAuto1()[[ifelse(is.null(input$iframeSegAuto), 1, input$iframeSegAuto)]],
                                    "Sequence two" = framesSegAuto2()[[ifelse(is.null(input$iframeSegAuto), 1, input$iframeSegAuto)]])
            image.args <- list(x = myframeAftSeg, asp = myasp(),
                               frame.plot = F, main = mytitle, xaxt = "n", yaxt = "n", col = colsSeg())
            if(!is.null(input$range2)) image.args$zlim = c(input$range2[1], input$range2[2])
            do.call(image, image.args)
        })
            

        ##------- Show final thresholds--------##        
        output$threshold1 <- renderText({
            if(is.null(input$plot_click1)) "Threshold 1: Not Selected"
            else paste("Threshold 1:", round(input$plot_click1$x, 2))
        })
        
        output$threshold2 <- renderText({
            if(is.null(input$plot_click2)) "Threshold 2: Not Selected"
            else paste("Threshold 2:", round(input$plot_click2$x, 2))
        })        

        ##------- Histogram Auto--------##
        output$histAuto <- renderPlot({
            mySeq <- switch(input$var2, 
                            "Sequence one" = frames1(),
                            "Sequence two" = frames2())
            myframe <- mySeq[[input$iframeSeg]]
            par(mar = c(2, 2, 2, 2), oma = c(1, 1, 1, 1))
            hist.args <- list(x = c(myframe), breaks = input$bins2, probability = TRUE,
                              col = "skyblue", main = "", xlab = "Pixel intensities")
                                   
            if(!is.null(input$range2)){
                hist.args$x = myframe[(myframe >= input$range2[1]) & (myframe <= input$range2[2])]
                hist.args$xlim = c(input$range2[1], input$range2[2])
            }
            do.call(hist, hist.args)
                                   
            if(input$manualAuto == "2" & input$plotDensity){
                mydensities <- switch(input$var2, 
                                      "Sequence one" = densities1(),
                                      "Sequence two" = densities2())
                mydensity <- mydensities[[input$iframeSeg]]
                probs <- summary(mydensity)$pro
                mus <- summary(mydensity)$mean
                vars <- summary(mydensity)$variance
                
                xs <- seq(input$range2[1], input$range2[2], length = 200)
                points(xs, gf(xs, probs, mus, vars), type = "l", col = "red")
            }
        })

        ## ##------- Record click value--------##        
        ## clicks <- reactiveValues(oneClick = NULL)

        ## ##------- Handle clicks--------##
        ## observe({ clicks$oneClick <- input$plot_click1 })
        
        ##------- Histogram Manual Choose1--------##
        output$histManual1 <- renderPlot({
            myframe <- frames1()[[ifelse(is.null(input$iframeSeg), 1, input$iframeSeg)]]
            par(mar = c(2, 2, 2, 2), oma = c(2, 2, 1, 1))
            hist.args <- list(x = c(myframe), breaks = input$bins2, probability = TRUE,
                              col = "skyblue", main = "", xlab = "Pixel intensities")
                                   
            if(!is.null(input$range2)){
                hist.args$x = myframe[(myframe >= input$range2[1]) & (myframe <= input$range2[2])]
                hist.args$xlim = c(input$range2[1], input$range2[2])
            }
            hist.ans <- do.call(hist, hist.args)
            if(!is.null(input$plot_click1)){
                abline(v = input$plot_click1$x, col = "#001f3f")
                text(x = input$plot_click1$x, y = max(hist.ans$density),
                     pos = 4, labels = round(input$plot_click1$x,2))
                }
        })

        ##------- Histogram Manual Choose2--------##
        output$histManual2 <- renderPlot({
            myframe <- frames2()[[input$iframeSeg]]
            par(mar = c(2, 2, 2, 2), oma = c(2, 2, 1, 1))
            hist.args <- list(x = c(myframe), breaks = input$bins2, probability = TRUE,
                              col = "skyblue", main = "", xlab = "Pixel intensities")
            if(!is.null(input$range2)){
                hist.args$x = myframe[(myframe >= input$range2[1]) & (myframe <= input$range2[2])]
                hist.args$xlim = c(input$range2[1], input$range2[2])
            }
            hist.ans <- do.call(hist, hist.args)
            if(!is.null(input$plot_click2)){
                abline(v = input$plot_click2$x, col = "#001f3f")
                text(x = input$plot_click2$x, y = max(hist.ans$density),
                     pos = 4, labels = round(input$plot_click2$x))
                }
        })        

        ##------- Histogram Manual show--------##
        output$histManualShow <- renderPlot({
            mySeq <- switch(input$var2, 
                            "Sequence one" = frames1(),
                            "Sequence two" = frames2())
            myframe <- mySeq[[input$iframeSeg]]
            par(mar = c(2, 2, 2, 2), oma = c(2, 2, 1, 1))
            hist.args <- list(x = c(myframe), breaks = input$bins2, probability = TRUE,
                              col = "skyblue", main = "", xlab = "Pixel intensities")
                                   
            if(!is.null(input$range2)){
                hist.args$x = myframe[(myframe >= input$range2[1]) & (myframe <= input$range2[2])]
                hist.args$xlim = c(input$range2[1], input$range2[2])
            }
            hist.ans <- do.call(hist, hist.args)
        })

        seq1Text <- reactive({
            if(input$useAuto1 == 0 & input$useThreshold1 == 0) return("Not set")
            input$useAuto1
            input$useThreshold1
            if(isolate(input$manualAuto) == "2") "Automatic segmentation"
            else paste("Manual segmentation with thr. =", round(isolate(manualThreshold1()), 2))
        })
        
        seq2Text <- reactive({
            if(input$useAuto2 == 0 & input$useThreshold2 == 0) return("Not set")
            input$useAuto2
            input$useThreshold2
            if(isolate(input$manualAuto) == "2") "Automatic segmentation"
            else paste("Manual segmentation with thr. =", round(isolate(manualThreshold2()), 2))
        })                

        frames1Seg <- reactive({
            if(input$useAuto1 == 0 & input$useThreshold1 == 0) return(frames1())
            input$useAuto1
            input$useThreshold1
            if(isolate(input$manualAuto) == "2") return(framesSegAuto1())
            else return(framesSegManual1())
        })
        
        frames2Seg <- reactive({
            if(input$useAuto2 == 0 & input$useThreshold2 == 0) return(frames2())
            input$useAuto2
            input$useThreshold2
            if(isolate(input$manualAuto) == "2") return(framesSegAuto2())
            else return(framesSegManual2())
        })                

        output$FinalSeg <- renderText({
            paste0("Sequence 1: ", seq1Text(), "\n", "Sequence 2: ", seq2Text(), "\n")
        })
        
########################
### Registration
########################

        mycolsReg <- reactive({switch(input$cols3,
                                      "heat" = heat.colors(50, alpha = 1),
                                      "gray" = colorRampPalette(brewer.pal(9, "Greys"))(50),
                                      "blue" = colorRampPalette(brewer.pal(9, "Blues"))(50),
                                      "green" = colorRampPalette(brewer.pal(9, "Greens"))(50))
                           })
        
        framesAftReg1 <- reactive({
            Ms1 <- lapply(frames1Seg(), calculateM)
            ans <- list()
            withProgress(message = 'Registration', value = 0, {
                n <- length(frames1Seg())
                for (i in 1:n){
                    ans[[i]] <- autoRegist(Ms1[[i]], frames1Seg()[[i]])
                    incProgress(1/n, detail = paste("Processing Seq 1\n Frame", i, "of", n))
                }
                return(ans)
            })
        })
        
        framesAftReg2 <- reactive({
            Ms2 <- lapply(frames2Seg(), calculateM)
            ans <- list()
            withProgress(message = 'Registration', value = 0, {
                n <- length(frames2Seg())
                for (i in 1:n){
                    ans[[i]] <- autoRegist(Ms2[[i]], frames2Seg()[[i]])
                    incProgress(1/n, detail = paste("Processing Seq 2\n Frame", i, "of", n))
                }
                return(ans)
            })
        })

        vClicks1 <- reactiveValues(
            click1 = NULL,  # Represents the first mouse click, if any
            click2 = NULL,    # After two clicks, this stores the range of x
            click3 = NULL,
            nextClick = 1
        )

        vClicks2 <- reactiveValues(
            click1 = NULL,  # Represents the first mouse click, if any
            click2 = NULL,    # After two clicks, this stores the range of x
            click3 = NULL,
            nextClick = 1
        )        

        observeEvent(input$plotClickReg1, {
            if (vClicks1$nextClick == 1) {
                vClicks1$click1 <- input$plotClickReg1
                vClicks1$nextClick <- 2
                vClicks1$click2 <- vClicks1$click3 <- NULL
            }else if(vClicks1$nextClick == 2){
                ## We already had a first click, so this is the second click.
                vClicks1$click2 <- input$plotClickReg1
                vClicks1$nextClick <- 3
            }else if(vClicks1$nextClick == 3){
                vClicks1$click3 <- input$plotClickReg1
                ## And clear the first click so the next click starts a new
                ## range.
                vClicks1$nextClick <- 1
            }
        })

        observeEvent(input$plotClickReg2, {
            if (vClicks2$nextClick == 1) {
                vClicks2$click1 <- input$plotClickReg2
                vClicks2$nextClick <- 2
                vClicks2$click2 <- vClicks2$click3 <- NULL
            }else if(vClicks2$nextClick == 2){
                ## We already had a first click, so this is the second click.
                vClicks2$click2 <- input$plotClickReg2
                vClicks2$nextClick <- 3
            }else if(vClicks2$nextClick == 3){
                vClicks2$click3 <- input$plotClickReg2
                ## And clear the first click so the next click starts a new
                ## range.
                vClicks2$nextClick <- 1
            }
        })        

        output$iframes3out <- renderUI({
            sliderInput("iframe3", 
                        label = "Which frame do you want to see?",
                        min = 1, max = nframes(), value = 1)
        })

        ##-----------------------------##
        ##------- Range of pixels -----##
        ##-----------------------------##
        output$sliderRange3 <- renderUI({
            mySeq <- switch(input$var3, 
                            "Sequence one" = frames1Seg(),
                            "Sequence two" = frames2Seg())
            myframe <- mySeq[[ifelse(is.null(input$iframe3), 1, input$iframe3)]]
            sliderInput("range3",
                        label = "Range of pixel intensities to display",
                        min = min(myframe), max = max(myframe),
                        value = c(min(myframe), max(myframe)))
        })

        ##-------------------------------------##
        ##------- Img before Registration -----##
        ##-------------------------------------##
        output$oneframeBfReg1 <- renderPlot({
            myframe <- frames1Seg()[[ifelse(is.null(input$iframe3),
                                            1, input$iframe3)]]
            par(mar = c(1, 1, 3, 1), oma = c(1,1,1,1))
            mytitle <- paste0("Before Registration\n", input$var3, "; Frame ", input$iframe3)
            par(mar = c(1, 1, 3, 1), oma = c(1,1,1,1))	
            imgArg <- list(x = 1:nrows(), y = 1:ncols(),
                           z = myframe, asp = myasp(),
                           frame.plot = F, main = mytitle,
                           xaxt = "n", yaxt = "n", col = mycolsReg())
            if(!is.null(input$range3)) imgArg$zlim <- c(input$range3[1], input$range3[2])
            do.call(image, imgArg)
            if(!is.null(vClicks1$click2)) segments(x0 = vClicks1$click1$x, y0 = vClicks1$click1$y,
                                                  x1 = vClicks1$click2$x, y1 = vClicks1$click2$y)
            if(!is.null(vClicks1$click3)) points(vClicks1$click3$x, vClicks1$click3$y)
        })

        ##-------------------------------------##
        ##------- Img before Registration -----##
        ##-------------------------------------##
        output$oneframeBfReg2 <- renderPlot({
            myframe <- frames2Seg()[[ifelse(is.null(input$iframe3), 1, input$iframe3)]]
            par(mar = c(1, 1, 3, 1), oma = c(1,1,1,1))
            mytitle <- paste0("Before Registration\n", input$var3, "; Frame ", input$iframe3)
            par(mar = c(1, 1, 3, 1), oma = c(1,1,1,1))	
            imgArg <- list(x = 1:nrows(), y = 1:ncols(),
                           z = myframe, asp = myasp(),
                           frame.plot = F, main = mytitle,
                           xaxt = "n", yaxt = "n", col = mycolsReg())
            if(!is.null(input$range3)) imgArg$zlim <- c(input$range3[1], input$range3[2])
            do.call(image, imgArg)
            if(!is.null(vClicks2$click2)) segments(x0 = vClicks2$click1$x, y0 = vClicks2$click1$y,
                                                  x1 = vClicks2$click2$x, y1 = vClicks2$click2$y)
            if(!is.null(vClicks2$click3)) points(vClicks2$click3$x, vClicks2$click3$y)
        })        

        ##-------------------------------------##
        ##------- Img after Registration ------##
        ##-------------------------------------##
        imgManualRegist1 <- reactive({
            if(is.null(vClicks1$click3)) return(NULL)
            manualRegist(frames1Seg()[[ifelse(is.null(input$iframe3), 1, input$iframe3)]], vClicks1$click1, vClicks1$click2, vClicks1$click3)
        })

        imgManualRegist2 <- reactive({
            if(is.null(vClicks2$click3)) return(NULL)
            manualRegist(frames2Seg()[[ifelse(is.null(input$iframe3), 1, input$iframe3)]], vClicks2$click1, vClicks2$click2, vClicks2$click3)
        })        
        
        output$oneframeAftReg1 <- renderPlot({
            mytitle <- paste0("After Registration\n", input$var3, "; Frame ", input$iframe3)
            par(mar = c(1, 1, 3, 1), oma = c(1,1,1,1))
            imgArg <- list(asp = myasp(), frame.plot = F, main = mytitle,
                          xaxt = "n", yaxt = "n", col = mycolsReg())
            if(is.null(vClicks1$click3)){
                imgArg$x <- matrix(0, nrows(), ncols())
                do.call(image, imgArg)
                text(x = 0.5, y = 0.5, labels = "Click left graph to select center line and reference point")
            }else{
                imgArg$x <- imgManualRegist1()
                if(!is.null(input$range3)) imgArg$zlim = c(input$range3[1], input$range3[2])
                do.call(image, imgArg)
                }
        })

        output$oneframeAftReg2 <- renderPlot({
            mytitle <- paste0("After Registration\n", input$var3, "; Frame ", input$iframe3)
            par(mar = c(1, 1, 3, 1), oma = c(1,1,1,1))
            imgArg <- list(asp = myasp(), frame.plot = F, main = mytitle,
                           xaxt = "n", yaxt = "n", col = mycolsReg())
            if(is.null(vClicks2$click3)){
                imgArg$x <- matrix(0, nrows(), ncols())
                do.call(image, imgArg)
                text(x = 0.5, y = 0.5, labels = "Click left graph to select center line and reference point")
            }else{
                imgArg$x <- imgManualRegist2()
                if(!is.null(input$range3)) imgArg$zlim = c(input$range3[1], input$range3[2])
                do.call(image, imgArg)
            }
        })

        RegDone1 <- reactiveValues(x = NULL)
        RegDone2 <- reactiveValues(x = NULL)        
        
        reg1Text <- reactive({
            if(is.null(RegDone1$x)) "Segmentation not done"
            else("Segmentation finished")            
        })

        reg2Text <- reactive({
            if(is.null(RegDone2$x)) "Segmentation not done"
            else("Segmentation finished")
        })        

        frames1Reg <- reactive({
            if(input$useReg1 == 0){
                return(frames1Seg())
            }else if(is.null(vClicks1$click3)){
                return(frames1Seg())
            }else{
                RegDone1$x <- 1
                lapply(frames1Seg(), manualRegist, first = vClicks1$click1, second = vClicks1$click2, third = vClicks1$click3)
            }
        })

        frames2Reg <- reactive({
            if(input$useReg2 == 0){
                return(frames2Seg())
            }else if(is.null(vClicks2$click3)){
                return(frames2Seg())
            }else{
                RegDone2$x <- 1
                lapply(frames2Seg(), manualRegist, first = vClicks2$click1, second = vClicks2$click2, third = vClicks2$click3)

            }
        })

        observeEvent(input$useReg1,{
            frames1Reg()
        })
        
        observeEvent(input$useReg2,{
            frames2Reg()
        })
        
        output$FinalReg <- renderText({
            input$useReg1
            input$useReg2
            paste0("Sequence 1: ", reg1Text(), "\n", "Sequence 2: ", reg2Text(), "\n")            
        })
        
########################
### LASR test
########################
        mycolsLasr <- reactive({
            switch(input$cols4,
                   "heat" = heat.colors(50, alpha = 1),
                   "gray" = colorRampPalette(brewer.pal(9, "Greys"))(50),
                   "blue" = colorRampPalette(brewer.pal(9, "Blues"))(50),
                   "green" = colorRampPalette(brewer.pal(9, "Greens"))(50))
        })

        output$iframe4out <- renderUI({
            sliderInput("iframe4", label = "Which difference frame do you want to see?",
                        min = 1, max = nframes(), value = 1)
        })
        diffImages <- reactive({
            ans <- list()
            for(i in 1:nframes()){
                ans[[i]] <- frames1Reg()[[i]] - frames2Reg()[[i]]
            }
            return(ans)
        })

        ## autoH1 <- reactive({
        ##     ans <- list()
        ##     withProgress(message = 'Choosing h', value = 0, {
        ##         for(i in 1:nframes()){
        ##             ans[[i]] <- choose_h(diffImages()[[i]])
        ##             incProgress(1/nframes(), detail = paste("Choosing h for frame", i, "of", nframes()))
        ##         }
        ##     })
        ##     return(ans)
        ## })

        myLoess <- reactive({
            onedf <- data.frame(row = rep(1:nrows(), ncols()),
                                col = rep(1:nrows(), each = ncols()),
                                pixel = as.vector(diffImages()[[ifelse(is.null(input$iframe4), 1, input$iframe4)]]))
            loessFit <- loess(pixel ~ row * col, data = onedf, span = input$hValue, degree = 2)
            ## Extract fitted value, standard error, etc. See predict.loess
            predict(loessFit, se = TRUE)            
        })

        qmap <- reactive({
            t.stat <- myLoess()$fit / myLoess()$se
            p.value <- 2 * (1 - pt(abs(t.stat), myLoess()$df))
            q.value <- p.adjust(p.value, method = "BH")
            matrix(q.value, nrows())        
        })

        ## for(i in 1:nframes()){
        ##     loessRes[[i]] <- reactive({
        ##         ## iii is a global identifier
        ##         myDiffImg <- diffImages()[[ifelse(is.null(input$iframe4), 1, input$iframe4)]]
        ##         onedf <- data.frame(row = rep(1:nrows, ncols),
        ##                             col = rep(1:nrows, each = ncols),
        ##                             pixel = as.vector(myDiffImg))
        ##         if(input$hSelect == "1") span <- input$hValue
        ##         if(input$hSelect == "2") span <- choose_h(myDiffImg)
        ##         loessFit <- loess(pixel ~ row * col, data = onedf, span = span, degree = 2)
        ##         ## Extract fitted value, standard error, etc. See predict.loess
        ##         ans <- predict(loessFit, se = TRUE)   
        ##     })
        ## }

        ## autoSmoothImgs <- reactive({
        ##     ans <- list()
        ##     for(i in 1:nframes()){
        ##         nrow1 <- nrow(diffImages()[[i]])
        ##         ncol1 <- ncol(diffImages()[[i]])
        ##         onedf <- data.frame(row = rep(1:nrow1, ncol1),
        ##                             col = rep(1:nrow1, each = ncol1),
        ##                             pixel = as.vector(diffImages()[[i]]))
        ##         loessFit <- loess(pixel ~ row * col, data = onedf, span = autoH1()[[i]], degree = 2)
        ##         ## Extract fitted value, standard error, etc. See predict.loess
        ##         ans <- predict(loessFit, se = TRUE)
        ##     }
        ##     return(ans)
        ## })
        
        ##-----------------------------##
        ##------- Range of pixels -----##
        ##-----------------------------##
        output$sliderRange4 <- renderUI({
            myframe <- diffImages()[[ifelse(is.null(input$iframe4), 1, input$iframe4)]]            
            sliderInput("range4",
                        label = "Range of pixel intensities to display",
                        min = floor(min(myframe)), max = ceiling(max(myframe)),
                        value = c(floor(min(myframe)), ceiling(max(myframe))))
        })
        
        ##------------------------------##
        ##------- Difference Image -----##
        ##------------------------------##
        output$diffImg <- renderPlot({
            myframe <- diffImages()[[ifelse(is.null(input$iframe4), 1, input$iframe4)]]            
            par(mar = c(1, 1, 3, 1), oma = c(1,1,1,1))
            mytitle <- paste0("Differnece Image\n", input$iframe4)
            imgArgs <- list(x = myframe, asp = myasp(),
                            frame.plot = F, main = mytitle,
                            xaxt = "n", yaxt = "n", col = mycolsLasr())
            imgArgs$zlim <- c(input$range4[1], input$range4[2])
            do.call(image, imgArgs)
        })

        ##------------------------------##
        ##------- Smoothed Image -------##
        ##------------------------------##
        output$autoSmoothImg <- renderPlot({
            ## myframe <- matrix(loessRes[[ifelse(is.null(input$iframe4), 1, input$iframe4)]]()$fit, nrow = nrows)
            ## Extract fitted value, standard error, etc. See predict.loess
            myframe <- matrix(myLoess()$fit, nrows())
            par(mar = c(1, 1, 3, 1), oma = c(1,1,1,1))
            mytitle <- paste0("Smoothed Image\n", input$iframe4)
            imgArgs <- list(x = myframe, asp = myasp(),
                            frame.plot = F, main = mytitle,
                            xaxt = "n", yaxt = "n", col = mycolsLasr())
            do.call(image, imgArgs)
        })

        ##------------------------------##
        ##----------- q-map ----------##
        ##------------------------------##
        output$qMap <- renderPlot({
            par(mar = c(1, 1, 3, 1), oma = c(1,1,1,1))
            mytitle <- paste0("q-Map Image\n", input$iframe4)
            qmapArgs <- list(x = qmap(), asp = myasp(), frame.plot = F, main = mytitle,
                             xaxt = "n", yaxt = "n", col = mycolsLasr())
            if(!is.null(input$qrange)) qmapArgs$zlim = c(input$qrange[1], input$qrange[2])
            do.call(image, qmapArgs)
        })

        ##-------------------------------##
        ##----------- sigMap ------------##
        ##-------------------------------##
        output$sigMap <- renderPlot({
            sigMap <- matrix(qmap() < input$qcutoff, nrows())
            par(mar = c(1, 1, 3, 1), oma = c(1,1,1,1))
            mytitle <- paste0("Significant Pixels\n", input$iframe4)
            image(sigMap, asp = myasp(),
                  frame.plot = F, main = mytitle,
                  xaxt = "n", yaxt = "n", col = mycolsLasr())
        })
        
########################
### Others
########################
        
    }
)



