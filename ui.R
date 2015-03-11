## ui.R

shinyUI(
    navbarPage("gLASR",

#########################
### Panel 0: Load data
#########################
               tabPanel("ReadFile",
                        sidebarLayout(
                            sidebarPanel(
                                h3("Welcome to gLASR"),
                                h6("Please upload your two csv files containing two sequences of vectorized image matrices. Or use the build-in data by checking the box at the bottom."),
                                conditionalPanel(
                                    condition = "input.defaultData == false",
                                    numericInput("nrows", label = h5("Input the number of rows in each image matrix"), value = 32),
                                    fileInput('file1', 'Upload .csv for Sequence 1', accept = '.csv'),
                                    fileInput('file2', 'Upload .csv for Sequence 2', accept = '.csv'),
                                    actionButton("genData", label = "Generate data")
                                ),
                                checkboxInput("defaultData", label = "Use build-in example", value = FALSE)
                            ),
                            
                            mainPanel(
                                br(),
                                br(),
                                br(),
                                
                                h1("gLASR", align = "center"),
                                h3("generalized Longitudinal Analysis and Self-Registration", align = "center"),
                                br(),
                                h5("Coded by: Yifan Xu and Jang Ik Cho"),
                                h5("Based on and improved upon Matlab code by Jang Ik Cho, Yifan Xu, Xiaofeng Wang and Jiayang Sun"),
                                br(),
                                h5("Reference: Wang, Xiaofeng, Jiayang Sun, and Kath Bogie. ''Spatial-temporal data mining procedure: LASR.'' Lecture Notes-Monograph Series (2006): 213-231.")
                            )
                        )
                        ),
                        
#########################
### Panel 1: Basic
#########################
               tabPanel("Basic",
                        fluidPage(
                            title = "gLASR",
                            
                            fluidRow(
                                column(4, plotOutput("oneframe1")),
                                column(8, plotOutput("hist1"))
                            ),
                            
                            hr(),
                            
                            fluidRow(
                                column(4,
                                       h2("gLASR"),
                                       h4("Welcome! Before we start, let's look at your data first. Please select the visualization parameters."),
                                       selectInput("var1", 
                                                   label = "Choose a sequence to display",
                                                   choices = c("Sequence one", "Sequence two"),
                                                   selected = "Sequence one"),
                                       uiOutput("iframeUIBasic")
                                       ),
                                column(4,
                                       sliderInput("bins1", 
                                                   label = "Number of bins in histogram",
                                                   min = 5, max = 100, value = 15),
                                       uiOutput("sliderRange1")),
                                
                                column(4,
                                       selectInput("cols1", 
                                                   label = "Color palette",
                                                   choices = c("heat", "gray", "blue", "green"),
                                                   selected = "gray"))
                            )
                        )
                        ),

#########################
### Panel 2: Segmentation
#########################               
               tabPanel("Segmentation",

                        sidebarLayout(
                            sidebarPanel(
                                selectInput("var2", 
                                            label = h4("Step 1: Choose a sequence"),
                                            choices = c("Sequence one", "Sequence two"),
                                            selected = "Sequence one"),
                                
                                radioButtons("manualAuto", label = h4("Step 2: Choose segmentation mode"),
                                             choices = list("Manual Segmentation" = 1, "Auto Segmentation" = 2), 
                                             selected = 1),
                                
                                conditionalPanel( ## Auto
                                    condition = "input.manualAuto == 2",
                                    checkboxInput("plotDensity", "Draw Gaussian mixture density?", FALSE),
                                    uiOutput("iframeAutoSeg"),
                                    
                                    conditionalPanel(
                                        condition = "input.var2 == 'Sequence one'",
                                        actionButton("useAuto1", "Use auto Segmentation for Sequence 1")
                                    ),
                                    conditionalPanel(
                                        condition = "input.var2 == 'Sequence two'",
                                        actionButton("useAuto2", "Use auto Segmentation for Sequence 2")
                                    )
                                ),
                                
                                conditionalPanel( ## Manual selection
                                    condition = "input.manualAuto == 1",

                                    h4("Step 3: Choose the threshold"),
                                    p("First select the reference frame, then click the histogram to the right. You can adjust how the histogram and the image are shown by adjusting the parameters below the panel to the right. Pixels whose values are below the threshold are set to zero."),
                                    uiOutput("iframeManualSeg"),
                                    conditionalPanel(
                                        condition = "input.var2 == 'Sequence one'",
                                        verbatimTextOutput("threshold1"),
                                        actionButton("useThreshold1", "Use this threshold for Sequence 1")
                                    ),
                                    
                                    conditionalPanel(
                                        condition = "input.var2 == 'Sequence two'",
                                        verbatimTextOutput("threshold2"),
                                        actionButton("useThreshold2", "Use this threshold for Sequence 2")
                                    )
                                ),
                                
                                hr(),

                                verbatimTextOutput("FinalSeg"),
                                actionButton("segSave", "Save current result")
                                
                            ),

                            ##-----------
                            ## Plots
                            ##-----------
                            mainPanel(
                                fluidRow(
                                    column(6, plotOutput("oneframe_bf_seg") ),
                                    conditionalPanel(
                                        condition = "input.manualAuto == 1",
                                        column(6, plotOutput("imgAftSegSelect") )
                                    ),
                                    conditionalPanel(
                                        condition = "input.manualAuto == 2",
                                        column(6, plotOutput("imgAftSegAuto") )
                                    )                                    
                                ),
                                
                                conditionalPanel( ## Histogram for Manual Segmentation
                                    condition = "input.manualAuto == 1",
                                    
                                    conditionalPanel(
                                        condition = "input.var2 == 'Sequence one'",
                                        plotOutput("histManual1", clickId = "plot_click1", height = "350px")
                                    ),
                                    
                                    conditionalPanel(
                                        condition = "input.var2 == 'Sequence two'",
                                        plotOutput("histManual2", clickId = "plot_click2", height = "350px")
                                    )
                                    
                                ),
                                
                                conditionalPanel( ## Histogram for Automatic Segmentation
                                    condition = "input.manualAuto == 2",
                                    plotOutput("histAuto")
                                ),

                                fluidRow(
                                    column(4,
                                           sliderInput("bins2", label = "Number of bins in histogram",
                                                       min = 5, max = 100, value = 15)
                                           ),
                                    column(4, uiOutput("sliderRange2")),
                                    column(4, selectInput("cols2", label = "Color palette",
                                            choices = c("heat", "gray", "blue", "green"),
                                                          selected = "gray"))
                                    )
                            )
                        )
                        ),
               
#########################
### Panel 3: Registration
#########################               
               tabPanel("Registration",
                        fluidPage(
                            fluidRow(
                                column(6,
                                       conditionalPanel(
                                           condition = "input.var3 == 'Sequence one'",
                                           plotOutput("oneframeBfReg1", clickId = "plotClickReg1", height = "350px")
                                       ),
                                       conditionalPanel(
                                           condition = "input.var3 == 'Sequence two'",
                                           plotOutput("oneframeBfReg2", clickId = "plotClickReg2", height = "350px")
                                       )                                       
                                       ),
                                
                                column(6,
                                       conditionalPanel(
                                           condition = "input.var3 == 'Sequence one'",
                                           plotOutput("oneframeAftReg1")
                                       ),
                                       conditionalPanel(
                                           condition = "input.var3 == 'Sequence two'",
                                           plotOutput("oneframeAftReg2")
                                       )
                                       )
                            ),
                            
                            fluidRow(
                                column(4,
                                       selectInput("var3", 
                                                   label = "Choose a sequence to display",
                                                   choices = c("Sequence one", "Sequence two"),
                                                   selected = "Sequence one"),
                                       uiOutput("iframe3out")                       
                                       ),
                                column(4,
                                       selectInput("cols3", 
                                                   label = "Color palette",
                                                   choices = c("heat", "gray", "blue", "green"),
                                                   selected = "blue"),
                                       uiOutput("sliderRange3")
                                       ),
                                column(4,
                                       conditionalPanel(
                                           condition = "input.var3 == 'Sequence one'",
                                           actionButton("useReg1", "Use these parameters to register Sequence one")
                                       ),
                                       conditionalPanel(
                                           condition = "input.var3 == 'Sequence two'",
                                           actionButton("useReg2", "Use these parameters to register Sequence two")
                                       ),
                                       verbatimTextOutput("FinalReg")
                                       )
                            )
                        )
                        ),
               
#########################
### Panel 4: LASR test
#########################
               tabPanel("Statistical Testing",
                        sidebarLayout(
                            sidebarPanel(
                                uiOutput("iframe4out"),

                                uiOutput("sliderRange4"),

                                selectInput("cols4", label = h4("Color palette"),
                                            choices = c("heat", "gray", "blue", "green"),
                                            selected = "blue"),

                                radioButtons("hSelect", label = h4("Smoothing bandwidth Selection"),
                                             choices = list("Manual" = 1, "Auto" = 2), selected = 1),

                                conditionalPanel(
                                    condition = "input.hSelect == 1",
                                    numericInput("hValue", label = h4("Value of h"), value = 0.01)
                                ),

                                sliderInput("qrange", label = h4("q-value range (visulization only)"),
                                            min = 0, max = 1, value = c(0, 1)),

                                sliderInput("qcutoff", label = h4("q-value cutoff for significance map"),
                                            min = 0, max = 0.2, value = 0.05)
                            ),
                            
                            mainPanel(
                                fluidRow(
                                    column(6, plotOutput("diffImg") ),
                                    column(6, plotOutput("autoSmoothImg") )
                                ),

                                fluidRow(
                                    column(6, plotOutput("qMap") ),
                                    column(6, plotOutput("sigMap"))
                                )
                                
                            )
                        )
                        )

#########################
### Panel 5: Others
#########################               
               )
)    

    
