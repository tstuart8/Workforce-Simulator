fluidPage(
  
  titlePanel(h1('Market Optimization', align = "center")),
  
  #headerPanel('Market Optimization'),
  fluidRow(
    column(width = 3,
           br(),
           br(),
           br(),
      selectInput('Priority', 'Choose which org gets scheduling priority', c("FS", "DTH"), selected='DTH'),
      
      selectInput('District', 'District', unique(points$district), selected='UT-Salt Lake City'),
      
      sliderInput('fsheadcount', 'FS Headcount', 
                  min=0, 
                  max = subset(headcount, District == 'UT-Salt Lake City')$FS + 10, 
                  value = subset(headcount, District == 'UT-Salt Lake City')$FS),
      
      sliderInput('dthheadcount', 'DTH FT Headcount', min=0, 
                  max = subset(headcount, District == 'UT-Salt Lake City')$DTH + 10,
                  value = subset(headcount, District == 'UT-Salt Lake City')$DTH),
      
      numericInput('fshpw', 'FS Points Per Week', value=14, min=10, max=20),
      
      numericInput('dthhpw', 'DTH Points Per Week', value=17, min=10, max=20)
    ),
    
    column(width = 9,
      plotOutput('histplot', height = 400),
      plotOutput('Util', height=300),
      plotOutput('Service', height=300)
    )
#    
#    column(width = 1,
#           br(),
#           br(),
#           br(),
#           br(),
#           br(),
#           br(),
#           br(),
#           br(),
#           br(),
#           br(),
#           imageOutput("arrows")
#    ),
    

  )
)
