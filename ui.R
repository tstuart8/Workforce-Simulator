fluidPage(
  
  titlePanel(h1('Market Optimization', align = "center")),
  
  #headerPanel('Market Optimization'),
  fluidRow(
    column(width = 3,
           
      selectInput('Priority', 'Choose which org gets scheduling priority', c("FS", "DTH"), selected='DTH'),
      
      selectInput('District', 'District', sort(unique(points$district)), selected='UT-Salt Lake City'),
      
      # sliderInput('fsheadcount', 'FS Headcount', 
      #             min=0, 
      #             max = subset(headcount, District == 'UT-Salt Lake City')$FS + 10, 
      #             value = subset(headcount, District == 'UT-Salt Lake City')$FS),
      # 
      # sliderInput('dthheadcount', 'DTH FT Headcount', min=0, 
      #             max = subset(headcount, District == 'UT-Salt Lake City')$DTH + 10,
      #             value = subset(headcount, District == 'UT-Salt Lake City')$DTH),
      
      
      selectInput('Quarter', 'Quarter', sort(unique(paste(year(headcount2$RecordWeek),quarters.Date(headcount2$RecordWeek)))), selected='2018 Q1'),
      
      # textOutput(quarter),
      
      div(style="display: inline-block;vertical-align:top; width: 150px;",textOutput("FSHC")),
      tags$head(tags$style("#FSHC{color: blue;
                                 font-size: 15px;
                                 font-weight: bold;
                                 }"
      )
      ),
      div(style="display: inline-block;vertical-align:top; width: 150px;",textOutput("DTHHC")),
      tags$head(tags$style("#DTHHC{color: #F56F06;
                                 font-size: 15px;
                                 font-weight: bold;
                                 }"
      )
      ),
      
      div(style="display: inline-block;vertical-align:top; width: 150px;",numericInput('FSHeadcount', "Additional FS headcount", value=0, min=-10, max=10)),
      
      div(style="display: inline-block;vertical-align:top; width: 150px;",numericInput('DTHHeadcount', "Additional DTH headcount", value=0, min=-10, max=10)),
      
      
      div(style="display: inline-block;vertical-align:top; width: 150px;",numericInput('FSMean', paste('FS Mean'), 
                   value= round(mean(subset(points_pro, district == 'UT-Salt Lake City' & TechOrg == 'FS')$Points), 0), 
                   min=0, 
                   max=20)),
      
      div(style="display: inline-block;vertical-align:top; width: 150px;",numericInput('FSSD', 'FS Std. Deviation', 
                   value= round(sd(subset(points_pro, district == 'UT-Salt Lake City' & TechOrg == 'FS')$Points), 0), 
                   min=0, 
                   max=10)),
      
      plotOutput('fsdist', height = 150, width = 300),
      
      
      div(style="display: inline-block;vertical-align:top; width: 150px;",numericInput('DTHMean', paste('DTH Mean'), 
                   value= round(mean(subset(points_pro, district == 'UT-Salt Lake City' & TechOrg == 'DTH')$Points, 0)),
                   min=0, 
                   max=20)),
      
      
      div(style="display: inline-block;vertical-align:top; width: 150px;",numericInput('DTHSD', 'DTH Std. Deviation', 
                   value= round(sd(subset(points_pro, district == 'UT-Salt Lake City' & TechOrg == 'DTH')$Points), 0), 
                   min=0, 
                   max=10)),
    
    plotOutput('dthdist', height = 150, width = 300)
    
    ),
    
    column(width = 1, offset = 0,
           br(),br(),br(),br(),br(),
           textOutput("pointsperweek"),
           tags$head(tags$style("#pointsperweek{color: black;
                                 font-size: 15px;
                                 font-weight: bold;
                                 text-decoration: underline;
                                 }"
            )
           ),
           br(),
           textOutput("average_fscompleted"),
           tags$head(tags$style("#average_fscompleted{color: blue;
                                 font-size: 18px;
                                 font-weight: bold;
                                 }"
            )
           ),
           br(),
           textOutput("average_dthcompleted"),
           tags$head(tags$style("#average_dthcompleted{color: #F56F06 ;
                                 font-size: 18px;
                                 font-weight: bold;
                                 }"
            )
           ),
           br(),
           textOutput("average_ticketsperweek"),
           tags$head(tags$style("#average_ticketsperweek{color: black;
                                 font-size: 18px;
                                 font-weight: bold;
                                 }"
            )
           ),
           br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
           textOutput("utilization"),
           tags$head(tags$style("#utilization{color: black ;
                                 font-size: 18px;
                                 font-weight: bold;
                                 text-decoration: underline;
                                 }"
            )
           ),
           br(),
           textOutput("average_fsutil"),
           tags$head(tags$style("#average_fsutil{color: blue;
                                 font-size: 18px;
                                 font-weight: bold;
                                 }"
            )
           ),
           br(),
           textOutput("average_dthutil"),
           tags$head(tags$style("#average_dthutil{color: #F56F06 ;
                                 font-size: 18px;
                                 font-weight: bold;
                                 }"
            )
           ),
           br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
           textOutput("servicelevel"),
           tags$head(tags$style("#servicelevel{color: black ;
                                 font-size: 18px;
                                 font-weight: bold;
                                 text-decoration: underline;
                                 }"
            )
           ),
           br(),
           textOutput("average_servicelevel"),
           tags$head(tags$style("#average_servicelevel{color: black ;
                                 font-size: 15px;
                                 font-weight: bold;
                                 }"
            )
           )
    ),
    
    column(width = 8,
      plotOutput('histplot', height = 400),
      plotOutput('Util', height=300),
      plotOutput('Service', height=300)
    )

  )
)
