
function(input, output, session) {
  
  observe({
    district = input$District
    
    sub = subset(points_pro, district == district & TechOrg == 'FS')
    val = mean(sub$Points)
    val2 = sd(sub$Points)
    
    updateNumericInput(session, 'FSMean', 'FS Mean',
                       value= round(mean(subset(points_pro, district == input$District & TechOrg == 'FS')$Points), 0),
                       min=0)

    updateNumericInput(session, 'FSSD', 'FS Std. Deviation',
                       value= round(sd(subset(points_pro, district == input$District & TechOrg == 'FS')$Points), 0),
                       min=1)
    
    subdth = subset(points_pro, district == district & TechOrg == 'DTH')
    valdth = mean(subdth$Points)
    val2dth = sd(subdth$Points)
    updateNumericInput(session, 'DTHMean', 'DTH Mean',
                       value= round(mean(subset(points_pro, district == input$District & TechOrg == 'DTH')$Points), 0),
                       min=0)

    updateNumericInput(session, 'DTHSD', 'DTH Std. Deviation',
                       value= round(sd(subset(points_pro, district == input$District & TechOrg == 'DTH')$Points), 0),
                       min=1)
    
    weeks = unique(points$Week)
    weekmax = max(points$Week)
    add = 7*1:12
    new_weeks = weekmax+add
    all_weeks = c(weeks, new_weeks)
    
    updateSelectInput(session, 'Quarter', 'Quarter', c(sort(unique(paste(year(all_weeks),quarters.Date(all_weeks)))), 'All'), selected='All')
    
  })
  
  output$fsdist <- renderPlot({
    subfs = subset(points_pro, district == input$District & TechOrg == 'FS')
    meanfs = round(mean(subfs$Points), 0)
    sdfs = round(sd(subfs$Points), 0)
    minfs = min(subfs$Points) - 10
    maxfs = max(subfs$Points) + 10
    xfs = seq(minfs, maxfs, length=1000)
    dxfs = dnorm(xfs, meanfs, sdfs)
    dyfs = dnorm(xfs, input$FSMean, input$FSSD)
    dens = data.frame(xfs, dxfs, dyfs)
    
    ggplot(data = dens, aes(x=xfs, y=dxfs)) + 
      geom_area(aes(y = dyfs), fill='light Blue') +
      geom_line(linetype=2, size=2) +
      labs(title="Point Distribution", x="Points", y="Probability") +
      theme(plot.title = element_text(size=18, face='bold', color = 'gray45'), 
            axis.title = element_text(size=12, face='bold', color = 'gray45'), 
            axis.text = element_text(size=12, face='bold', color = 'gray45')) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      xlim(c(0, maxfs))
  })
  
  output$dthdist <- renderPlot({
    subfs = subset(points_pro, district == input$District & TechOrg == 'FS')
    maxfs = max(subfs$Points) + 10
    
    
    subdth = subset(points_pro, district == input$District & TechOrg == 'DTH')
    meandth = round(mean(subdth$Points), 0)
    sddth = round(sd(subdth$Points), 0)
    mindth = min(subdth$Points) - 10
    maxdth = max(subdth$Points) + 10
    xdth = seq(mindth, maxdth, length=1000)
    dxdth = dnorm(xdth, meandth, sddth)
    dydth = dnorm(xdth, input$DTHMean, input$DTHSD)
    dens = data.frame(xdth, dxdth, dydth)
    
    ggplot(data = dens, aes(x=xdth, y=dxdth)) + 
      geom_area(aes(y = dydth), fill='Dark Orange') +
      geom_line(linetype=2, size=2) +
      labs(title="Point Distribution", x="Points", y="Probability") +
      theme(plot.title = element_text(size=18, face='bold', color = 'gray45'), 
            axis.title = element_text(size=12, face='bold', color = 'gray45'), 
            axis.text = element_text(size=12, face='bold', color = 'gray45')) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      xlim(c(0, maxfs))
  })
  
  output$Util <- renderPlot({
    ## Subset Input datasets
    district = subset(points, district == input$District)
    district$Pending = 0
    district$Service = 0
    district$fsutil = 0
    district$dthutil = 0
    fspointdist = subset(points_pro, district == input$District & TechOrg == 'FS')
    dthpointdist = subset(points_pro, district == input$District & TechOrg == 'DTH')
    
    ## Subset Input Headcount
    hc = subset(headcount2, district == input$District & weekdays(RecordWeek) == 'Monday')
    
    ##Merge headcount with district
    district = merge(district, hc, by.x = c("district", "Week"), by.y = c("district", "RecordWeek"))
    district$source = 'Historical'
    
    ## Create Empty Dataframe
    dat = matrix(0, nrow=12, ncol=10)
    colnames(dat) = c("district" ,"Week", "Points", "Pending", "Service", "fsutil", "dthutil", "DTH", "FS", "source")
    dat = data.frame(dat)
    dat$district = input$District
    maxweek = max(district$Week)
    dthcurrent = subset(district, Week == maxweek)$DTH
    fscurrent = subset(district, Week == maxweek)$FS
    dat$DTH = dthcurrent
    dat$FS = fscurrent
    dat$source = 'Forecast'
    
    ## Create Forecast and append to dataset
    set.seed(1)
    fit = ets(district$Points)
    pred = simulate(fit, 12)
    
    week1 = max(district$Week)
    add = 7*1:12
    weeks = week1+add
    dat$Week = weeks
    dat$Points = pred
    
    ## Combine historical and forecast
    final = data.frame(rbind(district, dat))
    row.names(final) = 1:nrow(final)
    final$source = factor(final$source, levels = c("Historical", "Forecast"))
    
    ##Adjust Headcount based on input
    if(input$Quarter != 'All'){
      test = paste(year(final$Week),quarters.Date(final$Week)) == input$Quarter
      addfs = ifelse(test == TRUE, input$FSHeadcount, 0)
      adddth = ifelse(test == TRUE, input$DTHHeadcount, 0)
    }else {
      addfs = rep(input$FSHeadcount, nrow(final))
      adddth = rep(input$DTHHeadcount, nrow(final))
    }
    final$DTH = final$DTH + adddth
    final$FS = final$FS + addfs
    
    ##Simulate weekly completed points
    final$fstotal = 0
    final$dthtotal = 0
    final$steady_state = 0
    
    ## Iterate over logic to calculate pending work and utilization
    for(i in 1:nrow(final)){
      ## Sample completed points for the week
      set.seed(1)
      final$fstotal[i] = sum(rnorm(final$FS[i], input$FSMean, input$FSSD))
      final$fstotal[i] = ifelse(final$fstotal[i] < 0, 0, final$fstotal[i])
      final$dthtotal[i] = sum(rnorm(final$DTH[i], input$DTHMean, input$DTHSD))
      final$dthtotal[i] = ifelse(final$dthtotal[i] < 0, 0, final$dthtotal[i])
      final$steady_state[i] = final$fstotal[i] + final$dthtotal[i]
      
      ##Calculate work completed, pending work, and utilization
      if(i == 1){final$Pending[i] = max(0, (final$fstotal[i] + final$dthtotal[i])*.33 + final$Points[i] - final$steady_state[i])}
      else{final$Pending[i] = max(0, final$Points[i] + final$Pending[i-1] - final$steady_state[i])}
      final$Service[i] = final$Pending[i]/final$steady_state[i]
      if (input$Priority == 'DTH'){
        if(i == 1){
          final$fsutil[i] = min(1, max(0, (final$Points[i] - final$dthtotal[i])/final$fstotal[i]))
          final$dthutil = min(1, max(0, final$Points[i]/final$dthtotal[i]))
        }
        else{
          final$fsutil[i] = min(1, max(0, ((final$Points[i] + final$Pending[i-1] - final$dthtotal[i])/final$fstotal[i])))
          final$dthutil[i] = min(1, max(0, ((final$Points[i] + final$Pending[i-1])/final$dthtotal[i])))
        }
      }
      else {
        if(i == 1){
          final$fsutil[i] = min(1, max(0, final$Points[i]/final$fstotal[i]))
          final$dthutil = min(1, max(0, (final$Points[i] - final$fstotal[i])/final$dthtotal[i]))
        }
        else{
          final$fsutil[i] = min(1, max(0, ((final$Points[i] + final$Pending[i-1])/final$fstotal[i])))
          final$dthutil[i] = min(1, max(0, ((final$Points[i] + final$Pending[i-1] - final$fstotal[i])/final$dthtotal[i])))
        }
      }
    }
    
    output$average_fsutil = renderText({
      mean(final$fsutil)
    })

    ggplot(data = final, aes(x=Week, y=fsutil)) + 
      geom_line(color = 'Blue', size=1.5) +
      geom_line(aes(y = dthutil), color = 'Dark Orange', size=1.5) +
      labs(title="Utilization", x="Forecast Week", y="Utilization %") +
      geom_text(data=data.frame(x=final$Week[1],y=.12), aes(x, y), label=paste("FS Utilization"), vjust=2, color="Blue") +
      geom_text(data=data.frame(x=final$Week[1],y=.2), aes(x, y), label=paste("DTH Utilization"), vjust=2, color="Dark Orange") +
      theme(plot.title = element_text(size=18, face='bold', color = 'gray45'), 
            axis.title = element_text(size=12, face='bold', color = 'gray45'), 
            axis.text = element_text(size=12, face='bold', color = 'gray45')) +
      scale_x_date(date_labels = "%b %Y", date_breaks = '2 months') +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  })
  
#########################################################
################ Service Level Plot #####################
########################################################
  
  output$Service <- renderPlot({
    ## Subset Input datasets
    district = subset(points, district == input$District)
    district$Pending = 0
    district$Service = 0
    district$fsutil = 0
    district$dthutil = 0
    fspointdist = subset(points_pro, district == input$District & TechOrg == 'FS')
    dthpointdist = subset(points_pro, district == input$District & TechOrg == 'DTH')
    
    ## Subset Input Headcount
    hc = subset(headcount2, district == input$District & weekdays(RecordWeek) == 'Monday')
    
    ##Merge headcount with district
    district = merge(district, hc, by.x = c("district", "Week"), by.y = c("district", "RecordWeek"))
    district$source = 'Historical'
    
    ## Create Empty Dataframe
    dat = matrix(0, nrow=12, ncol=10)
    colnames(dat) = c("district" ,"Week", "Points", "Pending", "Service", "fsutil", "dthutil", "DTH", "FS", "source")
    dat = data.frame(dat)
    dat$district = input$District
    maxweek = max(district$Week)
    dthcurrent = subset(district, Week == maxweek)$DTH
    fscurrent = subset(district, Week == maxweek)$FS
    dat$DTH = dthcurrent
    dat$FS = fscurrent
    dat$source = 'Forecast'
    
    ## Create Forecast and append to dataset
    set.seed(1)
    fit = ets(district$Points)
    pred = simulate(fit, 12)
    
    week1 = max(district$Week)
    add = 7*1:12
    weeks = week1+add
    dat$Week = weeks
    dat$Points = pred
    
    ## Combine historical and forecast
    final = data.frame(rbind(district, dat))
    row.names(final) = 1:nrow(final)
    final$source = factor(final$source, levels = c("Historical", "Forecast"))
    
    ##Adjust Headcount based on input
    if(input$Quarter != 'All'){
      test = paste(year(final$Week),quarters.Date(final$Week)) == input$Quarter
      addfs = ifelse(test == TRUE, input$FSHeadcount, 0)
      adddth = ifelse(test == TRUE, input$DTHHeadcount, 0)
    }else {
      addfs = rep(input$FSHeadcount, nrow(final))
      adddth = rep(input$DTHHeadcount, nrow(final))
    }
    final$DTH = final$DTH + adddth
    final$FS = final$FS + addfs
    
    ##Simulate weekly completed points
    final$fstotal = 0
    final$dthtotal = 0
    final$steady_state = 0
    
    ## Iterate over logic to calculate pending work and utilization
    for(i in 1:nrow(final)){
      ## Sample completed points for the week
      set.seed(1)
      final$fstotal[i] = sum(rnorm(final$FS[i], input$FSMean, input$FSSD))
      final$fstotal[i] = ifelse(final$fstotal[i] < 0, 0, final$fstotal[i])
      final$dthtotal[i] = sum(rnorm(final$DTH[i], input$DTHMean, input$DTHSD))
      final$dthtotal[i] = ifelse(final$dthtotal[i] < 0, 0, final$dthtotal[i])
      final$steady_state[i] = final$fstotal[i] + final$dthtotal[i]
      
      ##Calculate work completed, pending work, and utilization
      if(i == 1){final$Pending[i] = max(0, (final$fstotal[i] + final$dthtotal[i])*.33 + final$Points[i] - final$steady_state[i])}
      else{final$Pending[i] = max(0, final$Points[i] + final$Pending[i-1] - final$steady_state[i])}
      final$Service[i] = final$Pending[i]/final$steady_state[i]
      if (input$Priority == 'DTH'){
        if(i == 1){
          final$fsutil[i] = min(1, max(0, (final$Points[i] - final$dthtotal[i])/final$fstotal[i]))
          final$dthutil = min(1, max(0, final$Points[i]/final$dthtotal[i]))
        }
        else{
          final$fsutil[i] = min(1, max(0, ((final$Points[i] + final$Pending[i-1] - final$dthtotal[i])/final$fstotal[i])))
          final$dthutil[i] = min(1, max(0, ((final$Points[i] + final$Pending[i-1])/final$dthtotal[i])))
        }
      }
      else {
        if(i == 1){
          final$fsutil[i] = min(1, max(0, final$Points[i]/final$fstotal[i]))
          final$dthutil = min(1, max(0, (final$Points[i] - final$fstotal[i])/final$dthtotal[i]))
        }
        else{
          final$fsutil[i] = min(1, max(0, ((final$Points[i] + final$Pending[i-1])/final$fstotal[i])))
          final$dthutil[i] = min(1, max(0, ((final$Points[i] + final$Pending[i-1] - final$fstotal[i])/final$dthtotal[i])))
        }
      }
    }
    

    ggplot(data = final, aes(x=Week, y=Service)) + 
      geom_line(size = 1.5) +
      labs(title="Service Level", x="Forecast Week", y="Days Out") +
      theme(plot.title = element_text(size=18, face='bold', color = 'gray45'), 
            axis.title = element_text(size=12, face='bold', color = 'gray45'), 
            axis.text = element_text(size=12, face='bold', color = 'gray45')) +
      scale_x_date(date_labels = "%b %Y", date_breaks = '2 months')
  })


#########################################################
################ Work Demand Plot ######################
########################################################
    
  output$histplot <- renderPlot({
    ## Subset Input datasets
    district = subset(points, district == input$District)
    district$Pending = 0
    district$Service = 0
    district$fsutil = 0
    district$dthutil = 0
    fspointdist = subset(points_pro, district == input$District & TechOrg == 'FS')
    dthpointdist = subset(points_pro, district == input$District & TechOrg == 'DTH')
    
    ## Subset Input Headcount
    hc = subset(headcount2, district == input$District & weekdays(RecordWeek) == 'Monday')
    
    ##Merge headcount with district
    district = merge(district, hc, by.x = c("district", "Week"), by.y = c("district", "RecordWeek"))
    district$source = 'Historical'
    
    ## Create Empty Dataframe
    dat = matrix(0, nrow=12, ncol=10)
    colnames(dat) = c("district" ,"Week", "Points", "Pending", "Service", "fsutil", "dthutil", "DTH", "FS", "source")
    dat = data.frame(dat)
    dat$district = input$District
    maxweek = max(district$Week)
    dthcurrent = subset(district, Week == maxweek)$DTH
    fscurrent = subset(district, Week == maxweek)$FS
    dat$DTH = dthcurrent
    dat$FS = fscurrent
    dat$source = 'Forecast'
    
    ## Create Forecast and append to dataset
    set.seed(1)
    fit = ets(district$Points)
    pred = simulate(fit, 12)
    
    week1 = max(district$Week)
    add = 7*1:12
    weeks = week1+add
    dat$Week = weeks
    dat$Points = pred
    
    ## Combine historical and forecast
    final = data.frame(rbind(district, dat))
    row.names(final) = 1:nrow(final)
    final$source = factor(final$source, levels = c("Historical", "Forecast"))
    
    ##Adjust Headcount based on input
    if(input$Quarter != 'All'){
      test = paste(year(final$Week),quarters.Date(final$Week)) == input$Quarter
      addfs = ifelse(test == TRUE, input$FSHeadcount, 0)
      adddth = ifelse(test == TRUE, input$DTHHeadcount, 0)
    }else {
      addfs = rep(input$FSHeadcount, nrow(final))
      adddth = rep(input$DTHHeadcount, nrow(final))
    }
    final$DTH = final$DTH + adddth
    final$FS = final$FS + addfs
    
    ##Simulate weekly completed points
    final$fstotal = 0
    final$dthtotal = 0
    final$steady_state = 0
    
    ## Iterate over logic to calculate pending work and utilization
    for(i in 1:nrow(final)){
      ## Sample completed points for the week
      set.seed(1)
      final$fstotal[i] = sum(rnorm(final$FS[i], input$FSMean, input$FSSD))
      final$fstotal[i] = ifelse(final$fstotal[i] < 0, 0, final$fstotal[i])
      final$dthtotal[i] = sum(rnorm(final$DTH[i], input$DTHMean, input$DTHSD))
      final$dthtotal[i] = ifelse(final$dthtotal[i] < 0, 0, final$dthtotal[i])
      final$steady_state[i] = final$fstotal[i] + final$dthtotal[i]
      
      ##Calculate work completed, pending work, and utilization
      if(i == 1){final$Pending[i] = max(0, (final$fstotal[i] + final$dthtotal[i])*.33 + final$Points[i] - final$steady_state[i])}
      else{final$Pending[i] = max(0, final$Points[i] + final$Pending[i-1] - final$steady_state[i])}
      final$Service[i] = final$Pending[i]/final$steady_state[i]
      if (input$Priority == 'DTH'){
        if(i == 1){
          final$fsutil[i] = min(1, max(0, (final$Points[i] - final$dthtotal[i])/final$fstotal[i]))
          final$dthutil = min(1, max(0, final$Points[i]/final$dthtotal[i]))
        }
        else{
          final$fsutil[i] = min(1, max(0, ((final$Points[i] + final$Pending[i-1] - final$dthtotal[i])/final$fstotal[i])))
          final$dthutil[i] = min(1, max(0, ((final$Points[i] + final$Pending[i-1])/final$dthtotal[i])))
        }
      }
      else {
        if(i == 1){
          final$fsutil[i] = min(1, max(0, final$Points[i]/final$fstotal[i]))
          final$dthutil = min(1, max(0, (final$Points[i] - final$fstotal[i])/final$dthtotal[i]))
        }
        else{
          final$fsutil[i] = min(1, max(0, ((final$Points[i] + final$Pending[i-1])/final$fstotal[i])))
          final$dthutil[i] = min(1, max(0, ((final$Points[i] + final$Pending[i-1] - final$fstotal[i])/final$dthtotal[i])))
        }
      }
    }
    
    ##Prepare Plot
    
    if(input$Priority == 'DTH'){capacity1 = final$dthtotal} else{capacity1 = final$fstotal}
    capacity2 = final$dthtotal + final$fstotal
    text1 = ifelse(input$Priority == 'DTH', "DTH Staffing Level", "FS Staffing Level")
    text2 = ifelse(input$Priority == 'DTH', "FS Staffing Level", "DTH Staffing Level")
    color1 = ifelse(input$Priority == 'DTH', "Dark Orange", "Blue")
    color2 = ifelse(input$Priority == 'DTH', "Blue", "Dark Orange")
    final$capacity1 = capacity1
    final$capacity2 = capacity2

    ggplot(data = final, aes(x=Week, y=Points)) +
      geom_line(aes(linetype = source), size=1.5) +
      scale_x_date(date_labels = "%b %Y", date_breaks = '2 months') +
      labs(title="Historical Forecast", x="Week", y="Points") +
      theme(legend.position="none") +
      theme(plot.title = element_text(size=18, face='bold', color = 'gray45'),
            axis.title = element_text(size=12, face='bold', color = 'gray45'),
            axis.text = element_text(size=12, face='bold', color = 'gray45')) +
      geom_line(aes(x = Week, y = capacity1), color=color1) +
      geom_line(aes(x = Week, y = capacity2), color=color2) +
      geom_text(data=data.frame(x=min(final$Week) + 14,y=max(final$capacity2)-20), aes(x, y), label=paste(text1), vjust=-1, color=color1) +
      geom_text(data=data.frame(x=min(final$Week) + 14,y=max(final$capacity2)-60), aes(x, y), label=paste(text2), vjust=-1, color=color2)# +
      #ylim(c(0, max(final$Points) + 300))
    

  })
  
  # output$quarter = renderText({
  #   quarter = get(input$Quarter)
  #   names(quarter)
  # })
  
  # output$vivint = renderImage({
  #   
  #   filename <- normalizePath(file.path("vivint_smart_home.jpg"))
  #   
  #   # Return a list containing the filename and alt text
  #   list(src = filename)
  # }, deleteFile = FALSE)
  # 
  # output$arrows = renderImage({
  #   
  #   filename <- normalizePath(file.path("Arrows.PNG"))
  #   
  #   # Return a list containing the filename and alt text
  #   list(src = filename)
  # }, deleteFile = FALSE)
  
}