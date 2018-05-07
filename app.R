library(shiny)
library(shinydashboard)
library(ggplot2)
library(reshape2)

temperature = read.csv("temperature.txt", sep = " ")

ts <- temperature[,-14]
ts <- as.vector(t(ts[,-1]))
ts <- ts(ts, start = c(1989,1), frequency=12)

graph2<-{ggplot(data=temperature, aes(x=YEAR, y=ANNUAL, group=1)) +
  geom_line()+
  geom_point() + 
  labs(title="Plot of Annual Temperature",x="Year", y = "Annual Temperature")}

temperature_melt <- melt(temperature[, -14], id="YEAR") 

graph3<-{ggplot(data=temperature_melt,
       aes(x=YEAR, y=value, colour=variable)) +
  geom_line() + geom_point()  + 
  labs(title="Plot of Monthly Temperature",x="Year", y = "Monthly Temperature")}


precipitation = read.csv("precipitation.txt", sep = " ")
ts2 <- precipitation[,-14]
ts2 <- as.vector(t(ts2[,-1]))
ts2 <- ts(ts2, start = c(1989,1), frequency=12)

graph5<-{ggplot(data=precipitation, aes(x=YEAR, y=ANNUAL, group=1)) +
  geom_line()+
  geom_point() + 
  labs(title="Plot of Annual Precipitation",x="Year", y = "Annual Precipitation")}

precipitation_melt <- melt(precipitation[, -14], id="YEAR") 

graph6<-{ggplot(data=precipitation_melt,
       aes(x=YEAR, y=value, colour=variable)) +
  geom_line() + geom_point() +
  labs(title="Plot of Monthly Precipitation",x="Year", y = "Monthly Precipitation")}

snow = read.csv("snow.txt", sep = " ")
ts3 <- snow[,-14]
ts3 <- as.vector(t(ts3[,-1]))
ts3 <- ts(ts3, start = c(1989,1), frequency=12)


graph8<-{ggplot(data=snow, aes(x=SEASON, y=TOTAL, group=1)) +
  geom_line()+
  geom_point() + 
  labs(title="Plot of Total Snow",x="Season", y = "Total Snow") +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1))} 

snow_melt <- melt(snow[, -14], id="SEASON") 

graph9<-{ggplot(data=snow_melt,
       aes(x=SEASON, y=value, colour=variable)) +
  geom_point() +
  labs(title="Plot of Monthly Snow",x="Year", y = "Monthly Snow") +
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle=90, hjust=1))} 

highest = read.csv("highest.txt", sep = "\t")
lowest =  read.csv("lowest.txt", sep = "\t")
years = seq(1987, 2017)
df = data.frame(years=years, highest=highest$F, lowest=lowest$F)

hl_melt <- melt(df, id="years") 
graph10<-{ggplot(data=hl_melt,
       aes(x=years, y=value, colour=variable)) +
  geom_line() +
  geom_point() +
  labs(title="Plot of Highest and Lowest temperature",x="Year", y = "Temperature")}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "NYC weather"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Temperature", tabName = "temp"),
      menuItem("Precipitation", tabName = "precip"),
      menuItem("Snow", tabName = "snow"),
      menuItem("Relations", tabName = "relation")
    )
  ),
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "temp",
              fluidRow(
                box(selectInput("a_mode",
                                "Mode:",
                                choices = list("Time series", "Annual Temperature",
                                               "Temperature of the same month")), 
                    plotOutput("plot1"),
                    htmlOutput("wr1"), width = 12) 
              )
              ),
      
      tabItem(tabName = "precip",
              fluidRow(
                box(selectInput("b_mode",
                                "Mode:",
                                choices = list("Time series", "Annual Precipitation",
                                               "Precipitation of the same month")), 
                    plotOutput("plot2"),
                    htmlOutput("wr2"),width = 12)
              )
              ),
      tabItem(tabName = "snow",
              fluidRow(
                box(selectInput("c_mode",
                                "Mode:",
                                choices = list("Time series", "Annual Snow",
                                               "Snow of the same month")),
                    plotOutput("plot3"),
                    htmlOutput("wr3"),width = 12)
              )),
      tabItem(tabName = "relation",
              fluidRow(
                box(selectInput("d_mode",
                                "Mode:",
                                choices = list("Correlations","Extremes")),
                    htmlOutput("wr4"), 
                    plotOutput("plot4"),width = 12)
              ))
              )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
    if (input$a_mode == "Time series") {
      ts.plot(ts,gpars=list(xlab="Year", ylab="Temperature", lty=1))
    }
    
    if (input$a_mode == "Annual Temperature") {
      print(graph2)
    }     
    
    if (input$a_mode == "Temperature of the same month") {
      print(graph3)
    }     
    
  })
  
  output$plot2 <- renderPlot({
    
    if (input$b_mode == "Time series") {
      ts.plot(ts2,gpars=list(xlab="Year", ylab="Precipitation", lty=1))
    }
    
    if (input$b_mode == "Annual Precipitation") {
      print(graph5)
    }     
    
    if (input$b_mode == "Precipitation of the same month") {
      print(graph6)
    }     
    
  })
  
  output$plot3 <- renderPlot({
    
    if (input$c_mode == "Time series") {
      ts.plot(ts3,gpars=list(xlab="Year", ylab="Temperature", lty=1))
    }
    
    if (input$c_mode == "Annual Snow") {
      print(graph8)
    }     
    
    if (input$c_mode == "Snow of the same month") {
      print(graph9)
    }     
    
  })
  
  output$plot4 <- renderPlot({
    if (input$d_mode == "Extremes"){
      print(graph10)
    }
  })

  output$wr1 <- renderPrint({
    
    if (input$a_mode == "Time series") {
      cat("As the graph shows, the temperature does not seem to change much during the past 30 years, there is not much difference among different years.")
    }
    
    if (input$a_mode == "Annual Temperature") {
      cat("Here we can clearly see the difference since we zoom in a little bit. The annual temperatures fluctuate from 53 to 58 degrees, although the difference is not big we can see the warmer years seem to be consecutive for at least two year in a row before the annual temperature drops again.")
    }     
    
    if (input$a_mode == "Temperature of the same month") {
      cat("As we can see, the temperatures for NYC in Aug and Sep are the highest among 12 months, Dec, Jan, and Feb have the lowest temperatures. Notice that some months' temperature changes alot in different years. To test which month's temperature differs the most among 30 years, we compute the standard deviation of the 12 months and the year average.")
      for (month in c(2:14)) {
        cat("<br><br>")
        cat(c(colnames(temperature)[month],sd(temperature[,month])))
      }
      cat("<br><br>")
      cat("According to the numbers, the temperatures for December vary the most year by year. Jun temperatures stay stable.")
    }     
    
})
  
  output$wr2 <- renderPrint({
    
    if (input$b_mode == "Time series") {
      cat("We can see that there are two months in which the precipitaion is dramatically different from others - 2005 Oct and 2011 Aug. Also, unlike the temperature, we can observe a obvious change in precipitation during the past 30 years.")
    }
    
    if (input$b_mode == "Annual Precipitation") {
      cat("2011 does have the largest annual precipitation as we can predict from the time series, but 2005's precipitation is moderate although it has a very large precipitation in October. The range  of the annual precipitation is between 30 to 80 in which there is a very big difference.")
    }     
    
    if (input$b_mode == "Precipitation of the same month") {
      cat("Standard deviation list:")
      for (month in c(2:14)) {
        cat("<br><br>")
        cat(c(colnames(precipitation)[month],sd(precipitation[,month])))

      }
      cat("<br><br>")
      cat("From the data, the precipitation of Aug varies the most. Also, there is a huge difference among the precipitation of 30 years.")
    }     
    
  })
  
  output$wr3 <- renderPrint({
    
    if (input$c_mode == "Time series") {
      cat("Observe there's a difference of the snow amount in each year.")
    }
    
    if (input$c_mode == "Annual Snow") {
      cat("Annual snow ranges from 0 to 80, we can see normally there is a peak following a local min. Also, the differences among years are clear to see.")
    }     
    
    if (input$c_mode == "Snow of the same month") {
      cat("Standard deviation list:")
      for (month in c(2:14)) {
        cat("<br><br>")
        cat(c(colnames(snow)[month],sd(snow[,month])))

      }
      cat("<br><br>")
      cat("According to the result, the snow in Feb varies the most indicating it is relatively unpredicable compare to the other months. Interesting to see that NYC even get snow in Jun of 2017 showing the capriciousness of weather.")
    }     
    
  })
  
  output$wr4 <- renderPrint({
    if (input$d_mode == "Correlations"){
    cat("To find out if the temperature, precipatation, and snow amount are related, calculate the corvariance of each pair. <br><br>
          Temperature vs. Precipitation: -0.2279292<br><br>
          Temperature vs. Snow: -0.1447495<br><br>
          Precipitation vs. Snow: -0.193025<br><br>
          The results are very small no matter the value is positive or negative, which indicates that the three factors are not strongly related.")}
    if(input$d_mode == "Extremes"){
      cat("Curious of whether of not cold winters always follow hot summers, I plot the extreme temperatures of each year and do a corvariance test of the two.<br><br> 
            By doing the covariance test, highest vs. lowest : 0.2572172<br><br>
            The highest temperature and lowest temperature has positive correlation, but the number is very small, which indicates that the largest temperature and the lowest temperature are not strongly related. So hot summers may not indicate cold winters.")
    }
    
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
