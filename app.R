library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(leaflet)
library(bslib)
library(zoo)

set.seed(123)
n <- 1500

# Simulate 10 years of crash data
data <- tibble(
  date = sample(seq.Date(as.Date("2015-01-01"), as.Date("2024-12-31"), by="day"), n, replace = TRUE),
  hour = sample(0:23, n, replace = TRUE),
  lat = runif(n, 31.70, 31.85),
  lon = runif(n, -106.55, -106.35),
  crash = rbinom(n,1,0.6),
  arrest = rbinom(n,1,0.35)
)
data$weekday <- wday(data$date,label=TRUE)

monthly <- data %>%
  mutate(month=floor_date(date,"month")) %>%
  group_by(month) %>%
  summarise(crashes=sum(crash), arrests=sum(arrest), .groups="drop") %>%
  arrange(month) %>%
  mutate(moving_avg = rollmean(crashes, k=3, fill=NA, align="right"))

heat_data <- data %>%
  group_by(hour,weekday) %>%
  summarise(crashes=sum(crash),.groups="drop")

total_crashes <- sum(data$crash)
total_arrests <- sum(data$arrest)
peak_hour <- data %>% count(hour) %>% arrange(desc(n)) %>% slice(1) %>% pull(hour)

# Risk clock
risk_hour <- tibble(
  hour = 0:23,
  risk = case_when(hour >= 23 | hour <= 2 ~ 3, hour >= 18 ~ 2, TRUE ~ 1),
  label = case_when(hour >= 23 | hour <= 2 ~ "High", hour >= 18 ~ "Medium", TRUE ~ "Low")
)

# Quiz questions
quiz <- tibble(
  question = c(
    "Most crashes in El Paso occur on weekends.",
    "Alcohol-related crashes mostly happen between 10 PM and 2 AM.",
    "Drinking at home is always safer than at bars.",
    "Young adults (18-25) are more likely to be involved in alcohol-related crashes."
  ),
  options = list(
    c("True","False"), c("True","False"), c("True","False"), c("True","False")
  ),
  answer = c(TRUE, TRUE, FALSE, TRUE)
)

# UI
ui <- fluidPage(
  theme = bs_theme(version=5, bootswatch="flatly", primary="#e07b39", secondary="#2a5d84",
                   base_font=font_google("Roboto"), heading_font=font_google("Roboto Slab")),
  
  tags$head(tags$style(HTML("
      h1 {font-weight:700; font-size:3em; color:#e07b39;}
      h2 {font-weight:600; color:#2a5d84;}
      p {font-size:1.1em; line-height:1.5;}
      .section {padding:60px 40px;}
      .value-box {text-align:center; padding:20px; border-radius:10px; background-color:#fdf0e0; margin-bottom:20px; box-shadow:2px 2px 5px #aaa;}
      .quiz-question {background-color:#f1f1f1; padding:20px; border-radius:10px; margin-bottom:20px; box-shadow:1px 1px 3px #ccc;}
  "))),
  
  # Hero Section
  div(class="section", style="background-color:#f8f9fa;",
      h1("🍹 Alcohol & Driving in El Paso"),
      p("Explore patterns, learn risk times, and test your knowledge about DWI in our city."),
      fluidRow(
        column(4, div(class="value-box", h2(total_crashes), "Crashes")),
        column(4, div(class="value-box", h2(total_arrests), "DWI Arrests")),
        column(4, div(class="value-box", h2(paste0(peak_hour,":00")), "Peak Crash Hour"))
      )
  ),
  
  # Map
  div(class="section", h2("📍 Crash Map"),
      p("Click markers to learn about individual crashes."),
      leafletOutput("map", height=500)
  ),
  
  # Trend
  div(class="section", style="background-color:#f8f9fa;",
      h2("📈 Crash Trends Over 10 Years"),
      p("Dashed line = observed crashes; Solid line = 3-month moving average."),
      plotlyOutput("trend")
  ),
  
  # Hourly heatmap
  div(class="section", h2("⏰ Crash Frequency by Hour & Day"),
      p("Darker colors = more crashes"),
      plotlyOutput("heatmap", height=500)
  ),
  
  # Risk Clock
  div(class="section", style="background-color:#f8f9fa;",
      h2("🚦 DWI Risk Clock"),
      p("Red = high, Orange = medium, Blue = low risk by hour of day."),
      plotlyOutput("riskclock", height=600)
  ),
  
  # Quiz
  div(class="section", style="background-color:#fdf0e0;",
      h2("📝 DWI Myth Quiz"),
      uiOutput("quiz_ui"),
      actionButton("submit_quiz","Submit Answers"),
      br(), br(),
      verbatimTextOutput("quiz_results")
  )
)

# Server
server <- function(input, output, session){
  
  output$map <- renderLeaflet({
    leaflet(data) %>% addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(lng=~lon, lat=~lat, radius=5, color="#e07b39", stroke=FALSE, fillOpacity=0.7,
                       label=~paste0("Date: ",date," | Hour: ",hour,":00 | Crash: ",crash),
                       labelOptions = labelOptions(direction="auto"))
  })
  
  output$trend <- renderPlotly({
    ggplotly(
      ggplot(monthly, aes(x=month)) +
        geom_line(aes(y=crashes), color="#f4a261", linetype="dashed", linewidth=1.5) +
        geom_line(aes(y=moving_avg), color="#2a5d84", linetype="solid", linewidth=2) +
        geom_point(aes(y=crashes), color="#f4a261") +
        theme_minimal() +
        labs(x="Month", y="Crashes")
    )
  })
  
  output$heatmap <- renderPlotly({
    ggplotly(
      ggplot(heat_data, aes(hour, weekday, fill=crashes)) +
        geom_tile() +
        scale_fill_viridis_c(option="plasma") +
        theme_minimal() +
        labs(x="Hour", y="Day of Week", fill="Crashes")
    )
  })
  
  output$riskclock <- renderPlotly({
    theta <- seq(0, 360-15, length.out=24)
    plot_ly(type='barpolar', r=risk_hour$risk, theta=theta,
            color=risk_hour$label, colors=c("Low"="#2a5d84","Medium"="#f4a261","High"="#e07b39"),
            marker=list(line=list(color='black',width=1))) %>%
      layout(polar=list(radialaxis=list(showticklabels=FALSE,ticks=""),
                        angularaxis=list(rotation=90,direction="clockwise",
                                         tickmode="array", tickvals=seq(0,345,by=30),
                                         ticktext=c("12AM","1AM","2AM","3AM","4AM","5AM","6AM",
                                                    "7AM","8AM","9AM","10AM","11AM"))),
             showlegend=TRUE)
  })
  
  output$quiz_ui <- renderUI({
    lapply(1:nrow(quiz), function(i){
      div(class="quiz-question",
          p(paste0(i,". ",quiz$question[i])),
          radioButtons(paste0("q",i), NULL, choices=quiz$options[[i]], inline=TRUE)
      )
    })
  })
  
  observeEvent(input$submit_quiz,{
    user_answers <- sapply(1:nrow(quiz), function(i){
      val <- input[[paste0("q",i)]]
      if(is.null(val)) return(NA)
      as.logical(val=="TRUE")
    })
    correct <- quiz$answer == user_answers
    score <- sum(correct, na.rm=TRUE)
    
    output$quiz_results <- renderText({
      paste0("You got ", score, " out of ", nrow(quiz), " correct.\n\n",
             paste(sapply(1:nrow(quiz), function(i){
               paste0(i,". ",quiz$question[i],
                      " | Your answer: ", ifelse(is.na(user_answers[i]),"No answer",user_answers[i]),
                      " | Correct: ", quiz$answer[i])
             }), collapse="\n"))
    })
  })
}

shinyApp(ui, server)