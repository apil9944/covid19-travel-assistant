library(RColorBrewer)
library(scales)
library(dplyr)
library(plotly)
library(shiny)
library(shinythemes)
library(ggplot2)
library(ggthemes)
library(DT)
library(MOSS)


### Reading in Data ###
covid_data = readRDS("data/covid_data.rds")
gopassport_raw = read.csv('data/Gopassport.csv')

## Data transformations
covid_data$date <- as.Date(covid_data$date)
covid_data$new_cases[covid_data$new_cases < 0] <- 0
covid_data$new_cases_smoothed[covid_data$new_cases_smoothed < 0] <- 0
covid_data$population_vaccinated_proportion = covid_data$people_fully_vaccinated/covid_data$population

world_map <- map_data("world2", wrap = c(-30, 330))
world_map <- world_map %>%
  mutate(region = replace(region, region == "UK","United Kingdom")) %>% 
  mutate(region = replace(region, region == "USA","United States"))

# Final logistic regression model
covid = covid_data %>% 
  dplyr::select(location,date,stringency_index,
                total_deaths_per_million,
                total_cases_per_million,
                life_expectancy,
                gdp_per_capita,
                human_development_index,
                new_cases_per_million,
                people_fully_vaccinated_per_hundred) %>% 
  drop_na() %>% 
  group_by(location) %>%
  filter(date == max(date, na.rm=TRUE)) 

gopassport = gopassport_raw %>% 
  dplyr::select(GEOGRAPHY,RISK.RATING,VACCINATION.RATE) %>%  
  mutate(VACCINATION.RATE = as.numeric(gsub("%", "", VACCINATION.RATE))) %>%
  drop_na()

covid_risk = merge(covid, gopassport, 
                   by.x=c('location'),
                   by.y=c('GEOGRAPHY'))

M1b_data = covid_risk %>%
  mutate(Travel = ifelse(new_cases_per_million < 573.408 &
                           people_fully_vaccinated_per_hundred > 59,1,0))  %>%
  dplyr::select(!c(date, location, RISK.RATING,
                   new_cases_per_million,
                   people_fully_vaccinated_per_hundred))

model_M1b <- glm(Travel ~ ., data = M1b_data, family = binomial) %>% stepAIC(trace = FALSE)


# Travel logistic regression model

data=covid_data
data=data[,c('date','iso_code', 'location', 'gdp_per_capita','total_deaths_per_million','total_vaccinations_per_hundred','total_cases_per_million','new_cases', 'people_fully_vaccinated_per_hundred', 
             'new_cases_per_million')]
data=data[complete.cases(data),]
df = data %>% group_by(iso_code) %>% filter(date == max(date, na.rm=TRUE))

model1 = lm(total_cases_per_million~gdp_per_capita, data=df)
model2 = lm(total_deaths_per_million~total_vaccinations_per_hundred, data=df)

average_new_cases_permillion = mean(df$new_cases_per_million) #global average cases

df$Travel = ifelse(df$new_cases_per_million < average_new_cases_permillion & df$people_fully_vaccinated_per_hundred > 59, 1, 0) #in presentation, justify why we chose this numbers
lr = glm(Travel ~ gdp_per_capita + total_deaths_per_million + total_cases_per_million, data = df,family = 'binomial')


# Define UI for app that draws a histogram ----
ui <- fluidPage(theme = shinytheme("cerulean"),
                
                # App title ----
                titlePanel(h1(strong("COVID-19 Travel Assistant"))),
                br(),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  sidebarPanel(width = 3,
                               conditionalPanel("input.tabs == 'Vaccinations' || input.tabs == 'New Cases' || input.tabs == 'Restrictions' || input.tabs == 'Travel Advice'",
                                                selectInput("shortlist_countries", "Shortlist destinations you are interested in visiting:",
                                                            choices = unique(covid_data$location),
                                                            multiple = T,
                                                            selected = "Australia")
                               )
                               
                  ),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    tabsetPanel(id = "tabs",
                                navbarMenu(title = "Destination Explorer",
                                           tabPanel(title = "Vaccinations",
                                           plotlyOutput("total_vacc_plot")),
                                           tabPanel(title = "New Cases",
                                           plotlyOutput("new_cases_plot")),
                                           tabPanel(title = "Restrictions",
                                           plotlyOutput("stringency_plot"))
                                           
                                ),
                                tabPanel(
                                  title = "Travel Advice",
                                  h4("Travel Recommendations"),
                                  dataTableOutput('recommendations'),
                                  h4("New Cases and Vaccinations Trend"),
                                  br(),
                                  plotlyOutput("new_cases_trend")
                                ),
                                
                                tabPanel(
                                  title = "Recommendations",
                                  h4("Recommended destinations for you"),
                                  dataTableOutput("top_destinations")
                                )
                    )
                  )
                )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  fetch_data = reactive ({
    latest_date = '2022-02-01'
    subset = covid_data %>% filter(date == latest_date)
    return(subset)
  })
  
  fetch_travel_advice = reactive ({
    selected_risk = input$risk_level
    subset = travel_advice %>% filter(risk_level == selected_risk)
    #return (subset)
    return(travel_advice)
  })
  
  ## Explorer Plots
  
  #Vaccinations
  output$total_vacc_plot = renderPlotly({
    covid_data = fetch_data()
    world_map_with_data <- merge(world_map, covid_data,
                                 by.x = "region", by.y = "location",
                                 all.x = TRUE)
    world_map_with_data <- world_map_with_data[order(world_map_with_data$order), ]
    breaks <- c(0, 0.5, 0.7, 0.8, 0.9, 1)

    ## Simple processing to remove NAs
    world_map_with_data$population_vaccinated_proportion[is.na(world_map_with_data$population_vaccinated_proportion)] <- 0
    world_map_with_data$total_vaccinations_category <-
      cut(as.numeric(world_map_with_data$population_vaccinated_proportion),
          breaks, include.lowest = TRUE, right = FALSE, dig.lab=10)

    ## Generate color scheme
    green_col <- RColorBrewer::brewer.pal(length(breaks) - 1, "Greens")
    names(green_col) <- levels(world_map_with_data$total_vaccinations_category)

    ## Graphs
    g = ggplot(world_map_with_data,
           aes(x = long, y = lat, group = group, fill = total_vaccinations_category, text = paste("</br>",region,
                                                                                                  '</br> Proportion Vaccinated:', round(population_vaccinated_proportion*100, 0),'%'))) +
      geom_polygon() +
      scale_fill_manual(values = green_col) +
      xlab("") + ylab("") +  ggtitle("Map of World") +
      theme_void() +
      theme(legend.position = "bottom", aspect.ratio = 0.6) +
      labs(title = paste('Proportion of population vaccinated'),
           fill = "")
    ggplotly(g, height = 500, width = 1000, tooltip = "text")
  })
  
  # New Cases
  output$new_cases_plot = renderPlotly({
    covid_data = fetch_data()
    world_map_with_data <- merge(world_map, covid_data,
                                 by.x = "region", by.y = "location",
                                 all.x = TRUE)
    world_map_with_data <- world_map_with_data[order(world_map_with_data$order), ]
    breaks <- c(0, 100, 500, 1000, 5000, 10000)

    ## Simple processing to remove NAs
    world_map_with_data$new_cases_smoothed_per_million[is.na(world_map_with_data$new_cases_smoothed_per_million)] <- 0
    world_map_with_data$total_vaccinations_category <-
      cut(as.numeric(world_map_with_data$new_cases_smoothed_per_million),
          breaks, include.lowest = TRUE, right = FALSE, dig.lab=10)

    ## Generate color scheme
    green_col <- RColorBrewer::brewer.pal(length(breaks) - 1, "Reds")
    names(green_col) <- levels(world_map_with_data$total_vaccinations_category)

    ## Graphs
    g = ggplot(world_map_with_data,
               aes(x = long, y = lat, group = group, fill = total_vaccinations_category, text = paste("</br>",region,
                                                                                                      "</br> New Cases:", round(new_cases_smoothed_per_million, 0) , 'per million'))) +
      geom_polygon() +
      scale_fill_manual(values = green_col) +
      xlab("") + ylab("") +  ggtitle("Map of World") +
      theme_void() +
      theme(legend.position = "bottom", aspect.ratio = 0.6) +
      labs(title = paste('New COVID-19 cases'),
           fill = "")
    ggplotly(g, height = 500, width = 1000, tooltip = "text")
  })

  # Restriction Levels
  output$stringency_plot = renderPlotly({
    covid_data = fetch_data()
    world_map_with_data <- merge(world_map, covid_data,
                                 by.x = "region", by.y = "location",
                                 all.x = TRUE)
    world_map_with_data <- world_map_with_data[order(world_map_with_data$order), ]

    breaks <- seq(0, 100, by=20)
    ## Simple processing to remove NAs
    world_map_with_data$stringency_index[is.na(world_map_with_data$stringency_index)] <- 0
    world_map_with_data$total_vaccinations_category <-
      cut(as.numeric(world_map_with_data$stringency_index),
          breaks, include.lowest = TRUE, right = FALSE, dig.lab=10)

    ## Generate color scheme
    green_col <- RColorBrewer::brewer.pal(length(breaks) - 1, "Blues")
    names(green_col) <- levels(world_map_with_data$total_vaccinations_category )

    ## Graphs
    g = ggplot(world_map_with_data,
               aes(x = long, y = lat, group = group, fill = total_vaccinations_category, text = paste("</br>",region,
                                                                                                      "</br> Restriction Level:", round(stringency_index, 0)))) +
      geom_polygon() +
      scale_fill_manual(values = green_col) +
      xlab("") + ylab("") +  ggtitle("Map of World") +
      theme_void() +
      theme(legend.position = "bottom", aspect.ratio = 0.6) +
      labs(title = paste('Restriction Levels'),
           fill = "")
    ggplotly(g, height = 500, width = 1000, tooltip = "text")
  })

  ## Recommendation using user inputs/preferences
  output$recommendations = renderDataTable({
    set.seed(3888)
    target_countries = c(input$shortlist_countries)
    pred_results = c()
    for (i in 1:length(target_countries)) {
      country_stats = filter(covid_risk, location == target_countries[i])
      new_data = data.frame(country_stats)
      pred = ifelse(predict(model_M1b, new_data, type = 'response') > 0.5, "Recommended", "Not Recommended")
      pred_results = append(pred_results, pred)
    }
    country_pred = data.frame(target_countries, pred_results)
    colnames(country_pred) = c("Destination", "Travel Recommendation")
    country_pred
  })
  
  output$new_cases_trend = renderPlotly({
    countries = c(input$shortlist_countries)
    g1 = ggplot(covid_data %>% filter(location %in% countries, date >= "2022-01-01"), 
           aes(x = date, y = new_cases_smoothed_per_million, 
               group = location, color = location)) + geom_line(lwd = 0.5) +
      theme_bw() +
      ylab("New cases (per million)") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_date(date_breaks = "1 month") +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(color = "Destination") +
      xlab("Date") + ylab("New cases (per million)")
    g1 = ggplotly(g1)
    
    g2 = ggplot(covid_data %>% filter(location %in% countries, date >= "2022-01-01"), 
                aes(x = date, y = population_vaccinated_proportion, 
                    group = location, color = location)) + geom_line(lwd = 0.5) +
      theme_bw() +
      ylab("Population vaccination rate") +
      scale_y_continuous(labels = scales::comma) +
      scale_x_date(date_breaks = "1 month") +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(color = "Destination") +
      xlab("Date") + ylab("Vaccination rate")
    g2 = ggplotly(g2)
    
    fig <- subplot(g1, g2)
    fig
  })

  
  output$top_destinations = renderDataTable({
    set.seed(3888)
    target_countries = unique(covid_risk$location)
    pred_desc= c()
    pred_num = c()
    for (i in 1:length(target_countries)) {
      country_stats = filter(covid_risk, location == target_countries[i])
      new_data = data.frame(country_stats)
      
      pred_num_i = round((predict(model_M1b, new_data, type = 'response') * 100), 2)
      pred_desc_i = ifelse(pred_num_i > 50, "Recommended", "Not Recommended")
      pred_desc = append(pred_desc, pred_desc_i)
      pred_num = append(pred_num, pred_num_i)
    }
    country_pred = data.frame(target_countries, pred_num, pred_desc)
    country_pred = country_pred[order(-pred_num),] %>% filter(pred_desc == "Recommended")
    colnames(country_pred) = c("Destination", "Score","Travel Recommendation")
    country_pred
  })

}
# Run the application 
shinyApp(ui = ui, server = server)
