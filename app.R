library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(ggplot2)
library(ggcorrplot)
library(dplyr)
library(maps)
# Define the choices for selectInput
c1 <- colnames(read.csv("data/USArrests.csv"))[-1]  # Variable names from the dataset
c2 <- c("Murder", "Assault", "UrbanPop", "Rape")  # Arrest types

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Exploring US Arrest Dataset", titleWidth = 650),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Dataset", tabName = "data", icon = icon("user")),
      menuItem(text = "Visualization", tabName = "Viz", icon = icon("chart-line")),
      conditionalPanel(
        condition = "input.sidebar == 'Viz' && input.t2 == 'distro'",
        selectInput(inputId = "var1", label = "Select Variable", choices = c1, selected = "Rape")
      ),
      conditionalPanel(
        condition = "input.sidebar == 'Viz' && input.t2 == 'trends'",
        selectInput(inputId = "var4", label = "Select the Arrest Type", choices = c2)
      ),
      conditionalPanel(
        condition = "input.sidebar == 'Viz' && input.t2 == 'relation'",
        selectInput(inputId = "var2", label = "Select X Variable", choices = c1, selected = "Rape")
      ),
      conditionalPanel(
        condition = "input.sidebar == 'Viz' && input.t2 == 'relation'",
        selectInput(inputId = "var3", label = "Select Y Variable", choices = c1, selected = "Assault")
      ),
      menuItem(text = "Choropleth Map", tabName = "map", icon = icon("map"))
    )
  ),
  dashboardBody(
    tabItems(
      #first tab item
      tabItem(tabName = "data",
              tabBox(id="t1",width=12,
                     tabPanel("About",icon=icon("address-card"),fluidRow(
                       column(width=8,tags$img(src="card-crime.png",width=600,height=300),
                              tags$br(),
                              tags$a("Crime"),align="center"),
                       column(width=4,tags$br(),
                              tags$p("The USArrests dataset is from Kaggle contains
                                     data on violent crime rates for 50 U.S. states in 1973. this  data set provide statistics for four variables related to arrests.
                                     The dataset consists of 50 rows (one for each U.S. state) and 4 columns (variables).
                                     States and 4 variable Murder,Assaults,UrbanPop and Rape",style="font-size:16px;")
                       )
                     )),
                     tabPanel("Data",icon=icon("address-card"),dataTableOutput("dataT")),
                     tabPanel("Structure",icon=icon("address-card"),verbatimTextOutput("structure")),
                     tabPanel("Summary",icon=icon("address-card"),verbatimTextOutput("summary"))
              )
      ),
      #second tab item
      tabItem(tabName = "Viz",
              tabBox(id="t2",width = 12,
                     tabPanel(title="Crime Treands by state",value="treads",
                              fluidRow(tags$div(align="center",box(tableOutput("top5"),title =textOutput("head1"), collapsible = TRUE,status = "primary"),collapse=TRUE,solidHeader=TRUE),
                                       tags$div(align="center",box(tableOutput("low5"),title =textOutput("head2"), collapsible = TRUE,status = "primary"),collapse=TRUE,solidHeader=TRUE)),
                              plotlyOutput("bar")),
                     tabPanel(title="Distribution",value="distro",plotlyOutput("histplot")),
                     tabPanel(title="Correlation Matrix",plotlyOutput("cor")),
                     tabPanel(title="Relationship among Arrest types & Urban Population",value="relation",
                              radioButtons(inputId="fit",label = "Select smooth method",choices = c("loess","lm"),selected = "lm",inline=TRUE),
                              plotlyOutput("scatter"))
              )
      ),
      # Third tab item
      tabItem(tabName = "map",
              box(selectInput("crimetype", "Select Arrest Type", choices = c2, selected = "Rape", width = 250),
                  plotOutput("map_plot"), width = 12
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Load and prepare data
  my_data <- read.csv("data/USArrests.csv")
  my_df <- subset(my_data, select = -States)
  state_map<-map_data("state")
  state_map
  my_data1=my_data %>%
    mutate(States=tolower(States))
  merged=right_join(my_data1,state_map,by=c("States"="region"))
  str(merged)
  
  St=data.frame(abb=state.abb,stname=tolower(state.name),x=state.center$x,y=state.center$y)
  str(St)
  
  new_join=left_join(merged,St,by=c("States"="stname"))
  str(new_join)
  
  # Render the data table
  output$dataT <- renderDataTable({
    my_data
  })
  
  # Structure
  output$structure <- renderPrint({
    str(my_data)
  })
  
  # Summary
  output$summary <- renderPrint({
    summary(my_data)
  })
  
  # Input text outputs
  output$selected_var <- renderText({
    paste("Selected variable:", input$var)
  })
  
  output$selected_var_x <- renderText({
    paste("Selected x variable:", input$var2)
  })
  
  output$selected_var_y <- renderText({
    paste("Selected y variable:", input$var3)
  })
  
  output$selected_var4 <- renderText({
    paste("Selected variable:", input$var4)
  })
  
  #stacked histogram and boxplot
  output$histplot<-renderPlotly({
    p1=my_data %>%
      plot_ly() %>%
      add_histogram(~get(input$var1)) %>%
      layout(xaxis=list(title=input$var1))
    
    #box plot
    p2=my_data %>%
      plot_ly() %>%
      add_boxplot(~get(input$var1)) %>%
      layout(yaxis=list(showticklabels=F))
    
    #Stacking plots
    subplot(p2,p1,nrows=2) %>%
      hide_legend() %>%
      layout(title="Distribution chart -Histogram and Boxplot",
             yaxis=list(title="Frequency"))
    
  })
  
  # Scatter plot
  output$scatter <- renderPlotly({
    p <- my_data %>%
      ggplot(aes(x = get(input$var2), y = get(input$var3))) +
      geom_point() +
      geom_smooth(method = input$fit) +
      labs(title = paste("Relation Between", input$var2, "and", input$var3),
           x = input$var2,
           y = input$var3) +
      theme(plot.title = element_text(size = 10, hjust = 0.5))
    ggplotly(p)
  })
  
  # Correlation plot
  output$cor <- renderPlotly({
    corr <- cor(my_df, use = "pairwise.complete.obs")
    p.mat <- cor_pmat(my_df)
    corr.plot <- ggcorrplot(
      corr,
      hc.order = TRUE,
      lab = TRUE,
      outline.col = "white",
      p.mat = p.mat
    )
    ggplotly(corr.plot)
  })
  
  # Bar chart - State-wise trend
  output$bar <- renderPlotly({
    my_data %>%
      plot_ly() %>%
      add_bars(x = ~States, y = ~get(input$var4)) %>%
      layout(title = paste("Statewise Arrest for", input$var4),
             xaxis = list(title = "States"),
             yaxis = list(title = paste(input$var4, "Arrests per 100,000 residents")))
  })
  
  #rendering the box header
  output$head1<-renderText(
    paste("5 states with high rate of",input$var4,"Arrests")
  )
  
  #rendering the box header
  output$head2<-renderText(
    paste("5 states with low rate of",input$var4,"Arrests")
  )
  
  #rendering tables with 5 states with highest arrest for specific crime type
  output$top5<-renderTable({
    #top 5 states with high rates
    my_data %>%
      select(States,input$var4) %>%
      arrange(desc(get(input$var4))) %>%
      head(5)
  })
  
  #rendering tables with 5 states with low arrest for specific crime type
  output$low5<-renderTable({
    #top 5 rates with low rates
    my_data %>%
      select(States,input$var4) %>%
      arrange(get(input$var4)) %>%
      head(5)
    
  })
  
  
  # Choropleth map
  output$map_plot <- renderPlot({
    # Ensure `new_join` is defined here or loaded before running this part
    ggplot(new_join, aes(x = long, y = lat, fill = get(input$crimetype), group = group)) +
      geom_polygon(color = "black", size = 0.4) +
      scale_fill_gradient(low = "#73A5C6", high = "#001B3A", name = paste(input$crimetype, "Arrest rate")) +
      theme_void() +
      labs(title = paste("Choropleth map of", input$crimetype, "Arrests per 100,000 residents by state in 1973")) +
      theme(
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        legend.position = c(0.2, 0.1),
        legend.direction = "horizontal"
      ) +
      geom_text(aes(x = x, y = y, label = abb), size = 4, color = "white")
  })
}

# Run the app
shinyApp(ui = ui, server = server)

