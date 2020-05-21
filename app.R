#data and packages
library(ggplot2)
library(dplyr)
library(knitr)
library(dplyr)
library(kableExtra)
library(scales)
library(shinythemes)

dist_data <- read.csv("Data/dist_data.csv")

PP_By_Age <- read.csv("Data/PP_By_Age.csv")
PP_By_Crimes <- read.csv("Data/PP_By_Crimes.csv")
PP_By_Evictions <- read.csv("Data/PP_By_Evictions.csv")
PP_By_Income <- read.csv("Data/PP_By_Income.csv")
PP_By_Income_Change <- read.csv("Data/PP_By_Income_Change.csv")
PP_By_Rent <- read.csv("Data/PP_By_Rent.csv")
PP_By_Permits <- read.csv("Data/PP_By_Permits.csv")

crimes_summary <- read.csv("Data/crimes_summary.csv")
permits_summary <- read.csv("Data/permits_summary.csv")
evict_summary <- read.csv("Data/evict_summary.csv")
rent_summary <- read.csv("Data/rent_summary.csv")

Table8 <- read.csv("Data/Table8.csv")


Table8[9,3] <- " "
Table8[9,4] <- " "
Table8[9,6] <- " "

Table8[17,3] <- " "
Table8[17,4] <- " "
Table8[17,6] <- " "

Table8[21,3] <- " "
Table8[21,4] <- " "
Table8[21,6] <- " "

Table8[24,3] <- " "
Table8[24,4] <- " "
Table8[24,6] <- " "

Table8[28,3] <- " "
Table8[28,4] <- " "
Table8[28,6] <- " "

ui <- navbarPage("Development and Displacement", theme = shinytheme("journal"), 
                 
                 navbarMenu("EDA", 
                            tabPanel("Distributions", fluidPage(
                              titlePanel("Exploratory Data Analysis"),
                              sidebarLayout(
                                sidebarPanel(p("These figures present a variety of distributions from continuous predictor variables used in a Logistic Binomial regression model"),
                                             
                                             p("Data was drawn from a novel data set representing roughly 67,000 obersvations of around 25,000 low income residents of Boston. The data was collected between 2005 and 2019."),
                                             
                                             selectInput("hist_variable", h5("Select Variable"),
                                                         choices = list("Income", "Change in Income", "Age", "Rent", "Evictions",
                                                                        "Development Permits Issued", "Crimes Reported"), selected = "Income"),
                                             
                                             sliderInput("binwidth", h5("Number of Bins"), min = 20, max = 200, value = 100),
                                             
                                             "A full discussion of the results methods and can be found here",
                                             a("Development and Displacement", href = "https://froebbm.github.io/Thesis/index.html")),
                                
                                mainPanel(plotOutput("histogram", width = "750px", height = "560px")),
                                ))
                              ),
                            tabPanel("Trends", fluidPage(
                              titlePanel("Exploratory Data Analysis"),
                              sidebarLayout(
                                sidebarPanel(p("These figures present a variety of trends over time of predictor variables used in a Logistic Biomial regression model"),
                                             
                                             p("Each of these trends was found to be significant at the 0.05 level when subjected to linear trend estimation"),
                                             
                                             selectInput("trend_variable", h5("Select Variable"),
                                                         choices = list("Rent", "Evictions",
                                                                        "Development Permits Issued", "Crimes Reported")),
                                             
                                             "A full discussion of the results methods and can be found here",
                                             a("Development and Displacement", href = "https://froebbm.github.io/Thesis/index.html")),
                                
                                mainPanel(plotOutput("trend_lines", width = "750px", height = "560px"))
                              )
                            ))
                            ),
            
                 
                 tabPanel("Results", fluidPage(
                   
                   titlePanel("Predicted Probability"),
                   
                   sidebarLayout(
                     
                     sidebarPanel(
                       
                       p("These figures present a variety of results from a Logistic Binomial regression using a generalized linear model (GLM) with maximum likelihood settings and the outcome variable 'moved.'"),
                       
                       p("View variations of predicted probability of moving by various groups across a variety of continuous predictor variables."),
                       
                       selectInput("group", h5("Select Grouping Variable"),
                                   choices = list("None", "Relationship Status", "Changes to Dependents",
                                                  "Race", "Graduated High School", "Disability Status", "Gender")),
                       
                       helpText("Significance Values can be found on the Model Results Page"),
                       
                       helpText("Shaded Area represents + and - one standard error of the mean"),
                       
                       "A full discussion of the results methods and can be found here",
                       a("Development and Displacement", href = "https://froebbm.github.io/Thesis/index.html")
                       
                     ),
                     
                     
                     mainPanel(
                       tabsetPanel(
                         tabPanel("Age", plotOutput("Age", width = "750px", height = "560px")),
                         tabPanel("Income", plotOutput("Income", width = "750px", height = "560px")),
                         tabPanel("Income Change", plotOutput("Income_Change", width = "750px", height = "560px")),
                         tabPanel("Permits", plotOutput("Permits", width = "750px", height = "560px")),
                         tabPanel("Crimes", plotOutput("Crimes", width = "750px", height = "560px")),
                         tabPanel("Rent", plotOutput("Rent", width = "750px", height = "560px")),
                         tabPanel("Evictions", plotOutput("Evictions", width = "750px", height = "560px"))
                         
                         
                       )
                     )
                   )
                   
                 )),
                 
                 tabPanel("Model Results", 
                          fluidPage(
                            titlePanel("Model Results"),
                            sidebarLayout(
                              sidebarPanel(
                                p("Results of a Logistic regression using a generalized linear model (GLM) with maximum likelihood settings and the outcome variable 'moved.'"),
                                br(),
                                "A full discussion of the methods and can be found here",
                                a("Development and Displacement", href = "https://froebbm.github.io/Thesis/methods.html"),
                                br(),
                                br(),
                                p("p < .10. *. p < .05 **. p < .01. ***. p < .001 ****."), 
                                br(),
                                p("N = 7,447. OR = odds ratio; SE = standard error; 
                                  VIF = Variance Inflation Factor. ")
                                
                                           ),
                              
                              mainPanel(tableOutput("model_table"))
                                          )
                                    )
                          )
                 )


server <- function(input, output) {

  
  output$histogram <- renderPlot({
    
    xaxis <- switch(input$hist_variable,
                    "Income" = dist_data$Des_Deflated_AGI,
                    "Change in Income" = dist_data$Change_AGI, 
                    "Development Permits Issued" = dist_data$total_permits,
                    "Crimes Reported" = dist_data$total_crimes,
                    "Age" = dist_data$Des_Age,
                    "Rent" = dist_data$Org_Est_Rent_Adj,
                    "Evictions" = dist_data$evictions)
    
    my_title <- switch(input$hist_variable,
                       "Income" = "Distribution of Income",
                       "Change in Income" = "Distribution of Income Change", 
                       "Development Permits Issued" = "Distribution of Total Permits Issued",
                       "Crimes Reported" = "Distribution of Total Crimes Reported",
                       "Age" = "Distribution of Age",
                       "Rent" = "Distribution of Estimate Rent Per Bedroom",
                       "Evictions" = "Evictions Per Census Tract")
    
    xlab <- switch(input$hist_variable,
                   "Income" = "Income in 2019 Dollars",
                   "Change in Income" = "Income Change in 2019 Dollars", 
                   "Development Permits Issued" = "Permits Issued within 0.5km",
                   "Crimes Reported" = "Crimes Reported within 0.1km",
                   "Age" = "Age",
                   "Rent" = "Estimated Rent Per Bedroom in 2019 Dollars",
                   "Evictions" = "Evictions Per Census Tract")
    
    xlimts <- switch(input$hist_variable,
                   "Income" = c(0, 150000),
                   "Change in Income" = c(-80000, 80000), 
                   "Development Permits Issued" = c(1, 1500),
                   "Crimes Reported" =  c(1, 800),
                   "Age" = c(0,80),
                   "Rent" = c(0,8000),
                   "Evictions" = c(0,80))
    
    ggplot(dist_data) +
      geom_histogram(aes(x = xaxis), fill = "purple", alpha = 0.75, bins = input$binwidth) +
      labs(title = my_title, x = xlab, y = "Count of Residents") +
      scale_x_continuous(labels = comma) +
      xlim(xlimts)
      
    
  })
  
  output$trend_lines <- renderPlot({
    
    if (input$trend_variable == "Development Permits Issued"){
      
      ggplot(permits_summary) +
        geom_area(aes(x = vars, y = n), color = "purple", fill = "purple", alpha = 0.75) +
        labs(title = "Development Permits Issued Per Year, 2007 to March 2020", x = "Year", y = "Number of Permits Issued") 
      
    } else if (input$trend_variable == "Crimes Reported"){
      
      ggplot(crimes_summary) +
        geom_area(aes(x = vars, y = n), color = "purple", fill = "purple", alpha = 0.75) +
        labs(title = "Crimes Reported Per Year, 2012 to to March 2020", x = "Year", y = "Crimes Reported Per Year") 
      
    } else if (input$trend_variable == "Rent"){
      
      ggplot(rent_summary, aes(x = Year, y = mean_rent)) + 
        geom_ribbon(aes(ymin = LL_rent, ymax = UL_rent), fill = "purple", alpha = 0.5) + 
        geom_line(aes(x = Year),color = "black", size = 1, alpha = 1) +
        labs(title = "Mean Rent per Bedroom, 2011 to 2019", 
             x = "Year", y = "Estimated Rent Per Bedroom in 2019 Dollars") +
        scale_x_continuous(breaks = c(2012, 2015, 2018))
      
    } else if (input$trend_variable == "Evictions"){
      
      ggplot(evict_summary) +
        geom_area(aes(x = year, y = eviction.filings), color = "purple", fill = "purple", alpha = 0.75) +
        labs(title = "Evictions Per Year in Massachusetts, 2001 to 2016", x = "Year", y = "Number of Evictions") 
      
    }
                     
  })
  
  output$Age <- renderPlot({

    grouping <- switch(input$group,
                       "None" = "None", 
                       "Relationship Status" = "Relationship_Status", 
                       "Changes to Dependents" = "Dependent_Status",
                       "Race" = "Race", 
                       "Graduated High School" = "High_School_Grad", 
                       "Disability Status" = "Disability_Status",
                       "Gender" = "Gender")
    
    inputData <- subset(PP_By_Age, PP_By_Age$Grouping == grouping)
    
    require(scales)
    ggplot(inputData, aes(x = Des_Age, y = PredictedProb)) + 
      geom_ribbon(aes(ymin = LL, ymax = UL, fill = Grouping_Var), alpha = 0.5) + 
      geom_line(aes(x = Des_Age, color = Grouping_Var),size = 1, alpha = 1) +
      labs(title = "Predicted Probability of Moving by Age", 
           x = "Age", y = "Predicted Probability %") 
    
    })
  
  output$Income <- renderPlot({
    
    grouping <- switch(input$group,
                       "None" = "None", 
                       "Relationship Status" = "Relationship_Status", 
                       "Changes to Dependents" = "Dependent_Status",
                       "Race" = "Race", 
                       "Graduated High School" = "High_School_Grad", 
                       "Disability Status" = "Disability_Status",
                       "Gender" = "Gender")
    
    inputData <- subset(PP_By_Income, PP_By_Income$Grouping == grouping)
    
    ggplot(inputData, aes(x = Income_Group, y = PredictedProb)) + 
      geom_ribbon(aes(ymin = LL, ymax = UL, fill = Grouping_Var), alpha = 0.5) + 
      geom_line(aes(x = Income_Group, color = Grouping_Var),size = 1, alpha = 1) +
      labs(title = "Predicted Probability of Moving by Income", 
           x = "Income in 2019 Dollars", y = "Predicted Probability %")
    
  })
  
  output$Income_Change <- renderPlot({
    
    grouping <- switch(input$group,
                       "None" = "None", 
                       "Relationship Status" = "Relationship_Status", 
                       "Changes to Dependents" = "Dependent_Status",
                       "Race" = "Race", 
                       "Graduated High School" = "High_School_Grad", 
                       "Disability Status" = "Disability_Status",
                       "Gender" = "Gender")
    
    inputData <- subset(PP_By_Income_Change, PP_By_Income_Change$Grouping == grouping)
    
    ggplot(inputData, aes(x = Income_Change_Group, y = PredictedProb)) + 
      geom_ribbon(aes(ymin = LL, ymax = UL, fill = Grouping_Var), alpha = 0.5) + 
      geom_line(aes(x = Income_Change_Group, color = Grouping_Var),size = 1, alpha = 1) +
      labs(title = "Predicted Probability of Moving by Change in Income", 
           x = "Change in Income in 2019 Dollars", y = "Predicted Probability %")
    
  })
  
  output$Permits <- renderPlot({
    
    grouping <- switch(input$group,
                       "None" = "None", 
                       "Relationship Status" = "Relationship_Status", 
                       "Changes to Dependents" = "Dependent_Status",
                       "Race" = "Race", 
                       "Graduated High School" = "High_School_Grad", 
                       "Disability Status" = "Disability_Status",
                       "Gender" = "Gender")
    
    inputData <- subset(PP_By_Permits, PP_By_Permits$Grouping == grouping)
    
    ggplot(inputData, aes(x = Total_Permits_Group, y = PredictedProb)) + 
      geom_ribbon(aes(ymin = LL, ymax = UL, fill = Grouping_Var), alpha = 0.5) + 
      geom_line(aes(x = Total_Permits_Group, color = Grouping_Var),size = 1, alpha = 1) +
      labs(title = "Predicted Probability of Moving by Development Permits Issued", 
           x = "Development Permits Issued within 0.5km", y = "Predicted Probability %")
    
  })
  
  output$Crimes <- renderPlot({
    
    grouping <- switch(input$group,
                       "None" = "None", 
                       "Relationship Status" = "Relationship_Status", 
                       "Changes to Dependents" = "Dependent_Status",
                       "Race" = "Race", 
                       "Graduated High School" = "High_School_Grad", 
                       "Disability Status" = "Disability_Status",
                       "Gender" = "Gender")
    
    inputData <- subset(PP_By_Crimes, PP_By_Crimes$Grouping == grouping)
    
    ggplot(inputData, aes(x = Total_Crimes_Group, y = PredictedProb)) + 
      geom_ribbon(aes(ymin = LL, ymax = UL, fill = Grouping_Var), alpha = 0.5) + 
      geom_line(aes(x = Total_Crimes_Group, color = Grouping_Var),size = 1, alpha = 1) +
      labs(title = "Predicted Probability of Moving by Crimes Reported", 
           x = "Development Crimes Reported within 0.1km", y = "Predicted Probability %")
    
  })
  
  output$Rent <- renderPlot({
    
    grouping <- switch(input$group,
                       "None" = "None", 
                       "Relationship Status" = "Relationship_Status", 
                       "Changes to Dependents" = "Dependent_Status",
                       "Race" = "Race", 
                       "Graduated High School" = "High_School_Grad", 
                       "Disability Status" = "Disability_Status",
                       "Gender" = "Gender")
    
    inputData <- subset(PP_By_Rent, PP_By_Rent$Grouping == grouping)
    
    ggplot(inputData, aes(x = Rent_Groups, y = PredictedProb)) + 
      geom_ribbon(aes(ymin = LL, ymax = UL, fill = Grouping_Var), alpha = 0.5) + 
      geom_line(aes(x = Rent_Groups, color = Grouping_Var),size = 1, alpha = 1) +
      labs(title = "Predicted Probability of Moving by Estimated Rent", 
           x = "Estimated Rent in 2019 Dollars", y = "Predicted Probability %")
    
  })
  
  output$Evictions <- renderPlot({
    
    grouping <- switch(input$group,
                       "None" = "None", 
                       "Relationship Status" = "Relationship_Status", 
                       "Changes to Dependents" = "Dependent_Status",
                       "Race" = "Race", 
                       "Graduated High School" = "High_School_Grad", 
                       "Disability Status" = "Disability_Status",
                       "Gender" = "Gender")
    
    inputData <- subset(PP_By_Evictions, PP_By_Evictions$Grouping == grouping)
    
    ggplot(inputData, aes(x = Evictions_Group, y = PredictedProb)) + 
      geom_ribbon(aes(ymin = LL, ymax = UL, fill = Grouping_Var), alpha = 0.5) + 
      geom_line(aes(x = Evictions_Group, color = Grouping_Var),size = 1, alpha = 1) +
      labs(title = "Predicted Probability of Moving by Evictions", 
           x = "Evictions per Census Tract", y = "Predicted Probability %")
    
  })
  
  output$model_table <- function(){
    
    kable(Table8[1:4], caption = "Logistic Regression",
          col.names = c("Variable", "OR",	"SE",	"VIF")) %>%
      kable_styling("striped", full_width = F) %>%
      add_indent(c(10:15, 18:20, 22:23, 25:27, 29:30)) %>%
      add_header_above(c(" ", "General Linear Model" = 3))

  }
    
}

# Run app ----
shinyApp(ui, server)
