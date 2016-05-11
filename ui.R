

library(shiny)

# load pre-processed data
#studies <- read.csv('studies_R.csv')
studies <- readRDS('studies_R.rds')

# Define the overall UI
shinyUI(
  
  
 fluidPage(titlePanel("HIV Implementation Science Database"),

# first page: titles of studies
  sidebarLayout(
    
    
    # filter by:
    # Study design
    # Country
    # Year of study
    # Study size
    # Key populations -- check boxes, if unchecked then either-or, if checked then require 'yes'
    # Cascade step(s) targeted -- check boxes (or if want to allow excluded -- radio buttons Targeted, Excluded, All studies)
    # Mediators  (aka Behavioral Target) -- pull-down menu: All, Not reported, Reported (any type), or behavioral_target strings
    # Measure -- check boxes for Implementation Measures
    # Target (imp_target -- implementationt target) - check boxes for capability, opportunity, motivation
    # Outcomes (implementationt outcomes) - check boxes for capability (imp_outcome_c), opportunity (imp_outcome_o), motivation (imp_outcome_m)
    
    # Intervention Type -- pull-down menu
    # Intervention classification search terms?
    # Actor-Action: "Precision with which the actor and action were reported:"
    
    sidebarPanel(
  
      selectInput("study_design", 
                  "Study design:", 
                c("All", unique(as.character(studies$study_design)))),
   
      selectInput("country", 
                  "Country:", 
                c("All", unique(as.character(studies$country)))),
      
      sliderInput("pub_year_range", 
                  "Publication year range:", 
                  min = 2000, max = 2015, c(2003,2014), sep=""),  
      
      sliderInput("min_studyn", 
                  "Minimum study size, logarithmic scale -- log(N):", 
                  min = 0, max = 6, value = 0, step = 0.1, pre="10<sup>", post="</sup>"),  
     
      #If we get full date of pubs, switch to calendar input like flight dates:
      #dateRangeInput("pub_year", 
      #            "Publication Year:", 
      #            start=as.Date("2000-01-01"), end = as.Date("2016-01-01"), max=as.Date("2016-01-01")),  
      
      checkboxGroupInput("key_populations", "Key populations included in study", 
                         c('MSM','PWID','Pregnant women','TB','Infants','Children','Orphans','Street children','Adolescents','CSW','Disabled','Prisoners','Migrants'), 
                         selected = NULL, inline = FALSE),
      
      checkboxGroupInput("cascade_stages", "Stages of care included in study", 
                         c('Testing','Linkage','Staging','Pre-ART retention','Initiation','ART retention','Adherence'), 
                         selected = NULL, inline = FALSE),
      
      selectInput("behav_level", 
                  "Mediators or behavioral targets of intervention effect:", 
                  c("All", unique(as.character(studies$behav_level)), "At least one reported, any type", "None reported")),
          
      
      checkboxGroupInput("implementation_measures", "Implementation measures reported in the study:", 
                         c("Feasibility","Acceptability","Adoption","Fidelity","Cost","Sustainability"), 
                         selected = NULL, inline = FALSE),

      checkboxGroupInput("implementation_targets", "Implementation targets reported in the study:", 
                         c("Capability","Opportunity","Motivation"), 
                        selected = NULL, inline = FALSE),
      
      checkboxGroupInput("implementation_outcomes", "Implementation outcomes reported in the study:", 
                         c("Capability","Opportunity","Motivation","Behavioral outcome"), 
                         selected = NULL, inline = FALSE),
      
      
      # Outcomes (implementationt outcomes) - check boxes for capability (imp_outcome_c), opportunity (imp_outcome_o), motivation (imp_outcome_m)
      
      # Intervention Type -- pull-down menu
      # Intervention classification search terms?
      # Actor-Action: "Precision with which the actor and action were reported:"
      

      
      selectInput("addresses_preg", 
                         "Role of pregnant women:", 
                         c("All studies","Only studies that address pregnant women","Only studies that exclude pregnant women"))
      ),
    
    
      
    # Mediators  (aka Behavioral Target) -- pull-down menu: All, Not reported, Reported (any type), or behavioral_target strings
    
    # Create a new row for the table.
   mainPanel(
      tabsetPanel(
        tabPanel("List of Included Studies", dataTableOutput(outputId="studies_table")),
        tabPanel("Effects Forest Plot",  plotOutput(outputId="forest",height=2500)),
        tabPanel("Map of Studies", 
                 fluidRow(
                   column(12, plotOutput(outputId="worldmap")),
                   column(12, plotOutput(outputId="africaMap"))
                   )),
        tabPanel("Studies per Year", plotOutput(outputId="studies_per_year",height = 750)),
        tabPanel("Word Clouds",      plotOutput(outputId="wordclouds")),
        
#                  tags$div(class="header", checked=NA,
#                                        tags$p("This tab will generate word clouds from the free text fields of the data entry form. 
#                                         Copyright permitting, we could use the full texts of the articles to create word 
#                                         clouds and search forms. Abstracts available via PubMed can be searched online, 
#                                         as in this example:"),
#                                               tags$a(href="http://www.vesnam.com/Rblog/pubmedwordcloud/", 
#                                                      "http://www.vesnam.com/Rblog/pubmedwordcloud/")))
        
        
        tabPanel("Clustering",  tags$div(class="header", checked=NA,
                                         tags$p("This tab will contain a clustering visualization tool such as: "),
                                         tags$a(href="http://biit.cs.ut.ee/clustvis/", "ClustVis: http://biit.cs.ut.ee/clustvis/")))



        )
   )
  )
 )
)  