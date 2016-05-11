
# load pre-processed data
#data <- read.csv('studies_R.csv')
data <- readRDS('studies_R.rds')

# output:

# table of studies that match the search criteria: author, year, title

# series of bar charts showing % of studies that address key populations:
# MSM, PWID, Pregnant women, TB, Infants, Children, Orphans, Street children, Adolescents, CSW, Disabled, Prisoners, Migrants

# scatter plot of study subjects (N) vs year, colored by cascade step

# map of number of studies by country

# word clouds:
# title
# setting
# inclusion/excludion criteria
# intervention
# comparator
# comments


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




library(shiny)
library(rworldmap)
library(countrycode)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(rmeta)

world_country_names <- countrycode::countrycode_data$country.name
world_country_regex <- countrycode::countrycode_data$regex
world_country_codes <- countrycode::countrycode_data$iso3c
# the country regex for Korea and Vietnam are missing, add them:
world_country_regex[world_country_names == "Korea"] <- "korea"
world_country_regex[world_country_names == "Republic of Vietnam"] <- "vietnam"





# Define a server for the Shiny app
shinyServer(function(input, output) {
  
  
  output$wordclouds <- renderPlot({
    
    source('filterData.R', local=TRUE)
    
    if (nrow(data) != 0) {
    
      cloud_palette <- brewer.pal(8,"Dark2")
      
      par(mfrow=c(2,3))
      
      # Setting
      wordcloud(paste(data$setting, collapse=" "), rot.per=.15, colors=cloud_palette)
      
      # Study Objective
      wordcloud(paste(data$objective, collapse=" "), colors=cloud_palette)
      
      # Inclusion/Exclusion Criteria
      wordcloud(paste(data$iecriteria, collapse=" "), colors=cloud_palette)
      
      # Intervention
      wordcloud(paste(data$intervention, collapse=" "), colors=cloud_palette)
      # Alternatively, for just the name of the intervention, not free text field:
      # wordcloud(paste(data$interv_name_text, collapse=" "))
      
      # Stages of Care Cascade Targeted -- sparse
      # wordcloud(paste(data$cas_comments, collapse=" "))
      
      # Implementation Actor
      wordcloud(paste(data$imp_action_desc, collapse=" "), colors=cloud_palette)
      
      # Implementation Measures - a bit sparse
      #wordcloud(paste(data$imp_meas_comment, collapse=" "))
      # Implementation Target - a bit sparse
      #wordcloud(paste(data$imp_target_comment, collapse=" "))
      # Implementation Outcome - a bit sparse
      #wordcloud(paste(data$imp_outcome_comment, collapse=" "))
      
      # Implementation Measures, Targets, and Outcomes
      wordcloud(paste(c(as.character(data$imp_target_comment),as.character(data$imp_meas_comment),
                        as.character(data$imp_outcome_comment)), collapse=" "), colors=cloud_palette)
      
      # Behavioral target -- sparse and full of long words -- not using
      # wordcloud(paste(data$behav_target_desc, collapse=" "))
    
    }
    
    
    
  })
  

  
  output$worldmap <- renderPlot({
    
    
    source('filterData.R', local=TRUE)
    
    # Make sure we have data present before drawing the map
    if (nrow(data) != 0) {
      count_country  <- function(country_name){length(grep(country_name,data$country, perl=TRUE, ignore.case = TRUE))}
      
      country_count <- sapply(world_country_regex, count_country)
      
      # add Jamaica that was misspelled Jamacia
      country_count[world_country_names == "Jamaica"] <- count_country("Jamacia") + country_count[world_country_names == "Jamaica"]
      
      # inspect country counts
      # rbind(world_country_names,unname(country_count))
         
      country_data = data.frame(countrycode=world_country_codes, value=country_count)
      n = joinCountryData2Map(country_data, nameJoinColumn = "countrycode")
      mapCountryData(n, nameColumnToPlot = "value", addLegend=TRUE, missingCountryCol = "grey", 
                     mapTitle = 'Number of Studies per Country',   
                     catMethod="fixedWidth", colourPalette = brewer.pal(7,'YlGnBu'))
    }
    
  })
  
  
  
  output$africaMap <- renderPlot({
    
    source('filterData.R', local=TRUE)
    
    # Make sure we have data present before drawing the map
    if (nrow(data) != 0) {
      
      count_country  <- function(country_name){length(grep(country_name,data$country, perl=TRUE, ignore.case = TRUE))}
      country_count <- sapply(world_country_regex, count_country)
      
      # add Jamaica that was misspelled Jamacia
      country_count[world_country_names == "Jamaica"] <- count_country("Jamacia") + country_count[world_country_names == "Jamaica"]
      
      # inspect country counts
      # rbind(world_country_names,unname(country_count))
      
      country_data = data.frame(countrycode=world_country_codes, value=country_count)
      n = joinCountryData2Map(country_data, nameJoinColumn = "countrycode")
      mapCountryData(n, nameColumnToPlot = "value", addLegend=TRUE, missingCountryCol = "grey", 
                     mapTitle = "Number of Studies per Country, Africa Region",   
                     mapRegion = 'Africa',
                     catMethod="fixedWidth", colourPalette = brewer.pal(7,'YlGnBu'))
    }
    
  })
  
  
    #####
    output$forest <- renderPlot({
        
        # filter studies based on user inputs
        source('filterData.R', local=TRUE) 

        par(mfrow = c(4,2),cex.axis=1, cex.main=1.5,cex=1)
        # Testing
        fe<-meta.MH (cas_testing_ttotal, cas_testing_ctotal, cas_testing_tcases, cas_testing_ccases, data=data, names=author_year, na.action = na.omit)
        #re<-meta.DSL (cas_testing_ttotal, cas_testing_ctotal, cas_testing_tcases, cas_testing_ccases, data=data, names=author_year, na.action = na.omit)
        # Fail gracefully if fe doesnt contain anything
        if (length(fe$names) != 0 && fe$logOR != Inf && fe$logOR != -Inf){
          metaplot(fe$logOR, fe$selogOR, nn=fe$selogOR^-2,fe$names,
              summn=fe$logMH,sumse=fe$selogMH, sumnn=fe$selogMH^-2, logeffect=TRUE,
              colors=meta.colors(box = "royalblue", lines="darkblue", summary="orange"),ylab="")
          title(main='Testing')
        }
    
        
        # Linkage
        fe<-meta.MH (cas_linkage_ttotal, cas_linkage_ctotal, cas_linkage_tcases, cas_linkage_ccases, data=data, names=author_year, na.action = na.omit)
        if (length(fe$names) != 0 && fe$logOR != Inf && fe$logOR != -Inf){
          metaplot(fe$logOR, fe$selogOR, nn=fe$selogOR^-2,fe$names,
              summn=fe$logMH,sumse=fe$selogMH, sumnn=fe$selogMH^-2, logeffect=TRUE,
              colors=meta.colors(box = "royalblue", lines="darkblue", summary="orange"),ylab="")
          title(main='Linkage')
        }


        # Retention
        fe<-meta.MH (cas_retne_ttotal, cas_retne_ctotal, cas_retne_tcases, cas_retne_ccases, data=data, names=author_year, na.action = na.omit)
        if (length(fe$names) != 0 && fe$logOR != Inf && fe$logOR != -Inf){
          metaplot(fe$logOR, fe$selogOR, nn=fe$selogOR^-2,fe$names,
              summn=fe$logMH,sumse=fe$selogMH, sumnn=fe$selogMH^-2, logeffect=TRUE,
              colors=meta.colors(box = "royalblue", lines="darkblue", summary="orange"),ylab="")
          title(main='Retention')
        }
        

        # Staging
        fe<-meta.MH (cas_staging_ttotal, cas_staging_ctotal, cas_staging_tcases, cas_staging_ccases, data=data, names=author_year, na.action = na.omit)
        if (length(fe$names) != 0 && fe$logOR != Inf && fe$logOR != -Inf){
          metaplot(fe$logOR, fe$selogOR, nn=fe$selogOR^-2,fe$names,
              summn=fe$logMH,sumse=fe$selogMH, sumnn=fe$selogMH^-2, logeffect=TRUE,
              colors=meta.colors(box = "royalblue", lines="darkblue", summary="orange"),ylab="")
          title(main='Staging')
        }

        
        # Initiation
        fe<-meta.MH (cas_ini_ttotal, cas_ini_ctotal, cas_ini_tcases, cas_ini_ccases, data=data, names=author_year, na.action = na.omit)
        if (length(fe$names) != 0 && fe$logOR != Inf && fe$logOR != -Inf){
          metaplot(fe$logOR, fe$selogOR, nn=fe$selogOR^-2,fe$names,
              summn=fe$logMH,sumse=fe$selogMH, sumnn=fe$selogMH^-2, logeffect=TRUE,
              colors=meta.colors(box = "royalblue", lines="darkblue", summary="orange"),ylab="")
          title(main='Initiation')
        }
        

        # Retention on ART
        fe<-meta.MH (cas_retonart_ttotal, cas_retonart_ctotal, cas_retonart_tcases, cas_retonart_ccases, data=data, names=author_year, na.action = na.omit)
        if (length(fe$names) != 0 && fe$logOR != Inf && fe$logOR != -Inf){
          metaplot(fe$logOR, fe$selogOR, nn=fe$selogOR^-2,fe$names,
              summn=fe$logMH,sumse=fe$selogMH, sumnn=fe$selogMH^-2, logeffect=TRUE,
              colors=meta.colors(box = "royalblue", lines="darkblue", summary="orange"),ylab="")
          title(main='Retention on ART')
        }

        
        # Adherence
        fe<-meta.MH (cas_adhere_ttotal, cas_adhere_ctotal, cas_adhere_tcases, cas_adhere_ccases, data=data, names=author_year, na.action = na.omit)
        if (length(fe$names) != 0 && fe$logOR != Inf && fe$logOR != -Inf){
          metaplot(fe$logOR, fe$selogOR, nn=fe$selogOR^-2,fe$names,
              summn=fe$logMH,sumse=fe$selogMH, sumnn=fe$selogMH^-2, logeffect=TRUE,
              colors=meta.colors(box = "royalblue", lines="darkblue", summary="orange"),ylab="")
          title(main='Adherence')
        }

        
        # Suppression
        fe<-meta.MH (cas_sup_ttotal, cas_sup_ctotal, cas_sup_tcases, cas_sup_ccases, data=data, names=author_year, na.action = na.omit)
        if (length(fe$names) != 0 && fe$logOR != Inf && fe$logOR != -Inf){
          metaplot(fe$logOR, fe$selogOR, nn=fe$selogOR^-2,fe$names,
              summn=fe$logMH,sumse=fe$selogMH, sumnn=fe$selogMH^-2, logeffect=TRUE,
              colors=meta.colors(box = "royalblue", lines="darkblue", summary="orange"),ylab="")
          title(main='Suppression')
        }
    })
    #####
    
  
  # Table of study authors, years, and titles with filtering and search   
    output$studies_table <- renderDataTable({
        source('filterData.R', local=TRUE) 
        data <- data[,c('author','pub_year','title')]
    })
    
    output$studies_per_year <- renderPlot({
    source('filterData.R', local=TRUE) 
    if (nrow(data) != 0) {
      hist(data$pub_year, plot=T, main = "Number of Studies per Year", xlab = "Publication Year", ylab = "Number of Studies", col = 'grey', border = 'white')  
    }
  })
  
  
  
})

