
  if (input$country != "All"){
    data <- data[!is.na(data$country) & data$country == input$country,]
  }
  if (input$study_design != "All"){
    data <- data[data$study_design == input$study_design,]
  }
  if (input$pub_year_range[1]!=2000 | input$pub_year_range[2]!=2000 ){
    data <- data[!is.na(data$pub_year) & data$pub_year >= input$pub_year_range[1] & data$pub_year<= input$pub_year_range[2],] 
  }
  if (input$min_studyn > 0){
    data <- data[!is.na(data$studyn) & data$studyn >= 10^input$min_studyn,]
  }
  if (input$addresses_preg == "Only studies that address pregnant women"){
    data <- data[!is.na(data$kp_pregwomen) & data$kp_pregwomen == 1,]
  }
  if (input$addresses_preg == "Only studies that exclude pregnant women"){
    data <- data[!is.na(data$kp_pregwomen) & data$kp_pregwomen == 0,]
  }
  
  #### Key populations
  
  if (is.element('MSM',input$key_populations))
  {data <- data[!is.na(data$kp_msm) & data$kp_msm == 1,]}
  
  if (is.element('PWID',input$key_populations))
  {data <- data[!is.na(data$kp_pwid) & data$kp_pwid == 1,]}
  
  if (is.element('Pregnant women',input$key_populations))
  {data <- data[!is.na(data$kp_pregwomen) & data$kp_pregwomen == 1,]}
  
  if (is.element('TB',input$key_populations))
  {data <- data[!is.na(data$kp_tb) & data$kp_tb == 1,]}
  
  if (is.element('Infants',input$key_populations))
  {data <- data[!is.na(data$kp_infants) & data$kp_infants == 1,]}
  
  if (is.element('Children',input$key_populations))
  {data <- data[!is.na(data$kp_children) & data$kp_children == 1,]}
  
  if (is.element('Orphans',input$key_populations))
  {data <- data[!is.na(data$kp_orphans) & data$kp_orphans == 1,]}
  
  if (is.element('Street children',input$key_populations))
  {data <- data[!is.na(data$kp_street) & data$kp_street == 1,]}
  
  if (is.element('Adolescents',input$key_populations))
  {data <- data[!is.na(data$kp_adolescents) & data$kp_adolescents == 1,]}
  
  if (is.element('CSW',input$key_populations))
  {data <- data[!is.na(data$kp_csw) & data$kp_csw == 1,]}
  
  if (is.element('Disabled',input$key_populations))
  {data <- data[!is.na(data$kp_disabled) & data$kp_disabled == 1,]}
  
  if (is.element('Prisoners',input$key_populations))
  {data <- data[!is.na(data$kp_prisoners) & data$kp_prisoners == 1,]}
  
  if (is.element('Migrants',input$key_populations))
  {data <- data[!is.na(data$kp_migrants) & data$kp_migrants == 1,]}
  
  #### care cascade stages
  
  if (is.element('Testing',input$cascade_stages))
  {data <- data[!is.na(data$cas_testing) & data$cas_testing == 1,]}    
  
  if (is.element('Linkage',input$cascade_stages))
  {data <- data[!is.na(data$cas_linkage) & data$cas_linkage == 1,]}
  
  if (is.element('Staging',input$cascade_stages))
  {data <- data[!is.na(data$cas_staging) & data$cas_staging == 1,]}
  
  if (is.element('Pre-ART retention',input$cascade_stages))
  {data <- data[!is.na(data$cas_retonelig) & data$cas_retonelig == 1,]}
  
  if (is.element('Initiation',input$cascade_stages))
  {data <- data[!is.na(data$cas_initiation) & data$cas_initiation == 1,]}
  
  if (is.element('ART retention',input$cascade_stages))
  {data <- data[!is.na(data$cas_retonart) & data$cas_retonart == 1,]}
  
  if (is.element('Adherence',input$cascade_stages))
  {data <- data[!is.na(data$cas_adherence) & data$cas_adherence == 1,]}
  
  if (input$behav_level != "All"){
    
    if(input$behav_level == "At least one reported, any type"){
      data <- data[!is.na(data$behav_level)]}
    
    if(input$behav_level == "None reported"){
      data <- data[is.na(data$behav_level)]}
    
    if(is.element(input$behav_level, unique(na.omit(data$behav_level)))){
      data <- data[!is.na(data$behav_level) & data$behav_level == input$behav_level,]}
  }
  
  
  if (is.element('Capability',input$implementation_targets)){
    data <- data[data$imp_target_c == 1]}
  if (is.element('Opportunity',input$implementation_targets)){
    data <- data[data$imp_target_o == 1]}
  if (is.element('Motivation',input$implementation_targets)){
    data <- data[data$imp_target_m == 1]}    
  
  # note on Implementation Targets:
  # There was one case where "Implementation target specified" was answered as "No" (imp_targets = 0) but 
  # COM component present was "yes" for Opportunity. This strategy ignores "Implementation target specified". 
  
  if (is.element('Capability',input$implementation_outcomes)){
    data <- data[data$imp_outcome_c == 1]}
  if (is.element('Opportunity',input$implementation_outcomes)){
    data <- data[data$imp_outcome_o == 1]}
  if (is.element('Motivation',input$implementation_outcomes)){
    data <- data[data$imp_outcome_m == 1]} 
  if (is.element('Behavioral outcome',input$implementation_outcomes)){
    data <- data[data$imp_outcome_b == 1]}   
  