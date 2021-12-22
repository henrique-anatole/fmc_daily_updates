check_fishing_licences <- function(date_range) {
  
  #We filter on season based on the licence start and end date. For this forecasting, the licence needs to be valid at the beggining of the season.
  licenced_vessels <- load_fishing_licences() %>% 
    dplyr::filter(LIC_To_Date >= date_range[1], LIC_From_Date <= date_range[2]) %>%
    distinct()
  
  return(licenced_vessels)
  
}
 
check_movements <- function(date_range) {
  
  presence_interval <- lubridate::interval(date_range[1], date_range[2])
    
  vsl_movements <- load_movements() %>%
    dplyr::filter(int_overlaps(.$Presence_Interval, presence_interval))
  
  return(vsl_movements)
  
}

check_vms_data <- function(date_range) {
  
  # Import vms raw data from choosed period extended in 4 hours, to avoid minnor errors
  posit_raw <- load_vms_positions(start_date = date_range[1] - hours(4), end_date = date_range[2] + hours(4)) %>% 
    relocate(LATITUDE,LONGITUDE) #relocate is necessary to create points later
  
  # transform_decimals_to_projections
  projected_posits <- posit_raw %>%
    create_Points()
  
  # Import basic polygons to work with
  areas <<- load_ASDs()

  message(paste("vms areas loaded line 36"))
    
  vsl_positions_areas <- assign_areas(Input=projected_posits, Polys='areas', NamesOut="GAR_Long_Label") %>% 
    dplyr::left_join(dplyr::select(areas@data, GAR_ID, GAR_Name, GAR_Short_Label, GAR_Long_Label)) %>% 
    dplyr::filter(!is.na(TP_ID))
  
  #Last position of each vessel    
  vsls_last_posit <- vsl_positions_areas %>% 
    group_by(VSL_ID, VSL_Name) %>% 
    dplyr::summarise(LOC_DATE = max(LOC_DATE))  %>% 
    left_join(vsl_positions_areas) %>% 
    distinct() %>% 
    dplyr::arrange(VSL_Name)
  
  #The final value choosed was the date-time of the last position imported. The use of "current system date-time" may not be right if I'm filtering for a past period.
  vsls_last_posit$time_since_h <- round(difftime(vsls_last_posit$LOC_DATE, max(vsls_last_posit$LOC_DATE), units = "hour"), digits = 2) %>% abs()
  
  #All vessels in ccamlr area
  vessels_passing_ca <- vsl_positions_areas %>% 
    dplyr::filter(!is.na(GAR_Long_Label)) %>% 
    group_by(VSL_ID, VSL_Name, GAR_ID, GAR_Long_Label) %>% 
    dplyr::summarise(`Earlier posit` = min(LOC_DATE), `Later posit` = max(LOC_DATE))  %>% 
    dplyr::distinct() %>% 
    dplyr::arrange(VSL_Name) %>% 
    dplyr::left_join(dplyr::select(vsls_last_posit, VSL_ID, "Last_known_posit" = GAR_Long_Label, time_since_h), by="VSL_ID")
  
  #What about the frequency of reports been received?

    temp_all_posit <- NULL
    
    for (v in unique(vsl_positions_areas$VSL_ID)) {   # Should it be done only for vessels that passed through the CA area?
      all_posit <- vsl_positions_areas %>% 
        dplyr::filter(VSL_ID == v) %>% 
        arrange(LOC_DATE)
      
      #To avoid error when the vessel have only one position till that moment
      if (dim(all_posit)[1]==1) {
        
        all_posit$time_diff_h <- NA
        all_posit$distance_nm <- NA
        all_posit$speed_kn <- NA
        all_posit$heading_deg <- NA
        all_posit$changeheading_deg <- NA
        
      } else {
        #calculate time between positions
        all_posit$time_diff_h[2:nrow(all_posit)] <- difftime(all_posit$LOC_DATE[2:nrow(all_posit)], all_posit$LOC_DATE[1:nrow(all_posit)-1], units = "hour")
          #I will exclude any positions with less than 6 minutes (0.1 hours) of difference from the previous, as a potential signal duplication that can generate issues on distance and speed calculation
          # all_posit <- dplyr::filter(all_posit, time_diff_h > 0.01)
          
        #calculate distance in nautical miles between positions
        all_posit$distance_nm[2:nrow(all_posit)] <- round( geosphere::distGeo(all_posit[2:nrow(all_posit),c("LONGITUDE", "LATITUDE")] , all_posit[1:(nrow(all_posit)-1),c("LONGITUDE", "LATITUDE")])/1000*0.539957, digits = 2)
        #calculate speed between positions
        all_posit$speed_kn[2:nrow(all_posit)] <-  round( all_posit$distance[2:nrow(all_posit)]/all_posit$time_diff_h[2:nrow(all_posit)], digits = 2)
          #round time difference after calculations
          all_posit$time_diff_h <- round(all_posit$time_diff_h, digits = 3)
        #calculate heading for each position
        all_posit$heading_deg[2:nrow(all_posit)] <- geosphere::finalBearing(all_posit[2:nrow(all_posit),c("LONGITUDE", "LATITUDE")] , all_posit[1:(nrow(all_posit)-1),c("LONGITUDE", "LATITUDE")])
            all_posit$heading_deg <- ifelse(all_posit$heading>0,all_posit$heading,360+all_posit$heading) %>% round( digits = 2)
        
        #calculate heading change between positions
            change_heading<-function(x,y){
              change <- ifelse(abs(x-y)>180,abs(x-y)-360, abs(x-y)) %>% abs()
              return(change)
            }
        
            if (dim(all_posit)[1]==2) {
              all_posit$changeheading_deg <- NA
            } else {
        all_posit$changeheading_deg[3:nrow(all_posit)] <- round(change_heading(all_posit[3:nrow(all_posit),"heading_deg"] , all_posit[2:(nrow(all_posit)-1),"heading_deg"]), digits = 2)
            }
        #join final data for the vessel
        temp_all_posit <- rbind(temp_all_posit, all_posit)
        
      }
      
    }
    
    vsl_positions_areas <- temp_all_posit
  
  return(list(vsl_positions_areas=vsl_positions_areas, areas=areas, posit_raw=posit_raw, vessels_passing_ca=vessels_passing_ca))
  
}

check_ca_activities <- function(date_range) {
  
  # Import fishing licences and group licences with more than one area or species
  licenced_vessels <- check_fishing_licences(date_range)
  
  licenses_group <- licenced_vessels %>% 
    group_by(LIC_ID, LIC_From_Date, LIC_To_Date, VSL_ID, CTY_ISO_3_Code, GAR_ID) %>% 
    dplyr::summarise(Species = paste(unique(sort(TXN_Code)), collapse = ", "), Gears = paste(unique(sort(GTY_Name)), collapse = ", ")) %>% 
    ungroup()
  
     #clear duplicity on licences
    licenses_group <- dplyr::group_by(licenses_group, VSL_ID, GAR_ID, Species, Gears) %>% 
      dplyr::summarise(LIC_ID = max(LIC_ID)) %>% 
      ungroup() %>% 
      left_join(dplyr::select(licenced_vessels, LIC_ID, LIC_From_Date, LIC_To_Date), by = "LIC_ID") %>% 
      unique()
  
  #Get the movements data
  vsl_movements <- check_movements(date_range)
  
  #Get the vms data
  vms_data <- check_vms_data(date_range)
  
  vessels_passing_ca <- vms_data$vessels_passing_ca
  
  areas <- vms_data$areas #need it to filter only areas that matters as ASDs and avoid overposition from movements reporting
  
  # To confirm missing positions against movement notifications, for vessels not sending VMS
  vms_mov_join <- full_join(vessels_passing_ca, vsl_movements) %>% 
    dplyr::filter(GAR_Long_Label %in% areas@data$GAR_Long_Label) %>% 
    dplyr::select(-c("VMO_Submitted_CTY_ID", "VMO_Date_Received", "Presence_Interval")) %>% 
    left_join(licenses_group, by = c("VSL_ID" = "VSL_ID", "GAR_ID" = "GAR_ID"))
  
  # Aggregate results per area
  vessels_in_ca_temp <- vms_mov_join %>% 
    group_by(GAR_Long_Label, VMO_Activity) %>% 
    dplyr::summarise(vessels = n()) %>% 
    pivot_wider(names_from = VMO_Activity, values_from = vessels)
  
  vessels_in_ca_all <- areas@data[c("GAR_ID", "GAR_Long_Label")] %>% 
    left_join(vessels_in_ca_temp) %>% 
    arrange(GAR_Long_Label) %>% replace(is.na(.), 0)
  
  #To use on vms frequency reports
  vsl_positions_ca <- vms_data$vsl_positions_areas %>% 
    dplyr::filter(!is.na(GAR_Long_Label))
  
  # To confirm missing positions against movement notifications, for vessels not sending VMS
  vms_mov_all_posit <- full_join(vsl_positions_ca, vsl_movements) %>% 
    dplyr::filter(LOC_DATE %within% Presence_Interval | is.na(Presence_Interval) | is.na(LOC_DATE)) %>% 
    dplyr::filter(GAR_Long_Label %in% areas@data$GAR_Long_Label) %>% 
    dplyr::select(-c("VMO_Submitted_CTY_ID", "VMO_Date_Received", "Presence_Interval"))
  
  #Alert for vessels not reporting for more than 12 hours (after 48h we need to inform the flag state)
  posit_alert_4 <- vms_mov_join %>% 
    dplyr::filter(time_since_h > 12)
  
  return(list(vessels_in_ca_all = vessels_in_ca_all, vms_mov_join=vms_mov_join, vms_mov_all_posit=vms_mov_all_posit, posit_alert_4=posit_alert_4))
  
}

check_vms_frequency <- function(ca_activities) {
  
  vms_reporting <- ca_activities$vms_mov_all_posit %>% 
    dplyr::select(-c("x", "y", "ID", "coords.x1", "coords.x2", "VMO_Entry_Date", "VMO_Exit_Date", "GAR_Name", "GAR_Short_Label")) %>% 
    relocate(CTY_Name, .after = VSL_Name)
  
  # which vessels informed movement but are not reporting?
  vms_not_reporting <- vms_reporting %>% 
    dplyr::filter(is.na(TP_ID))
  
  #Which vessels are sending vms but not the movement report?
  movement_notific_missing <- vms_reporting %>% 
    dplyr::filter(is.na(VMO_Activity))
  
  # #What about the frequency of reports been received?
    posit_alert_1 <- vms_reporting %>% 
    dplyr::filter(time_diff_h > 12)
  
    posit_alert_2 <- vms_reporting %>% 
      dplyr::filter(time_diff_h < 12, time_diff_h > 3)
    
    posit_alert_3 <- vms_reporting %>% 
      dplyr::filter(time_diff_h < 3, time_diff_h > 1.5)
    
  #Alert number 4 was created before at the function "check_ca_activities"
  
  #The time difference in the filter for alarm 5 is to avoid high speeds calculated due multiple beacons reporting in smaller time frames
  posit_alert_5 <- vms_reporting %>% 
    dplyr::filter((speed_kn > 30 & time_diff_h > 0.2) | (speed_kn > 50 & time_diff_h > 0.01))
  
  #all CA vms positions with mov notifications
  vsls_positions_processed <- vms_reporting %>% 
       dplyr::filter(!is.na(TP_ID))
  
  return(list(vms_not_reporting=vms_not_reporting
              , movement_notific_missing=movement_notific_missing
              , vsls_positions_processed=vsls_positions_processed
              , posit_alert_1=posit_alert_1
              , posit_alert_2=posit_alert_2
              , posit_alert_3=posit_alert_3
              , posit_alert_5=posit_alert_5))
 
}
  
  
