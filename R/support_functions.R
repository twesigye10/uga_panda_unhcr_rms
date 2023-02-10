
# cleaning support for different datasets -----------------------

implement_cleaning_support <- function(input_df_raw_data, input_df_survey, input_df_choices, input_df_cleaning_log) {
  
  # find all new choices to add to choices sheet
  
  # gather choice options based on unique choices list
  df_grouped_choices<- input_df_choices %>% 
    group_by(list_name) %>% 
    summarise(choice_options = paste(name, collapse = " : "))
  
  # get new name and choice pairs to add to the choices sheet
  new_vars <- input_df_cleaning_log %>% 
    filter(type %in% c("change_response", "add_option")) %>% 
    left_join(input_df_survey, by = "name") %>% 
    filter(str_detect(string = type.y, pattern = "select_one|select one|select_multiple|select multiple")) %>% 
    separate(col = type.y, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop") %>% 
    left_join(df_grouped_choices, by = "list_name") %>%
    filter(!str_detect(string = choice_options, pattern = value ) ) %>%
    dplyr::rename(choice = value ) %>%
    select(name, choice) %>%
    distinct() %>% # to make sure there are no duplicates
    arrange(name)
  
  # create kobold object
  
  kbo <- kobold::kobold(survey = input_df_survey, 
                        choices = input_df_choices, 
                        data = input_df_raw_data, 
                        cleaning = input_df_cleaning_log)
  
  # modified choices for the survey tool
  df_choises_modified <- butteR:::xlsform_add_choices(kobold = kbo, new_choices = new_vars)
  
  # special treat for variables for select_multiple, we need to add the columns to the data itself
  df_survey_sm <- input_df_survey %>% 
    mutate(q_type = case_when(str_detect(string = type, pattern = "select_multiple|select multiple") ~ "sm",
                              str_detect(string = type, pattern = "select_one|select one") ~ "so",
                              TRUE ~ type)) %>% 
    select(name, q_type)
  
  # construct new columns for select multiple
  new_vars_sm <- new_vars %>% 
    left_join(df_survey_sm, by = "name") %>% 
    filter(q_type == "sm") %>% 
    mutate(new_cols = paste0(name,"/",choice))
  
  # add new columns to the raw data
  df_raw_data_modified <- input_df_raw_data %>% 
    butteR:::mutate_batch(nm = new_vars_sm$new_cols, value = F )
  
  # make some cleanup
  kbo_modified <- kobold::kobold(survey = input_df_survey %>% filter(name %in% colnames(df_raw_data_modified)), 
                                 choices = df_choises_modified, 
                                 data = df_raw_data_modified, 
                                 cleaning = input_df_cleaning_log)
  kbo_cleaned <- kobold::kobold_cleaner(kbo_modified)
  
  # handling Personally Identifiable Information(PII)
  input_vars_to_remove_from_data <- c("deviceid", 
                                      "audit",
                                      "audit_URL",
                                      "instance_name",
                                      "complainant_name",
                                      "complainant_id",
                                      "respondent_telephone",
                                      "name_pers_recording",
                                      "geopoint",
                                      "_geopoint_latitude",
                                      "_geopoint_longitude",
                                      "_geopoint_altitude",
                                      "_geopoint_precision")
  
  df_handle_pii <- kbo_cleaned$data %>% 
    mutate(across(any_of(input_vars_to_remove_from_data), .fns = ~na_if(., .)))
  
  # handling added responses after starting data collection and added responses in the cleaning process
  
  sm_colnames <-  df_handle_pii %>% 
    select(contains("/")) %>% 
    colnames() %>% 
    str_replace_all(pattern = "/.+", replacement = "") %>% 
    unique()
  
  df_handle_sm_data <- df_handle_pii
  
  for (cur_sm_col in sm_colnames) {
    df_updated_data <- df_handle_sm_data %>% 
      mutate(
        across(contains(paste0(cur_sm_col, "/")), .fns = ~ifelse(!is.na(!!sym(cur_sm_col)) & is.na(.) , FALSE, .)),
        across(contains(paste0(cur_sm_col, "/")), .fns = ~ifelse(is.na(!!sym(cur_sm_col)), NA, .))
      )
    df_handle_sm_data <- df_updated_data
  }
  
  df_final_cleaned_data <- df_handle_sm_data
}


# analysis ----------------------------------------------------------------

analysis_support_after_survey_creation <- function(input_ref_svy, input_dap) {
  
  # store analyses
  outputs <-list()
  
  # refugee -----------------------------------------------------------------
  
  dap_refugee <- input_dap %>% 
    filter(split %in% c("all", "refugee_only"))
  
  # no subsets
  refugee_variables_no_subsets <- dap_refugee %>% 
    pull(variable) %>% unique()
  
  # refugee overall, no additional subset
  outputs$ref_overall <- butteR::survey_collapse(df = input_ref_svy,
                                                 vars_to_analyze = refugee_variables_no_subsets) %>% 
    mutate(population = "refugee")
  
  #  subsets
  dap_refugee_subset1 <- input_dap %>%
    filter(split %in%  c("all","refugee_only"), !is.na(subset_1))
  
  # refugee overall, subset 1
  dap_refugee_subset_split <- dap_refugee_subset1 %>%
    split(.$subset_1)
  
  ref_overall_subset1 <-list()
  
  for(i in seq_along(dap_refugee_subset_split)){
    print(i)
    subset_temp <- dap_refugee_subset_split[[i]]
    subset_value <- unique(subset_temp$subset_1)
    vars_temp <- subset_temp %>% pull(variable)
    ref_overall_subset1[[subset_value]] <- butteR::survey_collapse(df = input_ref_svy,
                                                                   vars_to_analyze = vars_temp ,
                                                                   disag = c( subset_value)
    )
  }
  
  outputs$ref_overall_subset1 <- bind_rows(ref_overall_subset1) %>%
    mutate(population = "refugee")
  
  # merge analysis ----------------------------------------------------------
  
  bind_rows(outputs)
}



# duplicate uuids: these were phone calls ---------------------------------

check_duplicates_by_uuid <- function(input_tool_data) {
  input_tool_data %>% 
    group_by(i.check.uuid) %>% 
    mutate(rank = row_number()) %>% 
    filter(rank > 1) %>%  
    mutate(
      i.check.type = "remove_survey",
      i.check.name = "household_id",
      i.check.current_value = "",
      i.check.value = "",
      i.check.issue_id = "duplicate_uuid",
      i.check.issue = "The uuid: {i.check.uuid} is duplicate in the data",
      i.check.other_text = "",
      i.check.checked_by = "",
      i.check.checked_date = as_date(today()),
      i.check.comment = "", 
      i.check.reviewed = "",
      i.check.adjust_log = "",
      i.check.uuid_cl = "",
      i.check.so_sm_choices = "")%>% 
    dplyr::select(starts_with("i.check"))%>% 
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}

# check for duplicate hhid numbers
check_duplicate_hhid_numbers <- function(input_tool_data, input_sample_hhid_nos_list) {
  input_tool_data %>% 
    mutate(unique_hhid_number = household_id) %>% 
    group_by(i.check.location, i.check.household_id) %>% 
    filter(n() > 1, unique_hhid_number %in% input_sample_hhid_nos_list) %>% 
    mutate(i.check.type = "change_response",
           i.check.name = "household_id",
           i.check.current_value = household_id,
           i.check.value = "",
           i.check.issue_id = "hhid_c_duplicate_hhid_no",
           i.check.issue = glue("household_id: {household_id} is duplicated: check that its not a repeated survey"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.uuid_cl = "",
           i.check.so_sm_choices = "") %>% 
    ungroup() %>%
    dplyr::select(starts_with("i.check"))%>% 
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}

# check for point number not being in samples
check_hhid_number_not_in_samples <- function(input_tool_data, input_sample_hhid_nos_list) {
  input_tool_data %>% 
    mutate(unique_hhid_number = household_id) %>% 
    filter(!unique_hhid_number %in% input_sample_hhid_nos_list) %>% 
    mutate(i.check.type = "change_response",
           i.check.name = "household_id",
           i.check.current_value = household_id,
           i.check.value = "",
           i.check.issue_id = "hhid_c_hhid_no_not_in_sample",
           i.check.issue = glue("household_id: {household_id} not in samples"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "", 
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.uuid_cl = "",
           i.check.so_sm_choices = "") %>% 
    dplyr::select(starts_with("i.check"))%>% 
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}