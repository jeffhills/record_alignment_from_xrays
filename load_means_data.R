library(readxl)


jh_spine_levels_factors_df <- tibble(level = c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "l1", "l2", "l3", "l4", "l5")) %>%
  mutate(level_label = str_to_upper(level)) %>% 
  mutate(level_tilt = paste0(level, "_tilt")) %>%
  mutate(level_pelvic_angle = paste0(level, "_pelvic_angle")) %>%
  mutate(level_cobb_angle = paste0(level, "_s1")) %>%
  mutate(level_tilt_label = paste0(level_label, " Tilt")) %>%
  mutate(level_pelvic_angle_label = paste0(level_label, " Pelvic Angle")) %>%
  mutate(level_cobb_label = paste0(level_label, "-S1")) %>%
  select(level, level_tilt, level_pelvic_angle, level_cobb_angle, level_label, level_tilt_label, level_pelvic_angle_label, level_cobb_label) %>%
  mutate(across(everything(), fct_inorder)) %>%
  mutate(across(everything(), fct_rev))

levels_vector <- c('Occiput', 'O-C1', 'C1', 'C1-C2', 'C2', 'C2-C3', 'C3', 'C3-C4', 'C4', 'C4-C5', 'C5', 'C5-C6', 'C6', 'C6-C7', 'C7', 'C7-T1', 'T1', 'T1-T2', 'T2', 'T2-T3', 'T3', 'T3-T4', 'T4', 'T4-T5', 'T5', 'T5-T6', 'T6', 'T6-T7', 'T7', 'T7-T8', 'T8', 'T8-T9', 'T9', 'T9-T10', 'T10', 'T10-T11', 'T11', 'T11-T12', 'T12', 'T12-L1', 'L1', 'L1-L2', 'L2', 'L2-L3', 'L3', 'L3-L4', 'L4', 'L4-L5', 'L5', 'L5-S1', 'S1', 'Sacro-iliac', 'Iliac', 'S2AI')

vertebral_numbers_vector <- c(0,0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5, 11, 11.5, 12, 12.5, 13, 13.5, 14, 14.5, 15, 15.5, 16, 16.5, 17, 17.5, 18, 18.5, 19, 19.5, 20, 20.5, 21, 21.5, 22, 22.5, 23, 23.5, 24, 24.5, 25, 25.5, 26, 27)

jh_spine_levels_df <- tibble(vertebral_number = vertebral_numbers_vector, 
                             level_label = levels_vector, 
                             level = str_to_lower(levels_vector)) %>%
  mutate(across(-vertebral_number, fct_inorder)) %>%
  mutate(across(-vertebral_number, fct_rev))

data_import_df <- read_xlsx("data/updated_selected_var_means_data_9_18_2023.xlsx") %>%
  clean_names() %>%
  replace_na(list(disc_degeneration = "N", odi_8_sex_life = "0")) %>%
  select(-ends_with("frontal"), -ends_with("axial"))%>%
  select(patient_id,
         country,
         disc_degeneration,
         age,
         sex,
         bmi,
         ethnicity,
         # abnormality,
         odi,
         cobb_mt,
         cobb_pt,
         cobb_tl,
         pelvic_incidence,
         pelvic_tilt,
         sacral_slope,
         pelvic_thickness_mm,
         t1_t12 = t1t12,
         t4_t12 = t4t12,
         c2_c7 = c2c7,
         l1_l5 = l1l5,
         l1s1,
         l2s1,
         l3s1,
         l4s1,
         l5s1,
         oc2_cl,
         tk_max,
         tk_max_sup,
         tk_apex,
         tk_max_inf,
         ll_max,
         ll_max_sup,
         ll_apex,
         ll_max_inf,
         tl_inflection_segment,
         cervical_sagittal_vertical_axis_mm,
         c2_pelvic_angle,
         c3_pelvic_angle,
         c4_pelvic_angle,
         c5_pelvic_angle,
         c6_pelvic_angle,
         c7_pelvic_angle,
         t1_pelvic_angle,
         t2_pelvic_angle,
         t3_pelvic_angle,
         t4_pelvic_angle,
         t5_pelvic_angle,
         t6_pelvic_angle,
         t7_pelvic_angle,
         t8_pelvic_angle,
         t9_pelvic_angle,
         t10_pelvic_angle,
         t11_pelvic_angle,
         t12_pelvic_angle,
         l1_pelvic_angle,
         l2_pelvic_angle,
         l3_pelvic_angle,
         l4_pelvic_angle,
         l5_pelvic_angle,
         c2_c3_ha,
         c2_c4_ha,
         c2_c5_ha,
         c2_c6_ha,
         c2_c7_ha,
         c2_t1_ha,
         c2_t2_ha,
         c2_t3_ha,
         c2_t4_ha,
         c2_t5_ha,
         c2_t6_ha,
         c2_t7_ha,
         c2_t8_ha,
         c2_t9_ha,
         c2_t10_ha,
         c2_t11_ha,
         c2_t12_ha,
         c2_l1_ha,
         c2_l2_ha,
         c2_l3_ha,
         c2_l4_ha,
         c2_l5_ha,
         sagittal_vertical_axis_mm,
         left_knee_flexion,
         right_knee_flexion,
         spinal_3d_length_mm,
         cia_t1,
         cia_t2,
         cia_t3,
         cia_t4,
         cia_t5,
         cia_t6,
         cia_t7,
         cia_t8,
         cia_t9,
         cia_t10,
         cia_t11,
         cia_t12,
         patient_plane_c2_lateral,
         patient_plane_c3_lateral,
         patient_plane_c4_lateral,
         patient_plane_c5_lateral,
         patient_plane_c6_lateral,
         patient_plane_c7_lateral,
         patient_plane_t1_lateral,
         patient_plane_t2_lateral,
         patient_plane_t3_lateral,
         patient_plane_t4_lateral,
         patient_plane_t5_lateral,
         patient_plane_t6_lateral,
         patient_plane_t7_lateral,
         patient_plane_t8_lateral,
         patient_plane_t9_lateral,
         patient_plane_t10_lateral,
         patient_plane_t11_lateral,
         patient_plane_t12_lateral,
         patient_plane_l1_lateral,
         patient_plane_l2_lateral,
         patient_plane_l3_lateral,
         patient_plane_l4_lateral,
         patient_plane_l5_lateral,
         intervertebrae_rotations_3d_c2_c3_lateral,
         intervertebrae_rotations_3d_c3_c4_lateral,
         intervertebrae_rotations_3d_c4_c5_lateral,
         intervertebrae_rotations_3d_c5_c6_lateral,
         intervertebrae_rotations_3d_c6_c7_lateral,
         intervertebrae_rotations_3d_c7_t1_lateral,
         intervertebrae_rotations_3d_t1_t2_lateral,
         intervertebrae_rotations_3d_t2_t3_lateral,
         intervertebrae_rotations_3d_t3_t4_lateral,
         intervertebrae_rotations_3d_t4_t5_lateral,
         intervertebrae_rotations_3d_t5_t6_lateral,
         intervertebrae_rotations_3d_t6_t7_lateral,
         intervertebrae_rotations_3d_t7_t8_lateral,
         intervertebrae_rotations_3d_t8_t9_lateral,
         intervertebrae_rotations_3d_t9_t10_lateral,
         intervertebrae_rotations_3d_t10_t11_lateral,
         intervertebrae_rotations_3d_t11_t12_lateral,
         intervertebrae_rotations_3d_t12_l1_lateral,
         intervertebrae_rotations_3d_l1_l2_lateral,
         intervertebrae_rotations_3d_l2_l3_lateral,
         intervertebrae_rotations_3d_l3_l4_lateral,
         intervertebrae_rotations_3d_l4_l5_lateral
  ) %>%
  rename_if(str_detect(names(.), "l\\ds1"),
            ~str_replace_all(., pattern = "s1", replacement = "_s1")
  )%>%
  rename_if(str_detect(names(.), "patient_plane"),
            ~str_remove_all(., pattern = "patient_plane_")
  )

full_df_list <- list()

full_df_list$demographics_coronal_df <- data_import_df %>%
  select(patient_id,
         country,
         disc_degeneration,
         age,
         sex,
         bmi,
         ethnicity,
         # abnormality,
         odi,
         cobb_mt,
         cobb_pt,
         cobb_tl
  )



# glue_collapse(x = names(data_import_df), sep = ",\n")


full_df_list$rad_parameters <- data_import_df %>%
  select(patient_id, 
         pelvic_incidence,
         pelvic_tilt,
         sacral_slope,
         pelvic_thickness_mm,
         cervical_sagittal_vertical_axis_mm,
         sagittal_vertical_axis_mm,
         left_knee_flexion,
         right_knee_flexion,
         spinal_3d_length_mm)


full_df_list$vpa_df <- data_import_df %>%
  select(patient_id,
         c2_pelvic_angle,
         c3_pelvic_angle,
         c4_pelvic_angle,
         c5_pelvic_angle,
         c6_pelvic_angle,
         c7_pelvic_angle,
         t1_pelvic_angle,
         t2_pelvic_angle,
         t3_pelvic_angle,
         t4_pelvic_angle,
         t5_pelvic_angle,
         t6_pelvic_angle,
         t7_pelvic_angle,
         t8_pelvic_angle,
         t9_pelvic_angle,
         t10_pelvic_angle,
         t11_pelvic_angle,
         t12_pelvic_angle,
         l1_pelvic_angle,
         l2_pelvic_angle,
         l3_pelvic_angle,
         l4_pelvic_angle,
         l5_pelvic_angle) %>%
  mutate(c2pa_t1pa_mismatch = c2_pelvic_angle - t1_pelvic_angle, 
         c2pa_t4pa_mismatch = c2_pelvic_angle - t4_pelvic_angle, 
         c2pa_t9pa_mismatch = c2_pelvic_angle - t9_pelvic_angle, 
         c2pa_l1pa_mismatch = c2_pelvic_angle - l1_pelvic_angle,
         t1pa_t9pa_mismatch = t1_pelvic_angle - t9_pelvic_angle,
         t1pa_l1pa_mismatch = t1_pelvic_angle - l1_pelvic_angle,
         t4pa_t9pa_mismatch = t4_pelvic_angle - t9_pelvic_angle,
         t4pa_l1pa_mismatch = t4_pelvic_angle - l1_pelvic_angle,
         t9pa_l1pa_mismatch = t9_pelvic_angle - l1_pelvic_angle)


full_df_list$vert_tilt_df <- data_import_df %>%
  select(patient_id,
         pelvic_tilt,
         c2_pelvic_angle,
         c3_pelvic_angle,
         c4_pelvic_angle,
         c5_pelvic_angle,
         c6_pelvic_angle,
         c7_pelvic_angle,
         t1_pelvic_angle,
         t2_pelvic_angle,
         t3_pelvic_angle,
         t4_pelvic_angle,
         t5_pelvic_angle,
         t6_pelvic_angle,
         t7_pelvic_angle,
         t8_pelvic_angle,
         t9_pelvic_angle,
         t10_pelvic_angle,
         t11_pelvic_angle,
         t12_pelvic_angle,
         l1_pelvic_angle,
         l2_pelvic_angle,
         l3_pelvic_angle,
         l4_pelvic_angle,
         l5_pelvic_angle)%>%
  pivot_longer(cols = contains("pelvic_angle"), names_to = "level", values_to = "vpa") %>%
  mutate(vertebral_tilt = vpa - pelvic_tilt) %>%
  mutate(level = str_replace_all(level, "pelvic_angle", "tilt")) %>%
  select(patient_id, level, vertebral_tilt) %>%
  pivot_wider(names_from = level, values_from = vertebral_tilt)



lumbar_lordosis_df <- data_import_df %>% 
  select(patient_id,
         sacral_slope,
         l1_s1, 
         l2_s1,
         l3_s1,
         l4_s1,
         l5_s1) %>%
  mutate(l1_s1 = -1*l1_s1,
         l2_s1 = -1*l2_s1,
         l3_s1 = -1*l3_s1,
         l4_s1 = -1*l4_s1,
         l5_s1 = -1*l5_s1) %>%
  mutate(l1_slope = sacral_slope - l1_s1,
         l2_slope = sacral_slope - l2_s1,
         l3_slope = sacral_slope - l3_s1,
         l4_slope = sacral_slope - l4_s1,
         l5_slope = sacral_slope - l5_s1) %>%
  mutate(l1_l2_slope = (l1_slope + l2_slope)/2, 
         l2_l3_slope = (l2_slope + l3_slope)/2, 
         l3_l4_slope = (l3_slope + l4_slope)/2, 
         l4_l5_slope = (l4_slope + l5_slope)/2, 
         l5_s1_slope = (l5_slope + sacral_slope)/2)

tl_inflection_df <- data_import_df %>%
  select(patient_id, tl_inflection_segment) %>%
  left_join((jh_spine_levels_df %>%
               mutate(tl_inflection_segment = str_remove_all(level_label, "-")) %>%
               select(tl_inflection_segment, level_label))) %>%
  select(patient_id, tl_inflection = level_label)

sagittal_cobb_df <- data_import_df %>% 
  select(patient_id,
         sacral_slope,
         c2_lateral,
         c3_lateral,
         c4_lateral,
         c5_lateral,
         c6_lateral,
         c7_lateral,
         t1_lateral,
         t2_lateral,
         t3_lateral,
         t4_lateral,
         t5_lateral,
         t6_lateral,
         t7_lateral,
         t8_lateral,
         t9_lateral,
         t10_lateral,
         t11_lateral,
         t12_lateral,
         l1_lateral,
         l2_lateral,
         l3_lateral,
         l4_lateral,
         l5_lateral) %>%
  pivot_longer(c(-patient_id, -sacral_slope), names_to = "level", values_to = "slope")%>%
  mutate(level = str_remove_all(level, "_lateral")) %>%
  mutate(lordosis = round(sacral_slope - slope, 4)) %>%
  filter(str_starts(level, "t|c")) %>%
  select(patient_id, level, lordosis) %>%
  mutate(level = paste0(level, "_s1")) %>%
  pivot_wider(names_from = level, values_from = lordosis) %>%
  left_join(lumbar_lordosis_df) %>%
  left_join(tl_inflection_df) %>%
  select(patient_id, sacral_slope, everything())%>%
  left_join(data_import_df %>%
              select(patient_id, t1_t12, t4_t12, c2_c7))


full_df_list$sagittal_cobb_df <- sagittal_cobb_df

pre_r_type_df <- sagittal_cobb_df %>%
  select(patient_id, sacral_slope, contains("slope")) %>%
  pivot_longer(cols = c(-patient_id, -sacral_slope), names_to = "level", values_to = "slope") %>%
  mutate(level = str_remove_all(level, "_slope")) %>%
  mutate(level_order = case_when(
    level == "l1" ~ 1, 
    level ==  "l1_l2"~ 2,
    level ==  "l2"~ 3, 
    level ==   "l2_l3"~ 4,
    level ==   "l3"~ 5, 
    level ==   "l3_l4"~ 6, 
    level ==   "l4"~ 7, 
    level ==   "l4_l5"~ 8,
    level ==   "l5"~ 9, 
    level ==   "l5_s1"~ 10
  )) %>%
  arrange(patient_id, level_order) %>%
  mutate(level = fct_inorder(level)) %>%
  arrange(patient_id, abs(slope)) %>%
  group_by(patient_id) %>%
  mutate(slope_count = row_number()) %>%
  filter(slope_count < 3)  %>%
  ungroup() %>%
  arrange(patient_id, level) %>%
  group_by(patient_id)  %>%
  select(patient_id, sacral_slope, level, slope) %>%
  mutate(slope_count = row_number()) %>%
  ungroup()

type_1_2_pre_df <- sagittal_cobb_df %>%
  select(patient_id, sacral_slope, contains("inflection"), contains("slope")) %>%
  filter(sacral_slope < 35) %>%
  mutate(tl_inflection = droplevels(tl_inflection)) %>%
  pivot_longer(cols = c(-patient_id, -sacral_slope, -tl_inflection), names_to = "level", values_to = "slope") %>%
  mutate(level = str_remove_all(level, "_slope")) %>%
  mutate(level_order = case_when(
    level == "l1" ~ 1, 
    level ==  "l1_l2"~ 2,
    level ==  "l2"~ 3, 
    level ==   "l2_l3"~ 4,
    level ==   "l3"~ 5, 
    level ==   "l3_l4"~ 6, 
    level ==   "l4"~ 7, 
    level ==   "l4_l5"~ 8,
    level ==   "l5"~ 9, 
    level ==   "l5_s1"~ 10
  )) %>%
  arrange(patient_id, level_order) %>%
  mutate(level = fct_inorder(level)) %>%
  arrange(patient_id, abs(slope)) %>%
  group_by(patient_id) %>%
  mutate(slope_count = row_number()) %>%
  filter(slope_count < 3)  %>%
  ungroup() %>%
  arrange(patient_id, level) %>%
  group_by(patient_id)  %>%
  select(patient_id, tl_inflection, level, slope) %>%
  mutate(slope_count = row_number()) %>%
  ungroup()


type_1_2_df <- pre_r_type_df %>%
  filter(sacral_slope < 35) %>%
  select(-sacral_slope) %>%
  filter(slope_count == 1) %>%
  select(patient_id, level_1 = level, slope_1 =slope) %>%
  left_join(pre_r_type_df %>%
              filter(sacral_slope < 35) %>%
              select(-sacral_slope) %>%
              filter(slope_count == 2) %>%
              select(patient_id, level_2 = level, slope_2 =slope))  %>%
  mutate(slope_diff = abs(abs(slope_1) - abs(slope_2))) %>%
  mutate(r_type = case_when(
    level_1 == "l5" ~ "Type 1", 
    level_1 == "l5_s1" ~ "Type 1", 
    level_1 == "l4_l5" & abs(slope_1) > slope_diff  ~ "Type 1",
    TRUE ~ "Type 2"
  )) %>%
  ungroup() %>%
  mutate(lumbar_apex = if_else(r_type == "Type 1", "l5", level_1)) %>%
  select(patient_id, r_type, lumbar_apex) %>%
  distinct() 

type_3_df <- pre_r_type_df %>%
  filter(slope_count == 1) %>%
  filter(between(sacral_slope, 35, 45))%>%
  mutate(r_type = "Type 3") %>%
  mutate(lumbar_apex = as.character(level)) %>%
  select(patient_id, r_type, lumbar_apex)

type_4_df <- pre_r_type_df %>%
  filter(slope_count == 1) %>%
  filter(sacral_slope > 45)%>%
  mutate(r_type = "Type 4") %>%
  mutate(lumbar_apex = as.character(level)) %>% 
  select(patient_id, r_type, lumbar_apex)

r_types_df <- type_1_2_df %>%
  union_all(type_3_df) %>%
  union_all(type_4_df) %>%
  mutate(lumbar_apex = str_to_upper(str_replace_all(lumbar_apex, "_", "-")))


full_df_list$r_types_df <- r_types_df


full_cohort_select_measures_df <- full_df_list$demographics_coronal_df %>%
  left_join(full_df_list$rad_parameters)%>%
  left_join(full_df_list$vpa_df)%>%
  left_join(full_df_list$vert_tilt_df) %>%
  left_join(full_df_list$sagittal_cobb_df)%>%
  left_join(full_df_list$r_types_df) 


cohort_with_coronal_deformity <- full_cohort_select_measures_df %>%
  select(patient_id, odi, cobb_mt, cobb_pt, cobb_tl) %>%
  filter(cobb_mt > 10.5 | cobb_pt > 10.5 | cobb_tl > 10.5)

number_of_patients_excluded_for_coronal_deformity <- nrow(cohort_with_coronal_deformity)

degeneration_screened_df <- read_csv("data/no_deformity_cohort_exclude_for_degeneration.csv") %>%
  clean_names() 

exclude_for_degeneration_reconciled_df <- degeneration_screened_df %>%
  full_join(full_cohort_select_measures_df) %>%
  mutate(exclude_for_degeneration = case_when(exclude_for_degeneration == 1 ~ "Y",
                                              exclude_for_degeneration == 0 ~ "N")) %>%
  replace_na(list(exclude_for_degeneration = "unknown", disc_degeneration = "unknown")) %>%
  # filter(exclude_for_degeneration != disc_degeneration) %>%
  mutate(exclude_degeneration_reconciled = case_when(
    exclude_for_degeneration == disc_degeneration ~ disc_degeneration,
    exclude_for_degeneration == "N" & disc_degeneration == "unknown" ~ "N",
    exclude_for_degeneration == "Y" & disc_degeneration == "unknown" ~ "Y",
    exclude_for_degeneration == "unknown" & disc_degeneration == "N" ~ "N",
    exclude_for_degeneration == "unknown" & disc_degeneration == "Y" ~ "Y",   
    exclude_for_degeneration == "N" & disc_degeneration == "Y" ~ "Y",#   # THIS MAY EXCLUDE SOME UNNECESSARILY
    exclude_for_degeneration == "Y" & disc_degeneration == "N" ~ "Y",
    exclude_for_degeneration == "unknown" & disc_degeneration == "unknown" ~ "unknown"
  )) %>%
  select(patient_id, exclude_for_degeneration = exclude_degeneration_reconciled) 

odi_cutoff <- 20

exclude_for_odi_df <- full_cohort_select_measures_df %>%
  anti_join(cohort_with_coronal_deformity) %>%
  select(patient_id, odi) %>%
  arrange(desc(odi)) %>%
  filter(odi > odi_cutoff)

number_of_patients_excluded_for_odi <- nrow(exclude_for_odi_df)


patients_excluded_for_degeneration_df <- full_cohort_select_measures_df %>%
  anti_join(cohort_with_coronal_deformity) %>%
  anti_join(exclude_for_odi_df) %>%
  left_join(exclude_for_degeneration_reconciled_df) %>%
  select(patient_id, exclude_for_degeneration) %>%
  # mutate(exclude_for_degeneration = replace_na(exclude_for_degeneration, replace = 0)) %>%
  filter(exclude_for_degeneration == "Y") %>%
  select(patient_id) %>%
  distinct() 

number_of_patients_excluded_for_degeneration <- nrow(patients_excluded_for_degeneration_df)


full_cohort_df <- full_cohort_select_measures_df %>%
  filter(patient_id %in% cohort_with_coronal_deformity$patient_id == FALSE) %>%
  filter(patient_id %in% exclude_for_odi_df$patient_id == FALSE) %>%
  filter(patient_id %in% patients_excluded_for_degeneration_df$patient_id == FALSE)%>%
  mutate(r_type = factor(r_type, levels = c("Type 1", "Type 2", "Type 3", "Type 4"))) %>%
  mutate(patient_id = as.character(patient_id)) %>%
  mutate(id_length = str_length(patient_id)) %>%
  mutate(patient_id = case_when(
    id_length == 1 ~ paste0("000", patient_id), 
    id_length == 2 ~ paste0("00", patient_id), 
    id_length == 3 ~ paste0("0", patient_id), 
    id_length == 4 ~ paste0(patient_id)
  )) %>%
  select(-id_length) %>%
  # mutate(record_id = patient_id, means_data_complete = "Complete") %>%
  select(record_id = patient_id, everything())
