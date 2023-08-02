# MAPS - SRTS Scoring
# Author: LAG
# Last modified 2023-07-28

# The purpose of this code is to score the data from the MAPS-SRTS data 
# collection tool using the final scoring framework presented in 
# Ganzar et al (add citation)

# Setup ----
# Load packages
library(tidyverse)
library(irr)
library(here)

# Import raw MAPS-SRTS data with 1 observation per row
maps <- readr::read_csv(here("data/raw_data", "maps_srts_raw.csv"))

# Create subscales and recode school access segments ----
# Create function for school access segments
# select only School Access Segment variables and rename 
# Note: ensure the order of variable names below matches data set
SAS_score <- function(A_){
  maps %>% 
    select(c(starts_with(A_),"SchoolID" )) %>%
    setNames( c("A_00Street", "A_00Side", "A_00Start", 
                "A__00End", "A_01", "A_02","A_03", "A_04",
                "A_05", "A_06","A_07", "A_08", "A_09", 
                "A_10", "A_11", "A_12", "A_13", "A_14", 
                "A_15a", "A_15b", "A_16a", "A_16b", 
                "A_16c", "A_16d", "A_17a", "A_17b", "A_17c",
                "A_17d", "A_17e", "A_17f", "A_18a", "A_18b", 
                "A_18c", "A_18d", "A_18e", "A_19a", "A_19b",
                "A_19c", "A_19d", "A_20a", "A_20b", "A_20c", "A_21", 
                "A_22", "A_00Notes", "SchoolID")) %>%
    mutate(
      A_calming_pos = case_when(A_16a == 1 | A_16b == 1 | A_16c == 1 | A_16d == 1 ~ 1, # Positive streetscape subscale
                                A_16a == 0 & A_16b == 0 & A_16c == 0 & A_16d == 0 ~ 0),
      A_lights_pos = case_when(A_02 == 1 | A_02 == 2 | A_03 == 1 | A_03 == 2 ~ 1,
                               A_02 == 0 & A_03 == 0 ~ 0),
      A_driveways_pos = case_when(A_21 == 1 | A_21 == 2 | A_21 == 3 ~ 1,
                                  A_21 == 4 ~ 0),
      A_speed_dichot = case_when(A_13 == 5 | A_13 == 4 | A_13 == 3 | A_13 == 2 | A_13 == 1 ~ 1, 
                                 A_13 == 0 ~ 0),
      A_speed25_pos = case_when(A_13 == 5 | A_13 == 4 ~ 1,
                                A_13 == 3 | A_13 == 2 | A_13 == 1 | A_13== 0 ~ 0),
      A_speed_spec_pos = case_when(A_14 == 3 | A_14 == 2 | A_14 == 1 ~ 1,
                                   A_14 == 0 ~ 0),
      A_signs_pos = A_17a + A_17b + A_17c + A_17d,
      A_streetscape_pos = 
        A_01 + 
        A_calming_pos + 
        A_lights_pos +
        A_driveways_pos +
        A_20a +
        A_20b +
        A_20c +
        A_speed_dichot + 
        A_speed25_pos +
        A_speed_spec_pos + 
        A_signs_pos,
      A_sidewalk = case_when(A_07 == 2 | A_07 == 3 ~ 2, # Positive sidewalk subscale
                             A_07 == 1 | A_07 == 0 ~ 0),
      A_sidewalk_cont = case_when(A_07 == 3 ~ 1,
                                  A_07 == 2 | A_07 == 1 | A_07 == 0 ~ 0),
      A_sidewalk_width = case_when(A_08 == 3 ~ 3,
                                   A_08 == 2 | A_08 == 1 ~ 2,
                                   A_08 == -777 ~ 0),
      A_sidewalk_pos = A_sidewalk + A_sidewalk_cont + A_sidewalk_width,
      A_buffer_pos = case_when(A_10 == 1 ~ 1, # Positive buffer
                               A_10 == 0 | A_10 == -777 ~ 0), 
      A_bike_pos = case_when(A_06 == 5 | A_06 == 4 ~ 2, # Bike infrastructure 
                             A_06 == 3 | A_06 == 2 ~ 1,
                             A_06 == 1 | A_06 == 0 ~ 0), 
      A_trees_pos = case_when(A_11 == 2 ~ 2, # Shade
                              A_11 == 1 ~ 1,
                              A_11 == 0 | A_11 == -777 ~ 0), 
      A_awning_pos = case_when(A_12 == 2 ~ 2,
                               A_12 == 1 ~ 1,
                               A_12 == 0 | A_12 == -777 ~ 0),
      A_shade_pos = A_trees_pos + A_awning_pos,
      A_overall_pos = A_streetscape_pos + # Positive SAS
        A_sidewalk_pos +
        A_bike_pos,
      A_speed25_neg = case_when(A_13 == 5 | A_13 == 4 ~ 0, # Negative streetscape
                                A_13 == 3 | A_13 == 2 | A_13 == 1 | A_13== 0 ~ 1), 
      A_driveways_neg = case_when(A_21 == 1 | A_21 == 2 | A_21 == 3 ~ 0,
                                  A_21 == 4 ~ 1),
      A_streetscape_neg = A_speed25_neg + A_driveways_neg,
      A_cont_neg = case_when(A_07 == 3 ~ 0,
                             A_07 == 2 | A_07 == 1 | A_07 == 0 ~ 1), # Negative sidewalk
      A_trip_neg = case_when(A_09 == 1 | A_09 == 2 | A_09 ==-777 ~ 0,
                             A_09 == 3 | A_09 == 4 ~ 1),
      A_sidewalk_neg = A_cont_neg + A_trip_neg, 
      A_aes_neg = case_when(A_05 == 0 ~ 0, 
                            A_05 = 1 | A_05 == 2 | A_05 == 3 ~ 1), #Negative aesthetics 
      A_overall_neg = A_streetscape_neg + A_sidewalk_neg + A_aes_neg, # Negative SAS
      A_overall = A_overall_pos - A_overall_neg # Overall SAS  
    ) %>% 
    rename_with(~gsub("A_", A_, .x))
}

# Create subscales and recode each of the 4 school access segments 
A1 <- SAS_score("A1_")
A2 <- SAS_score("A2_")
A3 <- SAS_score("A3_")
A4 <- SAS_score("A4_")

# combine school access segments 1 - 4 in a single data frame
combined_A <- cbind(A1, A2, A3, A4)

# Create subscales and recode segments ----
# Create function for segments
# select only Segment variables and rename
# ensure the order of variable names matches data set
S_score <- function(S_){
  maps %>% 
    select(c(starts_with(S_),"SchoolID" )) %>%
    setNames(c("S_00Street", "S_00Side", "S_00Start", 
               "S_00End", "S_01", "S_02","S_03",
               "S_04","S_05", "S_06","S_07", "S_08",
               "S_09", "S_10", "S_11", "S_12", "S_13a",
               "S_13b", "S_14a", "S_14b", "S_14c",
               "S_14d",  "S_15a", "S_15b", "S_15c", 
               "S_15d", "S_15e", "S_15f", "S_16a",
               "S_16b", "S_16c", "S_16d", "S_16e",
               "S_17a", "S_17b", "S_17c", "S_17d",
               "S_18a", "S_18b", "S_18c", "S_19", "S_20",
               "SchoolID")) %>%  # Create subscales
    mutate(
      S_calming_pos = case_when(S_14a == 1 | S_14b == 1 | S_14c == 1 | S_14d == 1 ~ 1, # Positive streetscape subscale
                                S_14a == 0 & S_14b == 0 & S_14c == 0 & S_14d == 0 ~ 0),
      S_lights_pos = case_when(S_02 == 1 | S_02 == 2 | S_03 == 1 | S_03 == 2 ~ 1,
                               S_02 == 0 & S_03 == 0 ~ 0),
      S_driveways_pos = case_when(S_19 == 1 | S_19 == 2 | S_19 == 3 ~ 1,
                                  S_19 == 4 ~ 0),
      S_speed_dichot = case_when(S_13a == 5 | S_13a == 4 | S_13a == 3 | S_13a == 2 | S_13a == 1 ~ 1, 
                                 S_13a == 0 ~ 0),
      S_speed25_pos = case_when(S_13a == 5 | S_13a == 4 ~ 1,
                                S_13a == 3 | S_13a == 2 | S_13a == 1 | S_13a == 0 ~ 0),
      S_speed_spec_pos = case_when(S_13b == 5 |S_13b == 4 |S_13b == 3 | S_13b == 2 | S_13b == 1 ~ 1,
                                   S_13b == 0 ~ 0),
      S_signs_pos = S_15a + S_15b + S_15c + S_15d + S_15e + S_15f,
      S_streetscape_pos = 
        S_01 + 
        S_calming_pos + 
        S_lights_pos +
        S_driveways_pos +
        S_18a +
        S_18b +
        S_18c +
        S_speed_dichot + 
        S_speed25_pos +
        S_speed_spec_pos +
        S_signs_pos,
      S_sidewalk = case_when(S_07 == 2 | S_07 == 3 ~ 2, # Positive sidewalk subscale
                             S_07 == 1 | S_07 == 0 ~ 0),
      S_sidewalk_cont = case_when(S_07 == 3 ~ 1,
                                  S_07 == 2 | S_07 == 1 | S_07 == 0 ~ 0),
      S_sidewalk_width = case_when(S_08 == 3 ~ 3,
                                   S_08 == 2 | S_08 == 1 ~ 2,
                                   S_08 == -777 ~ 0),
      S_sidewalk_pos = S_sidewalk + S_sidewalk_cont + S_sidewalk_width,
      S_buffer_pos = case_when(S_10 == 1 ~ 1, # Positive buffer
                               S_10 == 0 | S_10 == -777 ~ 0), 
      S_bike_pos = case_when(S_06 == 5 | S_06 == 4 ~ 2, # Bike infrastructure 
                             S_06 == 3 | S_06 == 2 ~ 1,
                             S_06 == 1 | S_06 == 0 ~ 0), 
      S_trees_pos = case_when(S_11 == 2 ~ 2, # Shade
                              S_11 == 1 ~ 1,
                              S_11 == 0 | S_11 == -777 ~ 0), 
      S_awning_pos = case_when(S_12 == 2 ~ 2,
                               S_12 == 1 ~ 1,
                               S_12 == 0 | S_12 == -777 ~ 0),
      S_shade_pos = S_trees_pos + S_awning_pos,
      S_overall_pos = S_streetscape_pos + # Positive segment
        S_sidewalk_pos + 
        S_bike_pos,
      S_speed25_neg = case_when(S_13a == 5 | S_13a == 4 ~ 0, # Negative streetscape
                                S_13a == 3 | S_13a == 2 | S_13a == 1 | S_13a == 0 ~ 1), 
      S_driveways_neg = case_when(S_19 == 1 | S_19 == 2 | S_19 == 3 ~ 0,
                                  S_19 == 4 ~ 1),
      S_streetscape_neg = S_speed25_neg + S_driveways_neg,
      S_cont_neg = case_when(S_07 == 3 ~ 0,
                             S_07 == 2 | S_07 == 1 | S_07 == 0 ~ 1), # Negative sidewalk
      S_trip_neg = case_when(S_09 == 1 | S_09 == 2 | S_09 ==-777 ~ 0,
                             S_09 == 3 | S_09 == 4 ~ 1),
      S_sidewalk_neg = S_cont_neg + S_trip_neg, 
      S_aes_neg = case_when(S_05 == 0 ~ 0, 
                            S_05 = 1 | S_05 == 2 | S_05 == 3 ~ 1), #Negative aesthetics 
      S_overall_neg = S_streetscape_neg + S_sidewalk_neg, # Negative segment
      S_overall = S_overall_pos - S_overall_neg # Overall Segment  
    ) %>% 
    rename_with(~gsub("S_", S_, .x))
}

# Create subscales and recode each of the 14 segments
S1 <- S_score("S1_")
S2 <- S_score("S2_")
S3 <- S_score("S3_")
S4 <- S_score("S4_")
S5 <- S_score("S5_")
S6 <- S_score("S6_")
S7 <- S_score("S7_")
S8 <- S_score("S8_")
S9 <- S_score("S9_")
S10 <- S_score("S10_")
S11 <- S_score("S11_")
S12 <- S_score("S12_")
S13 <- S_score("S13_")
S14 <- S_score("S14_")

# Combine all segments
combined_S <- cbind(S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14)

# Create subscales and recode crossings ----
# Create function for crossings
# select only Crossing variables and rename, 
# ensure the order of variable names matches data set
C_score <- function(C_) {
  maps %>% 
    select(c(starts_with(C_),"SchoolID" )) %>%
    setNames(c( "C_01a", "C_01b","C_01bi","C_00Intersection1","C_00Intersection2",
                "C_00Direction1","C_00Direction2","C_01bii", "C_01c",
                "C_00Crossing1", "C_00Crossing2", "C_00Crossing3",
                "C_02a", "C_02b", "C_02c", "C_02d","C_03a",
                "C_03b", "C_03c", "C_03d", "C_04a", "C_04b",
                "C_05a", "C_05b", "C_05c", "C_05d", "C_06a",
                "C_06b", "C_06c", "C_07", "C_08", "C_09a",
                "C_09b", "C_09c", "C_00Notes", "SchoolID")) %>% 
    # Create subscales
    mutate(
      C_amenities_pos = # crosswalk amenities
        C_05a + 
        C_05b +
        C_05c +
        C_05d +
        C_06a +
        C_06b +
        C_06c,
      C_pre_curb_pos = case_when(C_04a == 1 ~ 1, # curb ramps positive
                                 C_04a == 2 | C_04a ==3 ~ 0),
      C_post_curb_pos = case_when(C_04b == 1 ~ 1,
                                  C_04b == 2 | C_04b ==3 ~ 0),
      C_curb_qual_pos = C_pre_curb_pos + C_post_curb_pos,
      C_control_pos = # intersection control
        C_02a +
        C_02b +
        C_02c +
        C_02d +
        C_03a +
        C_03b +
        C_03c +
        C_03d,
      C_overall_pos = C_amenities_pos + C_curb_qual_pos + C_control_pos, # positive crossing
      C_width = case_when(C_07 < 5 ~ 0, # road width
                          C_07 >= 5 ~ 1),
      C_pre_curb_neg = case_when(C_04a == 1 ~ 0, # curb ramps negative
                                 C_04a == 3 | C_04a == 2  ~ 1),
      C_post_curb_neg = case_when(C_04b == 1 ~ 0,
                                  C_04b == 3 | C_04b == 2 ~ 1),
      C_imped =  # crossing impediments
        C_pre_curb_neg + 
        C_post_curb_neg +
        C_09a + 
        C_09b,
      C_overall_neg =  C_width + C_imped, # negative crossing
      C_overall = C_overall_pos - C_overall_neg # overall crossing
    ) %>% 
    rename_with(~gsub("C_", C_, .x))
}

# Create subscales for all 10 crossings
C1 <- C_score("C1_")
C2 <- C_score("C2_")
C3 <- C_score("C3_")
C4 <- C_score("C4_")
C5 <- C_score("C5_")
C6 <- C_score("C6_")
C7 <- C_score("C7_")
C8 <- C_score("C8_")
C9 <- C_score("C9_")
C10 <- C_score("C10_")

# Combine all crossing dataframes
combined_C <- cbind(C1, C2, C3, C4, C5, C6, C7, C8, C9, C10)

# Setup for final scoring ----
# combine all sections back into one data frame
maps_scored <- cbind(combined_A, combined_S, combined_C)

# create function for row means by subscale 
subscale_sum <- function(df, section, subscale, var_name)  {
  df %>%
    select(starts_with(section) & ends_with(subscale)) %>%
    mutate(!!quo_name(var_name) := rowSums(., na.rm = TRUE))
}

# Scoring for school access segment ----
# school access segments subscale sums
A_calming_pos_sum <- subscale_sum(maps_scored, "A", "calming_pos","A_calming_pos_sum")
A_lights_pos_sum <- subscale_sum(maps_scored, "A", "lights_pos","A_lights_pos_sum")
A_driveways_pos_sum <- subscale_sum(maps_scored, "A", "driveways_pos","A_driveways_pos_sum")
A_speed_dichot_sum <- subscale_sum(maps_scored, "A", "speed_dichot","A_speed_dichot_sum")
A_speed25_pos_sum <- subscale_sum(maps_scored, "A", "speed25_pos","A_speed25_pos_sum")
A_speed_spec_pos_sum <- subscale_sum(maps_scored, "A", "speed_spec_pos","A_speed_spec_pos_sum")
A_signs_pos_sum <- subscale_sum(maps_scored, "A", "signs_pos","A_signs_pos_sum")
A_streetscape_pos_sum <- subscale_sum(maps_scored, "A", "streetscape_pos","A_streetscape_pos_sum")
A_sidewalk_sum <- subscale_sum(maps_scored, "A", "sidewalk","A_sidewalk_sum")
A_sidewalk_cont_sum <- subscale_sum(maps_scored, "A", "sidewalk_cont","A_sidewalk_cont_sum")
A_sidewalk_width_sum <- subscale_sum(maps_scored, "A", "sidewalk_width","A_sidewalk_width_sum")
A_sidewalk_pos_sum <- subscale_sum(maps_scored, "A", "sidewalk_pos","A_sidewalk_pos_sum")
A_buffer_pos_sum <- subscale_sum(maps_scored, "A", "buffer_pos","A_buffer_pos_sum")
A_bike_pos_sum <- subscale_sum(maps_scored, "A", "bike_pos","A_bike_pos_sum")
A_trees_pos_sum <- subscale_sum(maps_scored, "A", "trees_pos","A_trees_pos_sum")
A_awning_pos_sum <- subscale_sum(maps_scored, "A", "awning_pos","A_awning_pos_sum")
A_shade_pos_sum <- subscale_sum(maps_scored, "A", "shade_pos","A_shade_pos_sum")
A_overall_pos_sum <- subscale_sum(maps_scored, "A", "overall_pos","A_overall_pos_sum")
A_speed25_neg_sum <- subscale_sum(maps_scored, "A", "speed25_neg","A_speed25_neg_sum")
A_driveways_neg_sum <- subscale_sum(maps_scored, "A", "driveways_neg","A_driveways_neg_sum")
A_streetscape_neg_sum <- subscale_sum(maps_scored, "A", "streetscape_neg","A_streetscape_neg_sum")
A_cont_neg_sum <- subscale_sum(maps_scored, "A", "cont_neg","A_cont_neg_sum")
A_trip_neg_sum <- subscale_sum(maps_scored, "A", "trip_neg","A_trip_neg_sum")
A_sidewalk_neg_sum <- subscale_sum(maps_scored, "A", "sidewalk_neg","A_sidewalk_neg_sum")
A_aes_neg_sum <- subscale_sum(maps_scored, "A", "aes_neg","A_aes_neg_sum")
A_overall_neg_sum <- subscale_sum(maps_scored, "A", "overall_neg","A_overall_neg_sum")
A_overall_sum <- subscale_sum(maps_scored, "A", "overall","A_overall_sum")

# combine all school access segment sum data frames
A_sums_all <- cbind(A_calming_pos_sum, 
                    A_lights_pos_sum,
                    A_driveways_pos_sum,
                    A_speed_dichot_sum,
                    A_speed25_pos_sum,
                    A_speed_spec_pos_sum,
                    A_signs_pos_sum,
                    A_streetscape_pos_sum,
                    A_sidewalk_sum,
                    A_sidewalk_cont_sum,
                    A_sidewalk_width_sum,
                    A_sidewalk_pos_sum,
                    A_buffer_pos_sum,
                    A_bike_pos_sum,
                    A_trees_pos_sum,
                    A_awning_pos_sum,
                    A_shade_pos_sum,
                    A_overall_pos_sum,
                    A_speed25_neg_sum,
                    A_driveways_neg_sum,
                    A_streetscape_neg_sum,
                    A_cont_neg_sum,
                    A_trip_neg_sum,
                    A_sidewalk_neg_sum,
                    A_aes_neg_sum,	
                    A_overall_neg_sum,
                    A_overall_sum)

# filter for just the sum variables
A_sums <- A_sums_all %>%
  select(ends_with("sum"))

# Scoring for segments ----
# segments subscale sums
S_calming_pos_sum <- subscale_sum(maps_scored, "S", "calming_pos","S_calming_pos_sum")
S_lights_pos_sum <- subscale_sum(maps_scored, "S", "lights_pos","S_lights_pos_sum")
S_driveways_pos_sum <- subscale_sum(maps_scored, "S", "driveways_pos","S_driveways_pos_sum")
S_speed_dichot_sum <- subscale_sum(maps_scored, "S", "speed_dichot","S_speed_dichot_sum")
S_speed25_pos_sum <- subscale_sum(maps_scored, "S", "speed25_pos","S_speed25_pos_sum")
S_speed_spec_pos_sum <- subscale_sum(maps_scored, "S", "speed_spec_pos","S_speed_spec_pos_sum")
S_signs_pos_sum <- subscale_sum(maps_scored, "S", "signs_pos","S_signs_pos_sum")
S_streetscape_pos_sum <- subscale_sum(maps_scored, "S", "streetscape_pos","S_streetscape_pos_sum")
S_sidewalk_sum <- subscale_sum(maps_scored, "S", "sidewalk","S_sidewalk_sum")
S_sidewalk_cont_sum <- subscale_sum(maps_scored, "S", "sidewalk_cont","S_sidewalk_cont_sum")
S_sidewalk_width_sum <- subscale_sum(maps_scored, "S", "sidewalk_width","S_sidewalk_width_sum")
S_sidewalk_pos_sum <- subscale_sum(maps_scored, "S", "sidewalk_pos","S_sidewalk_pos_sum")
S_buffer_pos_sum <- subscale_sum(maps_scored, "S", "buffer_pos","S_buffer_pos_sum")
S_bike_pos_sum <- subscale_sum(maps_scored, "S", "bike_pos","S_bike_pos_sum")
S_trees_pos_sum <- subscale_sum(maps_scored, "S", "trees_pos","S_trees_pos_sum")
S_awning_pos_sum <- subscale_sum(maps_scored, "S", "awning_pos","S_awning_pos_sum")
S_shade_pos_sum <- subscale_sum(maps_scored, "S", "shade_pos","S_shade_pos_sum")
S_overall_pos_sum <- subscale_sum(maps_scored, "S", "overall_pos","S_overall_pos_sum")
S_speed25_neg_sum <- subscale_sum(maps_scored, "S", "speed25_neg","S_speed25_neg_sum")
S_driveways_neg_sum <- subscale_sum(maps_scored, "S", "driveways_neg","S_driveways_neg_sum")
S_streetscape_neg_sum <- subscale_sum(maps_scored, "S", "streetscape_neg","S_streetscape_neg_sum")
S_cont_neg_sum <- subscale_sum(maps_scored, "S", "cont_neg","S_cont_neg_sum")
S_trip_neg_sum <- subscale_sum(maps_scored, "S", "trip_neg","S_trip_neg_sum")
S_sidewalk_neg_sum <- subscale_sum(maps_scored, "S", "sidewalk_neg","S_sidewalk_neg_sum")
S_aes_neg_sum <- subscale_sum(maps_scored, "S", "aes_neg","S_aes_neg_sum")
S_overall_neg_sum <- subscale_sum(maps_scored, "S", "overall_neg","S_overall_neg_sum")
S_overall_sum <- subscale_sum(maps_scored, "S", "overall","S_overall_sum")

# combine all segment sum data frames
S_sums_all <- cbind(S_calming_pos_sum, 
                    S_lights_pos_sum,
                    S_driveways_pos_sum,
                    S_speed_dichot_sum,
                    S_speed25_pos_sum,
                    S_speed_spec_pos_sum,
                    S_signs_pos_sum,
                    S_streetscape_pos_sum,
                    S_sidewalk_sum,
                    S_sidewalk_cont_sum,
                    S_sidewalk_width_sum,
                    S_sidewalk_pos_sum,
                    S_buffer_pos_sum,
                    S_bike_pos_sum,
                    S_trees_pos_sum,
                    S_awning_pos_sum,
                    S_shade_pos_sum,
                    S_overall_pos_sum,
                    S_speed25_neg_sum,
                    S_driveways_neg_sum,
                    S_streetscape_neg_sum,
                    S_cont_neg_sum,
                    S_trip_neg_sum,
                    S_sidewalk_neg_sum,
                    S_aes_neg_sum,	
                    S_overall_neg_sum,
                    S_overall_sum)
# filter for just the sum variables
S_sums <- S_sums_all %>%
  select(ends_with("sum"))

# Scoring for crossings ----
# Crossing subscale sums
C_amenities_pos_sum <- subscale_sum(maps_scored, "C", "amenities_pos","C_amenities_pos_sum")
C_pre_curb_pos_sum <- subscale_sum(maps_scored, "C", "pre_curb_pos","C_pre_curb_pos_sum")
C_post_curb_pos_sum <- subscale_sum(maps_scored, "C", "post_curb_pos","C_post_curb_pos_sum")
C_curb_qual_pos_sum <- subscale_sum(maps_scored, "C", "curb_qual_pos","C_curb_qual_pos_sum")
C_control_pos_sum <- subscale_sum(maps_scored, "C", "control_pos","C_control_pos_sum")
C_overall_pos_sum <- subscale_sum(maps_scored, "C", "overall_pos","C_overall_pos_sum")
C_width_sum <- subscale_sum(maps_scored, "C", "width","C_width_sum")
C_pre_curb_neg_sum <- subscale_sum(maps_scored, "C", "pre_curb_neg","C_pre_curb_neg_sum")
C_post_curb_neg_sum <- subscale_sum(maps_scored, "C", "post_curb_neg","C_post_curb_neg_sum")
C_imped_sum <- subscale_sum(maps_scored, "C", "imped","C_imped_sum")
C_overall_neg_sum <- subscale_sum(maps_scored, "C", "overall_neg","C_overall_neg_sum")
C_overall_sum <- subscale_sum(maps_scored, "C", "overall","C_overall_sum")

# combine all crossing sum data frames
C_sums_all <- cbind(C_amenities_pos_sum,
                    C_pre_curb_pos_sum,
                    C_post_curb_pos_sum,
                    C_curb_qual_pos_sum,
                    C_control_pos_sum,
                    C_overall_pos_sum,
                    C_width_sum,
                    C_pre_curb_neg_sum,
                    C_post_curb_neg_sum,
                    C_imped_sum,
                    C_overall_neg_sum,
                    C_overall_sum)

# filter for just the sum variables
C_sums <- C_sums_all %>%
  select(ends_with("sum"))

# Add sum scores back to main data frame and export data ----
# remove duplicate variables
maps_sum_with_dup <- cbind(maps_scored, A_sums, S_sums, C_sums)
maps_sum_unique <- maps_sum_with_dup[!duplicated(as.list(maps_sum_with_dup))]

# create overall MAPS-SRTS score
maps_sum_unique <- maps_sum_unique %>%
  mutate(total_MAPS_score = A_overall_sum + S_overall_sum + C_overall_sum)

# export processed data
write.csv(maps_sum_unique, here("data/processed_data", "maps_srts_scored.csv"))
