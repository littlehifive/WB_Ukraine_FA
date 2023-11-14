library(tidyverse)
path = "/Users/michaelfive/Library/CloudStorage/GoogleDrive-wuzezhen33@gmail.com/My Drive/WB Ukraine Study/Data/wave1"
mplus_path = "/Users/michaelfive/Desktop/R Directory/WB_Ukraine_FA/Mplus/wave1"

# DASS parent baseline -------------------------------------------------------------------------

dat_dass_b <- haven::read_dta(file.path(path, "baseline-dass-parents.dta"))

dat_dass_b <- dat_dass_b |> 
  filter(!is.na(treatment)) |> # remove rows with NAs in treatment
  # this should be named householdid instead because a household can have more than 1 kid
  rename(studentid = `_index`) |> 
  select(studentid, treatment, female, age_w, dass_q1:dass_q7)

names(dat_dass_b)[5:11] <- paste0("dass", 1:7, "_1")

set.seed(1234)
dat_dass_b$half <- NA
dat_dass_b$half[dat_dass_b$treatment == 1] <- sample(c(rep(0, 580), rep(1, 581)), 
                                                     size = 1161, 
                                                     replace = FALSE)

dat_dass_b$half[dat_dass_b$treatment == 0] <- sample(c(rep(0, 581), rep(1, 580)), 
                                                     size = 1161, 
                                                     replace = FALSE)

write_csv(dat_dass_b, file.path(path, "dass_parent_1.csv"))
write.table(dat_dass_b, file.path(mplus_path, "dass_parent/dass_parent_1.txt"), 
            col.names = F, row.names = F)
  

# Student data
dat_stu_b <- haven::read_dta(file.path(path, "baseline-ses-mental-health.dta"))

dat_stu_b <- dat_stu_b |> 
  filter(inphase1 == 1) |> # remove rows with NAs in treatment
  rename(index = `_index`, remote_difficult = reomote_difficult,
         self_efficacy = self_efficacy_Q1, edu_goals = edu_goals_Q1) |> 
  select(studentid, index, treatment, girl, age_w, child_grade, in_ukr, 
         guardian_living, guardian_region,
         grit_Q1:grit_Q8, locus_of_control_Q1:locus_of_control_Q4,
         anxiety_stress_Q1:anxiety_stress_Q14,
         sleep_Q1:sleep_Q7,
         self_efficacy, edu_goals, remote_difficult,
         time_class, time_homework
         )

names(dat_stu_b)[10:47] <- c(
  paste0("grit", 1:8, "_1"),
  paste0("loc", 1:4, "_1"),
  paste0("dass", 1:14, "_1"),
  paste0("sleep", 1:7, "_1"),
  "efficacy_1", "edugoals_1", "remotediff_1", "timeclass_1", "timehomework_1"
)
  
dat_stu_e <- haven::read_dta(file.path(path, "endline-ses-mental-health.dta"))

dat_stu_e <- dat_stu_e |> 
  filter(inphase1 == 1) |> # remove rows with NAs in treatment
  rename(index = `_index`, edu_goals = edu_goals_Q1) |> 
  select(studentid,
         grit_Q1:grit_Q8, locus_of_control_Q1:locus_of_control_Q4,
         anxiety_stress_Q1:anxiety_stress_Q14,
         self_efficacy, edu_goals,
         time_class, time_homework, completed_math
  )

names(dat_stu_e)[2:32] <- c(
  paste0("grit", 1:8, "_2"),
  paste0("loc", 1:4, "_2"),
  paste0("dass", 1:14, "_2"),
  "efficacy_2", "edugoals_2", "timeclass_2", "timehomework_2", "compmath_2"
)

# join baseline and endline by studentid
dat_stu <- dat_stu_b |> 
  left_join(dat_stu_e, by = "studentid") |> 
  arrange(studentid)

# create training and testing sets
set.seed(1234)
dat_stu$half <- NA
dat_stu$half[dat_stu$treatment == 1] <- sample(c(rep(0, 643), rep(1, 642)), 
                                                     size = 1285, 
                                                     replace = FALSE)

dat_stu$half[dat_stu$treatment == 0] <- sample(c(rep(0, 629), rep(1, 630)), 
                                                     size = 1259, 
                                                     replace = FALSE)

# reverse code items
dat_stu <- dat_stu |> 
  mutate_at(vars(grit2_1, grit3_1, grit4_1, grit5_1, grit7_1, grit8_1,
                 grit2_2, grit3_2, grit4_2, grit5_2, grit7_2, grit8_2),
            function(x){6 - x})

write_csv(dat_stu, file.path(path, "dat_stu.csv"))
write.table(dat_stu, file.path(mplus_path, "dat_stu.txt"), 
            col.names = F, row.names = F)

# tutor data
dat_tutor <- haven::read_dta(file.path(path, "tutors-ses-mental-health.dta")) |> 
  janitor::clean_names()

dat_tutor_s <- dat_tutor |> 
  select(tutorid,
         dass_q1:dass_q7,
         empathy_q1:empathy_q9,
         auto_q1:auto_q10,
         bias_q1:bias_q13,
         hard_work_q1:hard_work_q3,
         prosocial_q1:prosocial_q16,
         perceptions_q1:perceptions_q8d
         )

names(dat_tutor_s) <- paste0(gsub("_q", "", names(dat_tutor_s)),
                             "_1")

set.seed(1234)
dat_tutor_s$half <- sample(rep(c(0,1), each = 100), replace = FALSE)

# coding errors in empathy q3-q9
dat_tutor_s <- dat_tutor_s |> 
  mutate_at(vars(empathy3_1:empathy9_1), 
            function(x){
              case_when(
                x == 2 ~ 1,
                x == 3 ~ 2,
                x == 4 ~ 3,
                x == 5 ~ 4
              )}
            )
  

write_csv(dat_tutor_s, file.path(path, "dat_tutor.csv"))
write.table(dat_tutor_s, file.path(mplus_path, "dat_tutor.txt"), 
            col.names = F, row.names = F)
