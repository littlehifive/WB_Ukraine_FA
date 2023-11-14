library(tidyverse)
path = "/Users/michaelfive/Library/CloudStorage/GoogleDrive-wuzezhen33@gmail.com/My Drive/WB Ukraine Study/Data/wave2"
mplus_path = "/Users/michaelfive/Desktop/R Directory/WB_Ukraine_FA/Mplus/wave2"


# DASS parent midline -------------------------------------------------------------------------

dat_dass_1 <- haven::read_dta(file.path(path, "baseline-dass-parents-wave2.dta"))

dat_dass_1 <- dat_dass_1 |> 
  filter(!is.na(treatment)) |> # remove rows with NAs in treatment
  # this should be named householdid instead because a household can have more than 1 kid
  rename(studentid = `_index`) |> 
  select(studentid, treatment, female, age_w, dass_q1:dass_q7)

names(dat_dass_1)[5:11] <- paste0("dass", 1:7, "_1")

set.seed(1234)
dat_dass_1$half <- NA
dat_dass_1$half[dat_dass_1$treatment == 1] <- sample(c(rep(0, 644), rep(1, 644)), 
                                                     size = 1288, 
                                                     replace = FALSE)

dat_dass_1$half[dat_dass_1$treatment == 0] <- sample(c(rep(0, 643), rep(1, 644)), 
                                                     size = 1287, 
                                                     replace = FALSE)

write_csv(dat_dass_1, file.path(path, "dass_parent_1.csv"))
write.table(dat_dass_1, file.path(mplus_path, "dass_parent/dass_parent_1.txt"), 
            col.names = F, row.names = F)


# Student data

## Baseline
dat_stu_b <- haven::read_dta(file.path(path, "baseline-ses-mental-students-wave2.dta"))

dat_stu_b <- dat_stu_b |> 
  filter(!is.na(treatment)) |>
  rename(index = `_index`, edu_goals = edu_goals_Q1) |> 
  select(studentid, index, treatment, girl, age_w, child_grade, in_ukr, 
         guardian_living, guardian_region,
         grit_Q1:grit_Q8, locus_of_control_Q1:locus_of_control_Q4,
         anxiety_stress_Q1:anxiety_stress_Q14,
         sleep_Q1:sleep_Q7,edu_goals
  )

names(dat_stu_b)[10:42] <- c(
  paste0("grit", 1:8, "_1"),
  paste0("loc", 1:4, "_1"),
  paste0("dass", 1:14, "_1"),
  paste0("sleep", 1:7, "_1")
)


# create training and testing sets
set.seed(1234)
dat_stu_b$half <- NA
dat_stu_b$half[dat_stu_b$treatment == 1] <- sample(c(rep(0, 689), rep(1, 690)), 
                                               size = 1379, 
                                               replace = FALSE)

dat_stu_b$half[dat_stu_b$treatment == 0] <- sample(c(rep(0, 694), rep(1, 694)), 
                                               size = 1388, 
                                               replace = FALSE)

# reverse code items
dat_stu_b <- dat_stu_b |> 
  mutate_at(vars(grit2_1, grit3_1, grit4_1, grit5_1, grit7_1, grit8_1),
            function(x){6 - x})

write_csv(dat_stu_b, file.path(path, "dat_stu_1.csv"))
write.table(dat_stu_b, file.path(mplus_path, "dat_stu_1.txt"), 
            col.names = F, row.names = F)

## Endline
dat_stu_e <- haven::read_dta(file.path(path, "endline-ses-mental-health-wave2.dta"))

dat_stu_e <- dat_stu_e |> 
  select(studentid, girl, age_w,
         grit_Q1_old:grit_Q8_old,
         grit_Q1:grit_Q8, locus_of_control_Q1:locus_of_control_Q4,
         anxiety_stress_Q1:anxiety_stress_Q21
  )

names(dat_stu_e)[4:41] <- c(
  paste0("grito", c(1, 5:8), "_2"),
  paste0("grit", 1:8, "_2"),
  paste0("loc", 1:4, "_2"),
  paste0("dass", 1:21, "_2")
)

dat_stu_e <- dat_stu_e |> 
  left_join(dat_stu_b |> select(studentid, treatment), by = "studentid")

# create training and testing sets
set.seed(1234)
dat_stu_e$half <- NA
dat_stu_e$half[dat_stu_e$treatment == 1] <- sample(c(rep(0, 370), rep(1, 370)), 
                                                   size = 740, 
                                                   replace = FALSE)

dat_stu_e$half[dat_stu_e$treatment == 0] <- sample(c(rep(0, 314), rep(1, 314)), 
                                                   size = 628, 
                                                   replace = FALSE)

# reverse code items
dat_stu_e <- dat_stu_e |> 
  mutate_at(vars(grito5_2, grito7_2, grito8_2),
            function(x){6 - x})

write_csv(dat_stu_e, file.path(path, "dat_stu_2.csv"))
write.table(dat_stu_e, file.path(mplus_path, "dat_stu_2.txt"), 
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
dat_tutor_s$half <- sample(c(rep(0, 173), rep(1, 174)), replace = FALSE)

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

