library("here")
library("dplyr")
library("tidyr")
library("purrr")
library("fs")
library("readxl")
library("stringr")
library("janitor")
library("glue")

temp_files <- dir_ls(here("data", "placement_data_2425")) |> 
  as_vector() 

dat <- read_xls(temp_files[length(temp_files)]) |> 
  clean_names() |> 
  select(
    date = fl_test_date, 
    ruid = rutgers_id, 
    netid, 
    name = student_name, 
    language, 
    placement = fl_original_place, 
    current_place = fl_current_place
  ) |> 
  filter(language %in% c("Spanish", "Portuguese")) |> 
  mutate(
    placement = if_else(is.na(placement), current_place, placement), 
    placement = str_remove(placement, "940|810"), 
    email = glue("{netid}@scarletmail.rutgers.edu")
  )

glimpse(dat)

# what is CST?
dat$current_place |> unique()

dat |> 
  group_by(placement) |> 
  count()

# There are repeated rows
dat |> 
  filter(placement == "CST")

# Check for repeated names
dat |> 
  group_by(netid) |> 
  count() |> 
  filter(n > 1) 

# get fsh students for yeon soo
dat |> 
  filter(placement == "FSH", language == "Spanish") |> 
  pull(email) |> 
  str_flatten(collapse = "; ")

# get 201, 203 and FSH
dat |> 
  filter(
    language == "Spanish", 
    placement %in% c("201", "203", "FSH")
  ) |> 
  pull(email) |> 
  str_flatten(collapse = "; ")

# Get portuguese heritage (NVM there arent any)
dat |> 
  filter(language == "Portuguese")

# get all
dat |> 
  pull(email) |> 
  unique() |> 
  str_flatten(collapse = "; ")
