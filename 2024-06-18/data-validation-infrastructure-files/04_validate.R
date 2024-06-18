# 1. Load the packages ########################################################
library(readxl)
library(validate)
library(yaml)
library(dplyr)

# 2. Load the data, validation rules, and code list ###########################
## 2.1 Load the data ----------------------------------------------------------
INEQUALITY_SIMD <- readxl::read_excel(
  path = "01_scotlands-population-2022.xlsx"
  , sheet = "INEQUALITY_SIMD"
  , skip = 4)

## 2.2 Load the validation rules -----------------------------------------------
rules <- validate::validator(.data = read.csv(file = "03_rules.csv"))

## 2.3 View the validation rules -----------------------------------------------
View(as.data.frame(rules))

## 2.4 Load the code list ------------------------------------------------------
code_list <- yaml::yaml.load_file(input = "02_code_list.yml")

## 2.5 View the code list ------------------------------------------------------
View(code_list)

# 3. Confront the data with the validation rules ##############################
validation <- validate::confront(
  dat = INEQUALITY_SIMD
  , x = rules
  , key = "id")

## 3.1 View a summary of the validation ---------------------------------------
validation %>% 
  summary() %>% 
  View()

## 3.2 View the results of the validation -------------------------------------
validation %>% 
  as.data.frame() %>% 
  View()

## 3.3 View all errors found --------------------------------------------------
validation %>% 
  as.data.frame() %>% 
  filter(value == FALSE) %>% 
  View()

