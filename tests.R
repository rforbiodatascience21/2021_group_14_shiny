library(tidyverse)

diabetes <- read_csv("/cloud/project/diabetes_app/data/diabetes.csv")

diabetes_clean <- diabetes %>%
  mutate(height = round(height * 2.54, 1), weight = round(weight / 2.205, 1)) %>%
  mutate(BMI = weight/((height/100)^2)) %>%
  mutate(BMI_class = case_when(BMI<18.5 ~ 'Underweight', 
                               18.5<=BMI & BMI<25 ~ 'Normal', 
                               25<=BMI & BMI<30 ~ 'Overweight', 
                               30<=BMI & BMI<35 ~ 'Severe Obesity', 
                               35<=BMI & BMI<40 ~ 'Morbid Obesity', 
                               BMI>=40 ~ 'Super Obesity')) %>%
  mutate(BMI_class = factor(BMI_class,
                            levels =  c('Underweight', 'Normal', 'Overweight', 'Severe Obesity', 'Morbid Obesity', 'Super Obesity')))

diabetes_clean %>%
  ggplot(aes(x=weight, y=height)) +
  geom_point()

