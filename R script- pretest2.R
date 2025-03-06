install.packages("tidyverse")   
library(tidyverse)

data_project <- read.csv2("C:/Users/Azam/Documents/Goethe Uni/WiSe 24-25/Thesis process/Data Analysis/Pre-test 2/data_project.csv")
str(data_project) 
head(data_project)
#==================================================== Cleaning data =======================================================
str(data_project)
data_project_clean <- data_project %>%
  filter(complete.cases(data_project[, c("v_1", "v_2", "v_3", "v_4", "v_5", "v_6", "v_7", "v_8", "v_9", "v_10")]))

#==================================================== t-Test ===============================================================
t_test_v1 <- t.test(data_project_clean$v_1, mu = 4)
t_test_v2 <- t.test(data_project_clean$v_2, mu = 4)
t_test_v3 <- t.test(data_project_clean$v_3, mu = 4)
t_test_v4 <- t.test(data_project_clean$v_4, mu = 4)
t_test_v5 <- t.test(data_project_clean$v_5, mu = 4)
t_test_v6 <- t.test(data_project_clean$v_6, mu = 4)
t_test_v7 <- t.test(data_project_clean$v_7, mu = 4)
t_test_v8 <- t.test(data_project_clean$v_8, mu = 4)
t_test_v9 <- t.test(data_project_clean$v_9, mu = 4)
t_test_v10 <- t.test(data_project_clean$v_10, mu = 4)

print(t_test_v1)
print(t_test_v2)
print(t_test_v3)
print(t_test_v4)
print(t_test_v5)
print(t_test_v6)
print(t_test_v7)
print(t_test_v8)
print(t_test_v9)
print(t_test_v10)
