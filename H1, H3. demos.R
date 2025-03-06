install.packages("tidyverse")   
library(tidyverse)
library(lme4)
library(ggplot2)
install.packages("reshape2")
library(reshape2)
install.packages("dplyr") 
library(dplyr)
install.packages("car")
library(car)
install.packages("mediation")
install.packages("lavaan")
install.packages("semPlot")  
library(mediation)
library(lavaan)
library(semPlot)


data_project_main<- read.csv2("C:/Users/Azam/Documents/Goethe Uni/WiSe 24-25/Thesis process/Data Analysis/Mian data 08,02,2025/data_project_main.csv")
str(data_project_main) 
head(data_project_main)
str(data_project_main)
head(data_project_main)
data_project_main[data_project_main == -77] <- NA
head(data_project_main)


###############################################H1: Slogan Liking##################################
#  liking variables 
liking_with_repetition <- c("v_41", "v_46", "v_48", "v_50", "v_52", "v_54", 
                            "v_233", "v_223", "v_225", "v_227", "v_229", "v_231")

liking_without_repetition <- c("v_42", "v_47", "v_49", "v_51", "v_53", "v_55", 
                               "v_243", "v_224", "v_226", "v_228", "v_230", "v_232")

#  fluency variables 
fluency_with_repetition <- c("v_244", "v_246", "v_248", "v_250", "v_252", "v_254", 
                             "v_256", "v_258", "v_260", "v_262", "v_264", "v_266")

fluency_without_repetition <- c("v_245", "v_247", "v_249", "v_251", "v_253", "v_255", 
                                "v_257", "v_259", "v_261", "v_263", "v_265", "v_267")


liking_score <- rowMeans(data_project_main[, c(liking_with_repetition, liking_without_repetition)], na.rm = TRUE)

slogan_type <- ifelse(rowMeans(!is.na(data_project_main[, fluency_with_repetition])) > 
                        rowMeans(!is.na(data_project_main[, fluency_without_repetition])),
                      "With_Repetition", "Without_Repetition")

slogan_type <- factor(slogan_type, levels = c("With_Repetition", "Without_Repetition"))

data_project_main$liking_score <- liking_score
data_project_main$slogan_type <- slogan_type

data_project_main$fluency_with_rep <- rowMeans(data_project_main[, fluency_with_repetition], na.rm = TRUE)
data_project_main$fluency_without_rep <- rowMeans(data_project_main[, fluency_without_repetition], na.rm = TRUE)

data_project_main$fluency_score <- rowMeans(data_project_main[, c("fluency_with_rep", "fluency_without_rep")], na.rm = TRUE)

summary(data_project_main$fluency_score) # Check summary statistics 
summary(data_project_main$fluency_with_rep)
summary(data_project_main$fluency_without_rep)

# data frame for analysis
data_prepared <- data.frame(
  lfdn = data_project_main$lfdn,  
  liking_score = data_project_main$liking_score,
  slogan_type = data_project_main$slogan_type,
  fluency_with_rep = data_project_main$fluency_with_rep,
  fluency_without_rep = data_project_main$fluency_without_rep,
  fluency_score = data_project_main$fluency_score
)



with_repetition_data <- data_prepared[data_prepared$slogan_type == "With_Repetition", ]
without_repetition_data <- data_prepared[data_prepared$slogan_type == "Without_Repetition", ]


set.seed(123)
with_repetition_sample <- with_repetition_data[sample(nrow(with_repetition_data), 55), ]
without_repetition_sample <- without_repetition_data[sample(nrow(without_repetition_data), 60), ]
data_prepared_balanced <- rbind(with_repetition_sample, without_repetition_sample)

data_prepared_balanced <- data_prepared_balanced %>%
  select(lfdn, everything())  # `lfdn` is the first column

data_prepared_balanced$slogan_type <- factor(data_prepared_balanced$slogan_type, 
                                             levels = c("With_Repetition", "Without_Repetition"))


head(data_prepared_balanced)
#######################################################################################
# Calculate mean and standard deviation 
descriptive_stats_dplyr <- data_prepared_balanced %>%
  group_by(slogan_type) %>%
  summarise(mean_liking = mean(liking_score), sd_liking = sd(liking_score))

# Print results
print(descriptive_stats_dplyr)


#################################################check for assumption of ANOVA test
# 1.Shapiro-Wilk normality test for the balanced data
shapiro.test(data_prepared_balanced$liking_score) 
# 2.Levene's test for homogeneity of variances
leveneTest(liking_score ~ slogan_type, data = data_prepared_balanced) 

# Perform an independent samples t-test
#t_test_result <- t.test(liking_score ~ slogan_type, data = balanced_data, var.equal = TRUE)
#print(t_test_result)

# Histogram for balanced data
hist(data_prepared_balanced$liking_score, main = "Distribution of Liking Scores ",
     xlab = "Liking Score", col = "lightblue", breaks = 10)


# Q-Q plot for normality check
qqnorm(data_prepared_balanced$liking_score)
qqline(data_prepared_balanced$liking_score, col = "red")

############################# So the assumption are met: ANOVA  Test  ######################################################
anova_result <- aov(liking_score ~ slogan_type, data = data_prepared_balanced) 
summary(anova_result)

boxplot(liking_score ~ slogan_type, data = data_prepared_balanced, col = c("darkblue", "lightgreen"),# Boxplot for ANOVA visualization
        main = "Slogan Liking by Word Repetition", ylab = "Liking Score (1-7)", xlab = "Slogan Type")
###############################to investigate more 
t.test(liking_score ~ slogan_type, data = data_prepared_balanced, var.equal = TRUE)
library(effsize)
cohen.d(liking_score ~ slogan_type, data = data_prepared_balanced)



###############################################H3: Fluency mediation/ Slogan Liking##################################
total_effect <- lm(liking_score ~ slogan_type, data = data_prepared_balanced)
summary(total_effect)

a_path <- lm(fluency_score ~ slogan_type, data = data_prepared_balanced)
summary(a_path)

mediation_model <- lm(liking_score ~ slogan_type + fluency_score, data = data_prepared_balanced)
summary(mediation_model)

library(mediation)

mediation_analysis <- mediate(a_path, mediation_model, treat = "slogan_type", mediator = "fluency_score", boot = TRUE, sims = 5000)

summary(mediation_analysis)


#########################################################Demographics variables 

data_project_main <- data_project_main %>%
  mutate(
    mother_language = case_when(
      v_94 == 1 ~ "English",
      v_95 == 1 ~ "Other",
      TRUE ~ NA_character_
    ),
    
    age = case_when(
      v_78 == 1 ~ "Under 18",
      v_79 == 1 ~ "18–24",
      v_80 == 1 ~ "25–34",
      v_81 == 1 ~ "35–44",
      v_82 == 1 ~ "45–54",
      v_83 == 1 ~ "55 or older",
      TRUE ~ NA_character_
    ),
    
    gender = case_when(
      v_84 == 1 ~ "Female",
      v_85 == 1 ~ "Male",
      v_86 == 1 ~ "Prefer not to say",
      TRUE ~ NA_character_
    ),
    
    education = case_when(
      v_89 == 1 ~ "High school or less",
      v_90 == 1 ~ "High school diploma or equivalent",
      v_91 == 1 ~ "Bachelor’s degree",
      v_92 == 1 ~ "Master’s degree or higher",
      TRUE ~ NA_character_
    )
  )


data_prepared_balanced <- data.frame(
  lfdn = data_project_main$lfdn,  
  liking_score = data_project_main$liking_score,
  slogan_type = data_project_main$slogan_type,
  fluency_with_rep = data_project_main$fluency_with_rep,
  fluency_without_rep = data_project_main$fluency_without_rep,
  fluency_score = data_project_main$fluency_score,
  mother_language = factor(data_project_main$mother_language, levels = c("English", "Other")),
  #age = factor(data_project_main$age, levels = c("18–24", "25–34", "35–44", "45–54", "55 or older")),
  gender = factor(data_project_main$gender, levels = c("Female", "Male", "Prefer not to say")),
  education = factor(data_project_main$education, levels = c("High school or less", 
                                                             "High school diploma or equivalent", 
                                                             "Bachelor’s degree", 
                                                             "Master’s degree or higher"))
)


data_project_main <- data_project_main %>%
  mutate(
    # Assign numeric scores to 'mother_language' based on the 'v_94' and 'v_95' columns
    mother_language_score = case_when(
      v_94 == 1 ~ 1,   # English = 1
      v_95 == 1 ~ 2,   # Other = 2
      TRUE ~ NA_real_  # Missing values will be treated as NA
    ),
    
    # Assign numeric scores to 'age' based on the 'v_78' to 'v_83' columns
    age_score = case_when(
      v_78 == 1 ~ 1,   # Under 18 = 1
      v_79 == 1 ~ 2,   # 18–24 = 2
      v_80 == 1 ~ 3,   # 25–34 = 3
      v_81 == 1 ~ 4,   # 35–44 = 4
      v_82 == 1 ~ 5,   # 45–54 = 5
      v_83 == 1 ~ 6,   # 55 or older = 6
      TRUE ~ NA_real_  # Missing values will be treated as NA
    ),
    
    # Assign numeric scores to 'gender' based on the 'v_84' to 'v_86' columns
    gender_score = case_when(
      v_84 == 1 ~ 1,   # Female = 1
      v_85 == 1 ~ 2,   # Male = 2
      v_86 == 1 ~ 3,   # Prefer not to say = 3
      TRUE ~ NA_real_  # Missing values will be treated as NA
    ),
    
    # Assign numeric scores to 'education' based on the 'v_89' to 'v_92' columns
    education_score = case_when(
      v_89 == 1 ~ 1,   # High school or less = 1
      v_90 == 1 ~ 2,   # High school diploma or equivalent = 2
      v_91 == 1 ~ 3,   # Bachelor’s degree = 3
      v_92 == 1 ~ 4,   # Master’s degree or higher = 4
      TRUE ~ NA_real_  # Missing values will be treated as NA
    )
  )



data_project_main <- data_project_main %>% drop_na(mother_language_score, age_score, gender_score, education_score)
total_effect_with_controls <- lm(liking_score ~ slogan_type + mother_language_score + age_score + gender_score + education_score, 
                                 data = data_project_main)

#summary(total_effect_with_controls)


###############################################
plot_demographics <- function(data, demographic_var, title, color_var) {
  ggplot(data, aes(x = slogan_type, y = liking_score, color = !!sym(demographic_var))) +
    geom_boxplot(alpha = 0.7) +
    labs(
      title = title,
      x = "Slogan Type (Repetition vs. No Repetition)",
      y = "Liking Score",
      color = color_var
    ) +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "top"
    )
}

#####Visualisation

plot_demographics(data_project_main, "mother_language", "Liking Score by Slogan Type and Mother Language", "Mother Language")

plot_demographics(data_project_main, "age", "Liking Score by Slogan Type and Age Group", "Age Group")

plot_demographics(data_project_main, "gender", "Liking Score by Slogan Type and Gender", "Gender")

plot_demographics(data_project_main, "education", "Liking Score by Slogan Type and Education Level", "Education Level")

plot_predicted_liking <- function(data, demographic_var, title, color_var) {
  ggplot(data, aes(x = slogan_type, y = predicted_liking_score, color = !!sym(demographic_var))) +
    geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.6) +
    geom_smooth(method = "loess", se = FALSE, aes(group = !!sym(demographic_var)), size = 1) +
    labs(
      title = title,
      x = "Slogan Type (Repetition vs. No Repetition)",
      y = "Predicted Liking Score",
      color = color_var
    ) +
    theme_minimal() +
    theme(
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "top"
    )
}

plot_predicted_liking(data_project_main, "mother_language", "Predicted Liking Score by Slogan Type and Mother Language", "Mother Language")

plot_predicted_liking(data_project_main, "age", "Predicted Liking Score by Slogan Type and Age Group", "Age Group")

plot_predicted_liking(data_project_main, "gender", "Predicted Liking Score by Slogan Type and Gender", "Gender")

plot_predicted_liking(data_project_main, "education", "Predicted Liking Score by Slogan Type and Education Level", "Education Level")








