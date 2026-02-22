
################################################### H2. Slogan Memory  #################################

slogans_with_repetition <- c("v_276", "v_278", "v_289", "v_315", "v_316", "v_328")
slogans_without_repetition <- c("v_280", "v_281", "v_294", "v_311", "v_313", "v_323")


data_long <- data_project_main %>%
  select(lfdn, all_of(c(slogans_with_repetition, slogans_without_repetition))) %>%
  pivot_longer(cols = -lfdn, names_to = "slogan", values_to = "memory")

data_long <- data_long %>%
  mutate(repetition = ifelse(slogan %in% slogans_with_repetition, 1, 0),
         memory = as.factor(memory))  # Ensure memory is a factor

str(data_long)


data_long$memory <- relevel(data_long$memory, ref = "2") # 1 = remembered, 2 = not remembered)

model <- glm(memory ~ repetition, data = data_long, family = binomial) # Fit logistic regression model

summary(model)



data_long$memory_numeric <- ifelse(data_long$memory == "1", 1, 0)
#mean and standard deviation 
descriptive_stats <- data_long %>% 
  group_by(repetition) %>%
  summarise(
    mean_memory = mean(memory_numeric, na.rm = TRUE),  
    sd_memory = sd(memory_numeric, na.rm = TRUE),
    n = n()
  )

#print(descriptive_stats)

####################################
descriptive_stats <- descriptive_stats %>%
  mutate(
    se_memory = sd_memory / sqrt(n),  # Standard Error
    lower_CI = mean_memory - 1.96 * se_memory,
    upper_CI = mean_memory + 1.96 * se_memory
  )
print(descriptive_stats)


# To visualize
ggplot(descriptive_stats, aes(x = factor(repetition, labels = c("No Repetition", "Repetition")), 
                              y = mean_memory, fill = factor(repetition))) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.2) + 
  scale_fill_manual(values = c("lightgray", "black")) +  
  labs(title = "Effect of Word Repetition on Slogan Memory",
       x = "Condition",
       y = "Proportion Remembered") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(size = 14))


###########
table_memory <- table(data_long$repetition, data_long$memory_numeric)
print(table_memory)

#Chi-Square test
chi_test <- chisq.test(table_memory)
print(chi_test)


##################################H4: Processing fluency/mediation/Memory slogan 
data_long <- merge(data_long, data_project_main[, c("lfdn", 
                                                    "v_244", "v_246", "v_248", "v_250", "v_252", "v_254", 
                                                    "v_256", "v_258", "v_260", "v_262", "v_264", "v_266", 
                                                    "v_245", "v_247", "v_249", "v_251", "v_253", "v_255", 
                                                    "v_257", "v_259", "v_261", "v_263", "v_265", "v_267")], 
                   by = "lfdn", all.x = TRUE)

data_long$fluency_composite <- rowMeans(data_long[, c("v_244", "v_246", "v_248", "v_250", "v_252", "v_254", 
                                                      "v_256", "v_258", "v_260", "v_262", "v_264", "v_266", 
                                                      "v_245", "v_247", "v_249", "v_251", "v_253", "v_255", 
                                                      "v_257", "v_259", "v_261", "v_263", "v_265", "v_267")], 
                                        na.rm = TRUE)


# 1. Total Effect (c)
model_total <- glm(memory_numeric ~ repetition, family = binomial, data = data_long)
summary(model_total)  

# 2: Assess IV - Mediator Relationship (a)
model_a <- lm(fluency_composite ~ repetition, data = data_long)
summary(model_a)  

# 3: Testing of Mediator and IV Effects (b and c')
model_b <- glm(memory_numeric ~ fluency_composite + repetition, family = binomial, data = data_long)
summary(model_b)  

#  4: Causal Mediation Analysis
mediation_model <- mediate(model_a, model_b, treat = "repetition", mediator = "fluency_composite", boot = TRUE, sims = 5000)
summary(mediation_model)  



#####################################################Demographics 
demographics <- data_project_main %>%
  mutate(
    lfdn = as.character(lfdn),
    mother_language = ifelse(v_94 == 1, "English", ifelse(v_95 == 1, "Other", NA)),
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

data_long$lfdn <- as.character(data_long$lfdn)

data_long <- left_join(data_long, demographics %>% select(lfdn, age, gender, education, mother_language), 
                       by = "lfdn")

# Logistic regression 
model_total <- glm(memory_numeric ~ repetition + age + gender + education + mother_language, 
                   family = binomial, data = data_long)
summary(model_total)  



#######visualzation
data_long$predicted_memory <- predict(model_total, type = "response")

ggplot(data_long, aes(x = repetition, y = predicted_memory, color = age)) +
  geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, aes(group = age), size = 1) +
  labs(
    title = "Effect of Word Repetition on Slogan Memory by Age Group",
    x = "Repetition (0 = No, 1 = Yes)",
    y = "Predicted Probability of Remembering Slogan",
    color = "Age Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggplot(data_long, aes(x = repetition, y = predicted_memory, color = gender)) +
  geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, aes(group = gender), size = 1) +
  labs(
    title = "Effect of Word Repetition on Slogan Memory by Gender",
    x = "Repetition (0 = No, 1 = Yes)",
    y = "Predicted Probability of Remembering Slogan",
    color = "Gender"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggplot(data_long, aes(x = repetition, y = predicted_memory, color = education)) +
  geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, aes(group = education), size = 1) +
  labs(
    title = "Effect of Word Repetition on Slogan Memory by Education Level",
    x = "Repetition (0 = No, 1 = Yes)",
    y = "Predicted Probability of Remembering Slogan",
    color = "Education Level"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggplot(data_long, aes(x = repetition, y = predicted_memory, color = mother_language)) +
  geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, aes(group = mother_language), size = 1) +
  labs(
    title = "Effect of Word Repetition on Slogan Memory by Mother Language",
    x = "Repetition (0 = No, 1 = Yes)",
    y = "Predicted Probability of Remembering Slogan",
    color = "Mother Language"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )





