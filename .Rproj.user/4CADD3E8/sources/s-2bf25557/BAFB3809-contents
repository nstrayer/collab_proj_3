library(tidyverse)

data <- read_csv('delineate_students.csv')

data_dict <- read_csv('data_dictionary.csv') %>% 
  select(
    variable = `Variable / Field Name`, 
    description = `Field Label`
  ) %>% 
  mutate(
    description = ifelse(variable == 'sex', 'Sex = Male', description),
    variable = str_remove_all(variable, ' ')
  ) %>% 
  bind_rows(
    data_frame(
      variable = 'homemed_steroids',
      description = 'Taking Steroids'
    )
  )

data_gathered <- data %>% 
  gather(variable, value) %>% 
  left_join(data_dict, by = 'variable')



continuous_vars <- c( 
  "age",              "charlson",              "hosp_los",              "il6", "il8",      "iqcode6mo", "iqcode_score", "mod_apache",           
  "mod_apache_noage", "OARS_ADL_ED_0h_recode", "OARS_ADL_ED_6m_recode", "PAI", "proteinC", "s100b",     "sTNFR1"               
)
# These have highly skewed distributions and thus we log transform them to get some normality. 
log_transform_vars <- c("il6", "il8", "sTNFR1")


binary_vars <- c(
  "delirium_0h",        "dementia_lib",           "ed_delirium",        "etiol4_cns_dich1", "etiol4_drug_dich1", "etiol4_infection_sys_dich1",
  "etiol4_metab_dich1", "etiol4_organ_dys_dich1", "etiol4_other_dich1", "homemed_steroids", "ltc",               "nonwhite",                  
  "sex"    
)

continuous_data <- data_gathered %>% 
  filter(variable %in% continuous_vars) %>% 
  mutate(
    value =       ifelse(variable %in% log_transform_vars, log2(value), value),
    description = ifelse(variable %in% log_transform_vars, paste(description, '| log'), description)
  ) 

continuous_data %>% 
  ggplot(aes(value)) +
  geom_density(fill = 'steelblue', color = 'steelblue') +
  geom_text(aes(x = value, y = 0), label = '|', alpha = 0.5, hjust = 0.5, vjust = 1) +
  facet_wrap(~description, scales = 'free', ncol = 3) +
  theme_minimal()


binary_data <- data_gathered %>% 
  filter(variable %in% binary_vars) %>% 
  mutate(value = as.integer(value))


binary_arranged <- binary_data %>% 
  group_by(description) %>% 
  mutate(num_yes = sum(value)) %>% 
  ungroup() %>% 
  arrange(num_yes) %>% 
  mutate(description = fct_inorder(factor(description)))
  
legend_data <- binary_arranged %>% 
  group_by(description) %>% 
  summarise(num_yes = first(num_yes)) %>% 
  ungroup() %>% 
  tail(1) %>% {
    new_df <- rbind(.,.)
    new_df$num_yes[1] = 0
    new_df
  } %>% 
  mutate(label = c('yes', 'no'))

binary_arranged %>%  
  mutate(value = ifelse(value == 1, 'yes', 'no')) %>% 
  ggplot(aes(x = description)) +
  geom_bar(aes(fill = value), position = 'stack') +
  geom_text(data = legend_data, aes(y = num_yes, label = label), hjust = 0, nudge_y = 5) +
  coord_flip() +
  guides(fill = FALSE) +
  labs(x = '')






