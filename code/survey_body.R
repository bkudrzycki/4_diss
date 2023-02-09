## ---- tbl-desc ----

df <- ys_panel %>% filter(wave == 0) %>% zap_labels() %>% 
  mutate(yos = ifelse(YS3_15 != 99, YS3_15, 0),
         app = YS3_13,
         cep = YS3_17_2,
         bepc = YS3_17_4,
         bac = YS3_17_6,
         cap = YS3_17_8,
         licence = YS3_17_11,
         master = YS3_17_12,
         fathapp = YS3_9,
         fath_primary = ifelse(YS3_10 > 2, 1, 0),
         fsecplus = ifelse(YS3_10 > 4 & YS3_10 != 10, 1, 0),
         mothapp = YS3_11,
         moth_primary = ifelse(YS3_12 > 2, 1, 0),
         msecplus = ifelse(YS3_12 > 4 & YS3_12 != 10, 1, 0),
         married = ifelse(YS3_6 == 1, 1, 0),
         withparents = ifelse(as.numeric(YS6_1) == 3, 1, 0),
         beninese = YS3_1,
         fon = ifelse(!is.na(YS3_4_4), 1, 0),
         christian = ifelse(!is.na(YS3_5_1) | !is.na(YS3_5_2) | !is.na(YS3_5_3) | !is.na(YS3_5_4), 1, 0),
         city = ifelse(YS3_3 == 4, 1, 0),
         total = 1) %>% 
  mutate(status = recode(status, "Self-Employed" = "Self-**\n**Employed"))

df %>% 
  tbl_summary(
    by=status, 
    # summarize a subset of the columns
    include = c(total, sex, baseline_age, beninese, fon, christian, city, graduation_age, first_employment_age, first_employment_duration, yos, app, cap, cep, bepc, bac, licence, master, fathapp, fath_primary, fsecplus, mothapp, moth_primary, msecplus, married, withparents, YS3_8, YS6_6, wealth_quintile, YS6_2,  YS6_11_1, YS6_11_2, YS6_11_5, YS6_11_8, status),
    missing = "no",
    # adding labels to table
    label = list(total = "N",
                 sex = "Male (=1)",
                 graduation_age = "Graduation age",
                 first_employment_age = "Age at first employment",
                 first_employment_duration = "Duration of transition in years¹",
                 yos = "Years of schooling",
                 app = "Completed apprenticeship (=1)",
                 cap = "Vocational certificate: CAP (=1)",
                 cep = "Primary diploma: CEP (=1)",
                 bepc = "Junior high diploma: BEPC (=1)",
                 bac = "Baccalauréat: BAC (=1)",
                 licence = "2nd cycle university: Licence (=1)",
                 master = "3rd cycle university: Maîtrise (=1)",
                 fathapp = "Father was an apprentice (=1)",
                 fath_primary = "Father completed primary (=1)",
                 fsecplus = "Father completed secondary (=1)",
                 mothapp = "Mother was an apprentice (=1)",
                 moth_primary = "Mother completed primary (=1)",
                 msecplus = "Mother completed secondary (=1)",
                 married = "Married (=1)",
                 withparents = "Living with parents (=1)",
                 YS3_8 = "No. of children",
                 YS6_6  = "People in household",
                 beninese = "Nationality: Beninese (=1)",
                 fon = "Ethnicity: Fon (=1)",
                 christian = "Religion: Christian (=1)",
                 city = "Grew up in a city (=1)",
                 wealth_quintile = "Wealth index quintile",
                 YS6_2 = "Home electrified (=1)",
                 YS6_11_1 = "Cell Phone (=1)",
                 YS6_11_2 = "Smartphone (=1)",
                 YS6_11_5 = "Motorcycle (=1)",
                 YS6_11_8 = "Television (=1)"),
    type = list(c(YS3_8, YS6_6, first_employment_duration, wealth_quintile) ~ "continuous",
                c(app, cep, bepc, bac, cap, licence, master, fathapp, fath_primary, fsecplus, mothapp, moth_primary) ~ "dichotomous"),
    statistic = list(all_categorical() ~ "{p}%",
                     all_continuous() ~ "{mean}",
                     total ~ "{N}")
  ) %>%  
  add_p() %>% 
  modify_header(update = all_stat_cols() ~  "**{level}**\n({round(p, 2)*100}%)") %>% 
  modify_spanning_header(c("stat_1", "stat_2", "stat_3", "stat_4", "stat_5") ~ "**Baseline Activity**") %>% 
  add_overall(col_label = "**Overall**") %>% 
  modify_footnote(update = everything() ~ NA) %>% 
  as_kable_extra(caption = "Descriptive Statistics by Baseline Activity",
                 booktabs = T,
                 linesep = "",
                 position = "H") %>%
  kableExtra::group_rows(start_row = 8,
                         end_row = 10,
                         group_label = "Employment Status") %>% 
  kableExtra::group_rows(start_row = 11,
                         end_row = 18,
                         group_label = "Education") %>% 
  kableExtra::group_rows(start_row = 19,
                         end_row = 25,
                         group_label = "Parents' Education") %>% 
  kableExtra::group_rows(start_row = 26,
                         end_row = 34,
                         group_label = "Household Characteristics and Assets") %>% 
  footnote(general = "\\\\scriptsize{Mean; \\\\%. Calculated using responses from baseline survey.}",
           number = c("To first employment."),
           threeparttable = T,
           escape = F,
           general_title = "") %>% 
  kableExtra::kable_styling(latex_options="scale_down", full_width = FALSE) %>%
  column_spec(2:7, width = "5em")