library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(openxlsx)
library(readstata13)
library(plm)
library(ggplot2)
library(panelr)
library(MatchIt)
library(tableone)
library(finalfit)
library(descr)
library(smd)
library(ggpubr)



# data <- read.dta13('../wls_bl_14_03.dta')
# rm(list = setdiff(ls(), "data"))

data_cens <- data %>% 
  select(idpub,
         rtype,
         familypub,
         personid,
         selsibidpub,
         selsibtype,
         sibcount,
         birthy = z_brdxdy,
         sex = z_sexrsp,
         twinr,
         
         
         # Round 05 CESD questions
         # z_iu001rec, z_iu002re, 
         z_iu003rer, z_iu004rer, z_iu005rer, z_iu006rer, z_iu007rer, z_iu008rer, z_iu009rer, z_iu010rer, z_iu011rer, z_iu012rer, z_iu013rer, z_iu014rer, z_iu015rer, z_iu016rer, z_iu017rer, z_iu018rer, z_iu019rer, z_iu020rer, z_iu021rer, z_iu022rer,
         
         # Round 06 CESD questions
         #z_ju001rec, z_ju002re, 
         z_ju003rer, z_ju004rer, z_ju005rer, z_ju006rer, z_ju007rer, z_ju008rer, z_ju009rer, z_ju010rer, z_ju011rer, z_ju012rer, z_ju013rer, z_ju014rer, z_ju015rer, z_ju016rer, z_ju017rer, z_ju018rer, z_ju019rer, z_ju020rer, z_ju021rer, z_ju022rer,
         
         
         # Round 05
         r05_age = z_ga003re,
         # Depression, IBS
         r05_dep = z_iu001rec,
         r05_depre = z_iu002re,
         r05_ibs = z_ix148rer,
         r05_ibssev = z_ix149rer,
         # Demographic characteristics
         r05_smk_ever = z_ix012rer,
         r05_smk_now = z_ix013rec,
         r05_bmi = z_ix011rec,
         r05_height_inch = z_ix010rec,
         r05_weight_pound = z_ix009rer,
         r05_hyptns = z_gx341re,
         r05_dbts = z_gx342re,
         r05_chlstrl = z_ix146rer,
         r05_cncr = z_gx348re,
         r05_alcl = z_gu028re,
         r05_incm = z_gp250rec,
         r05_educ = z_gb001re,
         
         
         # Round 06
         r06_age = z_ha003re,
         # Depression, IBS
         r06_dep = z_ju001rec,
         r06_depre = z_ju002re,
         r06_ibs = z_jx148rer,
         r06_ibssev = z_jx149rer,
         # Demographic characteristics
         r06_smk_ever = z_jx012rer,
         r06_smk_now = z_jx013rec,
         r06_bmi = z_jx011rec,
         r06_height_inch = z_jx010rec,
         r06_weight_pound = z_jx009rer,
         r06_hyptns = z_hx341re,
         r06_dbts = z_hx342re,
         r06_chlstrl = z_jx146rer,
         r06_cncr = z_hx348re,
         r06_alcl = z_hu028re,
         r06_incm = z_hpu50rec,
         r06_educ = z_hb001re
  ) %>% 
  
  # 데이터 전처리
  mutate(
    across(c(r05_ibs, r06_ibs, r05_smk_ever, r05_smk_now, r05_hyptns, r05_dbts, r05_chlstrl, r05_cncr, r05_educ, r06_smk_ever, r06_smk_now, r06_hyptns, r06_dbts, r06_chlstrl, r06_cncr, r06_educ), ~ case_when(
      . == "yes" ~ 1,
      . == "no" ~ 2,
      TRUE ~ NA_real_
    )),
    across(c(r05_ibssev, r06_ibssev), ~ case_when(
      . == "NOT AT ALL"  ~ 1,
      . == "VERY LITTLE" ~ 2,
      . == "some" ~ 3,
      . == "QUITE A BIT" ~ 4,
      . == "A GREAT DEAL" ~ 5,
      TRUE ~ NA_real_
    )),
    across(matches("^z_(iu|mu)\\d{3}rer$"), ~ case_when(
      . == "Zero Days"  ~ 0,
      . == "One Day"    ~ 1,
      . == "Two Days"   ~ 2,
      . == "Three Days" ~ 3,
      . == "Four Days"  ~ 4,
      . == "Five Days"  ~ 5,
      . == "Six Days"   ~ 6,
      . == "Seven Days" ~ 7,
      TRUE ~ NA_real_
    )),
    sex = case_when(
      sex == "male" ~ 1,
      sex == "female" ~ 2,
      TRUE ~ NA_real_
    ),
    # NA 처리
    across(c(r05_bmi, r06_bmi, r05_weight_pound, r06_weight_pound, r05_height_inch, r06_height_inch, r05_alcl, r06_alcl, r05_incm, r06_incm), ~ if_else(
      . < 0, NA_real_,
      .)),
    
    # 개인 고유 id 배정
    idone = as.integer(paste0(familypub, case_when(
      rtype == "g" ~ "1",
      rtype == "s" ~ "2",
      TRUE ~ "0"  # rtype이 g 또는 s가 아닐 경우 대비
    ))),
    
    # 키, 몸무게 단위 변환
    r05_height_cm = r05_height_inch * 2.54,
    r06_height_cm = r06_height_inch * 2.54,
    r05_weight_kg = r05_weight_pound * 0.453592,
    r06_weight_kg = r06_weight_pound * 0.453592,
    
    # BMI, 음주습관, 소득수준 categorical 변수로 변환
    r05_bmi_ctg = case_when(
      r05_bmi <= 18.5 ~ "S",
      r05_bmi > 18.5 & r05_bmi <= 25 ~ "M",
      r05_bmi >= 25 ~ "L",
      TRUE ~ NA_character_
    ),
    r06_bmi_ctg = case_when(
      r06_bmi <= 18.5 ~ "S",
      r05_bmi > 18.5 & r05_bmi <= 25 ~ "M",
      r06_bmi > 25 ~ "L",
      TRUE ~ NA_character_
    ),
    r05_alcl_ctg = case_when(
      (sex == 1 & r05_alcl <= 60) | (sex == 2 & r05_alcl <= 30) ~ "M",
      (sex == 1 & r05_alcl > 60) | (sex == 2 & r05_alcl > 30) ~ "H",
      TRUE ~ NA_character_
    ),
    r06_alcl_ctg = case_when(
      (sex == 1 & r06_alcl <= 60) | (sex == 2 & r06_alcl <= 30) ~ "M",
      (sex == 1 & r06_alcl > 60) | (sex == 2 & r06_alcl > 30) ~ "H",
      TRUE ~ NA_character_
    ),
    r05_incm_ctg = case_when(
      r05_incm <= median(r05_incm, na.rm = TRUE) ~ "L",
      r05_incm > median(r05_incm, na.rm = TRUE) ~ "H",
      TRUE ~ NA_character_
    ),
    r06_incm_ctg = case_when(
      r06_incm <= median(r06_incm, na.rm = TRUE) ~ "L",
      r06_incm > median(r06_incm, na.rm = TRUE) ~ "H",
      TRUE ~ NA_character_
    ),
    
    # Smoking status
    r05_smk_stat = case_when(
      (r05_smk_ever == 2 & r05_smk_now == 2) ~ "Never",
      (r05_smk_ever == 1 & r05_smk_now == 1) ~ "Smoker",
      (r05_smk_ever == 1 & r05_smk_now == 2) ~ "Ex-smoker",
      TRUE ~ NA_character_
    ),
    r06_smk_stat = case_when(
      (r06_smk_ever == 2 & r06_smk_now == 2) ~ "Never",
      (r06_smk_ever == 1 & r06_smk_now == 1) ~ "Smoker",
      (r06_smk_ever == 1 & r06_smk_now == 2) ~ "Ex-smoker",
      TRUE ~ NA_character_
    )
  )


# CES-D score에 맞게 변환 #
# 대상 열 이름 정의
target_columns <- c(
  paste0("z_iu", sprintf("%03d", 3:22), "rer"),
  paste0("z_ju", sprintf("%03d", 3:22), "rer")
)
# 범위에 따라 값을 변환하는 함수 정의
transform_values <- function(x) {
  case_when(
    x == 0 ~ 0,
    x >= 1 & x <= 2 ~ 1,
    x >= 3 & x <= 4 ~ 2,
    x >= 5 & x <= 7 ~ 3,
    TRUE ~ NA_real_ # 다른 값은 NA로 처리
  )
}
# 열 변환 후 새 변수에 저장
data_cens <- data_cens %>%
  mutate(across(
    .cols = all_of(target_columns),
    .fns = ~ transform_values(as.numeric(.)), # 요인을 숫자로 변환 후 변환 규칙 적용
    .names = "{.col}c" # 새 변수명: 기존 이름 + "c"
  ))
# CESD 점수 계산
# Round 5
# 합산 대상 열 이름 정의
target_columns <- paste0("z_iu", sprintf("%03d", 3:22), "rerc")
# 열 값을 숫자로 변환 후 합산
data_cens <- data_cens %>%
  mutate(
    r05_cesd = ifelse(
      r05_depre >= 17, 
      rowSums(across(all_of(target_columns), ~ as.numeric(.)), na.rm = TRUE), 
      NA
    )
  )
# Round 6
# 합산 대상 열 이름 정의
target_columns <- paste0("z_ju", sprintf("%03d", 3:22), "rerc")
# 열 값을 숫자로 변환 후 합산
data_cens <- data_cens %>%
  mutate(
    r06_cesd = ifelse(
      r06_depre >= 17, 
      rowSums(across(all_of(target_columns), ~ as.numeric(.)), na.rm = TRUE), 
      NA
    )
  )
# CESD 점수 계산 끝 #

data_cens <- data_cens %>% 
  mutate(
    mean_cesd = ifelse(
      !is.na(r05_cesd) & !is.na(r06_cesd), 
      (r05_cesd + r06_cesd) / 2, 
      NA
    )) %>% 
  mutate(
    mean_ibssev = ifelse(
      !is.na(r05_ibssev) & !is.na(r06_ibssev), 
      (r05_ibssev + r06_ibssev) / 2, 
      NA
    )
  )

data_long <- data_cens %>%
  pivot_longer(cols = starts_with("r0"),
               names_to = c("round", ".value"),
               names_pattern = "r(\\d+)_(.*)") 


# 환자군 조건: (round == 5 & ibs == 1) and (round == 6 & ibs == 1)
case_ids <- data_long %>%
  filter((round == '05' & ibs == 1) | (round == '06' & ibs == 1)) %>%
  group_by(idpub, rtype) %>%
  summarise(valid_rounds = n_distinct(round), .groups = "drop") %>%
  filter(valid_rounds == 2) %>%
  select(idpub, rtype)

# 대조군 조건: (round == 5 & ibs == 2) and (round == 6 & ibs == 2)
control_ids <- data_long %>%
  filter((round == '05' & ibs == 2) | (round == '06' & ibs == 2)) %>%
  group_by(idpub, rtype) %>%
  summarise(valid_rounds = n_distinct(round), .groups = "drop") %>%
  filter(valid_rounds == 2) %>%
  select(idpub, rtype)

# 환자군 및 대조군 데이터 추출
case_group <- data_cens %>%
  semi_join(case_ids, by = c("idpub", "rtype"))

control_group <- data_cens %>%
  semi_join(control_ids, by = c("idpub", "rtype"))

# 환자군은 1, 대조군은 0으로 레이블 설정
case_group$group <- 1
control_group$group <- 0

# 환자군과 대조군 병합
matching_data <- rbind(case_group, control_group)

# 1:10 매칭 수행 (sex)
matched_sex <- matchit(group ~ sex, data = matching_data, method = "nearest", ratio = 10)

# 매칭된 데이터 추출
matched_sex_data <- match.data(matched_sex)


# 데이터 형식 변환 (wide to long data structure)
case_long <- case_group %>%
  pivot_longer(cols = starts_with("r0"),
               names_to = c("round", ".value"),
               names_pattern = "r(\\d+)_(.*)") 

control_long <- control_group %>%
  pivot_longer(cols = starts_with("r0"),
               names_to = c("round", ".value"),
               names_pattern = "r(\\d+)_(.*)") 


# Table 1 작성
# 범주형, 연속형 변수 분류
num_vars <- c("r05_age", "r06_age",
              "r05_height_cm", "r05_weight_kg", "r05_cesd", 
              "r06_height_cm", "r06_weight_kg", "r06_cesd")
cat_vars <- c("sex", 
              "r05_bmi_ctg", "r05_smk_stat", "r05_alcl_ctg", "r05_incm_ctg", "r05_educ", "r05_hyptns", "r05_dbts", "r05_chlstrl", "r05_cncr",
              "r06_bmi_ctg", "r06_smk_stat", "r06_alcl_ctg", "r06_incm_ctg", "r06_educ", "r06_hyptns", "r06_dbts", "r06_chlstrl", "r06_cncr")
all_vars <- c(num_vars, cat_vars)

# Table 작성
tab1 <- CreateTableOne(vars = all_vars, factorVars = cat_vars, strata = "group", data = matched_sex_data)

print(tab1, showAllLevels = TRUE, formatOptions = list(big.mark = ","), smd = TRUE)

tab1Mat <- print(tab1, showAllLevels = TRUE, formatOptions = list(big.mark = ","), quote = FALSE, noSpaces = TRUE, printToggle = FALSE, smd = TRUE)
write.csv(tab1Mat, file = "Table1.csv")



# Figure1: Boxplot
matched_sex_data$group <- factor(matched_sex_data$group, levels = c(1,0), labels = c("IBS", "Control"))

# 평균값 구하기
round05_mean_cesd <- matched_sex_data %>%
  group_by(group) %>%
  summarise(mean_cesd = mean(r05_cesd, na.rm = TRUE))
print(round05_mean_cesd)
# group   mean_cesd
# <fct>       <dbl>
#   1 IBS          19.2
# 2 Control      16.4
round06_mean_cesd <- matched_sex_data %>%
  group_by(group) %>%
  summarise(mean_cesd = mean(r06_cesd, na.rm = TRUE))
print(round06_mean_cesd)
# group   mean_cesd
# <fct>       <dbl>
#   1 IBS          19.3
# 2 Control      16.6
all_mean_cesd <- matched_sex_data %>%
  group_by(group) %>%
  summarise(mean_cesd = mean((r05_cesd + r06_cesd) / 2, na.rm = TRUE))
print(all_mean_cesd)
# group   mean_cesd
# <fct>       <dbl>
#   1 IBS          19.2
# 2 Control      16.5


# Boxplot 작성
ggplot(matched_sex_data, aes(x = group, y = r05_cesd, fill = group)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Case" = "gray", "Control" = "gray")) +
  labs(
    title = "Comparison of CES-D score Between Case and Control Groups, 2003-2005",
    x = "Group",
    y = "CES-D score on 2003-2005"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  )  #+    stat_compare_means(method = "t.test", label = "p.format") # 2.9e-14

ggplot(matched_sex_data, aes(x = group, y = r06_cesd, fill = group)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Case" = "gray", "Control" = "gray")) +
  labs(
    title = "Comparison of CES-D score Between Case and Control Groups, 2011",
    x = "Group",
    y = "CES-D score on 2011"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none" )  #+    stat_compare_means(method = "t.test", label = "p.format") # p = 3.7e-12

ggplot(matched_sex_data, aes(x = group, y = mean_cesd, fill = group)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Case" = "gray", "Control" = "gray")) +
  labs(
    title = "Comparison of mean CES-D score Between Case and Control Groups",
    x = "Group",
    y = "Mean CES-D score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none" )# +    stat_compare_means(method = "t.test", label = "p.format") # p = 2e-16



# Figure 2: IBS severiy & Depression
# NA값 없는 데이터
case_r05_noNAibs <- case_group %>% 
  filter(group == 1 & !is.na(r05_ibssev))
case_r06_noNAibs <- case_group %>% 
  filter(group ==1 & !is.na(r06_ibssev))
case_mean_noNAibs <- case_group %>% 
  filter(group == 1 & !is.na(mean_ibssev))

# Boxplot 작성
ggplot(case_r05_noNAibs, aes(x = factor(r05_ibssev), y = r05_cesd)) +
  geom_boxplot(fill = "gray", color = "black") +
  labs(title = "Distribution of Depression severity by IBS Severity Level (Round 5)",
       x = "IBS Severity (5-point scale)",
       y = "Depression Severity") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none" )

ggplot(case_r06_noNAibs, aes(x = factor(r06_ibssev), y = r06_cesd)) +
  geom_boxplot(fill = "gray", color = "black") +
  labs(title = "Distribution of Depression severity by IBS Severity Level (Round 6)",
       x = "IBS Severity (5-point scale)",
       y = "Depression Severity") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none" )

ggplot(case_mean_noNAibs, aes(x = factor(mean_ibssev), y = mean_cesd)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "Distribution of Depression severity by IBS Severity Level (All)",
       x = "IBS Severity (5-point scale)",
       y = "Depression Severity") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none" )



# Figure 3: delta ibs severity ~ delta Depression severity
case_noNA <- case_group %>% 
  filter(group == 1 & !is.na(r05_cesd) & !is.na(r05_ibssev) & !is.na(r06_cesd) & !is.na(r06_ibssev))

case_noNA <- case_noNA %>% 
  mutate(dcesd = r06_cesd - r05_cesd,
         dibssev = r06_ibssev - r05_ibssev)

# Scatter plot 작성 (dibssev vs dcesd)
ggplot(case_noNA, aes(x = dibssev, y = dcesd)) +
  geom_point(color = "black", alpha = 0.6) +  # Add points
  geom_smooth(method = "lm", color = "gray", se = TRUE) +  # Add regression line
  labs(
    title = "Scatter Plot of dibssev vs dcesd",
    x = "Change in IBS severity over time",
    y = "Change in CES-D score over time"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
  )

# Pearson correlation coefficient
cor.test(case_noNA$dcesd, case_noNA$dibssev)
# t = 2.2408, df = 321, p-value = 0.02572
# 95 percent confidence interval:
#   0.01517974 0.23011562
# sample estimates:
#   cor 
# 0.1241031 

# Linear model Coefficient
model <- lm(dibssev ~ cesd, data = case_noNA)
summary(model) 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)   0.4351     0.3668   1.186   0.2364  
# dibssev       0.7296     0.3256   2.241   0.0257 *
#   Multiple R-squared:  0.0154,	Adjusted R-squared:  0.01233 
# F-statistic: 5.021 on 1 and 321 DF,  p-value: 0.02572

# Regression model
# ibs sev ~ dep sev (Pooled effects)
pooled_ibs <- plm (ibssev ~ cesd + factor(sex) , 
                  data = case_long, 
                  model = "pooling", 
                  index = c("idone", "round"))
summary(pooled_ibs)

# ibs sev ~ dep sev (Fixed effects)
fixed_ibs <- plm (ibssev ~ cesd + factor(sex), 
                 data = case_long, 
                 model = "within", 
                 index = c("idone", "round"))
summary(fixed_ibs)

# ibs sev ~ dep sev (Random effects)
random_ibs <- plm (ibssev ~ cesd + factor(sex), 
                  data = case_long, 
                  model = "random", 
                  index = c("idone", "round"))
summary(random_ibs)

# ibs Hausman test
phtest(pooled_ibs, fixed_ibs) #fixed > pooling
phtest(fixed_ibs, random_ibs) #random > fixed


# Regression model with covariates
# Factor (변수별 reference 배정)
case_long$smk_stat <- factor(case_long$smk_stat, levels = c("Never", "Ex-smoker", "Smoker"))
case_long$alcl_ctg <- factor(case_long$alcl_ctg, levels = c("M", "H"))
case_long$bmi_ctg <- factor(case_long$bmi_ctg, levels = c("M", "L"))
case_long$incm_ctg <- factor(case_long$incm_ctg, levels = c("L", "H"))
case_long$hyptns <- factor(case_long$hyptns, levels = c(1, 2))
case_long$dbts <- factor(case_long$dbts, levels = c(1, 2))
case_long$chlstrl <- factor(case_long$chlstrl, levels = c(1, 2))
case_long$cncr <- factor(case_long$cncr, levels = c(1, 2))

# 공변량(1) 모든 Demographic characteristics
# ibs sev ~ dep sev (Pooled effects)
pooled_adjusted <- plm (ibssev ~ cesd + age + factor(sex) + factor(bmi_ctg) + factor(alcl_ctg) + factor(smk_stat) + factor(incm_ctg) + factor(hyptns) + factor(dbts) + factor(chlstrl) + factor(cncr) ,
                        data = case_long, 
                        model = "pooling", 
                        index = c("idone", "round"))
summary(pooled_adjusted)

# ibs sev ~ dep sev (Fixed effects)
fixed_adjusted <- plm (ibssev ~ cesd + age + factor(sex) + factor(bmi_ctg) + factor(alcl_ctg) + factor(smk_ever) + factor(incm_ctg) + factor(hyptns) + factor(dbts) + factor(chlstrl) + factor(cncr) ,
                       data = case_long, 
                       model = "within", 
                       index = c("idone", "round"))
summary(fixed_adjusted)

# ibs sev ~ dep sev (Random effects)
random_adjusted <- plm (ibssev ~ cesd + age + factor(sex) + factor(bmi_ctg) + factor(alcl_ctg) + factor(smk_ever) + factor(incm_ctg) + factor(hyptns) + factor(dbts) + factor(chlstrl) + factor(cncr) ,
                        data = case_long, 
                        model = "random", 
                        index = c("idone", "round"))
summary(random_adjusted)


# 공변량(2) 음주, 콜레스테롤
pooled_adjusted_2 <- plm (ibssev ~ cesd  +  factor(alcl_ctg) + factor(chlstrl),
                         data = case_long,
                         model = "pooling",
                         index = c("idone", "round"))
summary(pooled_adjusted_2)

fixed_adjusted_2 <- plm (ibssev ~ cesd + factor(alcl_ctg) + factor(chlstrl),
                         data = case_long,
                         model = "within",
                         index = c("idone", "round"))
summary(fixed_adjusted_2)
# Coefficients:
#   Estimate Std. Error t-value Pr(>|t|)
# cesd               0.037368   0.013053  2.8629 0.004723
# factor(alcl_ctg)H -0.052608   0.229147 -0.2296 0.818691
# factor(chlstrl)2   0.059943   0.168893  0.3549 0.723088
# R-Squared:      0.046116
# Adj. R-Squared: -1.5102
# F-statistic: 2.7557 on 3 and 171 DF, p-value: 0.04405


random_adjusted_2 <- plm (ibssev ~ cesd + factor(alcl_ctg) + factor(chlstrl),
                       data = case_long,
                       model = "random",
                       index = c("idone", "round"))
summary(fixed_adjusted_2)

# Hausman test
phtest(pooled_adjusted_2, fixed_adjusted_2) #Fixed > pooling
phtest(random_adjusted_2, fixed_adjusted_2) #Fixed effects > Random effects


# 공변량(3) 콜레스테롤
pooled_adjusted_3 <- plm (ibssev ~ cesd  + factor(chlstrl),
                          data = case_long,
                          model = "pooling",
                          index = c("idone", "round"))
summary(pooled_adjusted_3)

fixed_adjusted_3 <- plm (ibssev ~ cesd + factor(chlstrl),
                         data = case_long,
                         model = "within",
                         index = c("idone", "round"))
summary(fixed_adjusted_3)

random_adjusted_3 <- plm (ibssev ~ cesd + factor(chlstrl),
                          data = case_long,
                          model = "random",
                          index = c("idone", "round"))
summary(fixed_adjusted_3)


# Hausman test
phtest(pooled_adjusted_3, fixed_adjusted_3) #Fixed > pooling
phtest(random_adjusted_3, fixed_adjusted_3) #Fixed effects > Random effects

