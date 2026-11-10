# Author : Brian Chang, Sharmila Uppalapati, Tyler Moore
# Date   : 2025-12-09
# Purpose: STAT 5101 - Team Project
##----------------------------------------------------------------------------##
library(tidyverse)
library(RColorBrewer)
library(MuMIn)
library(car)
##----------------------------------------------------------------------------##
# Global settings
options(ggsave.width = 8, ggsave.height = 6)
theme_set(theme_bw())
##----------------------------------------------------------------------------##
df <- read.csv("state_IA.csv", header = TRUE)

df<- df |> select(derived_race, derived_ethnicity, derived_sex, action_taken,
                  derived_dwelling_category, loan_type, loan_purpose,
                  loan_amount, income)
##----------------------------------------------------------------------------##
# Renaming and factoring key demographic variables
df <- df |> mutate(derived_race = as.factor(derived_race)) |> 
                                            rename(race = derived_race)

df <- df |> mutate(derived_ethnicity = as.factor(derived_ethnicity)) |> 
                                     rename(ethnicity = derived_ethnicity)

df <- df |> mutate(derived_sex = as.factor(derived_sex)) |> 
                                           rename(sex = derived_sex)

df <- df |> rename(status = action_taken) |> mutate(
  status = as.factor(status)) |> mutate(status = fct_recode(status,
                        `Loan originated` = "1",
                        `Application approved but not accepted` = "2",
                        `Application denied` = "3",
                        `Application withdrawn by applicant` = "4", 
                        `File closed for incompleteness` = "5",
                        `Purchased loan` = "6",
                        `Preapproval request denied` = "7",
                        `Preapproval request approved but not accepted` = "8"))

df <- df |> rename(dwelling = derived_dwelling_category) |> mutate(
  dwelling = as.factor(dwelling)) |> mutate(dwelling = fct_recode(dwelling,
        SingleFamily_SiteBuilt = "Single Family (1-4 Units):Site-Built",
        SingleFamily_Manufactured = "Single Family (1-4 Units):Manufactured",
        MultiFamily_SiteBuilt = "Multifamily:Site-Built",
        MultiFamily_Manufactured = "Multifamily:Manufactured"))

df <- df |> rename(type = loan_type) |> mutate(
  type = as.factor(type)) |> mutate(type = fct_recode(type,
                                                 `Conventional` = "1",
                                                 `FHA insured` = "2",
                                                 `VA guaranteed` = "3",
                                                 `RHS/FSA guaranteed` = "4"))

df <- df |> rename(purpose = loan_purpose) |> mutate(
  purpose = as.factor(purpose)) |> mutate(purpose = fct_recode(purpose,
                                              `Home purchase` = "1",
                                              `Home improvement` = "2",
                                              `Refinancing` = "31",
                                              `Cash-out refinancing` = "32",
                                              `Other purpose` = "4",
                                              `Not applicable` = "5"))

df <- df |> rename(amount = loan_amount)
df <- df |> mutate(income = income*1000)
##----------------------------------------------------------------------------## 
dim(df)
str(df)
names(df)
table(df$race)
prop.table(table(df$race))*100
table(df$ethnicity)
prop.table(table(df$ethnicity))*100
table(df$sex)
prop.table(table(df$sex))*100
table(df$status)
prop.table(table(df$status))*100 
table(df$dwelling) 
prop.table(table(df$dwelling))*100 
table(df$type) 
prop.table(table(df$type))*100 
table(df$purpose) 
prop.table(table(df$purpose))*100 
summary(df$amount) 
summary(df$income)
summary(df)
df |> filter(status == "Purchased loan") |> select(sex) |> summary() 
##----------------------------------------------------------------------------##
# Q1
# Does action_taken and derived_sex are independent?
##----------------------------------------------------------------------------
df_sex_status <- df |> filter(sex != "Sex Not Available") |> filter(
  status != "Preapproval request denied", 
  status != "Preapproval request approved but not accepted", 
  status != "Purchased loan", 
  status != "Application withdrawn by applicant", 
  status != "File closed for incompleteness") |> mutate( 
    status_grouped = fct_collapse(status, Approved = c("Loan originated",
                                      "Application approved but not accepted"),
                                  Denied = "Application denied"),
    status_grouped = fct_drop(status_grouped),
    sex = fct_drop(sex)) |>
  select(sex, status_grouped)
##----------------------------------------------------------------------------
plot1a <- ggplot(data = df_sex_status, mapping = aes(x = status_grouped,
                                                     fill = sex)) +
  geom_bar(position = "dodge") +
  labs(x = "Loan Status",
       fill = "Sex",
       title = "Fig 1a. Distribution of Loan Status by Sex") +
  theme(axis.title.y = element_blank()) +
  scale_fill_brewer(palette = "Blues")

plot1a
ggsave(plot1a, file = "Fig 1a. Distribution of Loan Status by Sex.png")
##----------------------------------------------------------------------------
df_sex_status$loanstatus <- factor(df_sex_status$status_grouped,
                                   levels = c("Approved",
                                              "Denied"))
df_sex_status$sex<- factor(df_sex_status$sex,
                           levels = c("Male", "Female", "Joint"))

tbl_sex_status <- table(df_sex_status$loanstatus , df_sex_status$sex)
tbl_sex_status
tbl_sex_status_prop <- prop.table(tbl_sex_status, margin = 2) * 100
round(tbl_sex_status_prop, digit = 1)
test_sex_status_chi <- chisq.test(tbl_sex_status, correct = F) #Test of Independence
test_sex_status_chi
test_sex_status_chi$expected #Expected Values
(test_sex_status_chi$residuals)^2
##----------------------------------------------------------------------------
plot1b <- ggplot(data = df_sex_status,
                 mapping = aes(x = sex, fill = status_grouped)) +
          geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Sex", y = "Proportion of Applications",
    fill = "Loan Status", title = "Fig 1b. Proportions of Loan Status by Sex")
  
plot1b
ggsave(plot1b, file = "Fig 1b. Proportions of Loan Status by Sex.png")
##----------------------------------------------------------------------------##
# Q2
# Is the average loan_amount the same for uninsured and insured types
# among disbursed loans?
##----------------------------------------------------------------------------
df_type_amount <- df |> filter(status %in% c("Loan originated", 
                                                 "Purchased loan")) |> 
  mutate(type_grouped = fct_collapse(type, Uninsured = "Conventional",
                                           Insured = c("FHA insured",
                                                       "VA guaranteed",
                                                       "RHS/FSA guaranteed")),
        type_grouped = fct_drop(type_grouped)) |>
  select(type_grouped, amount) 
##----------------------------------------------------------------------------
plot2a <- ggplot(data = df_type_amount, mapping = aes(x = type_grouped,
                                                          y = amount)) + 
  geom_boxplot(fill = "skyblue2") +
  labs(x = "Loan Type",
       y = "Loan Amount",
       title = "Fig 2a. Distribution of Loan Amount by Type") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
plot2a

ggsave(plot2a, file = "Fig 2a. Distribution of Loan Amount by Type.png")
##----------------------------------------------------------------------------
with(df_type_amount, tapply(amount, type_grouped, sd)) 
df_type_amount |> group_by(type_grouped) |> summarize(mean = mean(amount)) 

lm_type_amount_raw <- lm(amount~type_grouped, data=df_type_amount) 

# residual plot - checking equal variances
p1 <- ggplot(data = df_type_amount,
             mapping = aes(
               x = predict(lm_type_amount_raw),
               y = resid(lm_type_amount_raw)))
R1 <- p1 + geom_point(color = "skyblue") + geom_hline(yintercept = 0,
                                                      linetype = "dashed",
                                                      color = "red") +
  labs (title = "Fig 2a.1. Residual Plot for Loan Amount")
R1
ggsave(R1, file = "Fig 2a.1. Residual Plot for Loan Amount.png")

# qq plot - checking normality
p2 <- ggplot(df_type_amount, aes(sample = resid(lm_type_amount_raw))) + 
      stat_qq(color = "skyblue")
QQ1 <- p2 + stat_qq_line(linetype = "dashed", color = "red") +
  labs (title = "Fig 2a.2. QQ Plot for Loan Amount",
        x = "Theoritical Quantiles",
        y = "Sample Quantiles")
QQ1
ggsave(QQ1, file = "Fig 2a.2. QQ Plot for Loan Amount.png")
##----------------------------------------------------------------------------
# log transformation
##----------------------------------------------------------------------------
plot2b <- ggplot(data = df_type_amount, mapping = aes(x = type_grouped,
                                                          y = log(amount))) +
  geom_boxplot(fill = "skyblue2") +
  labs(x = "Loan Type",
       y = "log(Loan Amount)",
       title = "Fig 2b. Distribution of log(Loan Amount) by Type") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
plot2b

ggsave(plot2b, file = "Fig 2b. Distribution of log(Loan Amount) by Type.png")
##----------------------------------------------------------------------------
df_type_amount$logamount <- log(df_type_amount$amount)
with(df_type_amount, tapply(logamount, type_grouped, sd))

lm_type_amount_log <- lm(logamount~type_grouped, data=df_type_amount)

# residual plot on transformed data
p3 <- ggplot(data = df_type_amount,
             mapping = aes(x = predict(lm_type_amount_log),
                           y = resid(lm_type_amount_log)))
logR1 <- p3 + geom_point(color = "skyblue") + geom_hline(yintercept = 0,
                                                         linetype = "dashed",
                                                         color = "red") +
  labs (title = "Fig 2b.1. Residual Plot for log(Loan Amount)")
logR1
ggsave(logR1, file = "Fig 2b.1. Residual Plot for log(Loan Amount).png")

# qq plot on transformed data
p4 <- ggplot(df_type_amount, aes(sample = resid(lm_type_amount_log))) + 
  stat_qq(color = "skyblue")
logQQ1 <- p4 + stat_qq_line(linetype = "dashed", color = "red") +
  labs (title = "Fig 2b.2. QQ plot for log(Loan Amount)",
        x = "Theoritical Quantiles",
        y = "Sample Quantiles")
logQQ1
ggsave(logQQ1, file = "Fig 2b.2. QQ plot for log(Loan Amount).png")
##----------------------------------------------------------------------------
sum_type_amount_log <- df_type_amount |> group_by(type_grouped) |>
  summarise(n = n(),
            mean_log = mean(logamount),
            sd_log = sd(logamount),
            lb = mean_log - qt(0.975, n-1)* sd_log/sqrt(n),
            ub = mean_log + qt(0.975, n-1)* sd_log/sqrt(n))
##----------------------------------------------------------------------------
plot2c <- ggplot(data = sum_type_amount_log, mapping = aes(x = type_grouped,
                                                               y = mean_log,
                                                               ymin = lb,
                                                               ymax = ub)) +
  geom_pointrange() +
  labs(x = "Loan Type",
       y = "Mean(log(Loan Amount))",
       title = "Fig 2c. Mean Log(Loan Amount) by Loan Type with 95% C.I") + 
  scale_y_continuous(breaks = seq(11.5, 12.5, by = 0.2),
                                  limits = c(11.5, 12.5)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
plot2c

ggsave(plot2c, file =
         "Fig 2c. Mean Log(Loan Amount) by Loan Type with 95 percent C.I.png")
##----------------------------------------------------------------------------
#t-test on transformed data
test_type_amount_log_t <- t.test(logamount~type_grouped, data=df_type_amount)
test_type_amount_log_t

test_type_amount_log_t$estimate # means of two groups in log transformed data
diff(test_type_amount_log_t$estimate) # diff in means in log transformed data

exp(diff(test_type_amount_log_t$estimate)) #factor

test_type_amount_log_t$conf.int # CI for logamount
exp(test_type_amount_log_t$conf.int) # CI for amount

summary(lm(logamount ~ type_grouped, data = df_type_amount))
##----------------------------------------------------------------------------##
# Q2: Variation
# Is the average loan_amount the same for uninsured and insured types
# among disbursed loans? (with cut_off)
##----------------------------------------------------------------------------
# Cut-off for top 1% amount
##----------------------------------------------------------------------------
top_cutoff <- quantile(df$amount[df$status %in% c("Loan originated", 
                                                  "Purchased loan")], 0.99)
top_cutoff
##----------------------------------------------------------------------------
df_type_amount_trim <- df |> filter(status %in% c("Loan originated",
                                                  "Purchased loan")) |>
  filter(amount <= top_cutoff) |> mutate(type_grouped = fct_collapse(type, 
                                     Uninsured = "Conventional",
                                     Insured = c("FHA insured",
                                                 "VA guaranteed",
                                                 "RHS/FSA guaranteed")),
    type_grouped = fct_drop(type_grouped)) |>
  select(type_grouped, amount) 
##----------------------------------------------------------------------------
plot2d <- ggplot(data = df_type_amount_trim, mapping = aes(x = type_grouped,
                                                               y = amount)) + 
  geom_boxplot(fill = "skyblue2") +
  labs(x = "Loan Type",
       y = "Loan Amount",
       title = "Fig 2d. Distribution of Loan Amount by Type with Cut-off") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
plot2d

ggsave(plot2d,
       file = "Fig 2d. Distribution of Loan Amount by Type with Cut-off.png")
##----------------------------------------------------------------------------
with(df_type_amount_trim, tapply(amount, type_grouped, sd)) 
df_type_amount_trim |> group_by(type_grouped) |> summarize(mean = mean(amount)) 

lm_type_amount_trim_raw <- lm(amount~type_grouped, data=df_type_amount_trim)

# residual plot - checking equal variances
p5 <- ggplot(data = df_type_amount_trim,
             mapping = aes(
               x = predict(lm_type_amount_trim_raw),
               y = resid(lm_type_amount_trim_raw)))
R2 <- p5 + geom_point(color = "skyblue") + geom_hline(yintercept = 0,
                                                      linetype = "dashed",
                                                      color = "red") +
  labs (title = "Fig 2d.1. Residual Plot for Loan Amount with Cut-off")
R2
ggsave(R2, file = "Fig 2d.1. Residual Plot for Loan Amount with Cut-off.png")

# qq plot - checking normality
p6 <- ggplot(df_type_amount_trim, aes(sample = resid(lm_type_amount_trim_raw))) +
  stat_qq(color = "skyblue")
QQ2 <- p6 + stat_qq_line(linetype = "dashed", color = "red") +
  labs (title = "Fig 2d.2. QQ Plot for Loan Amount with Cut-off",
        x = "Theoritical Quantiles",
        y = "Sample Quantiles")
QQ2
ggsave(QQ2, file = "Fig 2d.2. QQ Plot for Loan Amount with Cut-off.png")
##----------------------------------------------------------------------------
# log transformation
##----------------------------------------------------------------------------
plot2e <- ggplot(data = df_type_amount_trim, mapping = aes(x = type_grouped,
                                                           y = log(amount))) + 
  geom_boxplot(fill = "skyblue2") +
  labs(x = "Loan Type",
       y = "log(Loan Amount)",
       title = "Fig 2e. Distribution of log(Loan Amount) by Type_Cut-off ") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
plot2e

ggsave(plot2e,
       file = "Fig 2e. Distribution of log(Loan Amount) by Type_Cut-off.png")
##----------------------------------------------------------------------------
df_type_amount_trim$logamount <- log(df_type_amount_trim$amount)
with(df_type_amount_trim, tapply(logamount, type_grouped, sd))

lm_type_amount_trim_log <- lm(logamount~type_grouped, data=df_type_amount_trim)

# residual plot on transformed data
p7 <- ggplot(data = df_type_amount_trim,
             mapping = aes(
               x = predict(lm_type_amount_trim_log),
               y = resid(lm_type_amount_trim_log)))
logR2 <- p7 + geom_point(color = "skyblue") +
  geom_hline(yintercept = 0,linetype = "dashed",
             color = "red") +
  labs (title = "Fig 2e.1. Residual Plot for log(Loan Amount)_Cut-off")
logR2
ggsave(logR2, file = "Fig 2e.1. Residual Plot for log(Loan Amount)_Cut-off.png")

# qq plot on transformed data
p8 <- ggplot(df_type_amount_trim,
             aes(sample = resid(lm_type_amount_trim_log))) +
      stat_qq(color = "skyblue")
logQQ2 <- p8 + stat_qq_line(linetype = "dashed", color = "red") +
  labs (title = "Fig 2e.2. QQ plot for log(Loan Amount)_Cut-off",
        x = "Theoritical Quantiles",
        y = "Sample Quantiles")
logQQ2
ggsave(logQQ2, file = "Fig 2e.2. QQ plot for log(Loan Amount)_Cut-off.png")
##----------------------------------------------------------------------------
sum_type_amount_trim_log <- df_type_amount_trim |> group_by(type_grouped) |>
  summarise(n = n(),
            mean_log = mean(logamount),
            sd_log = sd(logamount),
            lb = mean_log - (qt(0.975, n-1)* sd_log/sqrt(n)),
            ub = mean_log + (qt(0.975, n-1)* sd_log/sqrt(n)))
##----------------------------------------------------------------------------
plot2f <- ggplot(data = sum_type_amount_trim_log, mapping = aes(x = type_grouped,
                                                               y = mean_log,
                                                               ymin = lb,
                                                               ymax = ub)) +
  geom_pointrange() +
  labs(x = "Loan Type",
       y = "Mean(log(Loan Amount))",
    title = "Fig 2f. Mean log(Loan Amount) by Loan Type with 95% C.I_Cut-off") +
    scale_y_continuous(breaks = seq(11.5, 12.5, by = 0.2),
                                  limits = c(11.5, 12.5)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
plot2f

ggsave(plot2f, file =
         "Fig 2f. Mean log(Loan Amount) by Loan Type with 95pct C.I_Cut-off.png")
##----------------------------------------------------------------------------
#t-test on transformed data
test_type_amount_trim_log_t <- t.test(logamount~type_grouped,
                                      data=df_type_amount_trim)
test_type_amount_trim_log_t

test_type_amount_trim_log_t$estimate # means of two groups in log transformed data
diff(test_type_amount_trim_log_t$estimate) # diff in means in log transformed data

exp(diff(test_type_amount_trim_log_t$estimate)) #factor

test_type_amount_trim_log_t$conf.int # CI for logamount
exp(test_type_amount_trim_log_t$conf.int) # CI for amount

summary(lm(logamount ~ type_grouped, data = df_type_amount_trim))
##----------------------------------------------------------------------------##
# Q3
# Is there any relationship between applicant income and loan_amount among
# loans that were approved and disbursed directly by the lender?
##----------------------------------------------------------------------------
df_income_amount <- df |> filter(!is.na(income)) |> filter(income > 0) |>
  filter(status %in% c("Loan originated",
                       "Application approved but not accepted")) |>
  select(amount, income) 
dim(df_income_amount)
##----------------------------------------------------------------------------
plot3a <- ggplot(data = df_income_amount, mapping = aes(x = income,
                                                        y = amount)) +
  geom_point(color = "skyblue1") +
  labs(x = "Income",
       y = "Loan Amount",
       title = "Fig 3a. Association between Loan Amount and Income") 

plot3a
ggsave(plot3a, file = "Fig 3a. Association between Loan Amount and Income.png")
##----------------------------------------------------------------------------
lm_slr1_raw <- lm(amount~income, data=df_income_amount) 

# residual plot - checking equal variances
p9 <- ggplot(data = df_income_amount,
             mapping = aes(
               x = predict(lm_slr1_raw),
               y = resid(lm_slr1_raw)))
R3 <- p9 + geom_point(color = "skyblue") + geom_hline(yintercept = 0,
                                                      linetype = "dashed",
                                                      color = "red") +
  labs (title = "Fig 3a.1. Residual Plot for Loan Amount vs. Actual Income")
R3
ggsave(R3,
       file = "Fig 3a.1. Residual Plot for Loan Amount vs. Actual Income.png")

# qq plot - checking normality
p10 <- ggplot(df_income_amount, aes(sample = resid(lm_slr1_raw))) +
       stat_qq(color = "skyblue")
QQ3 <- p10 + stat_qq_line(linetype = "dashed", color = "red") +
  labs (title = "Fig 3a.2. QQ Plot for Loan Amount vs. Actual Income",
        x = "Theoritical Quantiles",
        y = "Sample Quantiles")
QQ3
ggsave(QQ3, file = "Fig 3a.2. QQ Plot for Loan Amount vs. Actual Income.png")
##----------------------------------------------------------------------------
# log transformation
##----------------------------------------------------------------------------
plot3b <- ggplot(data = df_income_amount, mapping = aes(x = log(income),
                                                        y = log(amount))) + 
  geom_point(color = "skyblue1") +
  labs(x = "log(Income)",
       y = "log(Loan Amount)",
       title = "Fig 3b. Association between log(Loan Amount) and log(Income)") +
  geom_smooth(method = "lm", se = TRUE)

plot3b
ggsave(plot3b, file =
         "Fig 3b. Association between log(Loan Amount) and log(Income).png")
##----------------------------------------------------------------------------
df_income_amount$logamount <- log(df_income_amount$amount)
df_income_amount$logincome <- log(df_income_amount$income)
##----------------------------------------------------------------------------
lm_slr1_log <- lm(logamount~logincome, data=df_income_amount)

# residual plot - checking equal variances
p11 <- ggplot(data = df_income_amount,
              mapping = aes(
                x = predict(lm_slr1_log),
                y = resid(lm_slr1_log)))
logR3 <- p11 + geom_point(color = "skyblue") + geom_hline(yintercept = 0,
                                                          linetype = "dashed",
                                                          color = "red") +
  labs (title = "Fig 3b.1. Residual Plot for log(Loan Amount) vs. log(Income)")
logR3
ggsave(logR3,
       file = "Fig 3b.1. Residual Plot for log(Loan Amount) vs. log(Income).png")

# qq plot - checking normality
p12 <- ggplot(df_income_amount, aes(sample = resid(lm_slr1_log))) +
       stat_qq(color = "skyblue")
logQQ3 <- p12 + stat_qq_line(linetype = "dashed", color = "red") +
  labs (title = "Fig 3b.2. QQ Plot for log(Loan Amount) vs. log(Income)",
        x = "Theoritical Quantiles",
        y = "Sample Quantiles")
logQQ3
ggsave(logQQ3,
       file = "Fig 3b.2. QQ Plot for log(Loan Amount) vs. log(Income).png")
##----------------------------------------------------------------------------
summary(lm_slr1_log)
##----------------------------------------------------------------------------##
# Q3: Variation
# Is there any change in relationship between applicant income and loan amount
# among loans that were approved and disbursed directly by the lender while
# taking other factors such as type and purpose into account? (MLR)
##----------------------------------------------------------------------------
df_income_amount_mlr <- df |> filter(!is.na(income)) |> filter(income > 0) |>
  filter(sex != "Sex Not Available") |>
  filter(status %in% c("Loan originated",
                       "Application approved but not accepted")) |>
  filter(race != "Race Not Available") |>
  filter(ethnicity %in% c("Hispanic or Latino", "Joint",
                          "Not Hispanic or Latino")) |>
  filter(purpose != "Not applicable") |> 
  mutate(sex = fct_drop(sex),
         status = fct_drop(status),
         race = fct_drop(race), ethnicity = fct_drop(ethnicity),
         purpose = fct_drop(purpose)) |> 
  select(amount, income, type, purpose, sex, race, ethnicity, dwelling)
dim(df_income_amount_mlr)
##----------------------------------------------------------------------------
plot3c <- ggplot(data = df_income_amount_mlr, mapping = aes(x = income,
                                                            y = amount)) + 
  geom_point(color = "skyblue1") +
  labs(x = "Income",
       y = "Loan Amount",
       title = "Fig 3c. Association between Loan Amount and Income") 

plot3c
ggsave(plot3c, file = "Fig 3c. Association between Loan Amount and Income.png")
##----------------------------------------------------------------------------
lm_slr2_raw <- lm(amount ~ income, data = df_income_amount_mlr)

# residual plot - checking equal variances
p13 <- ggplot(data = df_income_amount_mlr,
              mapping = aes(
                x = predict(lm_slr2_raw),
                y = resid(lm_slr2_raw)))
R4 <- p13 + geom_point(color = "skyblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Fig 3c.1. Residual Plot for Loan Amount vs. Actual Income")
R4
ggsave(R4,
       file = "Fig 3c.1. Residual Plot for Loan Amount vs. Actual Income.png")
# qq plot - checking normality
p14 <- ggplot(df_income_amount_mlr, aes(sample = resid(lm_slr2_raw))) +
       stat_qq(color = "skyblue")
QQ4 <- p14 + stat_qq_line(linetype = "dashed", color = "red") +
  labs(title = "Fig 3c.2. QQ Plot for Loan Amount vs. Actual Income",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")
QQ4
ggsave(QQ4, file = "Fig 3c.2. QQ Plot for Loan Amount vs. Actual Income.png")
##----------------------------------------------------------------------------
# log transformation
##----------------------------------------------------------------------------
plot3d <- ggplot(data = df_income_amount_mlr, mapping = aes(x = log(income),
                                                            y = log(amount))) +
  geom_point(color = "skyblue1") +
  labs(x = "log(Income)",
       y = "log(Loan Amount)",
       title = "Fig 3d. Association between log(Loan Amount) and log(Income)") 

plot3d
ggsave(plot3d,
       file = "Fig 3d. Association between log(Loan Amount) and log(Income).png")
##----------------------------------------------------------------------------
df_income_amount_mlr$logamount <- log(df_income_amount_mlr$amount)
df_income_amount_mlr$logincome <- log(df_income_amount_mlr$income)
##----------------------------------------------------------------------------
lm_slr2_log <- lm(logamount ~ logincome, data = df_income_amount_mlr)

# residual plot - checking equal variances
p15 <- ggplot(data = df_income_amount_mlr,
              mapping = aes(
                x = predict(lm_slr2_log),
                y = resid(lm_slr2_log)))
logR4 <- p15 + geom_point(color = "skyblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Fig 3d.1. Residual Plot for log(Loan Amount) vs. log(Income)")
logR4
ggsave(logR4,
       file = "Fig 3d.1. Residual Plot for log(Loan Amount) vs. log(Income).png")

# qq plot - checking normality
p16 <- ggplot(df_income_amount_mlr, aes(sample = resid(lm_slr2_log))) +
       stat_qq(color = "skyblue")
logQQ4 <- p16 + stat_qq_line(linetype = "dashed", color = "red") +
  labs(title = "Fig 3d.2. QQ Plot for log(Loan Amount) vs. log(Income)",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")
logQQ4
ggsave(logQQ4,
       file = "Fig 3d.2. QQ Plot for log(Loan Amount) vs. log(Income).png")
##----------------------------------------------------------------------------
summary(lm_slr2_log)
##----------------------------------------------------------------------------
#MLR - Selecting a model using dredge()
##--------------------------------------
options(na.status = "na.fail")
lm_mlr_base <- lm(logamount ~ type + purpose, df_income_amount_mlr) 
aicc.ms <- dredge(lm_mlr_base)
aicc.ms[1:nrow(aicc.ms),] #all
lm_mlr_aicc_best <- get.models(aicc.ms, delta==0)[[1]] #best
summary(lm_mlr_aicc_best)
##---------------------------------------
bic.ms <- dredge(lm_mlr_base, rank = "BIC")
bic.ms[1:nrow(bic.ms),] #all
lm_mlr_bic_best <- get.models(bic.ms, delta==0)[[1]] #best
summary(lm_mlr_bic_best)
##---------------------------------------
# AICc and BIC both selected model with purpose and type as best model.
# Adding log(income) variable to the selected model
##---------------------------------------
# Current Best model + log(Income)
##---------------------------------------
lm_mlr_full_log <- lm(logamount ~ logincome + purpose + type,
                           data = df_income_amount_mlr) 
##-----------------------------------------
# Verifying diagnostics of the Current Best model + log(Income)
##-----------------------------------------
vif(lm_mlr_full_log)
cooksd_vals <- cooks.distance(lm_mlr_full_log)
n <- nrow(df_income_amount_mlr)
df_income_amount_mlr$rownumber <- 1:n
png("Fig 3e.1. Cook's Distance - logIncome + Purpose + Type.png")
plot(df_income_amount_mlr$rownumber, cooksd_vals, 
     xlab = "Obsevation Number", ylab="Cook's D",
     main="Fig 3e.1. Cook's Distance - logIncome + Purpose + Type")
abline(h = (4/n), col="red")
inf.pts <- which(cooksd_vals > (4/n))
points(df_income_amount_mlr$rownumber[inf.pts],
       cooksd_vals[inf.pts], col="lightblue", pch=19, cex=1.3)
dev.off()
df_resid <- data.frame(raw_resid = residuals(lm_mlr_full_log),
                       std_resid = rstandard(lm_mlr_full_log),
                       pred_vals = predict(lm_mlr_full_log))

p17 <- ggplot(df_resid, aes(x = raw_resid, y = std_resid)) +
  geom_point(alpha = 0.6, color = "skyblue2") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Fig 3e.2. Standardized Residuals vs Raw Residuals",
       x = "Raw Residuals",
       y = "Standardized Residuals")
p17
ggsave(p17, file = "Fig 3e.2. Standardized Residuals vs Raw Residuals.png")

p18 <- ggplot(df_resid, aes(x = pred_vals, y = std_resid)) +
  geom_point(alpha = 0.6, color = "skyblue2") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Fig 3e.3. Standardized Residuals vs Predicted Values",
       x = "Predicted Values",
       y = "Standardized Residuals") 
p18
ggsave(p18, file = "Fig 3e.3. Standardized Residuals vs Predicted Values.png")
##-----------------------------------------
# Fitting Current Best model + log(Income) + log(Income)^2
##-----------------------------------------
lm_quad_mlr_full_log <- lm(logamount ~ logincome + I(logincome^2) + purpose + 
                              type, data = df_income_amount_mlr) 
##-----------------------------------------
# Fitting Current Best model + log(Income) + log(Income)^2 with Centering
##-----------------------------------------
df_income_amount_mlr$logincome_c <- df_income_amount_mlr$logincome - 
                                            mean(df_income_amount_mlr$logincome)

lm_quadC_mlr_full_log <- lm(logamount ~ logincome_c + I(logincome_c^2) + purpose
                                    + type, data = df_income_amount_mlr)
##-----------------------------------------
# Fitting Current Best model + log(Income) with Interactions
##-----------------------------------------
lm_mlr_full_int_log <- lm(logamount ~ logincome * purpose + logincome * type, 
                                             data = df_income_amount_mlr)
##-----------------------------------------
# Selecting best model from slr2, no interactions mlr, quadratic mlr, 
# quadratic mlr with centering, and interactions mlr models using model.sel()
##-----------------------------------------
model.sel(lm_slr2_log, lm_mlr_full_log, lm_quad_mlr_full_log,
          lm_quadC_mlr_full_log, lm_mlr_full_int_log)
model.sel(lm_slr2_log, lm_mlr_full_log, lm_quad_mlr_full_log,
          lm_quadC_mlr_full_log, lm_mlr_full_int_log, rank = "BIC")
##-----------------------------------------
# AICc and BIC both selected full interactions model as best model.
# Verifying diagnostics of the selected model
##-----------------------------------------
vif(lm_mlr_full_int_log)
cooksd_vals <- cooks.distance(lm_mlr_full_int_log)
n <- nrow(df_income_amount_mlr)
df_income_amount_mlr$rownumber <- 1:n
png("Fig 3f.1. Cook's Distance - logIncome X Purpose + logIncome X Type.png")
plot(df_income_amount_mlr$rownumber, cooksd_vals,
     xlab = "Obsevation Number", ylab="Cook's D",
     main="Fig 3f.1. Cook's Distance - logIncome*Purpose + logIncome*Type")
abline(h = (4/n), col="red")
inf.pts <- which(cooksd_vals > (4/n))
points(df_income_amount_mlr$rownumber[inf.pts],
       cooksd_vals[inf.pts], col="lightblue", pch=19, cex=1.3)
dev.off()
##------------------------------------------
# From model.sel() full interactions model appears to be the best model using
# both AICc and BIC but the VIF values are very inflated. The 3 predictors model
# without interactions is the second best choice.

# From dredge() The 2 predictors model without interactions is the best choice,
# when purpose and type are fitted excluding logIncome. So, this model is chosen
# and logincome is added to the model. It has acceptable Cook's D (<0.005)
# and VIF values (<2). Although some observations are >4/n, they are way under 1
# and close to the threshold.

# The final best model is lm_mlr_full_log with 3 predictors.
##------------------------------------------
# Summary of final best model
##------------------------------------------
summary(lm_mlr_full_log)
##------------------------------------------
df_predgrid <- expand.grid(logincome = seq(min(df_income_amount_mlr$logincome),
                                           max(df_income_amount_mlr$logincome),
                                                  length.out = 100),
                           purpose = unique(df_income_amount_mlr$purpose), 
                           type = unique(df_income_amount_mlr$type)) 

df_predgrid$pred <- predict(lm_mlr_full_log, newdata = df_predgrid)

plot3g <- ggplot() +
  geom_point(data = df_income_amount_mlr, aes(x = logincome,
                                              y = logamount,
                                              color = purpose), alpha = 0.1) + 
  geom_line(data = df_predgrid, aes(x = logincome, y = pred,
                                    color = purpose), linewidth = 0.5) + 
  facet_wrap(~ type, scales = "free") + 
  labs(title = "Fig 3g. Effects of Income and Loan Purpose",
       subtitle = "By Loan Type",
       x = "log(Income)",
       y = "Fitted log(Loan Amount)",
       color = "Loan Purpose") + scale_color_brewer(palette = "Blues")
plot3g
ggsave(plot3g, file = "Fig 3g. Effects of Income and Loan Purpose.png")

plot3h <- ggplot() +
  geom_point(data = df_income_amount_mlr, aes(x = logincome,
                                              y = logamount,
                                              color = type), alpha = 0.1) + 
  geom_line(data = df_predgrid, aes(x = logincome, y = pred,
                                    color = type), linewidth = 0.5) + 
  facet_wrap(~ purpose) + 
  labs(title = "Fig 3h. Effects of Income and Loan Type",
       subtitle = "By Loan Purpose",
       x = "log(Income)",
       y = "Fitted log(Loan Amount)",
       color = "Loan Type") + scale_color_brewer(palette = "Blues")
plot3h
ggsave(plot3h, file = "Fig 3h. Effects of Income and Loan Type.png")
##----------------------------------------------------------------------------##