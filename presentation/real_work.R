library(ggplot2)
library(dplyr)
library(tidyr)

# from cyrillic to latin --------------------------------------------------

normalize <- function(x){
  data.frame(apply(x, -1, function(x) stringi::stri_trans_general(x, 'latin')))
}


# working with regressions ------------------------------------------------
# здесь я использую закодированную таблицу

penek_mean_encoded <- read_excel("~/Desktop/penek_mean.xlsx")

df <- penek_mean_encoded

regression_enc_in <- lm(df$`СРЗНАЧ ОЦ` ~ df$Возраст + df$LA_encoded + df$Пол_encoded + df$Образование_encoded,
                        data = df)

summary(regression_enc_in)

plot(regression_enc_in)



regression_enc_INOV <- lm(df$`СРЗНАЧ ОВ` ~ df$`СРЗНАЧ ИН`, 
                        data = df)

summary(regression_enc_INOV)$coefficients

plot(regression_enc_INOV)

# попытка делать картинки с ggplot ----------------------------------------
# вроде влияет только возраст, но хз

age_table_in <- ggplot(data = penek_mean_encoded, aes(x=df$LA_encoded, y=df$`СРЗНАЧ ОЦ`)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm)

age_table_in


sex_table_in <- ggplot(data = penek_mean_encoded, aes(x=df$Образование_encoded, y=df$`СРЗНАЧ ИН`)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm)

education_table_ov <- ggplot(data = penek_mean_encoded, aes(x=df$Образование_encoded, y=df$`СРЗНАЧ ОВ`)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm)

education_table_in <- ggplot(data = penek_mean_encoded, aes(x=df$Образование_encoded, y=df$`СРЗНАЧ ИН`)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm)

education_table_in

la_table_in <- ggplot(data = penek_mean_encoded, aes(x=df$LA_encoded, y=df$`СРЗНАЧ ИН`)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm)

la_table_ov <- ggplot(data = penek_mean_encoded, aes(x=df$LA_encoded, y=df$`СРЗНАЧ ОВ`)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm)

# Overall -----------------------------------------------------------------


# sex <- df$Пол
# 
# a <- data.frame(table(sex))
# 
# a %>%
#   ggplot(aes(sex, Freq, fill = sex)) + 
#   geom_bar(stat = 'identity') +
#   xlab('Пол')+
#   ylab('Количество информантов')+
#   theme(legend.position="none")

# a <- data.frame(table(df$Образование))
# a$Var1 <- factor(a$Var1, levels = a$Var1[order(a$Freq)])
# 
#   ggplot(a, aes(Var1, Freq)) +
#   geom_bar(stat = 'identity')+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   scale_x_discrete(labels=c('дн', "11 кл.", "кн", "неок. выс.", "нет ср.", "ср. спец.", "9 кл.", "высшее", "студент"))+
#       xlab('Академический статус')+
#       ylab('Количество информантов')


# 2 part stats ------------------------------------------------------------

df2 <- second_part_stat

regression <- lm(df2$ins ~ df2$age + df2$ed + df2$att + df2$sex)

summary(regression)

plot(regression)

library(readr)
library(gridExtra)

jozhik <- read_delim("~/Desktop/jozhik.csv",
    ";", escape_double = FALSE, trim_ws = TRUE)

regression <- lm(df2$both ~ df2$age + df2$ed + df2$att + df2$sex)

knitr::kable(summary(regression)$coefficients)


# new total graph ---------------------------------------------------------

df <- penek_mean_encoded

# ggplot(data = data_pen, aes(data_pen$Возраст, colour = data_pen$Пол))+
#   geom_freqpoly(binwidth = 20)+
#   xlab('Возраст')+
#   ylab('Кол-во')+
#   labs(colour="Пол")

ggplot(df, aes(df$Образование_encoded, fill = df$Пол)) +
  geom_histogram(binwidth = 1)+
  xlab('Образование')+
  ylab('Кол-во')+
  labs(fill="Пол")




