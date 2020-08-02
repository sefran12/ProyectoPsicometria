library(tidyverse)
notas_IE1 <- read.delim(file = "raw_data/notasfinales.txt",
                        sep = ' ', stringsAsFactors = FALSE)
notas_IE1 <- notas_IE1 %>% 
    filter(notajurado2 != 0)

describe(notas_IE1)

plot(notas_IE1)

scoresIE <- notas_IE1 %>% 
    select(-ID, -notajurado1, -notajurado2, -notafinal) %>% 
    GGally::ggpairs()

scree(notas_IE1[,-c(1, 7, 12, 13, 14)])
fa.parallel(notas_IE1[,-c(1, 7, 12, 13, 14)])

notas_IE1 %>% 
    select(notajurado1, notajurado2) %>% 
    plot()

library(purrr)

notas_IE1 %>% 
    select(notajurado1, notajurado2) %>% 
    icc()

notas_IE1 %>% 
    select(primera_version, notafinal) %>% 
    icc()

notas_IE1 %>% 
    select(nota1:nota4, nota1.1:nota4.1) %>% 
    fa.parallel(fm = 'wls')

notas_fa_2 <- notas_IE1 %>% 
    select(nota1:nota4, nota1.1:nota4.1) %>% 
    fa(., nfactors = 2)

notas_fa_2$loadings
notas_fa_2$e.values

notas_fa_1 <- notas_IE1 %>% 
    select(nota1:nota4, nota1.1:nota4.1) %>% 
    fa(., nfactors = 1)

notas_fa_1$loadings
notas_fa_1$e.values

notas_fa_4 <- notas_IE1 %>% 
    select(nota1:nota4, nota1.1:nota4.1) %>% 
    fa(., nfactors = 4,
       scores = 'tenBerge', SMC = TRUE, fm = 'wls', rotate = 'varimax')

notas_fa_4$loadings
notas_fa_4$e.values

# Two or one real factors did influence the raters decisions.

factor_vs_real_scores <- data.frame(
    factor_score = 
        (notas_fa_1$scores-min(notas_fa_1$scores))/(max(notas_fa_1$scores) - min(notas_fa_1$scores)),
    real_score = notas_IE1$notatotalinicial
)

colnames(factor_vs_real_scores)[1] = 'factor_score'
max_ = max(notas_IE1$notatotalinicial)
min_ = min(notas_IE1$notatotalinicial)

factor_vs_real_scores <- factor_vs_real_scores %>% 
    mutate(factor_score = factor_score*(max_ - min_) + min_) 

factor_vs_real_scores %>% 
    plot()
abline(a = 0, b = 1, lwd = 1, col = 'red')
abline(h = 11:16 + 0.5, lty = 2)
abline(v = 11:16 + 0.5, lty = 2)

lm(data = factor_vs_real_scores,
   formula = real_score ~ factor_score) %>% 
    summary()

factor_vs_real_scores %>% 
    transmute(factor = round(factor_score+0.01),
              real = round(real_score+0.01),
              discrepancy = factor - real,
              cumdisc = cumsum(discrepancy))

library(stringr)

notas_IE1 %>% 
    mutate(anho_ingreso = factor(str_sub(ID, 5, 6))) %>%
    select(-ID) %>% 
    GGally::ggpairs()

notas_IE1 %>% 
    mutate(anho_ingreso = factor(str_sub(ID, 5, 6))) %>%
    lm(notafinal ~ anho_ingreso - 1, data = .) %>% 
    summary()

notas_IE1 %>% 
    mutate(anho_ingreso = factor(str_sub(ID, 5, 6))) %>%
    group_by(anho_ingreso) %>% 
    select(-ID) %>% 
    summarise_all(mean) %>% t() %>% `colnames<-`(., 12:18) %>% 
    plot.ts(type = 'b')
