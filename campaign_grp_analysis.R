library(tidyverse)
library(data.table)
library(lubridate)
library(scales)
library(bit64)

# 맥 한글 깨짐
library(extrafont)
font_import()

# data import

group_1_a <- fread("data/group_1_a.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
group_1_b <- fread("data/group_1_b.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
group_2_a <- fread("data/group_2_a.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
group_2_b <- fread("data/group_2_b.csv", stringsAsFactors = FALSE, encoding = "UTF-8")


# Column Add and Convert Type to char
group_1_a <- group_1_a %>%
  mutate(GROUP = 'GROUP_1_A')
group_1_a$CUST_NO <- as.character(group_1_a$CUST_NO)

group_1_b <- group_1_b %>%
  mutate(GROUP = 'GROUP_1_B')
group_1_b$CUST_NO <- as.character(group_1_b$CUST_NO)

group_2_a <- group_2_a %>%
  mutate(GROUP = 'GROUP_2_A')
group_2_a$CUST_NO <- as.character(group_2_a$CUST_NO)

group_2_b <- group_2_b %>%
  mutate(GROUP = 'GROUP_2_B')
group_2_b$CUST_NO <- as.character(group_2_b$CUST_NO)


all_data <- rbind(group_1_a, group_1_b, group_2_a, group_2_b)

## Data Selection

all_data[,c(1:5, 14)] %>%
  arrange(CUST_NO) %>%
  mutate(CUST_NO = str_sub(CUST_NO, -7, -1)) -> sample_dt

sample_dt <- sample_dt %>%
  mutate(RESPONSE = ifelse(CNT_ORDER > 0, 1, 0))
sample_dt

## `Group` to factor

sample_dt$GROUP <- as.factor(sample_dt$GROUP)

# NA is 0
sample_dt[is.na(sample_dt)] <- 0

# Response Rate
sample_dt %>%
  group_by(GROUP) %>%
  summarise(RESPONSE_RATE = mean(RESPONSE))

# 정규성 검증 (귀무가설 표본의 모집단이 정규분포를 따른다)
shapiro.test(sample_dt$RESPONSE) # p-value가 0.05 미만이므로 귀무가설을 기각
hist(sample_dt$RESPONSE, freq = FALSE)
lines(density(sample_dt$RESPONSE))

# 등분산성 검증 (귀무가설 그룹간의 분산이 서로 같다)
library(car)
leveneTest(y = sample_dt$RESPONSE, group = sample_dt$GROUP)
# p-value가 0.05 미만이므로 귀무가설을 기각 (등분산성 만족하지 않음)

# 구매반응
# 비모수적 방법
kruskal.test(RESPONSE ~ GROUP, data = sample_dt)

# 다중 비교
## 사후검정 bonferroni 

pairwise.wilcox.test(sample_dt$RESPONSE, sample_dt$GROUP,
                     p.adjust.method = "bonferroni")

## 사후검정 games.howell
games.howell <- function(grp, obs) {
  
  
  
  #Create combinations
  
  combs <- combn(unique(grp), 2)
  
  
  
  # Statistics that will be used throughout the calculations:
  
  # n = sample size of each group
  
  # groups = number of groups in data
  
  # Mean = means of each group sample
  
  # std = variance of each group sample
  
  n <- tapply(obs, grp, length)
  
  groups <- length(tapply(obs, grp, length))
  
  Mean <- tapply(obs, grp, mean)
  
  std <- tapply(obs, grp, var)
  
  
  
  statistics <- lapply(1:ncol(combs), function(x) {
    
    
    
    mean.diff <- Mean[combs[2,x]] - Mean[combs[1,x]]
    
    
    
    #t-values
    
    t <- abs(Mean[combs[1,x]] - Mean[combs[2,x]]) / sqrt((std[combs[1,x]] / n[combs[1,x]]) + (std[combs[2,x]] / n[combs[2,x]]))
    
    
    
    # Degrees of Freedom
    
    df <- (std[combs[1,x]] / n[combs[1,x]] + std[combs[2,x]] / n[combs[2,x]])^2 / # Numerator Degrees of Freedom
      
      ((std[combs[1,x]] / n[combs[1,x]])^2 / (n[combs[1,x]] - 1) + # Part 1 of Denominator Degrees of Freedom 
         
         (std[combs[2,x]] / n[combs[2,x]])^2 / (n[combs[2,x]] - 1)) # Part 2 of Denominator Degrees of Freedom
    
    
    
    #p-values
    
    p <- ptukey(t * sqrt(2), groups, df, lower.tail = FALSE)
    
    
    
    # Sigma standard error
    
    se <- sqrt(0.5 * (std[combs[1,x]] / n[combs[1,x]] + std[combs[2,x]] / n[combs[2,x]]))
    
    
    
    # Upper Confidence Limit
    
    upper.conf <- lapply(1:ncol(combs), function(x) {
      
      mean.diff + qtukey(p = 0.95, nmeans = groups, df = df) * se
      
    })[[1]]
    
    
    
    # Lower Confidence Limit
    
    lower.conf <- lapply(1:ncol(combs), function(x) {
      
      mean.diff - qtukey(p = 0.95, nmeans = groups, df = df) * se
      
    })[[1]]
    
    
    
    # Group Combinations
    
    grp.comb <- paste(combs[1,x], ':', combs[2,x])
    
    
    
    # Collect all statistics into list
    
    stats <- list(grp.comb, mean.diff, se, t, df, p, upper.conf, lower.conf)
    
  })
  
  
  
  # Unlist statistics collected earlier
  
  stats.unlisted <- lapply(statistics, function(x) {
    
    unlist(x)
    
  })
  
  
  
  # Create dataframe from flattened list
  
  results <- data.frame(matrix(unlist(stats.unlisted), nrow = length(stats.unlisted), byrow=TRUE))
  
  
  
  # Select columns set as factors that should be numeric and change with as.numeric
  
  results[c(2, 3:ncol(results))] <- round(as.numeric(as.matrix(results[c(2, 3:ncol(results))])), digits = 3)
  
  
  
  # Rename data frame columns
  
  colnames(results) <- c('groups', 'Mean Difference', 'Standard Error', 't', 'df', 'p', 'upper limit', 'lower limit')
  
  
  
  return(results)
  
}

games.howell(grp = sample_dt$GROUP, obs = sample_dt$RESPONSE)

## END CODE