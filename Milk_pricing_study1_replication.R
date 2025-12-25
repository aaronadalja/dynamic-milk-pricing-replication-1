#######################################################################################
### STUDY 1 - R replication code for                                                ###
### Dynamic pricing to reduce retail dairy shrink:                                  ###
### Evidence from lab and grocery store experiments                                 ###
#######################################################################################

# DATE: DECEMBER 2025
# Editor: Aaron Adalja

rm(list = ls())
objects()
options(error=recover, scipen=999, max.print = 9999)

# Any package that is required by the script below is given here:----
# Check to see if packages are installed, if not install.
inst_pkgs = load_pkgs =  c("data.table", "tidyverse", "magrittr", "tidyselect", "stringr", 
                           "systemfit", "knitr", "lubridate", "mfx", "texreg","plm",
                           "margins", "gridExtra", "gtools", "openxlsx", "patchwork","lfe", "censReg")
inst_pkgs = inst_pkgs[!(inst_pkgs %in% installed.packages()[,"Package"])]
if(length(inst_pkgs)) install.packages(inst_pkgs)

# Dynamically load packages---
pkgs_loaded = lapply(load_pkgs, require, character.only=T)

raw <- read.csv("./rawdata_lab.csv", stringsAsFactors = F)

case_likert <- function(likvar) {
  case_when(
    likvar == "Strongly disagree" ~ -2,
    likvar == "Somewhat disagree" ~ -1,
    likvar == "Neither agree nor disagree" ~ 0,
    likvar == "Somewhat agree" ~ 1,
    likvar == "Strongly agree" ~ 2
  )
}

clean1 <- raw %>% unite("pid", c("Session", "Subject"), sep="_", remove=F) %>%
  filter(!is.na(Bid)) %>%
  mutate(Treatment = factor(Treatment, levels=c("Control", "T1", "T2"), labels=c("Control", "General", "Personal")),
         SLSq = SL^2,
         SL3 = SL^3,
         buy_milk_minweekly = (Frequency_Consumption %in% c("Once a week", "A few times a week", "Everyday"))*1,
         buy_milk_daily = (Frequency_Consumption %in% c("Everyday"))*1,
         Size = factor(Size, levels=c("1 gallon (128 fl oz)", "Half gallon (64 fl oz)", "1 quart (32 fl oz)", "1 pint (16 fl oz)", "Other:", "Prefer not to say"),
                       labels=c("gal", "half gal", "qt", "pt", "oth", "pna")),
         cons_moreweek = (Time %in% c("More than a week"))*1,
         safety = (Quality_Safety %in% c("Food safety"))*1,
         attn_always = (Attention %in% c("Yes, always"))*1,
         choose_longSL = (Choose %in% c("Yes, always"))*1,
         primaryshopper = (Shopper %in% c("Yes"))*1,
         veg = (Vegan %in% c("Yes"))*1,
         sig_issue = case_likert(Significant_Issue),
         feelbad = case_likert(Feeling),
         feelwrong = case_likert(Wrong),
         considerwaste = case_likert(Consideration),
         helpreduce = case_likert(Help_Reduction),
         sharesocial = case_likert(Social_Contributes),
         sig_issue = case_likert(Significant_Issue),
         network = case_likert(Promotion),
         discardafter = case_likert(Discard_After),
         discardon = case_likert(Discard_On),
         smellmilk = case_likert(Discard_Smell),
         tastemilk = case_likert(Discard_Taste),
         label_quality = case_likert(LD_indication),
         label_text = case_likert(LD_words),
         female = (Gender %in% c("Female"))*1,
         colldeg = (Education %in% c("Bachelor's degree in college (4-year)", "Master's degree", "Doctoral degree", "Professional degree (JD, MD)"))*1,
         inc100k = (Income %in% c("$100,000-$149,999", "$150,000 or more"))*1,
         married = (Marietal_St %in% c("Married"))*1,
         child = (Child %in% c("Yes"))*1,
         hhgt1 = (Family_Size %in% c("2","3","4","5","6","More than 6"))*1,
         minSL = (Minimum_SL %in% c(1:7))*1
         
         
  )

## Create a Round 1 Column that we can use to difference the bids anchored to their initial value w/o SL info ##
round1 <- clean1 %>% filter(Round == 1) %>% dplyr::select(pid, Bid) %>% rename(Bid1 = Bid)
clean2 <- clean1 %>% left_join(round1) %>% mutate(BidDiff = Bid - Bid1)

cleanSL <- clean2 %>% filter(Round != 1)

## OLS Model ##
lm1 <- lm(Bid ~ Treatment*SL + Treatment*SLSq + Age + smellmilk + sig_issue, data=cleanSL)
summary(lm1)
clus.lm1 <- coeftest(lm1, vcov = vcovCL(lm1, cluster = ~ pid))
rse.lm1 <- coeftest(lm1, vcov = vcovHC(lm1))

## Fixed Effects Panel Model ##
fe1 <- plm(Bid ~ Treatment*SL + Treatment*SLSq, data=cleanSL, effect = "individual", model = "within", index = c("pid"))
rse.fe1 <- summary(fe1, vcov=vcovHC(fe1))
summary(fixef(fe1, type = "dmean"))
summary(fe1)

## Tobit Model ##
## Make a dataframe with no NAs for smellmilk or sig_issue
cleanSL.tobit <- cleanSL %>% dplyr::select(Day, pid, Session, Treatment, Subject, Round, Bid, BidDiff, SL, SLSq, SL3, Age, smellmilk, sig_issue) %>% drop_na()

tobit1 <- censReg(Bid ~ Treatment*SL + Treatment*SLSq + Age + smellmilk + sig_issue, left=0, data=cleanSL.tobit)
sum.tobit1 <- summary(tobit1)
clus.tobit1 <- coeftest(tobit1, vcov = vcovCL(tobit1, cluster = ~ pid, type="HC"))


## TABLE 2 ##
wordreg(list(lm1,fe1,tobit1), file="./reg_results.docx", stars=c(0.01, 0.05, 0.1), digits=3,
        override.se = list(clus.lm1[,2], rse.fe1$coefficients[,2], clus.tobit1[,2]),
        override.pvalues = list(clus.lm1[,4], rse.fe1$coefficients[,4], clus.tobit1[,4]))


## TABLE 1 ##
clean_anova <- cleanSL %>% dplyr::select(-c(Round, SL, SLSq, SL3, f.SL)) %>% group_by(across(-c(Bid, BidDiff))) %>%
  summarize(mean_bid = mean(Bid, na.rm=T), max_bid = max(Bid, na.rm=T), min_bid = min(Bid, na.rm=T), med_bid=median(Bid, na.rm=T),
            mean_diff = mean(BidDiff, na.rm=T), max_diff = max(BidDiff, na.rm=T), min_diff = min(BidDiff, na.rm=T)) %>%
  ungroup()


demographic_factor = c('mean_bid', 'min_bid', 'max_bid', 'max_diff', 'min_diff', 'med_bid', 'Age', 'female', 'colldeg',
                       'inc100k', 'married', 'hhgt1', 'buy_milk_minweekly', 'buy_milk_daily', 'safety', 'veg', 'child',
                       'primaryshopper', 'cons_moreweek', 'choose_longSL', 'minSL')

for(i in demographic_factor){
  print(paste('--------------------------', i, '---------------------'))
  aov.res <- aov(get(i)~Treatment, data = clean_anova)
  print(model.tables(aov.res, 'means'), digits=2)
  print(summary(aov.res))
}


bevahior_ones = c('sig_issue', 'feelbad', 'feelwrong', 'considerwaste', 'helpreduce', 'sharesocial', 'network', 
                  'discardafter', 'discardon', 'smellmilk', 'tastemilk', 'label_quality', 'label_text')
for(i in bevahior_ones){
  print(paste('--------------------------', i, '---------------------'))
  aov.res <- aov(get(i)~Treatment, data = clean_anova)
  print(model.tables(aov.res, 'means'), digits=2)
  print(summary(aov.res))
}


## FIGURE 1 - BOXPLOT ##
cleanSL$f.SL <- factor(cleanSL$SL)
clean.pdata <- cleanSL %>% filter(!is.na(Bid) & Bid > 0)

ggplot(data=clean.pdata) +
  geom_boxplot(aes(x=f.SL, y=Bid), size=1) +
  scale_y_continuous(limits=c(0,15)) +
  facet_wrap(vars(Treatment)) +
  theme_minimal() +
  labs(x="Shelf Life") +
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20), 
        panel.spacing = unit(0.5, "cm"),  strip.text.x = element_text(size=20),
        axis.title = element_text(size=20))


## SHELF-LIFE BASED PRICING MODEL ##
clean2 %>% filter((SL >= 0 | is.na(SL)) & !is.na(Bid) & (Bid > 0.01)) %>% mutate(
  group = case_when(
    is.na(SL) ~ "baseline",
    SL %in% 0:3 ~ "0-3 days",
    SL %in% 4:7 ~ "4-7 days",
    SL >= 8 ~ "8+ days",
    TRUE ~ NA_character_
  ),
  f.group = factor(group, levels=c("baseline", "0-3 days", "4-7 days", "8+ days"))
) %>% group_by(f.group) %>% summarize(count=n(), mean_bid = mean(Bid), 
                                    min_bid = min(Bid),
                                    bid_5 = quantile(Bid, probs=c(0.05)),
                                    bid_25 = quantile(Bid, probs=c(0.25)),
                                    bid_50 = quantile(Bid, probs=c(0.5)),
                                    bid_75 = quantile(Bid, probs=c(0.75)),
                                    bid_95 = quantile(Bid, probs=c(0.95)),
                                    max_bid = max(Bid))

pricedata.fe <- clean2 %>% filter((Round != 1) & (SL >= 0) & !is.na(Bid) & (Bid > 0)) %>% mutate(logbid = log(Bid))

## Fixed Effects Panel Model ##
fe3 <- plm(logbid ~ Treatment*SL + Treatment*SLSq + Treatment*SL3, data=pricedata.fe, effect = "individual", model = "within", index = c("pid"))
summary(fe3)
sum.fe3 <- summary(fe3, vcov=vcovHC(fe3))
within_intercept(fe3)
weighted.mean(fixef(fe3), pdim(fe3)$Tint$Ti)

## OLS Model ##
lm3 <- lm(logbid ~ Treatment*SL + Treatment*SLSq + Treatment*SL3 + Age + smellmilk + sig_issue, data=pricedata.fe)
summary(lm3)
rse.lm3 <- coeftest(lm3, vcov = vcovCL(lm3, cluster = ~ pid))

## APPENDIX TABLE E1 ##
wordreg(list(lm3, fe3), file="./pricing.docx", digits=5, stars = c(0.01, 0.05, 0.1))

        