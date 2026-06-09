# Check the representativeness of the Numerator panel
# (OA Table E1 in optimal_fees_OA.tex).

library(EconTools)

# Function for defining income groups
define.income.group <- function(x){
    y <- dplyr::case_when(
        x < 20000 ~ "Under $20k",
        x >= 20000 & x < 40000 ~ "$20k-40k",
        x >= 40000 & x < 60000 ~ "$40k-60k",
        x >= 60000 & x < 80000 ~ "$60k-80k",
        x >= 80000 & x <= 125000 ~ "$80k-125k",
        x > 125000 ~ "Over $125k"
    )
    return(y)
}


inpath.ppl <- 'data/numerator/people_table_combined.csv'
inpath.static <- 'data/numerator/static_users-combined.rds'
inpath.acs    <- 'data/ACS/ACS_one_year_2021_v3.csv'

outpath <- 'output/explore_numerator/representativeness.csv'

ppl <- read.dat(inpath.ppl)
static <- readRDS(inpath.static)
acs <- read.dat(inpath.acs)

# Determine static panelists in April 2021
start.date <- as.Date('2021-04-01')
end.date   <- as.Date('2021-06-30')


static <- static[which(static$START_DATE <= start.date &
                        static$END_DATE   >= end.date), ]

ppl <- dplyr::inner_join(ppl, static, by = 'USER_ID')

# Delete living with partner
ppl$MARITAL_STATUS[ppl$MARITAL_STATUS == 'Living with partner'] <- 'Never married'


# By age, income, race, martial status
## redefine age buckets
young.ages <- c('18-20', '21-24', '25-34')
ppl[which(ppl$AGE_BUCKET %in% young.ages), 'AGE_BUCKET'] <- '18-34'
age.tab <- table(ppl$AGE_BUCKET)
age.tab <- age.tab/sum(age.tab)

mstatus.tab <- table(ppl$MARITAL_STATUS)
mstatus.tab <- mstatus.tab/sum(mstatus.tab)

inc.tab <- table(ppl$INCOME_BUCKET_LONG)
inc.tab <- inc.tab/sum(inc.tab)


## subset to adults
acs <- acs[which(acs$AGE >= 18), ]

acs$AGE_BUCKET <- '65+'
acs$AGE_BUCKET[which(acs$AGE <= 34)]                 <- '18-34'
acs$AGE_BUCKET[which(acs$AGE >= 35 & acs$AGE <= 44)] <- '35-44'
acs$AGE_BUCKET[which(acs$AGE >= 45 & acs$AGE <= 54)] <- '45-54'
acs$AGE_BUCKET[which(acs$AGE >= 55 & acs$AGE <= 64)] <- '55-64'

acs$INCOME_BUCKET_TOT_COARSE <- define.income.group(acs$INCTOT)
acs$INCOME_BUCKET_FAM_COARSE <- define.income.group(acs$FTOTINC)


ppl$INCOME_BUCKET_COARSE <- with(ppl, dplyr::case_when(
    INCOME_BUCKET_LONG == "Less than $20,000" ~ "Under $20k",
    INCOME_BUCKET_LONG %in% c("$20,000-$29,999", "$30,000-$39,999") ~ "$20k-40k",
    INCOME_BUCKET_LONG %in% c("$40,000-$49,999", "$50,000-$59,999") ~ "$40k-60k",
    INCOME_BUCKET_LONG %in% c("$60,000-$69,999", "$70,000-$79,999") ~ "$60k-80k",
    INCOME_BUCKET_LONG %in% c("$80,000-$89,999", "$90,000-$99,999", "$100,000-$124,999") ~ "$80k-125k",
    INCOME_BUCKET_LONG %in% c("$125,000-$149,999", "$150,000-$174,999", "$175,000-$199,999",
                         "$200,000-$224,999", "$225,000-$249,999", "$250,000 +") ~ "Over $125k"
))


mstatus.key <- c('Married', 'Married', 'Separated', 'Divorced', 'Widower', 'Never married')
acs$MARITAL_STATUS <- mstatus.key[acs$MARST]

age.acs <- c()
for (k in names(age.tab)){
    age.acs[k] <- sum(acs$PERWT[which(acs$AGE_BUCKET == k)])
}
age.acs <- age.acs/sum(age.acs)


mstatus.acs <- c()
for (k in names(mstatus.tab)){
    mstatus.acs[k] <- sum(acs$PERWT[which(acs$MARITAL_STATUS == k)])
}
mstatus.acs <- mstatus.acs/sum(mstatus.acs)

# Compare ages
age.compare <-
    data.frame(var = names(age.tab),
               nmr = as.numeric(age.tab),
               acs = age.acs[names(age.tab)])

# Compare marital status
mstatus.compare <-
    data.frame(var = names(mstatus.tab),
               nmr = as.numeric(mstatus.tab),
               acs = mstatus.acs[names(mstatus.tab)])

# Compare income (family income, HHWT-weighted)
inc.tab     <- prop.table(table(ppl$INCOME_BUCKET_COARSE))
inc.acs.fam <- prop.table(xtabs(HHWT ~ INCOME_BUCKET_FAM_COARSE, data = acs))[names(inc.tab)]

inc.labels <- c("Under $20k",
                "$20k-40k",
                "$40k-60k",
                "$60k-80k",
                "$80k-125k",
                "Over $125k")

inc.compare <- data.frame(nmr = as.numeric(inc.tab[inc.labels]),
                          acs = as.numeric(inc.acs.fam[inc.labels]))
inc.compare$var <- inc.labels



# Ethnicity
ethnic.tab <- prop.table(table(ppl$ETHNICITY))

ethnic.key <- c('White/Caucasian', 'Black or African American', 'Other',
                'Asian', 'Asian', 'Asian',
                'Other', 'Other', 'Other')
acs$ETHNICITY <- ethnic.key[acs$RACE]

## Add in Hispanic/Latino
idx <- which(acs$HISPAN %in% 1:4)
acs$ETHNICITY[idx] <- 'Hispanic/Latino'

ethnic.labels <- c("White/Caucasian", "Black or African American", "Hispanic/Latino",
                   "Asian", "Other")

ethnic.acs <- xtabs(PERWT ~ ETHNICITY, data = acs)
ethnic.acs <- prop.table(ethnic.acs)[ethnic.labels]



ethnic.compare <- data.frame(nmr  = as.numeric(ethnic.tab[ethnic.labels]),
                             acs  = as.numeric(ethnic.acs))
ethnic.compare$var <- names(ethnic.tab[ethnic.labels])

## Has children
child.nmr <- mean(ppl$HAS_CHILDREN == 'Yes')
child.acs <- weighted.mean(acs$NCHILD >= 1, acs$PERWT)
child.compare <- data.frame(nmr = child.nmr, acs = child.acs, var = 'Has children')

# Produce table
## Add indicators to final rows
age.compare$lineflag     <- 0
inc.compare$lineflag     <- 0
mstatus.compare$lineflag <- 0
child.compare$lineflag   <- 0
ethnic.compare$lineflag  <- 0
age.compare$lineflag[nrow(age.compare)]         <- 1
inc.compare$lineflag[nrow(inc.compare)]         <- 1
mstatus.compare$lineflag[nrow(mstatus.compare)] <- 1
child.compare$lineflag[nrow(child.compare)]     <- 1
ethnic.compare$lineflag[nrow(ethnic.compare)]   <- 1

## Combine tables
combo.tab <- do.call(dplyr::bind_rows, list(age.compare, inc.compare, mstatus.compare,
                                            child.compare, ethnic.compare))
combo.tab$nmr <- sprintf('%0.2f', combo.tab$nmr)
combo.tab$acs <- sprintf('%0.2f', combo.tab$acs)

combo.tab$var <- sub('$', '\\$', combo.tab$var, fixed = TRUE)

write.dat(combo.tab, file = outpath)

