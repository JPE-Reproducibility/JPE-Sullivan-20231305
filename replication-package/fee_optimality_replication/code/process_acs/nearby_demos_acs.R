# Create a version of the ACS data that describes nearby ZCTAs

library(dplyr)
library(EconTools)

# Specify paths
inpath.acs  <- 'data/ACS/processed/ACS_data.csv'
outpath.acs <- 'data/ACS/processed/ACS_nearby.csv'

acs <- read.dat(inpath.acs, colClasses = c('zcta' = 'character'))

acs.nearby <- acs

age.vars <- grep('^X[0-9]+', colnames(acs), value = TRUE)
marital.vars <- c('married', 'widowed', 'divorced', 'separated', 'never_married')
edu.vars <- c('less_than_hs', 'hs', 'college', 'advanced')

zcta.map <- readRDS('data/geo/zcta_map.rds')

ZCTAs <- acs$zcta


Pop     <- list()
Age     <- list()
Marital <- list()
Edu     <- list()

for (zcta in names(zcta.map)){
    Z.z <- zcta.map[[zcta]]
    idx.z <- which(acs$zcta %in% Z.z)

    # Combine ZCTAs
    ## Population
    pop.z         <- sum(acs$population[idx.z])
    pop.over.15.z <- sum(acs$pop_over_15[idx.z])
    pop.over.18.z <- sum(acs$pop_over_18[idx.z])
    ## Age
    age.z <- c()
    for (av in age.vars){
        age.z[av] <- sum(acs[idx.z, av])
    }
    ## Marital status
    marital.z <- c()
    weights.z <- acs$pop_over_15[idx.z]/pop.over.15.z
    for (mv in marital.vars){
        marital.z[mv] <- sum(acs[idx.z, mv]*weights.z)
    }
    ## Education
    edu.z <- c()
    for (ev in edu.vars){
        edu.z[ev] <- sum(acs[idx.z, ev])
        edu.z[paste0('share_', ev)] <- edu.z[ev]/pop.over.18.z
    }

    Pop[[zcta]] <- c(population  = pop.z,
                     pop_over_15 = pop.over.15.z,
                     pop_over_18 = pop.over.18.z)
    Age[[zcta]]     <- age.z
    Marital[[zcta]] <- marital.z
    Edu[[zcta]]     <- edu.z
}

Pop.mat     <- as.data.frame(Reduce(rbind, Pop))
Age.mat     <- as.data.frame(Reduce(rbind, Age))
Marital.mat <- as.data.frame(Reduce(rbind, Marital))
Edu.mat     <- as.data.frame(Reduce(rbind, Edu))

Pop.mat$zcta     <- names(zcta.map)
Age.mat$zcta     <- names(zcta.map)
Marital.mat$zcta <- names(zcta.map)
Edu.mat$zcta     <- names(zcta.map)

acs.nearby <- inner_join(Pop.mat, Age.mat, by = 'zcta')
acs.nearby <- inner_join(acs.nearby, Marital.mat, by = 'zcta')
acs.nearby <- inner_join(acs.nearby, Edu.mat, by = 'zcta')

# Age shares
for (av in grep('^X[0-9]+', colnames(acs.nearby), value = TRUE)){
    acs.nearby[, sub('^X', 'share_', av)] <- acs.nearby[, av]/acs.nearby$population
}

# Add some variables
acs.nearby$shr_20_to_34 <- acs.nearby$share_20_to_24 + acs.nearby$share_25_to_29 + acs.nearby$share_30_to_34
acs.nearby$shr_35_to_65 <- acs.nearby$share_35_to_39 + acs.nearby$share_40_to_44 + acs.nearby$share_45_to_49 +
                           acs.nearby$share_50_to_54 + acs.nearby$share_55_to_59 + acs.nearby$share_60_to_64
acs.nearby$shr_65p       <- acs.nearby$share_65.
acs.nearby$share_65.    <- NULL

# Save
write.dat(acs.nearby, file = outpath.acs)

