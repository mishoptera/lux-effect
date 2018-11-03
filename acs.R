library(acs)
setwd("~/Dropbox/AoH/dataMain")
sites<-read.csv("censusTractBlockCodes.csv", header=T)

block<-sites$block
tract<-sites$tract
county<-sites$county
tract<-sprintf("%06d", tract)

geo=geo.make(state="NC", county=county,  tract=tract, block.group=block, combine=FALSE)

# table B19001: "Household Income"
income<-acs.fetch(geography=geo, table.number="B19001")
write.csv(estimate(income), file="income.csv")


# table B01003: "Total Population"
acs.fetch(geography=geo, table.number="B01003")

