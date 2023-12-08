




#Schwenk et al 2012
#1. overall goal: manage forests for multiple values
#2. set objectives: objectives: 1. store carbon, 2. produce timber/wood products, 3. sustain biodiversity
#3. assign weights
#4. choose management prescriptions
#5. calculate partial utilities
#6. calculate total utilities



#1a. partial utlities carbon and timber

#The partial utility for C storage, UC,i,j, was calculated as the mean annual C stored during the
# 100-year simulation (at site i for prescription j ), divided by the mean annual C for the 
# site-prescription combination with the maximum C storage (considering all four prescriptions).

# mean annual C store not planted/ (max c (planted or not planted)) 

#1b. partial utlities biodiversity

#average occupandy for each bird species
#rescaled occurpancy estimates so species with greater occupancy did not have a disproportionate influence
#then summed across all 51 species

# sum( mean occupancy not planted species 1/ max occupancy for not planted species 1) / max species occupancy
# across all prescriptions (planted or not planted)



#2. Total utility
#weights equal, and then weights unbalanced

# for each site (71) sum(weightC * CpartUtil + weightT * TpartUtil + weightB * BpartUtil)/ number of sites

#we don't need part 3 because we didn't ahve different site types

# maximum utlity under each weiting scenario.