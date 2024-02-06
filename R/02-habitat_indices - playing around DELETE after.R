# CWD late decay (class 3 & 4 Fauteux et al. 2012)
PlotCWD <- cwdVol(cwd, line)
# Voles
SmMammals <- PlotCWD[Decay_class >= 3, .(LateCWDvol= sum(VolHa)), by=c("PlotID")] # maybe take out class 5 if others also don't include it
SmMammals <- merge(FR_treatments, SmMammals, all.x=TRUE)
SmMammals[is.na(LateCWDvol), LateCWDvol:= 0]

# apply logistic equaiton to late decay cwd
SmMammals[, voleLateCWD := exp(0.5/(1+8*exp(-0.3*LateCWDvol/10)))-0.75]

plot(x=LateCWDvol$TimeSinceFire, y=LateCWDvol$voleLateCWD)

# Deer mice late decay
SmMammals[, dMiceLateCWD := exp(0.6/(1+9*exp(-0.18*LateCWDvol)))-1]

# voles and deer mice average together


# TREES *needs more work, Quebec paper only goes to 20 baph, we max at 40
PlotBAPH <- BAPH(A1trees, B1trees)
# Voles
voles.tree <- PlotBAPH[DBH >= 10, .(BA = sum(BAPH)), by = c("PlotID")]
voles.tree <- merge(FR_treatments, voles.tree, all.x = TRUE)
voles.tree[is.na(BA), BA := 0]

plot(x=voles.tree$TimeSinceFire, y=voles.tree$BA)
