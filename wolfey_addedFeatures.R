#works for three splits of under 5 minute rounds per instance in one csv file

# read in the separate csv files of the Mazvidal and Till fight then the Diaz and Pettis fight
MaTi <- read.csv('wolfey.csv', header=TRUE, sep=',', na.strings=c('','NA'))

wolf <- read.csv('sara_SarahSarahEatenByWolves.csv', sep=',', 
                 header=T, na.strings=c('','NA')) #N*n*s df 

colnames(MaTi) <- colnames(wolf)

library(dplyr)

# remove the instances with no action from either one
Added <- filter(MaTi, MaTi$FighterActionReactions.X1 !=0 | MaTi$FightersActionsReactions.X2 !=0)
rm(MaTi)
rm(wolf)

Sym <- strsplit(as.character(Added$FighterActionReactions.X1), ',')

sq1 <- lapply(Sym,'[',1)
#landed in first sequence
kicks_sq1 <- grep('land.*kick', sq1)
elbows_sq1 <- grep('land.*elbow', sq1)
knees_sq1 <- grep('land.*knee', sq1)
jab_sq1 <- grep('land.*jab', sq1)
cross_sq1 <- grep('land.*cross', sq1)
hook_sq1 <- grep('land.*hook', sq1)
upper_sq1 <- grep('land.*upp', sq1)
takedown_sq1 <- grep('land.*takedown', sq1)
hammer_sq1 <- grep('land.*hammer', sq1)

#missed in 1st sequence
kicks_sq1m <- grep('miss.*kick', sq1)
elbows_sq1m <- grep('miss.*elbow', sq1)
knees_sq1m <- grep('miss.*knee', sq1)
jab_sq1m <- grep('miss.*jab', sq1)
cross_sq1m <- grep('miss.*cross', sq1)
hook_sq1m <- grep('miss.*hook', sq1)
upper_sq1m <- grep('miss.*upp', sq1)
takedown_sq1m <- grep('miss.*takedown', sq1)
hammer_sq1m <- grep('miss.*hammer', sq1)


sq2 <- lapply(Sym,'[',2)
#landed in second sequence
kicks_sq2 <- grep('land*kick', sq2)
elbows_sq2 <- grep('land.*elbow', sq2)
knees_sq2 <- grep('land.*knee', sq2)
jab_sq2 <- grep('land.*jab', sq2)
cross_sq2 <- grep('land.*cross', sq2)
hook_sq2 <- grep('land.*hook', sq2)
upper_sq2 <- grep('land.*upp', sq2)
takedown_sq2 <- grep('land.*takedown', sq2)
hammer_sq2 <- grep('land.*hammer', sq2)

#missed in 2nd sequence
kicks_sq2m <- grep('miss.*kick', sq2)
elbows_sq2m <- grep('miss.*elbow', sq2)
knees_sq2m <- grep('miss.*knee', sq2)
jab_sq2m <- grep('miss.*jab', sq2)
cross_sq2m <- grep('miss.*cross', sq2)
hook_sq2m <- grep('miss.*hook', sq2)
upper_sq2m <- grep('miss.*upp', sq2)
takedown_sq2m <- grep('miss.*takedown', sq2)
hammer_sq2m <- grep('miss.*hammer', sq2)


sq3 <- lapply(Sym,'[',3)
#landed in 3rd sequence
kicks_sq3 <- grep('land.*kick', sq3)
elbows_sq3 <- grep('land.*elbow', sq3)
knees_sq3 <- grep('land.*knee', sq3)
jab_sq3 <- grep('land.*jab', sq3)
cross_sq3 <- grep('land.*cross', sq3)
hook_sq3 <- grep('land.*hook', sq3)
upper_sq3 <- grep('land.*upp', sq3)
takedown_sq3 <- grep('land.*takedown', sq3)
hammer_sq3 <- grep('land.*hammer', sq3)

#missed in 3rd sequence
kicks_sq3m <- grep('miss.*kick', sq3)
elbows_sq3m <- grep('miss.*elbow', sq3)
knees_sq3m <- grep('miss.*knee', sq3)
jab_sq3m <- grep('miss.*jab', sq3)
cross_sq3m <- grep('miss.*cross', sq3)
hook_sq3m <- grep('miss.*hook', sq3)
upper_sq3m <- grep('miss.*upp', sq3)
takedown_sq3m <- grep('miss.*takedown', sq3)
hammer_sq3m <- grep('miss.*hammer', sq3)


Sym2 <- strsplit(as.character(Added$FightersActionsReactions.X2), ',')

sq1b <- lapply(Sym2,'[',1)#lands 1st sequence X2
kicks_sq1b <- grep('land.*kick', sq1b)
elbows_sq1b <- grep('land.*elbow', sq1b)
knees_sq1b <- grep('land.*knee', sq1b)
jab_sq1b <- grep('land.*jab', sq1b)
cross_sq1b <- grep('land.*cross', sq1b)
hook_sq1b <- grep('land.*hook', sq1b)
upper_sq1b <- grep('land.*upp', sq1b)
takedown_sq1b <- grep('land.*takedown', sq1b)
hammer_sq1b <- grep('land.*hammer', sq1b)

#received by X1 in 1st sequence equivalent to hits landed 1st seq of x2
kicks_sq1r <- grep('land.*kick', sq1b)
elbows_sq1r <- grep('land.*elbow', sq1b)
knees_sq1r <- grep('land.*knee', sq1b)
jab_sq1r <- grep('land.*jab', sq1b)
cross_sq1r <- grep('land.*cross', sq1b)
hook_sq1r <- grep('land.*hook', sq1b)
upper_sq1r <- grep('land.*upp', sq1b)
takedown_sq1r <- grep('land.*takedown', sq1b)
hammer_sq1r <- grep('land.*hammer', sq1b)

#missed in 1st sequence X2
kicks_sq1bm <- grep('miss.*kick', sq1b)
elbows_sq1bm <- grep('miss.*elbow', sq1b)
knees_sq1bm <- grep('miss.*knee', sq1b)
jab_sq1bm <- grep('miss.*jab', sq1b)
cross_sq1bm <- grep('miss.*cross', sq1b)
hook_sq1bm <- grep('miss.*hook', sq1b)
upper_sq1bm <- grep('miss.*upp', sq1b)
takedown_sq1bm <- grep('miss.*takedown', sq1b)
hammer_sq1bm <- grep('miss.*hammer', sq1b)

#received by x2 in 1st seq equivalent to lands by x1
kicks_sq1br <- grep('land.*kick', sq1)
elbows_sq1br <- grep('land.*elbow', sq1)
knees_sq1br <- grep('land.*knee', sq1)
jab_sq1br <- grep('land.*jab', sq1)
cross_sq1br <- grep('land.*cross', sq1)
hook_sq1br <- grep('land.*hook', sq1)
upper_sq1br <- grep('land.*upp', sq1)
takedown_sq1br <- grep('land.*takedown', sq1)
hammer_sq1br <- grep('land.*hammer', sq1)

sq2b <- lapply(Sym2,'[',2)#lands 2nd sequence x2
kicks_sq2b <- grep('land.*kick', sq2b)
elbows_sq2b <- grep('land.*elbow', sq2b)
knees_sq2b <- grep('land.*knee', sq2b)
jab_sq2b <- grep('land.*jab', sq2b)
cross_sq2b <- grep('land.*cross', sq2b)
hook_sq2b <- grep('land.*hook', sq2b)
upper_sq2b <- grep('land.*upp', sq2b)
takedown_sq2b <- grep('land.*takedown', sq2b)
hammer_sq2b <- grep('land.*hammer', sq2b)

# received by X1 in 2nd sequence equivalent to hits landed by x2 seq 2
kicks_sq2r <- grep('land.*kick', sq2b)
elbows_sq2r <- grep('land.*elbow', sq2b)
knees_sq2r <- grep('land.*knee', sq2b)
jab_sq2r <- grep('land.*jab', sq2b)
cross_sq2r <- grep('land.*cross', sq2b)
hook_sq2r <- grep('land.*hook', sq2b)
upper_sq2r <- grep('land.*upp', sq2b)
takedown_sq2r <- grep('land.*takedown', sq2b)
hammer_sq2r <- grep('land.*hammer', sq2b)

#missed in 2nd sequence x2
kicks_sq2bm <- grep('miss.*kick', sq2b)
elbows_sq2bm <- grep('miss.*elbow', sq2b)
knees_sq2bm <- grep('miss.*knee', sq2b)
jab_sq2bm <- grep('miss.*jab', sq2b)
cross_sq2bm <- grep('miss.*cross', sq2b)
hook_sq2bm <- grep('miss.*hook', sq2b)
upper_sq2bm <- grep('miss.*upp', sq2b)
takedown_sq2bm <- grep('miss.*takedown', sq2b)
hammer_sq2bm <- grep('miss.*hammer', sq2b)

#received 2nd seq by x2 equivalent to hits landed by x1 in seq 2
kicks_sq2br <- grep('land.*kick', sq2)
elbows_sq2br <- grep('land.*elbow', sq2)
knees_sq2br <- grep('land.*knee', sq2)
jab_sq2br <- grep('land.*jab', sq2)
cross_sq2br <- grep('land.*cross', sq2)
hook_sq2br <- grep('land.*hook', sq2)
upper_sq2br <- grep('land.*upp', sq2)
takedown_sq2br <- grep('land.*takedown', sq2)
hammer_sq2br <- grep('land.*hammer', sq2)

sq3b <- lapply(Sym2,'[',3)#lands 3rd sequence x2
kicks_sq3b <- grep('land.*kick', sq3b)
elbows_sq3b <- grep('land.*elbow', sq3b)
knees_sq3b <- grep('land.*knee', sq3b)
jab_sq3b <- grep('land.*jab', sq3b)
cross_sq3b <- grep('land.*cross', sq3b)
hook_sq3b <- grep('land.*hook', sq3b)
upper_sq3b <- grep('land.*upp', sq3b)
takedown_sq3b <- grep('land.*takedown', sq3b)
hammer_sq3b <- grep('land.*hammer', sq3b)

#received by X1 in 3rd sequence equivalent to hits landed by X2 in seq 3
kicks_sq3r <- grep('land.*kick', sq3b)
elbows_sq3r <- grep('land.*elbow', sq3b)
knees_sq3r <- grep('land.*knee', sq3b)
jab_sq3r <- grep('land.*jab', sq3b)
cross_sq3r <- grep('land.*cross', sq3b)
hook_sq3r <- grep('land.*hook', sq3b)
upper_sq3r <- grep('land.*upp', sq3b)
takedown_sq3r <- grep('land.*takedown', sq3b)
hammer_sq3r <- grep('land.*hammer', sq3b)

#missed in 3rd sequence x2
kicks_sq3bm <- grep('miss.*kick', sq3b)
elbows_sq3bm <- grep('miss.*elbow', sq3b)
knees_sq3bm <- grep('miss.*knee', sq3b)
jab_sq3bm <- grep('miss.*jab', sq3b)
cross_sq3bm <- grep('miss.*cross', sq3b)
hook_sq3bm <- grep('miss.*hook', sq3b)
upper_sq3bm <- grep('miss.*upp', sq3b)
takedown_sq3bm <- grep('miss.*takedown', sq3b)
hammer_sq3bm <- grep('miss.*hammer', sq3b)

#received in seq 3 by x2 equivalent to hits landed by x1 in seq3
kicks_sq3br <- grep('land.*kick', sq3)
elbows_sq3br <- grep('land.*elbow', sq3)
knees_sq3br <- grep('land.*knee', sq3)
jab_sq3br <- grep('land.*jab', sq3)
cross_sq3br <- grep('land.*cross', sq3)
hook_sq3br <- grep('land.*hook', sq3)
upper_sq3br <- grep('land.*upp', sq3)
takedown_sq3br <- grep('land.*takedown', sq3)
hammer_sq3br <- grep('land.*hammer', sq3)


# # add in individual fight actions landed (stated in the notes)for each fighter
# kicks_Mazvidal <- grep('^land.*kick.', Added$FighterActionReactions.X1)
# kicks_Till <- grep('^land.*kick.', Added$FighterActionReactions.X2)
# 
# elbows_Mazvidal <- grep('^land.*elbow.', Added$FighterActionReactions.X1)
# elbows_Till <- grep('^land.*elbow.', Added$FighterActionReactions.X2)
# 
# knees_Mazvidal <- grep('^land.*knee.', Added$FighterActionReactions.X1)
# knees_Till <- grep('^land.*knee.', Added$FighterActionReactions.X2)
# 
# jab_Mazvidal <- grep('^land.*jab.', Added$FighterActionReactions.X1)
# jab_Till <- grep('^land.*jab.', Added$FighterActionReactions.X2)
# 
# cross_Mazvidal <- grep('^land.*cross.', Added$FighterActionReactions.X1)
# cross_Till <- grep('^land.*cross.', Added$FighterActionReactions.X2)
# 
# hook_Mazvidal <- grep('^land.*hook.', Added$FighterActionReactions.X1)
# hook_Till <- grep('^land.*hook.', Added$FighterActionReactions.X2)
# 
# upper_Mazvidal <- grep('^land.*upp.', Added$FighterActionReactions.X1)
# upper_Till <- grep('^land.*upp.', Added$FighterActionReactions.X2)
# 
# takedown_Mazvidal <- grep('^land.*takedown.', Added$FighterActionReactions.X1)
# takedown_Till <- grep('^land.*takedown.', Added$FighterActionReactions.X2)
# 
# hammer_Mazvidal <- grep('^land.*hammer.', Added$FighterActionReactions.X1)
# hammer_Till <- grep('^land.*hammer.', Added$FighterActionReactions.X2)

added_landed <- mutate(Added, Crossl.X1=0, Kneel.X1=0, Elbowl.X1=0, Hookl.X1=0, Jabl.X1=0, Kickl.X1=0,
                       Crossl.X2=0, Kneel.X2=0, Elbowl.X2=0, Hookl.X2=0, Jabl.X2=0, Kickl.X2=0, upperl.X1=0,
                       upperl.X2=0, takedownl.X1=0, takedownl.X2=0, hammerl.X1=0, hammerl.X2=0
                       , Cross2l.X1=0, Knee2l.X1=0, Elbow2l.X1=0, Hook2l.X1=0, Jab2l.X1=0, Kick2l.X1=0,
                       Cross2l.X2=0, Knee2l.X2=0, Elbow2l.X2=0, Hook2l.X2=0, Jab2l.X2=0, Kick2l.X2=0, upper2l.X1=0,
                       upper2l.X2=0, takedown2l.X1=0, takedown2l.X2=0, hammer2l.X1=0, hammer2l.X2=0
                       , Cross3l.X1=0, Knee3l.X1=0, Elbow3l.X1=0, Hook3l.X1=0, Jab3l.X1=0, Kick3l.X1=0,
                       Cross3l.X2=0, Knee3l.X2=0, Elbow3l.X2=0, Hook3l.X2=0, Jab3l.X2=0, Kick3l.X2=0, upper3l.X1=0,
                       upper3l.X2=0, takedown3l.X1=0, takedown3l.X2=0, hammer3l.X1=0, hammer3l.X2=0)

added_missed <- mutate(added_landed, Crossm.X1=0, Kneem.X1=0, Elbowm.X1=0, Hookm.X1=0, Jabm.X1=0, Kickm.X1=0,
                       Crossm.X2=0, Kneem.X2=0, Elbowm.X2=0, Hookm.X2=0, Jabm.X2=0, Kickm.X2=0, upperm.X1=0,
                       upperm.X2=0, takedownm.X1=0, takedownm.X2=0, hammerm.X1=0, hammerm.X2=0
                       , Cross2m.X1=0, Knee2m.X1=0, Elbow2m.X1=0, Hook2m.X1=0, Jab2m.X1=0, Kick2m.X1=0,
                       Cross2m.X2=0, Knee2m.X2=0, Elbow2m.X2=0, Hook2m.X2=0, Jab2m.X2=0, Kick2m.X2=0, upper2m.X1=0,
                       upper2m.X2=0, takedown2m.X1=0, takedown2m.X2=0, hammer2m.X1=0, hammer2m.X2=0
                       , Cross3m.X1=0, Knee3m.X1=0, Elbow3m.X1=0, Hook3m.X1=0, Jab3m.X1=0, Kick3m.X1=0,
                       Cross3m.X2=0, Knee3m.X2=0, Elbow3m.X2=0, Hook3m.X2=0, Jab3m.X2=0, Kick3m.X2=0, upper3m.X1=0,
                       upper3m.X2=0, takedown3m.X1=0, takedown3m.X2=0, hammer3m.X1=0, hammer3m.X2=0)

added_received <- mutate(added_missed, Crossr.X1=0, Kneer.X1=0, Elbowr.X1=0, Hookr.X1=0, Jabr.X1=0, Kickr.X1=0,
                         Crossr.X2=0, Kneer.X2=0, Elbowr.X2=0, Hookr.X2=0, Jabr.X2=0, Kickr.X2=0, upperr.X1=0,
                         upperr.X2=0, takedownr.X1=0, takedownr.X2=0, hammerr.X1=0, hammerr.X2=0
                         , Cross2r.X1=0, Knee2r.X1=0, Elbow2r.X1=0, Hook2r.X1=0, Jab2r.X1=0, Kick2r.X1=0,
                         Cross2r.X2=0, Knee2r.X2=0, Elbow2r.X2=0, Hook2r.X2=0, Jab2r.X2=0, Kick2r.X2=0, upper2r.X1=0,
                         upper2r.X2=0, takedown2r.X1=0, takedown2r.X2=0, hammer2r.X1=0, hammer2r.X2=0
                         , Cross3r.X1=0, Knee3r.X1=0, Elbow3r.X1=0, Hook3r.X1=0, Jab3r.X1=0, Kick3r.X1=0,
                         Cross3r.X2=0, Knee3r.X2=0, Elbow3r.X2=0, Hook3r.X2=0, Jab3r.X2=0, Kick3r.X2=0, upper3r.X1=0,
                         upper3r.X2=0, takedown3r.X1=0, takedown3r.X2=0, hammer3r.X1=0, hammer3r.X2=0)

Added <- added_received
rm(added_received);rm(added_missed);rm(added_landed)

# Added[cross_Mazvidal,'Cross.X1'] <- 1
# Added[cross_Till,'Cross.X2'] <- 1
# Added[hook_Mazvidal,'Hook.X1'] <- 1
# Added[hook_Till,'Hook.X2'] <- 1
# Added[jab_Mazvidal,'Jab.X1'] <- 1
# Added[jab_Till,'Jab.X2'] <- 1
# Added[knees_Mazvidal,'Knee.X1'] <- 1
# Added[knees_Till,'Knee.X2'] <- 1
# Added[elbows_Mazvidal,'Elbow.X1'] <- 1
# Added[elbows_Till,'Elbow.X2'] <- 1
# Added[kicks_Mazvidal,'Kick.X1'] <- 1
# Added[kicks_Till,'Kick.X2'] <- 1
# Added[upper_Mazvidal,'upper.X1'] <- 1
# Added[upper_Till,'upper.X2'] <- 1
# Added[takedown_Mazvidal,'takedown.X1'] <- 1
# Added[takedown_Till,'takedown.X2'] <- 1
# Added[hammer_Mazvidal,'hammer.X1'] <- 1
# Added[hammer_Till,'hammer.X2'] <- 1


Added[cross_sq1,'Crossl.X1'] <- 1
Added[cross_sq1b,'Crossl.X2'] <- 1
Added[hook_sq1,'Hookl.X1'] <- 1
Added[hook_sq1b,'Hookl.X2'] <- 1
Added[jab_sq1,'Jabl.X1'] <- 1
Added[jab_sq1b,'Jabl.X2'] <- 1
Added[knees_sq1,'Kneel.X1'] <- 1
Added[knees_sq1b,'Kneel.X2'] <- 1
Added[elbows_sq1,'Elbowl.X1'] <- 1
Added[elbows_sq1b,'Elbowl.X2'] <- 1
Added[kicks_sq1,'Kickl.X1'] <- 1
Added[kicks_sq1b,'Kickl.X2'] <- 1
Added[upper_sq1,'upperl.X1'] <- 1
Added[upper_sq1b,'upperl.X2'] <- 1
Added[takedown_sq1,'takedownl.X1'] <- 1
Added[takedown_sq1b,'takedownl.X2'] <- 1
Added[hammer_sq1,'hammerl.X1'] <- 1
Added[hammer_sq1b,'hammerl.X2'] <- 1

Added[cross_sq2,'Cross2l.X1'] <- 1
Added[cross_sq2b,'Cross2l.X2'] <- 1
Added[hook_sq2,'Hook2l.X1'] <- 1
Added[hook_sq2b,'Hook2l.X2'] <- 1
Added[jab_sq2,'Jab2l.X1'] <- 1
Added[jab_sq2b,'Jab2l.X2'] <- 1
Added[knees_sq2,'Knee2l.X1'] <- 1
Added[knees_sq2b,'Knee2l.X2'] <- 1
Added[elbows_sq2,'Elbow2l.X1'] <- 1
Added[elbows_sq2b,'Elbow2l.X2'] <- 1
Added[kicks_sq2,'Kick2l.X1'] <- 1
Added[kicks_sq2b,'Kick2l.X2'] <- 1
Added[upper_sq2,'upper2l.X1'] <- 1
Added[upper_sq2b,'upper2l.X2'] <- 1
Added[takedown_sq2,'takedown2l.X1'] <- 1
Added[takedown_sq2b,'takedown2l.X2'] <- 1
Added[hammer_sq2,'hammer2l.X1'] <- 1
Added[hammer_sq2b,'hammer2l.X2'] <- 1

Added[cross_sq3,'Cross3l.X1'] <- 1
Added[cross_sq3b,'Cross3l.X2'] <- 1
Added[hook_sq3,'Hook3l.X1'] <- 1
Added[hook_sq3b,'Hook3l.X2'] <- 1
Added[jab_sq3,'Jab3l.X1'] <- 1
Added[jab_sq3b,'Jab3l.X2'] <- 1
Added[knees_sq3,'Knee3l.X1'] <- 1
Added[knees_sq3b,'Knee3l.X2'] <- 1
Added[elbows_sq3,'Elbow3l.X1'] <- 1
Added[elbows_sq3b,'Elbow3l.X2'] <- 1
Added[kicks_sq3,'Kick3l.X1'] <- 1
Added[kicks_sq3b,'Kick3l.X2'] <- 1
Added[upper_sq3,'upper3l.X1'] <- 1
Added[upper_sq3b,'upper3l.X2'] <- 1
Added[takedown_sq3,'takedown3l.X1'] <- 1
Added[takedown_sq3b,'takedown3l.X2'] <- 1
Added[hammer_sq3,'hammer3l.X1'] <- 1
Added[hammer_sq3b,'hammer3l.X2'] <- 1

Added[cross_sq1m,'Crossm.X1'] <- 1
Added[cross_sq1bm,'Crossm.X2'] <- 1
Added[hook_sq1m,'Hookm.X1'] <- 1
Added[hook_sq1bm,'Hookm.X2'] <- 1
Added[jab_sq1m,'Jabm.X1'] <- 1
Added[jab_sq1bm,'Jabm.X2'] <- 1
Added[knees_sq1m,'Kneem.X1'] <- 1
Added[knees_sq1bm,'Kneem.X2'] <- 1
Added[elbows_sq1m,'Elbowm.X1'] <- 1
Added[elbows_sq1bm,'Elbowm.X2'] <- 1
Added[kicks_sq1m,'Kickm.X1'] <- 1
Added[kicks_sq1bm,'Kickm.X2'] <- 1
Added[upper_sq1m,'upperm.X1'] <- 1
Added[upper_sq1bm,'upperm.X2'] <- 1
Added[takedown_sq1m,'takedownm.X1'] <- 1
Added[takedown_sq1bm,'takedownm.X2'] <- 1
Added[hammer_sq1m,'hammerm.X1'] <- 1
Added[hammer_sq1bm,'hammerm.X2'] <- 1

Added[cross_sq2m,'Cross2m.X1'] <- 1
Added[cross_sq2bm,'Cross2m.X2'] <- 1
Added[hook_sq2m,'Hook2m.X1'] <- 1
Added[hook_sq2bm,'Hook2m.X2'] <- 1
Added[jab_sq2m,'Jab2m.X1'] <- 1
Added[jab_sq2bm,'Jab2m.X2'] <- 1
Added[knees_sq2m,'Knee2m.X1'] <- 1
Added[knees_sq2bm,'Knee2m.X2'] <- 1
Added[elbows_sq2m,'Elbow2m.X1'] <- 1
Added[elbows_sq2bm,'Elbow2m.X2'] <- 1
Added[kicks_sq2m,'Kick2m.X1'] <- 1
Added[kicks_sq2bm,'Kick2m.X2'] <- 1
Added[upper_sq2m,'upper2m.X1'] <- 1
Added[upper_sq2bm,'upper2m.X2'] <- 1
Added[takedown_sq2m,'takedown2m.X1'] <- 1
Added[takedown_sq2bm,'takedown2m.X2'] <- 1
Added[hammer_sq2m,'hammer2m.X1'] <- 1
Added[hammer_sq2bm,'hammer2m.X2'] <- 1

Added[cross_sq3m,'Cross3m.X1'] <- 1
Added[cross_sq3bm,'Cross3m.X2'] <- 1
Added[hook_sq3m,'Hook3m.X1'] <- 1
Added[hook_sq3bm,'Hook3m.X2'] <- 1
Added[jab_sq3m,'Jab3m.X1'] <- 1
Added[jab_sq3bm,'Jab3m.X2'] <- 1
Added[knees_sq3m,'Knee3m.X1'] <- 1
Added[knees_sq3bm,'Knee3m.X2'] <- 1
Added[elbows_sq3m,'Elbow3m.X1'] <- 1
Added[elbows_sq3bm,'Elbow3m.X2'] <- 1
Added[kicks_sq3m,'Kick3m.X1'] <- 1
Added[kicks_sq3bm,'Kick3m.X2'] <- 1
Added[upper_sq3m,'upper3m.X1'] <- 1
Added[upper_sq3bm,'upper3m.X2'] <- 1
Added[takedown_sq3m,'takedown3m.X1'] <- 1
Added[takedown_sq3bm,'takedown3m.X2'] <- 1
Added[hammer_sq3m,'hammer3m.X1'] <- 1
Added[hammer_sq3bm,'hammer3m.X2'] <- 1

Added[cross_sq1r,'Crossr.X1'] <- 1
Added[cross_sq1br,'Crossr.X2'] <- 1
Added[hook_sq1r,'Hookr.X1'] <- 1
Added[hook_sq1br,'Hookr.X2'] <- 1
Added[jab_sq1r,'Jabr.X1'] <- 1
Added[jab_sq1br,'Jabr.X2'] <- 1
Added[knees_sq1r,'Kneer.X1'] <- 1
Added[knees_sq1br,'Kneer.X2'] <- 1
Added[elbows_sq1r,'Elbowr.X1'] <- 1
Added[elbows_sq1br,'Elbowr.X2'] <- 1
Added[kicks_sq1r,'Kickr.X1'] <- 1
Added[kicks_sq1br,'Kickr.X2'] <- 1
Added[upper_sq1r,'upperr.X1'] <- 1
Added[upper_sq1br,'upperr.X2'] <- 1
Added[takedown_sq1r,'takedownr.X1'] <- 1
Added[takedown_sq1br,'takedownr.X2'] <- 1
Added[hammer_sq1r,'hammerr.X1'] <- 1
Added[hammer_sq1br,'hammerr.X2'] <- 1

Added[cross_sq2r,'Cross2r.X1'] <- 1
Added[cross_sq2br,'Cross2r.X2'] <- 1
Added[hook_sq2r,'Hook2r.X1'] <- 1
Added[hook_sq2br,'Hook2r.X2'] <- 1
Added[jab_sq2r,'Jab2r.X1'] <- 1
Added[jab_sq2br,'Jab2r.X2'] <- 1
Added[knees_sq2r,'Knee2r.X1'] <- 1
Added[knees_sq2br,'Knee2r.X2'] <- 1
Added[elbows_sq2r,'Elbow2r.X1'] <- 1
Added[elbows_sq2br,'Elbow2r.X2'] <- 1
Added[kicks_sq2r,'Kick2r.X1'] <- 1
Added[kicks_sq2br,'Kick2r.X2'] <- 1
Added[upper_sq2r,'upper2r.X1'] <- 1
Added[upper_sq2br,'upper2r.X2'] <- 1
Added[takedown_sq2r,'takedown2r.X1'] <- 1
Added[takedown_sq2br,'takedown2r.X2'] <- 1
Added[hammer_sq2r,'hammer2r.X1'] <- 1
Added[hammer_sq2br,'hammer2r.X2'] <- 1

Added[cross_sq3r,'Cross3r.X1'] <- 1
Added[cross_sq3br,'Cross3r.X2'] <- 1
Added[hook_sq3r,'Hook3r.X1'] <- 1
Added[hook_sq3br,'Hook3r.X2'] <- 1
Added[jab_sq3r,'Jab3r.X1'] <- 1
Added[jab_sq3br,'Jab3r.X2'] <- 1
Added[knees_sq3r,'Knee3r.X1'] <- 1
Added[knees_sq3br,'Knee3r.X2'] <- 1
Added[elbows_sq3r,'Elbow3r.X1'] <- 1
Added[elbows_sq3br,'Elbow3r.X2'] <- 1
Added[kicks_sq3r,'Kick3r.X1'] <- 1
Added[kicks_sq3br,'Kick3r.X2'] <- 1
Added[upper_sq3r,'upper3r.X1'] <- 1
Added[upper_sq3br,'upper3r.X2'] <- 1
Added[takedown_sq3r,'takedown3r.X1'] <- 1
Added[takedown_sq3br,'takedown3r.X2'] <- 1
Added[hammer_sq3r,'hammer3r.X1'] <- 1
Added[hammer_sq3br,'hammer3r.X2'] <- 1

#Added <- Added[,-2]
#Seconds <- mutate(Added, SecondsIntoRound=300-(as.numeric(Added$Time)))

#seconds <- Seconds[,c(1,181,2:180)]
#rm(Seconds);rm(Added)

Sec <- as.data.frame(Added$SecondsIntoRound)
colnames(Sec) <- 'previousAction'
initial <- as.data.frame(0)
colnames(initial) <- 'previousAction'
lastAction <- rbind(initial,Sec)
lastAction <- lastAction[1:92,]

Added <- cbind(Added,lastAction)
Added <- Added[,c(1:2,182,3:181)]
Added <- Added[,-4]
Added$lastAction <- as.integer(Added$lastAction)

#make sure the lastAction field is int and not num class, str() to check
last <- mutate(Added, SecondsLastRoundAction=if_else(Added$Round==1,
                                                       Added$SecondsIntoRound-Added$lastAction,
                                                       Added$SecondsIntoRound))
last <- last[,c(1:3,182,4:181)]

landX1 <- colnames(last)[c(21:26,33,35,37,39:44,51,53,55,57:62,69,71,73)]
landX2 <- colnames(last)[c(27:32,34,36,38,45:50,52,54,56,63:68,70,72,74)]

missX1 <- colnames(last)[c(75:80,87,89,91,93:98,105,107,109,111:116,123,125,127)]
missX2 <- colnames(last)[c(81:86,88,90,92,99:104,106,108,110,117:122,124,126,128)]

recvX1 <- colnames(last)[c(129:134,141,143,145,147:152,159,161,163,165:170,177,179,181)]
recvX2 <- colnames(last)[c(135:140,142,144,146,153:158,160,162,164,171:176,178,180,182)]

x1l <- mutate(last, TotLandsX1=last[,21]+last[,22]+last[,23]+last[,24]+last[,25]+
                last[,26]+last[,33]+last[,35]+last[,37]+last[,39]+last[,40]+
                last[,41]+last[,42]+last[,43]+last[,44]+last[,51]+last[,53]+
                last[,55]+last[,57]+last[,58]+last[,59]+last[,60]+last[,61]+
                last[,62]+last[,69]+last[,71]+last[,73])
x1m <- mutate(x1l, TotMissedX1=last[,75]+last[,76]+last[,77]+last[,78]+last[,79]+
                last[,80]+last[,87]+last[,89]+last[,91]+last[,93]+last[,94]+
                last[,95]+last[,96]+last[,97]+last[,98]+last[,105]+last[,107]+
                last[,109]+last[,111]+last[,112]+last[,113]+last[,114]+last[,115]+
                last[,116]+last[,123]+last[,125]+last[,127])
x1r <- mutate(x1m, TotReceivedX1=last[,129]+last[,130]+last[,131]+last[,132]+last[,133]+
                last[,134]+last[,141]+last[,143]+last[,145]+last[,147]+last[,148]+
                last[,149]+last[,150]+last[,151]+last[,152]+last[,159]+last[,161]+
                last[,163]+last[,165]+last[,166]+last[,167]+last[,168]+last[,169]+
                last[,170]+last[,177]+last[,179]+last[,181])

x2l <- mutate(x1r, TotLandsX2=last[,27]+last[,28]+last[,29]+last[,30]+last[,31]+
                last[,32]+last[,34]+last[,36]+last[,38]+last[,45]+last[,46]+
                last[,47]+last[,48]+last[,49]+last[,50]+last[,52]+last[,54]+
                last[,56]+last[,63]+last[,64]+last[,65]+last[,66]+last[,67]+
                last[,68]+last[,70]+last[,72]+last[,74])
x2m <- mutate(x2l, TotMissedX2=last[,81]+last[,82]+last[,83]+last[,84]+last[,85]+
                last[,86]+last[,88]+last[,90]+last[,92]+last[,99]+last[,100]+
                last[,101]+last[,102]+last[,103]+last[,104]+last[,106]+last[,108]+
                last[,110]+last[,117]+last[,118]+last[,119]+last[,120]+last[,121]+
                last[,122]+last[,124]+last[,126]+last[,128])
x2r <- mutate(x2m, TotReceivedX2=last[,135]+last[,136]+last[,137]+last[,138]+last[,139]+
                last[,140]+last[,142]+last[,144]+last[,146]+last[,153]+last[,154]+
                last[,155]+last[,156]+last[,157]+last[,158]+last[,160]+last[,162]+
                last[,164]+last[,171]+last[,172]+last[,173]+last[,174]+last[,175]+
                last[,176]+last[,178]+last[,180]+last[,182])

Added <- x2r[,c(1,2,3,4:7,183:185,11:13,186:188,17:182)]

break1 <- Added$lastAction > Added$SecondsIntoRound
break2 <- row.names(Added[break1,])
bk2 <- as.numeric(break2)
split1 <- bk2[1]
#split2 <- bk2[2]


Table1 <- Added[1:(split1-1),]
#Table2 <- Added[split1:(split2-1),]
Table3 <- Added[split1:(length(Added$Round)),]

Table4 <- mutate(Table1, cmTotHitsR.X1=cumsum(TotReceivedX1), 
                 cmTotHitsL.X1=cumsum(TotLandsX1),
                 cmTotHitsM.X1=cumsum(TotMissedX1),
                 cmTotHitsR.X2=cumsum(TotReceivedX2),
                 cmTotHitsL.X2=cumsum(TotLandsX2),
                 cmTotHitsM.X2=cumsum(TotMissedX2))

# Table5 <- mutate(Table2, cmTotHitsR.X1=cumsum(TotReceivedX1), 
#                  cmTotHitsL.X1=cumsum(TotLandsX1),
#                  cmTotHitsM.X1=cumsum(TotMissedX1),
#                  cmTotHitsR.X2=cumsum(TotReceivedX2),
#                  cmTotHitsL.X2=cumsum(TotLandsX2),
#                  cmTotHitsM.X2=cumsum(TotMissedX2))

Table6 <- mutate(Table3, cmTotHitsR.X1=cumsum(TotReceivedX1), 
                 cmTotHitsL.X1=cumsum(TotLandsX1),
                 cmTotHitsM.X1=cumsum(TotMissedX1),
                 cmTotHitsR.X2=cumsum(TotReceivedX2),
                 cmTotHitsL.X2=cumsum(TotLandsX2),
                 cmTotHitsM.X2=cumsum(TotMissedX2))

Table7 <- rbind(Table4,Table6)



colnames(Table7)

landX1 <- c(21:26,33,35,37,39:44,51,53,55,57:62,69,71,73)
landX2 <- c(27:32,34,36,38,45:50,52,54,56,63:68,70,72,74)

missX1 <- c(75:80,87,89,91,93:98,105,107,109,111:116,123,125,127)
missX2 <- c(81:86,88,90,92,99:104,106,108,110,117:122,124,126,128)

recvX1 <- c(129:134,141,143,145,147:152,159,161,163,165:170,177,179,181)
recvX2 <- c(135:140,142,144,146,153:158,160,162,164,171:176,178,180,182)

Table8 <- Table7[,c(1:20,landX1,landX2,missX1,missX2,recvX1,recvX2)]
write.csv(Table8, 'wolfey_addedFeatures.csv', row.names=F)#mazvidal Till df
