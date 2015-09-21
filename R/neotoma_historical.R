library(neotoma)

load('data/compiled.pollen.RData')

all_melt <- melt(compiled.pollen, id.vars = c("sitename", "depth", "age", 
                                              "date.type", "lat", "long", "dataset"))

mod_pol$ID <- 1:nrow(mod_pol)
neo_melt <- melt(mod_pol,id=c("ID","LATDD","LONDD","XXX","XXX.1","ELEVATION","PolSum"))

trans_taxa <- read.csv('data/mod_to_neotoma.csv',header = FALSE)

neo_melt$neo <- trans_taxa[match(neo_melt$variable,trans_taxa[,1]),2]

all_melt <- all_melt[!all_melt$value ==0 & all_melt$variable %in% neo_melt$neo,]
all_melt <- all_melt[!all_melt$date.type %in% "Radiocarbon years BP", ]

neo_melt <- neo_melt[!neo_melt$value ==0,]
neo_melt <- neo_melt[neo_melt$neo %in% levels(all_melt$variable),]

all_points <- data.frame(point = c(all_melt$sitename, neo_melt$ID),
                         age =   c(all_melt$age, rep("Modern", nrow(neo_melt))),
                         value = c(all_melt$value, neo_melt$value),
                         variable = c(as.character(all_melt$variable), as.character(neo_melt$variable)))

all_cast <- dcast(all_melt, value ~ variable + age, fun=sum)
