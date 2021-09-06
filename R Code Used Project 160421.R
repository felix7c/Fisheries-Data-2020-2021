#FINAL YEAR PROJECT CODE: PRED-PREY MASS RELATIONSHIPS
library(tidyverse)

# The dataset used was from the file Predator_and_prey_body_sizes_in_marine_food_webs_vsn4.txt
# which can be downloaded from
# https://figshare.com/collections/PREDATOR_AND_PREY_BODY_SIZES_IN_MARINE_FOOD_WEBS/3300257
# This file was then converted to a comma-separated value (CSV) file using Microsoft Excel and imported into RStudio

pred_prey_data <- read.csv("Predator_and_prey_body_sizes_in_marine_food_webs_vsn4_csvcopy.csv")

#show unique units of mass
unique(pred_prey_data$Prey.mass.unit)
unique(pred_prey_data$Predator.mass.unit)
#convert to grams
pred_prey_data$Prey.mass[pred_prey_data$Prey.mass.unit == "mg"] <- 0.001*pred_prey_data$Prey.mass[pred_prey_data$Prey.mass.unit == "mg"]
pred_prey_data$Prey.mass.unit[pred_prey_data$Prey.mass.unit == "mg"] <- "g"
unique(pred_prey_data$Prey.mass.unit) #shows now all in grams

#assign to new dataframes
pred_mass_g <-pred_prey_data$Predator.mass #all in grams anyway
prey_mass_g <-pred_prey_data$Prey.mass # now all converted to grams
summary(pred_mass_g)

#to infer log10 power law relationship
log10fit=lm(log10(prey_mass_g)~log10(pred_mass_g),data=pred_prey_data)
summary(log10fit)


#code for log10 pred-prey mass graph with location linear models (with tweaked font sizes etc) (for Figure 1)
#from 'pp location graph.R'
#library(ggplot2)
pred_prey_data %>%
  ggplot(aes(x= log10(pred_mass_g), y= log10(prey_mass_g), label= Geographic.location, colour = Geographic.location)) + #could add fill=
  geom_point(shape=20, colour = "black", show.legend = FALSE) +
  geom_smooth(method = "lm",se = FALSE, show.legend = TRUE)+ #lin models by location, also shows legend at side
  ggtitle("Log-Log Plot of Predator-Prey Masses (Base 10 Logarithms)") +
  labs(colour = "Geographic Location")+
  theme(legend.text = element_text(size = 14))+ #legend text size
  theme(legend.title = element_text(size = 16,face="bold"))+
  guides(col = guide_legend(nrow = 26))+ #number of rows in legend box
  xlab("log10(Predator Mass (g))") +
  ylab("log10(Prey Mass (g))") +
  theme(axis.text=element_text(size=20))+ #axis scale text size
  theme(axis.title=element_text(size=24))+ #xlab,ylab size
  theme(plot.title=element_text(size=34, face ="bold"))+ #title size
  geom_abline(aes(slope = 1,intercept = 0)) + #1:1 line
  geom_abline(aes(slope = 1.014350,intercept = -3.082167), color="red", size = 0.5, show.legend=FALSE) #regression line for overall dataset


#location lm code -creates table of values for each individual lin model by location (table 1)
uniquelocationlist = sort(unique(pred_prey_data$Geographic.location)) #list of locations sorted alphabetically
lmlocationtable = data.frame(location = character(0), beta0 = numeric(0), beta1 =numeric(0), rsquared=numeric(0), pvalue =numeric(0), numberofobs =numeric(0))
for (i in 1:25){
  lmsummary = lm(log10(Prey.mass)~log10(Predator.mass),data=pred_prey_data[which(pred_prey_data$Geographic.location == as.character(uniquelocationlist[i])),])
  #save key details about each lin model to a dataframe
  lmlocationtable[i,1] = uniquelocationlist[i] #location
  lmlocationtable[i,2] = lmsummary$coefficients[1] #intercept beta0
  lmlocationtable[i,3] = lmsummary$coefficients[2] #beta1
  lmlocationtable[i,4] = summary(lmsummary)$r.squared #rsquared value
  lmlocationtable[i,5] = summary(lmsummary)$coefficients[,4][2] #p value
  lmlocationtable[i,6] = count(pred_prey_data[which(pred_prey_data$Geographic.location == uniquelocationlist[i]),]) #no of pred obs
}

#round p values to 3d.p. & replace very small p values with 'P < 0.001'
lmlocationtable[,5] <- round(lmlocationtable[,5], digits  = 3)
lmlocationtable[,5][lmlocationtable[,5] < 0.001] <- "<0.001"

library(xtable)
#import table into R
print(xtable(lmlocationtable, type = "latex", digits = c(0,0,3,3,3,3,0)), file = "lmlocationtableR.tex") #digits sets decimal places for each column

#cape cod code - investing predator & prey values in this location
library("plyr")
library("dplyr") #already loaded in tidyverse, but need to reload it after using "plyr" package
count(pred_prey_data$Geographic.location) #counts no of obs. for each location
capecodobs= pred_prey_data[which(pred_prey_data$Geographic.location == "Cape Cod Bay, Gulf of Maine, New England"),] #obs from this location

count(capecodobs$Predator)
count(capecodobs$Predator.common.name)

count(capecodobs$Prey)
count(capecodobs$Prey.common.name) #to see common names

summary(capecodobs$Predator.mass)
summary(capecodobs$Prey.mass)

count(capecodobs$Predator.mass.unit) #check all in grams
#all preds are Atlantic bluefin tuna here


capecodfit1=lm(log10(Prey.mass)~log10(Predator.mass),data=capecodobs)
summary(capecodfit1) #fit for data inc outlier

summary(pred_prey_data$Predator.mass)
summary(capecodobs$Predator.mass)

frenchpolyobs= pred_prey_data[which(pred_prey_data$Geographic.location == "French Polynesian EEZ"),]
#4011 obs
frenchpolyfit1=lm(log10(Prey.mass)~log10(Predator.mass),data=frenchpolyobs)
summary(frenchpolyfit1)
#R^2 = 0.0003114, p value = 0.2639

count(frenchpolyobs$Predator)
count(frenchpolyobs$Predator.common.name)
summary(frenchpolyobs$Predator.mass)
summary(frenchpolyobs$Prey.mass)

#split dataset into the two different tuna pred species
bigeyefrenchpolyobs= frenchpolyobs[which(frenchpolyobs$Predator.common.name == "Bigeye tuna"),]
yellowfinfrenchpolyobs= frenchpolyobs[which(frenchpolyobs$Predator.common.name == "Yellowfin tuna"),]

count(bigeyefrenchpolyobs$Predator)
count(yellowfinfrenchpolyobs$Predator)

summary(bigeyefrenchpolyobs$Predator.mass)
summary(bigeyefrenchpolyobs$Prey.mass)

summary(yellowfinfrenchpolyobs$Predator.mass)
summary(yellowfinfrenchpolyobs$Prey.mass)

count(bigeyefrenchpolyobs$Prey.common.name)
count(yellowfinfrenchpolyobs$Prey.common.name)





#PPMR
PPMR <- pred_mass_g/prey_mass_g
summary(PPMR)

#PPMR BOXPLOT 
#library(ggplot2)
ppdata2 <- pred_prey_data #add ppmr to dataframe
ppdata2$ppmr = PPMR
#USES LATIN NAME (Predator)
# sorted by median predator mass

#ppmr boxplot (Figure 2)
ggplot(ppdata2, aes(x=reorder(Predator,Predator.mass, median), y=log10(ppmr))) + # median
  geom_boxplot() +
  xlab("Predator Species") +
  ylab("log10(PPMR)") +
  ggtitle("Boxplot of log10(PPMR) Against Predator Species") +
  theme(axis.title=element_text(size=16))+ #xlab,ylab size
  theme(plot.title=element_text(size=20, face ="bold"))+ #title size
  theme(panel.background = element_rect(fill = "white"))+
  theme(axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"), 
        axis.text.x = element_text(angle = 90, hjust = 1)) #so at 90deg angle , could alter text size

#checking obs of certain species
median(ppdata2$ppmr[which(ppdata2$Predator == "Oncorhynchus nerka")])
median(ppdata2$ppmr[which(ppdata2$Predator == "Micromesistius poutassou")])

median(ppdata2$Predator.mass[which(ppdata2$Predator == "Oncorhynchus nerka")])
median(ppdata2$Predator.mass[which(ppdata2$Predator == "Micromesistius poutassou")])

count(ppdata2$Predator) #counts no of obs. for each species
median(ppdata2$ppmr[which(ppdata2$Predator == "Chionodraco rastrospinosus")]) #lowest median ppmr
median(ppdata2$ppmr[which(ppdata2$Predator == "Thaleichthys pacificus")]) #highest median ppmr
summary(ppdata2$Predator.mass[which(ppdata2$Predator == "Chionodraco rastrospinosus")])
summary(ppdata2$Prey.mass[which(ppdata2$Predator == "Chionodraco rastrospinosus")])
ppdata2$Predator[which(ppdata2$ppmr <= 0.2)] #shows the 2 lowest individual ppmr values are of Chionodraco rastrospinosus (icefish)
icefishlowppmr = ppdata2[which(ppdata2$ppmr <= 0.2),] #shows the 2 lowest individual ppmr values are of Chionodraco rastrospinosus (icefish)


#looking at euphausid furcilia prey masses
euphausidprey = ppdata2[which(ppdata2$Prey == "Euphausid furcilia"),]
summary(euphausidprey$Prey.mass)
euphausidprey$Prey.mass

#phylo
uniquespecieslist = unique(ppdata2$Predator) #list of all species in dataset (latin name)
uniquespecies1 = t(uniquespecieslist)
write.table(uniquespecies1, file = "uniquespecies1.txt", sep = ",",col.names = FALSE, row.names =FALSE, quote =FALSE)
# then copy from notepad to website

#testtree1.txt was created at
#https://phylot.biobyte.de/index.cgi
#(didnt recognise some species, need to pay for bigger tree)
testtree1 <- "((((((((((((((((((((Carcharhinus_limbatus,Carcharhinus_isodon,Carcharhinus_brevipinna)Carcharhinus,(Rhizoprionodon_terraenovae)Rhizoprionodon)Carcharhinidae)Carcharhiniformes)Galeoidea)Galeomorphii)Selachii)Elasmobranchii)Chondrichthyes,((((((((((((((Hypomesus_pretiosus)Hypomesus)Hypomesinae,((Thaleichthys_pacificus)Thaleichthys)Osmerinae)Osmeridae)Osmeriformes)Stomiatii,((((((((((((Thunnus_thynnus)Thunnus)Thunnini)Scombrinae)Scombridae)Scombriformes)Pelagiaria,((((((Hemilepidotus_spinosus)Hemilepidotus)Cottidae,((Psychrolutes_paradoxus)Psychrolutes)Psychrolutidae)Cottales)Cottioidei)Perciformes)Eupercaria)Percomorphaceae)Euacanthomorphacea)Acanthomorphata)Ctenosquamata)Eurypterygia)Neoteleostei)Euteleosteomorpha)Clupeocephala)Osteoglossocephalai)Teleostei)Neopterygii)Actinopteri)Actinopterygii)Euteleostomi)Teleostomi)Gnathostomata)Vertebrata)Craniata)Chordata)Deuterostomia)Bilateria)Eumetazoa)Metazoa)Opisthokonta)Eukaryota)cellular_organisms);"
#copied from testree1.txt from the phyloT website


#unique taxons, incase names are missing?
uniquetaxonlist = unique(ppdata2$Predator) #list of all species in dataset (taxon id)
uniquetaxon = t(uniquetaxonlist)
write.table(uniquetaxon, file = "uniquetaxon.txt", sep = ",",col.names = FALSE, row.names =FALSE, quote =FALSE)
# then copy from notepad to website

library(ape)

#copied from notepad file
treespecies1 <- "(((((((((((((((((((((((((((Clupea_harengus,Clupea_pallasii)Clupea)Clupeinae)Clupeidae)Clupeoidei)Clupeiformes)Clupei)Otomorpha,(((((((((((Cynoscion_regalis)Cynoscion)Sciaenidae,((Dicentrarchus_labrax)Dicentrarchus)Moronidae)Eupercaria_incertae_sedis,((((Lophius_piscatorius,Lophius_budegassa,Lophius_americanus)Lophius)Lophiidae)Lophioidei)Lophiiformes,(((Ammodytes_hexapterus)Ammodytes)Ammodytidae)Uranoscopiformes,(((((Hexagrammos_decagrammus)Hexagrammos,(Ophiodon_elongatus)Ophiodon)Hexagrammidae)Hexagrammales,(((Anarhichas_lupus,Anarhichas_minor)Anarhichas)Anarhichadidae,(((Lumpenella_longirostris)Lumpenella,(Lumpenus_sagitta)Lumpenus)Lumpeninae)Stichaeidae)Zoarcales,(((Gasterosteus_aculeatus)Gasterosteus)Gasterosteidae)Gasterosteales,(((Psychrolutes_paradoxus)Psychrolutes)Psychrolutidae,((Hemilepidotus_spinosus,Hemilepidotus_hemilepidotus)Hemilepidotus)Cottidae,(((Hemitripterus_americanus)Hemitripterus)Hemitripterinae)Agonidae)Cottales)Cottioidei,(((Chionodraco_rastrospinosus)Chionodraco)Channichthyidae)Notothenioidei,(((((Sebastes_sp.)unclassified_Sebastes)Sebastes)Sebastinae)Sebastidae)Scorpaenoidei,(((Eutrigla_gurnardus)Eutrigla)Triglidae)Triglioidei)Perciformes)Eupercaria,((((Trachurus_trachurus)Trachurus)Carangidae)Carangiformes,(((Xiphias_gladius)Xiphias)Xiphiidae)Istiophoriformes,((((Paralichthys_dentatus)Paralichthys)Paralichthyidae,((Arnoglossus_imperialis)Arnoglossus)Bothidae,((Lepidorhombus_whiffiagonis,Lepidorhombus_boscii)Lepidorhombus,(Scophthalmus_aquosus)Scophthalmus)Scophthalmidae)Pleuronectoidei)Pleuronectiformes)Carangaria,((((((Thunnus_alalunga,Thunnus_obesus,Thunnus_albacares,Thunnus_thynnus)Thunnus)Thunnini,((Scomber_scombrus)Scomber)Scombrini)Scombrinae)Scombridae,((Pomatomus_saltatrix)Pomatomus)Pomatomidae)Scombriformes)Pelagiaria)Percomorphaceae)Euacanthomorphacea,(((((((Merluccius_bilinearis,Merluccius_productus,Merluccius_merluccius)Merluccius)Merlucciidae,((Gadus_morhua)Gadus,(Pollachius_pollachius,Pollachius_virens)Pollachius,(Melanogrammus_aeglefinus)Melanogrammus,(Trisopterus_luscus,Trisopterus_minutus)Trisopterus,(Merlangius_merlangus)Merlangius,(Micromesistius_poutassou)Micromesistius)Gadidae,((Urophycis_tenuis,Urophycis_regia)Urophycis)Phycidae,((Molva_molva)Molva)Lotidae)Gadoidei)Gadiformes)Gadariae,((((Zeus_faber)Zeus)Zeidae)Zeiformes)Zeariae)Zeiogadaria)Paracanthomorphacea)Acanthomorphata,((((Benthosema_glaciale)Benthosema,(Diaphus_garmani)Diaphus,(Ceratoscopelus_maderensis)Ceratoscopelus,(Myctophum_punctatum,Myctophum_asperum)Myctophum,(Hygophum_benoiti)Hygophum,(Lampanyctus_crocodilus)Lampanyctus)Myctophidae)Myctophiformes)Myctophata)Ctenosquamata)Eurypterygia)Neoteleostei,(((((Oncorhynchus_keta,Oncorhynchus_gorbuscha,Oncorhynchus_nerka)Oncorhynchus)Salmoninae)Salmonidae)Salmoniformes)Protacanthopterygii,(((((Hypomesus_pretiosus)Hypomesus)Hypomesinae,((Thaleichthys_pacificus)Thaleichthys)Osmerinae)Osmeridae)Osmeriformes)Stomiatii)Euteleosteomorpha)Clupeocephala)Osteoglossocephalai)Teleostei)Neopterygii)Actinopteri)Actinopterygii)Euteleostomi)Teleostomi,((((((((Galeorhinus_galeus)Galeorhinus,(Mustelus_canis)Mustelus)Triakidae,((Carcharhinus_limbatus,Carcharhinus_brevipinna,Carcharhinus_isodon)Carcharhinus,(Rhizoprionodon_terraenovae)Rhizoprionodon)Carcharhinidae,((Scyliorhinus_canicula)Scyliorhinus)Scyliorhinidae)Carcharhiniformes)Galeoidea)Galeomorphii,((((Squalus_acanthias)Squalus)Squalidae)Squaliformes)Squalomorphii)Selachii,((((Raja_undulata,Raja_clavata,Raja_montagui)Raja)Rajidae)Rajiformes)Batoidea)Elasmobranchii)Chondrichthyes)Gnathostomata)Vertebrata)Craniata)Chordata)Deuterostomia)Bilateria)Eumetazoa)Metazoa)Opisthokonta)Eukaryota)cellular_organisms);"

vert.tree<-read.tree(text=treespecies1) #importing tree

#Phylogenetic tree (Figure 3)
plot(vert.tree,no.margin=TRUE,edge.width=1, cex =0.7)

#checking obs of certain species
which(ppdata2$Predator == "Cynoglossus sp." )
count(ppdata2$Predator) #counts no of obs. for each species

unique(ppdata2$Predator) #see list of unique predators

ppdata3 = ppdata2
ppdata3 = ppdata3[!(ppdata3$Predator %in% c("Theragra chalcogramma","Scorpaenodes sp.","Carangoides sp.","Acanthocepola sp.","Cynoglossus sp.","Loligo pealeii","Notolepis rissoi","Paralichthys oblongus","Myoxocephalus octodecimspinosus","Urophysis chuss","Raja erinacea","Torpedo nobiliana","Gilbertidia sigalutes","Rhamphocottus richardsoni","Raja ocellata","Aspitrigla cuculus","Leucoraja  fullonica   ","Raja naevus","Pleuragramma antarcticum","Nototheniops larseni")),] #remove rows of species that arent in tree
unique(ppdata3$Predator)# note it needed spaces after some species names, see unique pred names e.g Leucoraja  fullonica   
#now have 73 unqiue Predator names, as 20 species have been removed
#ppdata3 contains the observations excluding those that aren't included on the tree
#ideally want to include as many species on the tree in future.

#looking at prey habits of species in tree:

rajamobs = ppdata2[which(ppdata2$Predator == "Raja montagui"),]
rajacobs = ppdata2[which(ppdata2$Predator == "Raja clavata"),]
rajauobs = ppdata2[which(ppdata2$Predator == "Raja undulata"),]
mean(rajamobs$ppmr) #mean ppmr
mean(rajacobs$ppmr) #mean ppmr
mean(rajauobs$ppmr) #mean ppmr

myctophumaspobs = ppdata2[which(ppdata2$Predator == "Myctophum asperum"),]
myctophumpunobs = ppdata2[which(ppdata2$Predator == "Myctophum punctatum"),]
unique(myctophumaspobs$Predator.lifestage)#all larva
unique(myctophumpunobs$Predator.lifestage)#all larva
mean(myctophumaspobs$ppmr)
mean(myctophumpunobs$ppmr)
unique(myctophumaspobs$Geographic.location)#western north pacific
unique(myctophumpunobs$Geographic.location)#catalan sea
unique(myctophumaspobs$Prey.common.name)
unique(myctophumpunobs$Prey.common.name)
myctophumaspobs$Prey.common.name
myctophumpunobs$Prey.common.name
mean(myctophumaspobs$Prey.common.name == "Ostracod") #0.441048
mean(myctophumpunobs$Prey.common.name == "Copepod copepodite") #0.625
mean(myctophumpunobs$Prey.common.name == "Copepod copepodite") #0.625


pacificherringobs = ppdata2[which(ppdata2$Predator == "Clupea pallasii"),]
pacificherringobs$Prey.common.name
#see proportion of prey obs that are copepod:
mean(pacificherringobs$Prey.common.name == "copepod") #67.85% of prey obs are copepod
mean(pacificherringobs$Prey.common.name == "amphipod") #14.29% of prey obs are amphipod
mean(pacificherringobs[which(pacificherringobs$Prey.common.name == "copepod"),]$Prey.mass)
#mean mass of copepod for these is 0.00002760g
mean(pacificherringobs$ppmr) #mean ppmr
mean(pacificherringobs$Predator.mass)
summary(pacificherringobs$Predator.mass)
pacificherringobs$Predator.lifestage

atlanticherringobs= ppdata2[which(ppdata2$Predator == "Clupea harengus"),]
atlanticherringobs$Prey.common.name
mean(atlanticherringobs$Prey.common.name == "Goby") #76.92%% of prey obs are Goby
atlanticherringobs$Predator.lifestage



#phytools
#phylogenetic signal
library("phytools")
#want to replace space between genus and species with underscore, to match species names in vert.tree
ppdata3$Predator <- sub(" ", "_", ppdata3$Predator)
unique(ppdata3$Predator)

PPMRnames <- ppdata3$ppmr #could set as log10 instead
names(PPMRnames) <- ppdata3$Predator

vert.tree$edge.length #shows no branch lengths currently
#PPMRnames

treewlengths = compute.brlen(vert.tree, method = "Grafen", power = 1) #computes branch lengths using Grafen method, to add to tree
phylo1 <- phylosig(treewlengths, PPMRnames, test = TRUE) #compares tree with PPMR

print(phylo1) #prints value of K
