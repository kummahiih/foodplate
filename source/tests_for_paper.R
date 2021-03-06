#### Lisenssi  ####

#
#@copyright: 2013 by Pauli Rikula <pauli.rikula@gmail.com>
#@license: MIT <http://www.opensource.org/licenses/mit-license.php>
#

#### ajoymp�rist� ####

#t�m� R -skripti on kehitetty R -versiolle 'R version 3.0.1 (2013-05-16) -- "Good Sport"'
#k�ytt�en RStudio -kehitysty�kalua

#rootpath == polku jonka alla 'csv datat' -hakemisto 
#esimerkiksi, jos t�m� tiedosto sijaitsee hakemistossa "C:/projektit/foodweb/prog"

rootpath="C:/projektit/foodweb/prog"

setwd(rootpath)

#k�ytettyjen otsioiden m�ritelm�t
source("otsikot.R")

#sy�tteiden m��ritelm�t
source("syotteet.R")

#sy�tteiden luku csv -tiedostosta
source("lue_csv_taulukot.R")

#reseptien tietojen laskenta
source("reseptit.R")

#yhden aterian pisteytykset
source("aterian_pisteytykset.R")

source("pareto.R")

source("yhteenveto.R")

taulukot <- lue.csv.taulukot(rootpath=rootpath)
otsikot <- taulukot@otsikot

#tallennetaan k�ytetyt sy�tteet 
tallenna.csv.Syotteet(taulukot, rootpath=rootpath)

#lasketaan resepteille tietoja
reseptitiedot <- laske.Reseptitiedot(taulukot,otsikot)

#tallennetaan lasketut tiedot
tallenna.csv.Reseptitiedot(reseptitiedot,rootpath=rootpath)

#"Male.18.30.years"
#"Female.10.years"
sukup_ika="Female.10.years"

scores <- laske.reseptit_score(taulukot, otsikot, reseptitiedot, sukup_ika)

name <- paste("tulokset/reseptit_scores_",sukup_ika,".csv")
write.table(scores, 
            file=name,sep=";",quote=FALSE,dec=",", col.names=NA)





