#### Lisenssi  ####

#
#@copyright: 2013 by Pauli Rikula <pauli.rikula@gmail.com>
#@license: MIT <http://www.opensource.org/licenses/mit-license.php>
#


#käytettyjen otsioiden märitelmät
source("otsikot.R")

#syötteiden määritelmät
source("syotteet.R")

setClass(
  "Reseptitiedot",
  representation(
    syotteet="Syotteet",
    reseptien_energiat="matrix",
    reseptien_ravinne="matrix",
    reseptien_ymparistohaitat="matrix",
    reseptien_haitakkeet="matrix",
    reseptien_tilavuudet="matrix",
    reseptien_luokkien_tilavuus="matrix"
    )
  )

laske.reseptien_energiat <- function(taulukot, otsikot)
{
  #lasketaan reseptien energiat
  reseptien_energiat <- taulukot@reseptit_ainesosat %*% taulukot@ainesosien_energiat
  rownames(reseptien_energiat) <- otsikot@reseptit
  colnames(reseptien_energiat) <- c("Energy.kJ")
  
  return(reseptien_energiat)
}

#lasketaan reseptien ravinnepitoisuusmatriisi
laske.reseptien_ravinne <- function(taulukot, otsikot)
{
  reseptien_ravinne <- taulukot@reseptit_ainesosat %*% taulukot@ainesosa_ravinne
  rownames(reseptien_ravinne) <- otsikot@reseptit
  colnames(reseptien_ravinne) <- otsikot@ravinteet
  
  return(reseptien_ravinne)
}

laske.reseptien_ymparistohaitat <- function(taulukot, otsikot)
{
  #lasketaan reseptien ympäristöhaitat
  reseptien_ymparistohaitat <- taulukot@reseptit_ainesosat %*% taulukot@ainesosien_ymparistohaitat
  rownames(reseptien_ymparistohaitat) <- otsikot@reseptit
  colnames(reseptien_ymparistohaitat) <- otsikot@ymparistohaitat
  
  return(reseptien_ymparistohaitat)
}

laske.reseptien_haitakkeet <- function(taulukot, otsikot)
{
  #lasketaan resptien haitallisten aineiden pitoisuudet
  #tämä pitäis vielä jakaa ilmeisesti painolla ja sitten vielä tuhannella, 
  #mutta ei se reseptien scorejen suuruusjärjestystä tällä mallilla muuttaisi
  reseptien_haitakkeet <- taulukot@reseptit_ainesosat %*% taulukot@aineosien_haitakkeet
  rownames(reseptien_haitakkeet) <- otsikot@reseptit
  colnames(reseptien_haitakkeet) <- otsikot@haittakkeet
  
  return(reseptien_haitakkeet)
}

laske.reseptien_tilavuudet <- function(taulukot, otsikot)
{

  reseptien_tilavuudet <- taulukot@reseptit_ainesosat %*% taulukot@ainesosien_ominaistilavuus
  rownames(reseptien_tilavuudet) <- otsikot@reseptit
  colnames(reseptien_tilavuudet) <- c("volume_L")
  
  return(reseptien_tilavuudet)
}

laske.reseptien_luokkien_tilavuus <- function(taulukot, otsikot)
{
  reseptien_luokkien_tilavuus <- matrix(ncol=otsikot@ainesosien_luokittelu_n, nrow=otsikot@reseptit_n)
  colnames(reseptien_luokkien_tilavuus) <- otsikot@ainesosien_luokittelu
  rownames(reseptien_luokkien_tilavuus) <- otsikot@reseptit
  for(col in otsikot@ainesosien_luokittelu)
  {
    reseptien_luokkien_tilavuus[,col] <- taulukot@reseptit_ainesosat %*% (taulukot@ainesosien_luokittelu[,col] * taulukot@ainesosien_ominaistilavuus)
  }
  
  
  return(reseptien_luokkien_tilavuus)
}

laske.Reseptitiedot <- function(taulukot, otsikot)
{
  reseptien_tilavuudet=laske.reseptien_tilavuudet(taulukot, otsikot)
  lasketut_reseptitiedot <- new(
    "Reseptitiedot",
    syotteet=taulukot,
    reseptien_energiat=laske.reseptien_energiat(taulukot, otsikot),
    reseptien_ravinne=laske.reseptien_ravinne(taulukot, otsikot),
    reseptien_ymparistohaitat=laske.reseptien_ymparistohaitat(taulukot, otsikot),
    reseptien_haitakkeet=laske.reseptien_haitakkeet(taulukot, otsikot),
    reseptien_tilavuudet=reseptien_tilavuudet,
    reseptien_luokkien_tilavuus=laske.reseptien_luokkien_tilavuus(taulukot, otsikot)
    )
  return(lasketut_reseptitiedot)
}

tallenna.csv.Reseptitiedot <- function(reseptitiedot, rootpath)
{
  #tallennetaanresepteiden lasketut tiedot
  oldwd <- getwd()
  setwd(rootpath)
  
  write.table(reseptitiedot@reseptien_energiat,        file="tulokset/reseptien_energiat_2.csv",sep=";",quote=FALSE,dec=",", col.names=NA)
  write.table(reseptitiedot@reseptien_ravinne,         file="tulokset/reseptien_ravinne_2.csv",sep=";",quote=FALSE,dec=",", col.names=NA)
  write.table(reseptitiedot@reseptien_ymparistohaitat, file="tulokset/reseptien_ymparistohaitat_2.csv",sep=";",quote=FALSE,dec=",", col.names=NA)
  write.table(reseptitiedot@reseptien_haitakkeet,      file="tulokset/reseptien_haitakkeet_2.csv",sep=";",quote=FALSE,dec=",", col.names=NA)
  write.table(reseptitiedot@reseptien_tilavuudet,      file="tulokset/reseptien_tilavuudet_2.csv",sep=";",quote=FALSE,dec=",", col.names=NA)
  write.table(reseptitiedot@reseptien_luokkien_tilavuus,      file="tulokset/reseptien_luokkien_tilavuus_2.csv",sep=";",quote=FALSE,dec=",", col.names=NA)
  
  
  
  setwd(oldwd)
}
