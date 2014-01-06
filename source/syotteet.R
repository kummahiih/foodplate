#
#@copyright: 2013 by Pauli Rikula <pauli.rikula@gmail.com>
#@license: MIT <http://www.opensource.org/licenses/mit-license.php>
#

source("otsikot.R")

setClass(
  "Syotteet", 
  representation(
    otsikot="Otsikot",
    suositukset_energy="matrix",
    suositukset_nutrients_xb="matrix",
    suositukset_nutrients_xdot="matrix",
    suositukset_nutrients_xl="matrix",
    suositukset_nutrients_xu="matrix",
    suositukset_volume = "matrix",
    
    reseptit_ainesosat="matrix",
    ainesosa_ravinne="matrix",
    ainesosien_energiat="matrix",
    ainesosien_ymparistohaitat="matrix",
    ravinne_painot="numeric",
    aineosien_haitakkeet="matrix",
    ainesosien_luokittelu="matrix",
    ainesosien_ominaistilavuus ="matrix"
  )
)

tallenna.csv.Syotteet <- function(obj,rootpath)
{
  #tallennetaan kaytetyt tiedot .. voidaan tarkistaa, että mitä tuli luettua
  oldwd <- getwd()
  setwd(rootpath)
  
  write.table(obj@reseptit_ainesosat, 
              file="kaytetyt syotteet/reseptit_ainesosat.csv",sep=";",quote=FALSE,dec=",", col.names=NA)
  write.table(obj@suositukset_energy, 
              file="kaytetyt syotteet/suositukset_energy.csv",sep=";",quote=FALSE,dec=",", col.names=NA)
  write.table(obj@suositukset_nutrients_xb, 
              file="kaytetyt syotteet/suositukset_nutrients_xb.csv",sep=";",quote=FALSE,dec=",", col.names=NA)
  write.table(obj@suositukset_nutrients_xdot, 
              file="kaytetyt syotteet/suositukset_nutrients_xdot.csv",sep=";",quote=FALSE,dec=",", col.names=NA)
  write.table(obj@suositukset_nutrients_xl, 
              file="kaytetyt syotteet/suositukset_nutrients_xl.csv",sep=";",quote=FALSE,dec=",", col.names=NA)   
  write.table(obj@suositukset_nutrients_xu, 
              file="kaytetyt syotteet/suositukset_nutrients_xu.csv",sep=";",quote=FALSE,dec=",", col.names=NA)              
  write.table(obj@suositukset_volume, 
              file="kaytetyt syotteet/suositukset_volume.csv",sep=";",quote=FALSE,dec=",", col.names=NA)    
  
  write.table(obj@reseptit_ainesosat, 
              file="kaytetyt syotteet/reseptit_ainesosat.csv",sep=";",quote=FALSE,dec=",", col.names=NA)                                               
  write.table(obj@ainesosa_ravinne, 
              file="kaytetyt syotteet/ainesosa_ravinne.csv",sep=";",quote=FALSE,dec=",", col.names=NA)                                                                      
  write.table(obj@ainesosien_energiat, 
              file="kaytetyt syotteet/ainesosien_energiat.csv",sep=";",quote=FALSE,dec=",", col.names=NA)                                                                                       
  write.table(obj@ainesosien_ymparistohaitat, 
              file="kaytetyt syotteet/ainesosien_ymparistohaitat.csv",sep=";",quote=FALSE,dec=",", col.names=NA)                                                                                                          
  write.table(obj@ravinne_painot, 
              file="kaytetyt syotteet/ravinne_painot.csv",sep=";",quote=FALSE,dec=",", col.names=NA)         
  write.table(obj@aineosien_haitakkeet, 
              file="kaytetyt syotteet/aineosien_haitakkeet.csv",sep=";",quote=FALSE,dec=",", col.names=NA)

  write.table(obj@ainesosien_luokittelu, 
              file="kaytetyt syotteet/ainesosien_luokittelu.csv",sep=";",quote=FALSE,dec=",", col.names=NA)
  
  write.table(obj@ainesosien_ominaistilavuus, 
              file="kaytetyt syotteet/ainesosien_ominaistilavuus.csv",sep=";",quote=FALSE,dec=",", col.names=NA)
  
  setwd(oldwd)
}

#funktioita evolutiivisia algoritmeja varten

uusilla_ainesosilla <- function(otsikot, taulukot)
{
  reseptit_ainesosat_m= matrix(taulukot@reseptit_ainesosat, nrow=otsikot@reseptit_n, ncol=otsikot@ainesosat_n)
  colnames(reseptit_ainesosat_m) <- otsikot@ainesosat
  luotu <- new(
    "Syotteet",
    otsikot=otsikot,
    suositukset_energy=taulukot@suositukset_energy,
    suositukset_nutrients_xb=taulukot@suositukset_nutrients_xb,
    suositukset_nutrients_xdot=taulukot@suositukset_nutrients_xdot,
    suositukset_nutrients_xl=taulukot@suositukset_nutrients_xl,
    suositukset_nutrients_xu=taulukot@suositukset_nutrients_xu,
    suositukset_volume = taulukot@suositukset_volume,
    
    reseptit_ainesosat= reseptit_ainesosat_m,
    ainesosa_ravinne=taulukot@ainesosa_ravinne,
    ainesosien_energiat=taulukot@ainesosien_energiat,
    ainesosien_ymparistohaitat=taulukot@ainesosien_ymparistohaitat,
    ravinne_painot=taulukot@ravinne_painot,
    aineosien_haitakkeet=taulukot@aineosien_haitakkeet,
    ainesosien_luokittelu=taulukot@ainesosien_luokittelu,
    ainesosien_ominaistilavuus = taulukot@ainesosien_ominaistilavuus
    
  )
  
  return(luotu)
}


luo_uudet_reseptit <- function(otsikot, taulukot)
{
  reseptit_ainesosat= matrix(nrow=otsikot@reseptit_n, ncol=otsikot@ainesosat_n)
  for(resepti_ind in 1:otsikot@reseptit_n)
  {    
    uusi_resepti <- taulukot@reseptit_ainesosat[resepti_ind,] + rnorm(otsikot@ainesosat_n, 0, 1)
    for(ainesosa_ind in 1:otsikot@ainesosat_n)
    {
      if(uusi_resepti[ainesosa_ind] < 0.0)
        uusi_resepti[ainesosa_ind] <- 0.0
    }
    
    reseptit_ainesosat[resepti_ind,] <- uusi_resepti
    
  }
  taulukot@reseptit_ainesosat <- reseptit_ainesosat
  return(taulukot)
}

luo_uudet_reseptit_reseptista <- function(otsikot, taulukot, ind)
{
  reseptit_ainesosat= matrix(nrow=otsikot@reseptit_n, ncol=otsikot@ainesosat_n)
  for(resepti_ind in 1:otsikot@reseptit_n)
  {    
    reseptit_ainesosat[resepti_ind,] <- taulukot@reseptit_ainesosat[ind,]
  }
  taulukot@reseptit_ainesosat <- reseptit_ainesosat
  
  #jos taulukkoja ei palauteta ja sijoiteta alkuperäisten taulukkojen tilalle, jäävät muutokset vain tähän voimaan
  #en tiedä, että miksi näin käy  
  return(taulukot)
}


mutatoi_reseptia <- function(otsikot, taulukot, sd)
{
  for(resepti_ind in 1:otsikot@reseptit_n)
  {
    mutation = sample(c(mutatoi_reseptia_ind, vaihda_ainetta), 1)
    
    taulukot@reseptit_ainesosat[resepti_ind,] <- mutation(otsikot, taulukot, sd, resepti_ind)
  }
  #jos taulukkoja ei palauteta ja sijoiteta alkuperäisten taulukkojen tilalle, jäävät muutokset vain tähän voimaan
  #en tiedä, että miksi näin käy
  return(taulukot)
}

mutatoi_reseptia_ind <- function(otsikot, taulukot, sd, resepti_ind, resepti_ind2=NA)
{
  uusi_resepti <- taulukot@reseptit_ainesosat[resepti_ind,] + rnorm(otsikot@ainesosat_n, 0, sd)
  for(ainesosa_ind in 1:otsikot@ainesosat_n)
  {
    if(uusi_resepti[ainesosa_ind] < 0.0)
      uusi_resepti[ainesosa_ind] <- 0.0
  }
  return(uusi_resepti)
}

vaihda_ainetta <- function(otsikot, taulukot, sd, resepti_ind, resepti_ind2=NA)
{
  
  sd = sd * otsikot@ainesosat_n
  
  ind = 1:otsikot@ainesosat_n
  resepti_ind_1 = sample(ind,1)
  resepti_ind_2 = sample(ind[ind!=resepti_ind_1],1)
  
  uusi_resepti <- taulukot@reseptit_ainesosat[resepti_ind,]
  v2 <- taulukot@reseptit_ainesosat[resepti_ind,resepti_ind_2]
  v1 <- taulukot@reseptit_ainesosat[resepti_ind,resepti_ind_2]
  
  uusi_resepti[resepti_ind_1] <- v2 + rnorm(1, 0, sd)
  uusi_resepti[resepti_ind_2] <- v1 + rnorm(1, 0, sd)
  
  for(ainesosa_ind in c(resepti_ind_1, resepti_ind_2))
  {
    if(uusi_resepti[ainesosa_ind] < 0.0)
      uusi_resepti[ainesosa_ind] <- 0.0
  }
  
  #jos taulukkoja ei palauteta ja sijoiteta alkuperäisten taulukkojen tilalle, jäävät muutokset vain tähän voimaan
  #en tiedä, että miksi näin käy
  return(uusi_resepti)
}

risteyta <-  function(otsikot, taulukot, sd, resepti_ind, resepti_ind2)
{
  uusi_resepti <- 1:otsikot@ainesosat_n 
  for(ainesosa_ind in 1:otsikot@ainesosat_n)
  {
    uusi_resepti[ainesosa_ind] <- 
      taulukot@reseptit_ainesosat[
        sample(c(resepti_ind, resepti_ind2),1),
        ainesosa_ind] + rnorm(1, 0, sd)
    if(uusi_resepti[ainesosa_ind] < 0.0)
      uusi_resepti[ainesosa_ind] <- 0.0
  }
  return(uusi_resepti)
  
}
mutatoi_puolet <- function(otsikot, taulukot, sd)
{
  keskikohta = sample(1:otsikot@reseptit_n, 1)
  
  for(ind in 1:(otsikot@reseptit_n/2))
  {
    resepti_ind = ((keskikohta + ind) %%  otsikot@reseptit_n) + 1
    uusi_resepti <- taulukot@reseptit_ainesosat[resepti_ind,] + rnorm(otsikot@ainesosat_n, 0, sd)
    for(ainesosa_ind in 1:otsikot@ainesosat_n)
    {
      if(uusi_resepti[ainesosa_ind] < 0.0)
        uusi_resepti[ainesosa_ind] <- 0.0
    }
    taulukot@reseptit_ainesosat[resepti_ind,] <- uusi_resepti
  }
  #jos taulukkoja ei palauteta ja sijoiteta alkuperäisten taulukkojen tilalle, jäävät muutokset vain tähän voimaan
  #en tiedä, että miksi näin käy
  return(taulukot)
}


