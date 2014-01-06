#
#@copyright: 2013 by Pauli Rikula <pauli.rikula@gmail.com>
#@license: MIT <http://www.opensource.org/licenses/mit-license.php>
#

source("otsikot.R")
source("syotteet.R")
lue.csv.taulukot <- function(rootpath )
{
  #rootpath == polku jonka alla 'csv datat' -hakemisto 
  oldwd <- getwd()
  setwd(rootpath)
  
  
  
  
  #rootpath == polku jonka alla 'csv datat' -hakemisto 
  #esimerkiksi, jos tämä tiedosto sijaitsee hakemistossa "C:/projektit/foodweb/prog" 
  
  print(c("initializing from", rootpath))
  
  ####tiedostojen luku ####
  
  #oletetaan, että tämän tiedoston hakemistossa on kansio "csv datat", 
  #jonne käytetyt csv -tiedostot on sijoitettu
  
  #read.table ei toimi, jos joissain sarakkeissa myös muuta tekstiä headereiden lisäksi
  #luetun sarakkeen tietojen oltava samaa tyyppiä. tekstejä tai lukuja
  #piti muuttaa csv -tiedostoja tätä varten
  
  
  #vanhat arvot
  
  #exposure                = 
  #  read.table("csv datat/exposure.csv", header=TRUE, sep=";",dec=",")
    
  #pysty = reseptien ainesosat, vaaka = ainesosien ravinteet    
  #ravitsemus              = 
  # read.table("csv datat/ravitsemus.csv", header=TRUE, sep=";",dec=",")
  
  #ymparisto            =     
  #  read.table("csv datat/ymparisto.csv", header=TRUE, sep=";",dec=",")
  
  
  #vaaka = resepti, pysty = reseptin ainesosat  
  #tiedostossa ovat eripäin. tehdään transpoosi
  #reseptit_csv          =  
  #  t(read.table("csv datat/reseptit.csv", header=TRUE, sep=";",dec=","))
  
  exposure                = 
    read.table("csv datat/exposure_DioxMercCad.csv", header=TRUE, sep=";",dec=",")
  
  ravitsemus              = 
   read.table("csv datat/ravitsemus (1).csv", header=TRUE, sep=";",dec=",")
  
  #vaaka = resepti, pysty = reseptin ainesosat  
  #tiedostossa ovat eripäin. tehdään transpoosi
  reseptit_csv          =  
    t(read.table("csv datat/reseptit (1).csv", header=TRUE, sep=";",dec=","))
  
  ymparisto            =     
    read.table("csv datat/ymparisto (1).csv", header=TRUE, sep=";",dec=",")
  
  
  #pystytään lukemaan vain yksi taulukko yhdestä csv -tiedostosta.
  #tämän takia csv -tiedostoja piti purkaa useammiksi csv -tiedostoiksi
  
  suositukset.energy    =    
    read.table("csv datat/suositukset_energy.csv", header=TRUE, sep=";",dec=",")
  suositukset.volume =    
    read.table("csv datat/suositukset_volume.csv", header=TRUE, sep=";",dec=",")
  suositukset.nutrients =    
    read.table("csv datat/suositukset_nutrients.csv", header=TRUE, sep=";",dec=",")
  suositukset.nutrients_xl =   
    read.table("csv datat/suositukset_nutrients_xl.csv", header=TRUE, sep=";",dec=",",row.names=1)
  suositukset.nutrients_xdot =   
    
    read.table("csv datat/suositukset_nutrients_xdot.csv", header=TRUE, sep=";",dec=",",row.names=1)
  suositukset.nutrients_xb =   
    read.table("csv datat/suositukset_nutrients_xb.csv", header=TRUE, sep=";",dec=",",row.names=1)
  suositukset.nutrients_xu =   
    read.table("csv datat/suositukset_nutrients_xu.csv", header=TRUE, sep=";",dec=",",row.names=1)
  
  ymparisto_weights    =     
    read.table("csv datat/ymparisto_weights.csv", header=TRUE, sep=";",dec=",")
  
  
  #palautetaan vanha work directory
  setwd(oldwd)
  
  #### luetun datan testausta ####
  
  #testejä otsikoiden järjestyksille
  
  #tässä on reseptien ainesosat 
  reseptit_csv.ainesosat <- c(reseptit_csv[2,])
  #tässäkin pitäisi olla: 
  ravitsemus.ainesosat <- c(t(ravitsemus[2]))
  ravitsemus.ainesosat <- ravitsemus.ainesosat[1:length(ravitsemus.ainesosat)]
  #tarkistetaan:
  if(length (ravitsemus.ainesosat) != length(reseptit_csv.ainesosat))
    print("VIRHE: ravitsemus.ainesosat != reseptit_csv.ainesosat")
  
  for( i in seq(1,length(ravitsemus.ainesosat))) 
  {
    if(ravitsemus.ainesosat[i] != reseptit_csv.ainesosat[i])
    {
      warning("VIRHE: (ravitsemus vs reseptit)"," line ", i," "," vs ",ravitsemus.ainesosat[i], reseptit_csv.ainesosat[i])
    }
    if(ymparisto[i,2] != reseptit_csv.ainesosat[i])
    {
      warning("VIRHE: (ymparisto vs reseptit)"," line ", i," ", ymparisto[i,2]," vs ", reseptit_csv.ainesosat[i] )    
    }
    if(exposure[i,2] != reseptit_csv.ainesosat[i] )
    {
      warning("VIRHE: (altistus vs reseptit)"," line ", i, " ", ymparisto[i,2]," vs ", reseptit_csv.ainesosat[i] )     
    }
  }
  #TODO: lisää testejä
  
  
  #tällä voi muuttaa alkiot yksitellen taulukoista, joissa sarakkeissa sekaisin tekstiä ja liukulukuja
  todouble <- function (string, old=",", new = ".")
  { # convert string delimited by "_" into strings delimited by "."
    as.double(paste (unlist (strsplit (string, old)), collapse=new))
  }
  
  
  
  lue.otsikot <- function()
  {
    #### muodostetaan muuttujat ja matriisit laskuja varten ####
    
    ##### luetaan tehtävien matriisien otsikkotiedot #####
    
    #laitetaan ainesosat ja ainesosien määrä ylös
    ainesosat_n <- length(reseptit_csv[2,])
    ainesosat <- c(reseptit_csv[2,])
    
    #otetaan reseptit ylös
    # csv -tiedostossa oli 4 turhaa riviä
    reseptit_n = length(reseptit_csv[,1]) - 4
    reseptit = rownames(reseptit_csv)[5: length(rownames(reseptit_csv))]
    
    
    #ravitsemus.csv -tiedoston tiedoista tehdään ainesosa vs ravinne -matriisi
    #fatty.acids.saturated on ensimmäinen ravinne
    #potassium on viimeinen ravinne
    
    ravinteet <- c(colnames(ravitsemus))
    
    #indeksit
    eka_ravinne_ind <- match(c("fatty.acids.saturated"),ravinteet)
    vika_ravinne_ind <- match(c("potassium"),ravinteet)
    
    #ravinteet ylös
    ravinteet <-ravinteet[eka_ravinne_ind: vika_ravinne_ind]
    ravinteet_n = length(ravinteet)
    
    rm(eka_ravinne_ind)
    rm(vika_ravinne_ind)
    
    
    #ainesosien ympäristöhaitat
    ymparistohaitat <- colnames(ymparisto)[5:7]
    ymparistohaitat_n = length(ymparistohaitat)
    
    #otetaan ylös sukup x ikaluokka -arvot
    sukup_ika <- colnames(suositukset.nutrients_xb)
    sukup_ika_n <- length(sukup_ika)
    
    
    #aineosien haitallisten aineiden talteenotto
    haittakkeet <- colnames(exposure)[4:length(colnames(exposure))]
    haittakkeet_n <- length(haittakkeet)
    
    #ainesosien luokittelu
    ainesosien_luokittelu <- c("isVeggie","isProtein","isCarb","isBread","isDrink","isDessert")
    ainesosien_luokittelu_n <- length(ainesosien_luokittelu)
    
    luetut_otsikot <- new(
      "Otsikot",
      ainesosat_n=ainesosat_n,
      ainesosat=ainesosat,
      reseptit_n=reseptit_n,
      reseptit=reseptit,
      ravinteet_n=ravinteet_n,
      ravinteet=ravinteet,
      ymparistohaitat_n=ymparistohaitat_n,
      ymparistohaitat=ymparistohaitat,
      sukup_ika_n=sukup_ika_n,
      sukup_ika=sukup_ika,
      haittakkeet_n=haittakkeet_n,
      haittakkeet=haittakkeet,
      ainesosien_luokittelu_n=ainesosien_luokittelu_n,
      ainesosien_luokittelu=ainesosien_luokittelu
    )
    return(luetut_otsikot)
  }
  
  #luetaan ne ...
  otsikot <- lue.otsikot()
  
  
  #### luetaan matriisit #####
  
  lue.reseptit_ainesosat <- function()
  {
    #luetaan reseptien ainesosat
    reseptit_ainesosat <- matrix(nrow=otsikot@reseptit_n, ncol=otsikot@ainesosat_n)
    rownames(reseptit_ainesosat) <- otsikot@reseptit
    colnames(reseptit_ainesosat) <- otsikot@ainesosat
    
    for( col in seq(1,otsikot@ainesosat_n)) 
    {
      for( row in seq(1,otsikot@reseptit_n) )
      {
        #data alkaa viidenneltä riviltä ja ekalta sarakkeelta (reseptien nimet on rivien otsikoita)
        reseptit_ainesosat[row,col] <- todouble(reseptit_csv[row+4,col])
      }
    }
    return(reseptit_ainesosat)
  }
  
  lue.ainesosa_ravinne <- function()
  {
    #luetaan ainesosien ravinteet
    ainesosa_ravinne <- matrix(nrow=otsikot@ainesosat_n, ncol= otsikot@ravinteet_n)
    rownames(ainesosa_ravinne) <- otsikot@ainesosat
    colnames(ainesosa_ravinne) <- otsikot@ravinteet
    
    for( col in seq(1,otsikot@ravinteet_n)) 
    {
      for( row in seq(1,otsikot@ainesosat_n) )
      {
        #data alkaa ensimmäiseltä  riviltä ja kolmannelta sarakkeelta (reseptien nimet on rivien otsikoita)
        ainesosa_ravinne[row,col] <- ravitsemus[row,col+2]
      }
    }
    return(ainesosa_ravinne)
  }
  
  
  lue.ainesosien_energiat <- function()
  {
    #luetaan ainesosien energiapitoisuudet
    ainesosien_energiat_c <- ravitsemus$Energy
    ainesosien_energiat <- matrix(nrow=otsikot@ainesosat_n,ncol=1)
    rownames(ainesosien_energiat) <- otsikot@ainesosat
    colnames(ainesosien_energiat) <- c("Energy")
    
    for(i in seq(from=1,to=length(ainesosien_energiat)))
    {
      ainesosien_energiat[i,1] <- ainesosien_energiat_c[i]
    }
    return(ainesosien_energiat)
  }
  
  
  lue.ainesosien_ymparistohaitat <- function()
  {
    #luetaan ainesosien ympäritöhaitat
    ainesosien_ymparistohaitat <- matrix(nrow=otsikot@ainesosat_n, ncol=3)
    
    colnames(ainesosien_ymparistohaitat) <- otsikot@ymparistohaitat
    rownames(ainesosien_ymparistohaitat) <- otsikot@ainesosat
    
    for( col in seq(1,3)) 
    {
      for( row in seq(1,otsikot@ainesosat_n) )
      {
        #data alkaa ensimmäiseltä  riviltä ja viidenneltä sarakkeelta
        ainesosien_ymparistohaitat[row,col] <- ymparisto[row,col+4]
      }
    }
    return(ainesosien_ymparistohaitat)
  }
  
  
  lue.ravinne_painot <- function()
  {
    #ravinteiden painotukset ylös
    ravinne_painot <- suositukset.nutrients$Score.weight
    return(ravinne_painot)
  }
  
  lue.aineosien_haitakkeet <- function()
  {
    #luetaan aineosien haitakkeet
    aineosien_haitakkeet <- matrix(nrow=otsikot@ainesosat_n, ncol=otsikot@haittakkeet_n)
    rownames(aineosien_haitakkeet) <- otsikot@ainesosat
    colnames(aineosien_haitakkeet) <- otsikot@haittakkeet
    
    for( aineosa_ind in seq(1, otsikot@ainesosat_n)) 
    {
      for( haitake_ind in seq(1, otsikot@haittakkeet_n) )
      {
        #data alkaa ensimmäiseltä  riviltä ja viidenneltä sarakkeelta
        aineosien_haitakkeet[aineosa_ind,haitake_ind] = exposure[aineosa_ind, haitake_ind+3]
      }
    }
    return(aineosien_haitakkeet)
  }
  
  lue.ainesosien_luokittelu <- function()
  {
    #luetaan aineosien luokat
    ainesosien_luokittelu <- matrix(nrow=otsikot@ainesosat_n, ncol=otsikot@ainesosien_luokittelu_n)
    
    rownames(ainesosien_luokittelu) <- otsikot@ainesosat
    colnames(ainesosien_luokittelu) <- otsikot@ainesosien_luokittelu
    
    ravinteet <- c(colnames(ravitsemus))
    
    #indeksit
    eka_luokat_ind <- match(c(otsikot@ainesosien_luokittelu[1]), ravinteet)
    vika_luokat_ind <- match(c(otsikot@ainesosien_luokittelu[otsikot@ainesosien_luokittelu_n]), ravinteet)
    
    
    for( aineosa_ind in seq(1, otsikot@ainesosat_n)) 
    {
      for( luokka_ind in seq(1, otsikot@ainesosien_luokittelu_n) )
      {
        #data alkaa   riviltä ja  sarakkeelta
        ainesosien_luokittelu[aineosa_ind,luokka_ind] = ravitsemus[aineosa_ind, eka_luokat_ind + luokka_ind -1]
      }
    }
    return(ainesosien_luokittelu)
  }
  
  
  lue.ainesosien_ominaistilavuus <- function()
  {
    ainesosien_ominaistilavuus <- matrix(nrow=otsikot@ainesosat_n,ncol=1)
    rownames(ainesosien_ominaistilavuus) <- otsikot@ainesosat
    colnames(ainesosien_ominaistilavuus) <- c("bulk_L_g")
    
    for( aineosa_ind in seq(1, otsikot@ainesosat_n))
    {
      ainesosien_ominaistilavuus[aineosa_ind,"bulk_L_g"] <- 1 / ravitsemus[aineosa_ind, "bulkDensity_g_L"]
    }
    return(ainesosien_ominaistilavuus)
  }
  
  lue.suositukset_energy <- function()
  {
    suositukset_energy <- matrix(nrow=otsikot@sukup_ika_n,ncol=1)
    rownames(suositukset_energy) <- otsikot@sukup_ika
    colnames(suositukset_energy) <- c("Energy.kJ")
    
    for(sukup_ika_ind in seq(1:otsikot@sukup_ika_n))
    {
      suositukset_energy[sukup_ika_ind,"Energy.kJ"] <- suositukset.energy[sukup_ika_ind, "Energy.kJ"]
    }
    return(suositukset_energy)
  }
  
  lue.suositukset_tilavuus <- function()
  {
    suositukset_tilavuus <- matrix(nrow=otsikot@sukup_ika_n,ncol=1)
    rownames(suositukset_tilavuus) <- otsikot@sukup_ika
    colnames(suositukset_tilavuus) <- c("max.volume")
    
    for(sukup_ika_ind in seq(1:otsikot@sukup_ika_n))
    {
      suositukset_tilavuus[sukup_ika_ind,"max.volume"] <- suositukset.volume[sukup_ika_ind, "max.volume"]
    }
    return(suositukset_tilavuus)
  }
  
  luetut_taulukot = new(
    "Syotteet",
    otsikot=otsikot,
    suositukset_energy=lue.suositukset_energy(),
    suositukset_nutrients_xb=as.matrix(suositukset.nutrients_xb),
    suositukset_nutrients_xdot=as.matrix(suositukset.nutrients_xdot),
    suositukset_nutrients_xl=as.matrix(suositukset.nutrients_xl),
    suositukset_nutrients_xu=as.matrix(suositukset.nutrients_xu),
    suositukset_volume = lue.suositukset_tilavuus(),
    
    reseptit_ainesosat=lue.reseptit_ainesosat(),
    ainesosa_ravinne=lue.ainesosa_ravinne(),
    ainesosien_energiat=lue.ainesosien_energiat(),
    ainesosien_ymparistohaitat=lue.ainesosien_ymparistohaitat(),
    ravinne_painot=lue.ravinne_painot(),
    aineosien_haitakkeet=lue.aineosien_haitakkeet(),
    ainesosien_luokittelu = lue.ainesosien_luokittelu(),
    ainesosien_ominaistilavuus = lue.ainesosien_ominaistilavuus()
    
  )
  
  return(luetut_taulukot)
}