#### Lisenssi  ####

#
#@copyright: 2013 by Pauli Rikula <pauli.rikula@gmail.com>
#@license: MIT <http://www.opensource.org/licenses/mit-license.php>
#

#### ajoympäristö ####

#tämä R -skripti on kehitetty R -versiolle 'R version 3.0.1 (2013-05-16) -- "Good Sport"'
#käyttäen RStudio -kehitystyökalua

#käytettyjen otsioiden märitelmät
source("otsikot.R")

#syötteiden määritelmät
source("syotteet.R")

#syötteiden luku csv -tiedostosta
source("lue_csv_taulukot.R")

#reseptien tiotojen laskenta
source("reseptit.R")

source("pareto.R")

setClass(
  "AterianPisteet",
  representation(
    reseptit_ravinne_score="matrix",
    reseptit_energia_score="matrix",
    reseptit_haitake_score="matrix"    
  )
)

laske.score <- function (ravinne, xl, xdot, xb, xu)
{
  score <- 0
  if(ravinne < xl)
  {
    score <- 0
  }
  else if(ravinne < xdot)
  {
    score <- 1 - (xdot - ravinne)/(xdot - xl)
  }
  else if(ravinne < xb)
  {
    score <- 1
  }
  else 
  {
    score <- (xu - ravinne)/(xu - xb)
  }
  return(score)
}

laske.reseptit_score <- function(taulukot, otsikot, reseptitiedot, sukup_ika,maksimi_pareto=NA)
{
  reseptit_ravinne_score <- matrix(nrow=otsikot@reseptit_n, ncol=(otsikot@ravinteet_n +1))
  rownames(reseptit_ravinne_score) <- otsikot@reseptit
  colnames(reseptit_ravinne_score) <- c( "Energy.kJ", otsikot@ravinteet )
  
  for(resepti_ind in seq(1,otsikot@reseptit_n))
  {
    for(ravinne_ind in seq(1,otsikot@ravinteet_n))
    {
      
      ravinne <- reseptitiedot@reseptien_ravinne[resepti_ind, ravinne_ind]
      
      xl    <- taulukot@suositukset_nutrients_xl[ravinne_ind, sukup_ika]
      xdot  <- taulukot@suositukset_nutrients_xdot[ravinne_ind, sukup_ika]      
      xb    <- taulukot@suositukset_nutrients_xb[ravinne_ind, sukup_ika] 
      xu    <- taulukot@suositukset_nutrients_xu[ravinne_ind, sukup_ika]
      
      score <- laske.score(ravinne, xl, xdot, xb, xu)
      
      reseptit_ravinne_score[resepti_ind, otsikot@ravinteet[ravinne_ind] ] <- score        
    }
    
    

    e <- reseptitiedot@reseptien_energiat[resepti_ind,"Energy.kJ"]
    s_e <- taulukot@suositukset_energy[sukup_ika,"Energy.kJ"]
    
    e_score <- laske.score(e, 0, 0.8 * s_e, 1.2 * s_e, 2 * s_e)
        
    reseptit_ravinne_score[resepti_ind,"Energy.kJ"] <- e_score 
    
    
  } 
  
  reseptit_haitake_score <- laske.reseptit_haitake_score(taulukot, otsikot, reseptitiedot)
  
  score_mtx = cbind(reseptit_ravinne_score,
                    reseptit_haitake_score, 
                    reseptitiedot@reseptien_ymparistohaitat[,"CO2"],
                    reseptitiedot@reseptien_ymparistohaitat[,"PO4"])
  
  #            c("Energy score","Nutrients score",                       haitakkeet                  ympäristö
  operaatiot <-c(TRUE,           rep(TRUE,otsikot@ravinteet_n),    rep(FALSE,otsikot@haittakkeet_n), FALSE, FALSE)
  #print(length(operaatiot))
  #print(dim(score_mtx))
  
  front <- laske_pareto(score_mtx, operaatiot, 1, maksimi_pareto)  
  
  yhteenveto <- cbind(score_mtx, front)
  
  colnames(yhteenveto) <- c( colnames(reseptit_ravinne_score), colnames(reseptit_haitake_score), "CO2", "PO4", "front")
  
  return(yhteenveto)
}


laske.reseptit_ravinne_score <- function(taulukot, otsikot, reseptitiedot)
{
  #muodostetaan matriisi reseptien ravinne score -arvoille per jokainen sukup x ikäluokka
  
  reseptit_ravinne_score <- matrix(nrow=otsikot@reseptit_n, ncol=otsikot@sukup_ika_n)
  rownames(reseptit_ravinne_score) <- otsikot@reseptit
  colnames(reseptit_ravinne_score) <- otsikot@sukup_ika
  
  #nämäkin voisi laskea
  #reseptit_ravinne_micronutrition_score <- matrix(nrow=reseptit_n, ncol=sukup_ika_n)
  #rownames(reseptit_ravinne_score) <- reseptit
  #colnames(reseptit_ravinne_score) <- sukup_ika
  
  #reseptit_ravinne_vaarallisuus_score <- matrix(nrow=reseptit_n, ncol=sukup_ika_n)
  #rownames(reseptit_ravinne_score) <- reseptit
  #colnames(reseptit_ravinne_score) <- sukup_ika
  
  #reseptit_ravinne_energia_score <- matrix(nrow=reseptit_n, ncol=sukup_ika_n)
  #rownames(reseptit_ravinne_score) <- reseptit
  #colnames(reseptit_ravinne_score) <- sukup_ika
  
  
  
  # arvo  < xl -> 0
  # arvo  < x* -> 1 - (x* - arvo)/(x* - xl)
  # x* == xdot
  # arvo  < xb -> 1
  # arvo  < xu -> (xu - arvo)/(xu - xb)
{
  for(resepti_ind in seq(1,otsikot@reseptit_n))
  {
    for(sukup_ika_ind in seq(1,otsikot@sukup_ika_n))
    {
      score_vec <- vector(length=otsikot@ravinteet_n)
      
      for(ravinne_ind in seq(1,otsikot@ravinteet_n))
      {
        ravinne <- reseptitiedot@reseptien_ravinne[resepti_ind, ravinne_ind]
        
        
        
        xl    <- taulukot@suositukset_nutrients_xl[ravinne_ind, sukup_ika_ind]
        xdot  <- taulukot@suositukset_nutrients_xdot[ravinne_ind, sukup_ika_ind]      
        xb    <- taulukot@suositukset_nutrients_xb[ravinne_ind, sukup_ika_ind] 
        xu    <- taulukot@suositukset_nutrients_xu[ravinne_ind, sukup_ika_ind]
        
        score <- laske.score(ravinne, xl, xdot, xb, xu)
        
        
        score_vec[ravinne_ind] <- score        
      }
      weighted_score <- (taulukot@ravinne_painot %*% score_vec)[1,1]
      
      reseptit_ravinne_score[resepti_ind, sukup_ika_ind] <- weighted_score      
    }
  } 
}
  return(reseptit_ravinne_score)
}

laske.reseptit_energia_score <- function(taulukot, otsikot, reseptitiedot)
{
  #lasketaan energia -score  
  reseptit_energia_score <- matrix(nrow=otsikot@reseptit_n, ncol=otsikot@sukup_ika_n)
  rownames(reseptit_energia_score) <- otsikot@reseptit
  colnames(reseptit_energia_score) <- otsikot@sukup_ika
  
  
  for(resepti_ind in seq(1,otsikot@reseptit_n))
  {
    for(sukup_ika_ind in seq(1,otsikot@sukup_ika_n))
    {
      e <- reseptitiedot@reseptien_energiat[resepti_ind,"Energy.kJ"]
      s_e <- taulukot@suositukset_energy[sukup_ika_ind,"Energy.kJ"]
      
      score <- laske.score(e, 0, 0.8 * s_e, 1.2 * s_e, 2 * s_e)
      
      reseptit_energia_score[resepti_ind,sukup_ika_ind] <- score 
    }
  }
  
  return(reseptit_energia_score)
}

laske.reseptit_haitake_score <- function(taulukot, otsikot, reseptitiedot)
{
  #lasketaan energia -score  
  reseptit_haitake_score <- matrix(nrow=otsikot@reseptit_n, ncol=otsikot@haittakkeet_n)
  rownames(reseptit_haitake_score) <- otsikot@reseptit
  colnames(reseptit_haitake_score) <- otsikot@haittakkeet
  
  
  for(resepti_ind in seq(1,otsikot@reseptit_n))
  {
    #TODO hae nämä jostain taulukosta
    # raja-arvot ovat 55 kg painoiselle ihmiselle (käytetään sitä esimerkkinä artikkelissa). 
    #PCDD/F: 0,11 ; Mercury: 10214; Cadmium: 19643. Muun painoisille saa suoraan lineaarisesti skaalaamalla raja-arvot.
    
    
    for(sukup_ika_ind in seq(1,otsikot@sukup_ika_n))
    {
      x <- reseptitiedot@reseptien_haitakkeet[resepti_ind,"PCDD.F....DL.PCB.TEQ"]
      if( x < 0.11 * 0.3 )
        reseptit_haitake_score[resepti_ind,"PCDD.F....DL.PCB.TEQ"] <- 0
      else
        reseptit_haitake_score[resepti_ind,"PCDD.F....DL.PCB.TEQ"] <- 1 / (0.7 * 0.11) *x  -(1/0.7 *0.3)
      
      x <- reseptitiedot@reseptien_haitakkeet[resepti_ind,"Mercury"]
      if( x < 10214 * 0.3 )
        reseptit_haitake_score[resepti_ind,"Mercury"] <- 0
      else
        reseptit_haitake_score[resepti_ind,"Mercury"] <- 1 / (0.7 * 10214) *x  -(1/0.7 *0.3)
      
      x <- reseptitiedot@reseptien_haitakkeet[resepti_ind,"Cadmium"]
      if( x < 19643 * 0.3 )
        reseptit_haitake_score[resepti_ind,"Cadmium"] <- 0
      else
        reseptit_haitake_score[resepti_ind,"Cadmium"] <- 1 / (0.7 * 19643) *x  -(1/0.7 *0.3)
          
    }
  }
  
  return(reseptit_haitake_score)
}



laske.AterianPisteet <- function(taulukot, otsikot, reseptitiedot)
{
  laskettu = new(
    "AterianPisteet",
    reseptit_ravinne_score=laske.reseptit_ravinne_score(taulukot, otsikot, reseptitiedot),
    reseptit_energia_score=laske.reseptit_energia_score(taulukot, otsikot, reseptitiedot),
    reseptit_haitake_score = laske.reseptit_haitake_score(taulukot, otsikot, reseptitiedot)
    )
  
  return(laskettu)
}


tallenna.csv.AterianPisteet <- function(obj,rootpath)
{
  #tallennetaan kaytetyt tiedot .. voidaan tarkistaa, että mitä tuli luettua
  oldwd <- getwd()
  setwd(rootpath)
  write.table(obj@reseptit_ravinne_score, 
              file="tulokset/reseptit_ravinne_score_2.csv",sep=";",quote=FALSE,dec=",", col.names=NA)
  write.table(obj@reseptit_energia_score, 
              file="tulokset/reseptit_energia_score_2.csv",sep=";",quote=FALSE,dec=",", col.names=NA)
  write.table(obj@reseptit_haitake_score, 
              file="tulokset/reseptit_haitake_score_2.csv",sep=";",quote=FALSE,dec=",", col.names=NA)
  setwd(oldwd)
}