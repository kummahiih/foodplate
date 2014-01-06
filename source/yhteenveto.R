#
#@copyright: 2013 by Pauli Rikula <pauli.rikula@gmail.com>
#@license: MIT <http://www.opensource.org/licenses/mit-license.php>
#


laske.yhteenveto_2 <- function(taulukot,otsikot, sukup_ika)
{
  #lasketaan resepteistä tietoja
  reseptitiedot <- laske.Reseptitiedot(taulukot,otsikot)
  #lasketaan pisteet yhdelle aterialle
  aterianPisteytykset <- laske.reseptit_ravinne_score_2(taulukot, otsikot, reseptitiedot)
  
  score_mtx <- cbind(aterianPisteytykset, reseptitiedot@reseptien_haitakkeet, reseptitiedot@reseptien_ymparistohaitat[,"CO2"],reseptitiedot@reseptien_ymparistohaitat[,"PO4"])
  return(score_mtx)
}



#### yhteenvetomatriisin koonta ####
lue.yhteenveto <- function(sukup_ika, otsikot, reseptitiedot, aterianPisteytykset)
{
  #                  1                 2              3    4     5     6
  results_cols <- c("Energy score","Nutrients score","CO2","PO4",
                    "MCPA.ekv.kg","PCDD.F....DL.PCB.TEQ", "Mercury",  "Cadmium",
                    "volume","isVeggie","isProtein","isCarb","isBread","isDrink","isDessert")
  results <- matrix(nrow=otsikot@reseptit_n, ncol=length(results_cols))
  colnames(results) <- results_cols
  rownames(results) <- otsikot@reseptit
  for(resepti_ind in seq(1,otsikot@reseptit_n))
  {
    results[resepti_ind, 1] <- aterianPisteytykset@reseptit_energia_score[resepti_ind, sukup_ika]
    results[resepti_ind, 2] <- aterianPisteytykset@reseptit_ravinne_score[resepti_ind, sukup_ika]

    results[resepti_ind, "CO2"] <- reseptitiedot@reseptien_ymparistohaitat[resepti_ind,"CO2"]
    results[resepti_ind, "PO4"] <- reseptitiedot@reseptien_ymparistohaitat[resepti_ind,"PO4"]
    results[resepti_ind, "MCPA.ekv.kg"] <- reseptitiedot@reseptien_ymparistohaitat[resepti_ind,"MCPA.ekv.kg"]
    
    
    for(col in c("PCDD.F....DL.PCB.TEQ", "Mercury",  "Cadmium"))
    {
      results[resepti_ind, col] <- aterianPisteytykset@reseptit_haitake_score[resepti_ind,col] 
    }
    
    results[resepti_ind, "volume"] <- reseptitiedot@reseptien_tilavuudet[resepti_ind]
    for(col in c("isVeggie","isProtein","isCarb","isBread","isDrink","isDessert"))
    {
      results[resepti_ind, col] <- reseptitiedot@reseptien_luokkien_tilavuus[resepti_ind,col] 
    }
  }
  
  return(results)
}

laske.yhteenveto <- function(taulukot,otsikot, sukup_ika)
{
  #lasketaan resepteistä tietoja
  reseptitiedot <- laske.Reseptitiedot(taulukot,otsikot)
  #lasketaan pisteet yhdelle aterialle
  aterianPisteytykset <- laske.AterianPisteet(taulukot, otsikot, reseptitiedot)
  yhteenveto <- lue.yhteenveto(sukup_ika, otsikot, reseptitiedot, aterianPisteytykset)
  return(yhteenveto)
}

tallenna.csv.yhteenveto <-  function(sukup_ika="Male.18.30.years", otsikot, reseptitiedot, aterianPisteytykset, rootpath)
{
  #tallennetaan kaytetyt tiedot .. voidaan tarkistaa, että mitä tuli luettua
  oldwd <- getwd()
  setwd(rootpath)
  
  yhteenveto <- lue.yhteenveto(sukup_ika, otsikot, reseptitiedot, aterianPisteytykset)
  
  #            c("Energy score","Nutrients score",  "CO2", "PO4","MCPA.ekv.kg","MCPA.ekv.kg","PCDD.F....DL.PCB.TEQ", "Mercury",  "Cadmium",)
  operaatiot <-c(TRUE,           TRUE,              FALSE, FALSE, FALSE,       FALSE,        FALSE,                   FALSE,      FALSE)
  front <- laske_pareto(yhteenveto, operaatiot, 1)  
  
  energynutrients <- cbind(yhteenveto[,"Nutrients score"], yhteenveto[,"Energy score"])
  colnames(energynutrients) <- c("Nutrients score", "Energy score")
  energy_nutrient_front <- laske_pareto(yhteenveto, c(TRUE,TRUE), 1) 
  
  yhteenveto <- cbind(yhteenveto,front,energy_nutrient_front)
  
  name <- paste("tulokset/yhteenvetojen_frontit_2",sukup_ika,".csv")
  write.table(yhteenveto, 
              file=name,sep=";",quote=FALSE,dec=",", col.names=NA)
  
  setwd(oldwd)
}