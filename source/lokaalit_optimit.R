#### Lisenssi  ####

#
#@copyright: 2013 by Pauli Rikula <pauli.rikula@gmail.com>
#@license: MIT <http://www.opensource.org/licenses/mit-license.php>
#

#### ajoympäristö ####

#tämä R -skripti on kehitetty R -versiolle 'R version 3.0.1 (2013-05-16) -- "Good Sport"'
#käyttäen RStudio -kehitystyökalua

#rootpath == polku jonka alla 'csv datat' -hakemisto 
#esimerkiksi, jos tämä tiedosto sijaite hakemistossa "C:/projektit/foodweb/prog"

rootpath="C:/projektit/foodweb/prog"

setwd(rootpath)

#käytettyjen otsioiden märitelmät
source("otsikot.R")

#syötteiden määritelmät
source("syotteet.R")

#syötteiden luku csv -tiedostosta
source("lue_csv_taulukot.R")

#reseptien tietojen laskenta
source("reseptit.R")

#yhden aterian pisteytykset
source("aterian_pisteytykset.R")

source("pareto.R")

source("yhteenveto.R")

source("reunaehdot.R")







test_recipt <- function( tulostetiedoto_prefiksi, fromindex, sukup_ika, printpics, taulukot, otsikot, reunaehdot)
{
  
  print(c("resepti #",fromindex,sukup_ika))
  
  #            c("Energy score","Nutrients score",  "CO2", "PO4","MCPA.ekv.kg","MCPA.ekv.kg","PCDD.F....DL.PCB.TEQ", "Mercury",  "Cadmium",)
  operaatiot <-c(TRUE,           TRUE,              FALSE, FALSE, FALSE,       FALSE,        FALSE,                   FALSE,      FALSE)
  
  
  
  kopio_taulukoista <- uusilla_ainesosilla(otsikot, taulukot)
  print("uudet")
  kopio_taulukoista <- luo_uudet_reseptit_reseptista(otsikot, kopio_taulukoista, fromindex)
  
  print(c("käytetään pohjana reseptia", otsikot@reseptit[fromindex]))
  for(iteration in 1:100)
  {
    print(c("iteration",iteration))
    yhteenveto <- laske.yhteenveto(kopio_taulukoista, otsikot,sukup_ika)
    front <- laske_pareto(yhteenveto, operaatiot, 1) 
    
    if(printpics)
    {
      name <- paste("plots\\p",tulostetiedoto_prefiksi,"_",toString(fromindex),"_",toString(iteration),toString(sukup_ika),".png")
      png(name, width = 800, height = 600)
      
      energynutrients <- cbind(yhteenveto[,"Nutrients score"], yhteenveto[,"Energy score"])
      colnames(energynutrients) <- c("Nutrients score", "Energy score")
      energy_nutrient_front <- laske_pareto(yhteenveto, c(TRUE,TRUE), 1) 
      
      cols = character(nrow(yhteenveto))
      cols[1:nrow(yhteenveto)] = "black"
      cols[which(energy_nutrient_front == 1)] = "green"
      cols[which(energy_nutrient_front == 2)] = "yellow"
      cols[which(energy_nutrient_front == 3)] = "blue"
      cols[which(energy_nutrient_front == 4)] = "red"
      
      yhteenveto <- cbind(yhteenveto,front,energy_nutrient_front)
      
      pairs(yhteenveto,col=cols,main=toString(iteration))
      
      dev.off()
    }
    
    #pudotetaan resepti yhtä alemmalle frontille, jos se ei täytä kriteereitä
    if(reunaehdot@tarkasta_energia_minimi)
    {
      for(resepti_ind in 1:otsikot@reseptit_n)
      {
        if( yhteenveto[resepti_ind, "Energy score"] < reunaehdot@energia_minimi )
            front[resepti_ind] = front[resepti_ind] + 1             
      }
    }
    
    if(reunaehdot@tarkasta_ravinne_minimi)
    {
      for(resepti_ind in 1:otsikot@reseptit_n)
      {
        if(yhteenveto[resepti_ind, "Nutrients score"] < reunaehdot@ravinne_minimi )
          front[resepti_ind] = front[resepti_ind] + 1             
      }
    }
    
    if(reunaehdot@tarkasta_maksimi_tilavuus)
    {
      for(resepti_ind in 1:otsikot@reseptit_n)
      {
        if(yhteenveto[resepti_ind, "volume"] > reunaehdot@maksimi_tilavuus[sukup_ika,1] )
        {
          front[resepti_ind] = front[resepti_ind] + 1
        }
      }
    }
    
    if(all(front==1)) 
    {
      print("hienosäätöä 0.01")
      kopio_taulukoista<- mutatoi_puolet(otsikot,kopio_taulukoista, 0.1)
           
      next;
    }
    
    if(all(front==2) ) 
    {
      print("hienosäätöä 0.1")
      kopio_taulukoista<- mutatoi_puolet(otsikot,kopio_taulukoista, 1.0)
      
      next;
    }
    
    if(all(front==3)) 
    {
      print("hienosäätöä 0.5")
      kopio_taulukoista<- mutatoi_puolet(otsikot,kopio_taulukoista, 2.0)
      
      next;
    }
    
    if(all(front==4) ) 
    {
      print("hienosäätöä 1.0")
      kopio_taulukoista<- mutatoi_puolet(otsikot,kopio_taulukoista, 3.0)
      
      next;
    }
    
    pienin_front = min(front)
    
    parhaat_frontit = which(front==pienin_front)
    
    
    print("korvauksia")
    for(resepti_ind in 1:otsikot@reseptit_n)
    {    
      if(front[resepti_ind] == pienin_front)
      {
        next
      }
      
      korvaus_resepti_ind = sample(parhaat_frontit, 1)
       
      uusi_resepti <- kopio_taulukoista@reseptit_ainesosat[korvaus_resepti_ind,] + rnorm(otsikot@ainesosat_n, 0, 0.2)
      for(ainesosa_ind in 1:otsikot@ainesosat_n)
      {
        if(uusi_resepti[ainesosa_ind] < 0.0)
          uusi_resepti[ainesosa_ind] <- 0.0
      }
      kopio_taulukoista@reseptit_ainesosat[resepti_ind,] <- uusi_resepti

      
    } 
  }
  
  yhteenveto <- laske.yhteenveto(kopio_taulukoista, otsikot,sukup_ika)
  
  front <- laske_pareto(yhteenveto, operaatiot, 1)  
  
  energynutrients <- cbind(yhteenveto[,"Nutrients score"], yhteenveto[,"Energy score"])
  colnames(energynutrients) <- c("Nutrients score", "Energy score")
  energy_nutrient_front <- laske_pareto(yhteenveto, c(TRUE,TRUE), 1) 
  

  
  yhteenveto <- cbind(yhteenveto,front,energy_nutrient_front)

  if(printpics)
  {
    cols = character(nrow(yhteenveto))
    cols[1:nrow(yhteenveto)] = "black"
    cols[which(energy_nutrient_front == 1)] = "green"
    cols[which(energy_nutrient_front == 2)] = "yellow"
    cols[which(energy_nutrient_front == 3)] = "blue"
    cols[which(energy_nutrient_front == 4)] = "red"
    
    name <- paste("plots\\p",tulostetiedoto_prefiksi,"_",toString(fromindex),"_",toString(sukup_ika),".png")
    png(name, width = 800, height = 600)            
    
    pairs(yhteenveto,col=cols,main=toString(iteration))
    dev.off();
  }
        
  filename_reseptit <- paste("tulokset/reseptit_ainesosat_",tulostetiedoto_prefiksi,toString(fromindex),toString(sukup_ika),"_100.csv")
  colnames(kopio_taulukoista@reseptit_ainesosat) <- otsikot@ainesosat
  rownames(kopio_taulukoista@reseptit_ainesosat) <- 1:otsikot@reseptit_n
  write.table(kopio_taulukoista@reseptit_ainesosat, 
              file=filename_reseptit,sep=";",quote=FALSE,dec=",", col.names=NA)
  
  rownames(yhteenveto) <- 1:otsikot@reseptit_n
  
  filename_yhteenveto <- paste("tulokset/yhteenveto_",tulostetiedoto_prefiksi, toString(fromindex),toString(sukup_ika),"_100.csv")
  
  write.table(yhteenveto, 
              file=filename_yhteenveto,sep=";",quote=FALSE,dec=",", col.names=NA)
  
}

test_6 <- function()
{
  #3 oli rajoitteilla (sis tilavuus)
  #4 on paremmalla korvauksella
  #_5 on uudella aineistolla ja 
  #6 on paremmalla energiascorella
  #7 : muutettu haitake ja energiascorea 
  tulostetiedoto_prefiksi <- "local_optim_7"
  taulukot <- lue.csv.taulukot(rootpath=rootpath)
  otsikot <- taulukot@otsikot  

  #"Male.18.30.years"  --nämä on testattu ja ovat tuloksissa hieman eri nimillä
  #"Female.10.years"
  sukup_ika="Female.10.years"
  reunaehdot = new(
    "Reunaehdot",
    maksimi_tilavuus = taulukot@suositukset_volume
    )

  reunaehdot@energia_minimi = 0.8
  reunaehdot@tarkasta_energia_minimi = TRUE
  
  reunaehdot@ravinne_minimi = 0.8
  reunaehdot@tarkasta_ravinne_minimi = TRUE
  
  reunaehdot@tarkasta_maksimi_tilavuus = TRUE

 
  
  for(i in 2:2 ) #otsikot@reseptit_n
  {
    test_recipt(tulostetiedoto_prefiksi, i,sukup_ika, FALSE, taulukot, otsikot, reunaehdot)
  }
}


