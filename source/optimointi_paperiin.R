#### Lisenssi  ####

#
#@copyright: 2013 by Pauli Rikula <pauli.rikula@gmail.com>
#@license: MIT <http://www.opensource.org/licenses/mit-license.php>
#

#### ajoympäristö ####

#tämä R -skripti on kehitetty R -versiolle 'R version 3.0.1 (2013-05-16) -- "Good Sport"'
#käyttäen RStudio -kehitystyökalua

#rootpath == polku jonka alla 'csv datat' -hakemisto 
#esimerkiksi, jos tämä tiedosto sijaitsee hakemistossa "C:/projektit/foodweb/prog"

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

laske.funktio <- function(scores, fn)
{
  fun <-  match.fun(fn)
  result = matrix(nrow=1, ncol=length(colnames(scores)))
  cn <-colnames(scores)
  for(col in 1:length(result))
  {
    result[1,col] = fun(scores[,col])
    cn[col] <- paste(fn, cn[col])
  }
  colnames(result) <- cn
  return(result)
}


laske.score_log <- function(scores)
{
  log <- cbind(laske.funktio(scores,"min"), laske.funktio(scores,"max"), laske.funktio(scores,"mean"))
  return(log)
}


siisti_reseptit <- function(otsikot, m, minimiarvo)
{
  for(resepti_ind in 1:otsikot@reseptit_n)
  { 
    for(ainesosa_ind in 1:otsikot@ainesosat_n)
    {
      if(m[resepti_ind, ainesosa_ind] < minimiarvo)
      {
        m[resepti_ind, ainesosa_ind] = 0
      }
    }
  }
  return(m)
}

setClass(
  "Tulokset",
  representation(
    yhteenveto="matrix",
    loki="matrix"
  )
)


optimointi <- function( tulostetiedoto_prefiksi, fromindex, sukup_ika, taulukot, otsikot , reunaehdot)
{  
  print( c("resepti #", fromindex, sukup_ika))

  print("reseptitiedot")  
  reseptitiedot <- laske.Reseptitiedot(taulukot,otsikot)
  alkup_resepti <- taulukot@reseptit_ainesosat[fromindex,]
  print("alkup score")    
  alkup_score <- laske.reseptit_score(taulukot, otsikot, reseptitiedot, sukup_ika)
  print("uusi taulukko .. luonti")  
  kopio_taulukoista <- uusilla_ainesosilla(otsikot, taulukot)
  
  otsikot@reseptit_n <- 1000  
  otsikot@reseptit <- as.character(1:otsikot@reseptit_n)
  kopio_taulukoista@otsikot <- otsikot
  print("uusi taulukko alustus")  
  kopio_taulukoista <- luo_uudet_reseptit_reseptista(otsikot, kopio_taulukoista, fromindex)
  
  print("reseptitiedot")  
  reseptitiedot <- laske.Reseptitiedot(kopio_taulukoista,otsikot)
  print("scores")  
  scores <- laske.reseptit_score(kopio_taulukoista, otsikot, reseptitiedot, sukup_ika)
  print(dim(scores))
  print("loki")    
  alkup_loki = laske.score_log(scores) 
  loki <- alkup_loki
  
  print(c("mutatointi aluksi")) 
  kopio_taulukoista<- mutatoi_puolet(otsikot, kopio_taulukoista, 10.0)
  iteraatioita = 40
  for(iteration in 1:iteraatioita)
  {
    
    kopio_taulukoista@reseptit_ainesosat <- siisti_reseptit(otsikot, kopio_taulukoista@reseptit_ainesosat, 1.0)
    
    print(c("iteration",iteration,"/", iteraatioita))
    reseptitiedot <- laske.Reseptitiedot(kopio_taulukoista,otsikot)

    pintoja <- 2
    if(iteration == iteraatioita)
      pintoja <- NA
      
    scores <- laske.reseptit_score(kopio_taulukoista,otsikot, reseptitiedot, sukup_ika, pintoja)
    
    #raja-arvojen tarkastus
    #pudotetaan resepti yhtä alemmalle frontille, jos se ei täytä kriteereitä
    if(reunaehdot@tarkasta_energia_minimi)
    {
      for(resepti_ind in 1:otsikot@reseptit_n)
      {
        if( scores[resepti_ind, "Energy.kJ"] < reunaehdot@energia_minimi )
          scores[resepti_ind, "front"] = scores[resepti_ind, "front"] + 1             
      }
    }
    for(resepti_ind in 1:otsikot@reseptit_n)
    {
      if(reunaehdot@tarkasta_maksimi_tilavuus)
      {
        if(reseptitiedot@reseptien_tilavuudet[resepti_ind] > reunaehdot@maksimi_tilavuus[sukup_ika,1] )
        {
          scores[resepti_ind, "front"] = scores[resepti_ind, "front"] + 1  
        }
      }
      
      if(reunaehdot@tarkasta_maksimi_co2)
      {
        if(reseptitiedot@reseptien_ymparistohaitat[resepti_ind,"CO2"] >  reunaehdot@maksimi_co2  )
        {
          scores[resepti_ind, "front"] = scores[resepti_ind, "front"] + 1  
        }
      }
      
      if(reunaehdot@tarkasta_maksimi_po4)
      {
        if(reseptitiedot@reseptien_ymparistohaitat[resepti_ind,"PO4"] >  reunaehdot@maksimi_po4  )
        {
          scores[resepti_ind, "front"] = scores[resepti_ind, "front"] + 1  
        }
      }
    }
    
   
    
    
    
    
    #loki
    lokirivi <- laske.score_log(scores)
    print( c("pareto 1", length(which(scores[ ,"front"] == 1)) ))    
    loki <- rbind(loki,lokirivi)    
    plot(loki[,"mean PO4"], type="o", col="blue")
    
    if(iteration != iteraatioita)
    {
      pienin_front = min(scores[,"front"])    
      
      
      if(all(scores[ ,"front"]==pienin_front)) 
      {
        print("hienosäätöä")
        kopio_taulukoista<- mutatoi_puolet(otsikot,kopio_taulukoista, 10.0)    
        kopio_taulukoista@reseptit_ainesosat[fromindex,] <- alkup_resepti
        next;
      }
      
      parhaat_frontit = which(scores[,"front"]==pienin_front)
      
      
      print("korvauksia")
      k <- 0
      for(resepti_ind in 1:otsikot@reseptit_n)
      {    
        if(scores[resepti_ind, "front"] == pienin_front)
        {
          next
        }
        k <- k+1
        korvaus_resepti_ind = sample(parhaat_frontit, 1)   
        korvaus_resepti_ind_2 = sample(parhaat_frontit, 1) 
        
        mutation <- sample(c("mutatoi_reseptia_ind", "vaihda_ainetta", "risteyta"), 1) 
        fun <-  match.fun(mutation)
        uusi_resepti <- fun(otsikot, kopio_taulukoista, 0.2, korvaus_resepti_ind, korvaus_resepti_ind_2)
        
        kopio_taulukoista@reseptit_ainesosat[resepti_ind,] <- uusi_resepti
  
      }
      print(c("korvattu:", k))
      if(k <= 0.1 * otsikot@reseptit_n) 
      {
        print("hienosäätöä")
        kopio_taulukoista<- mutatoi_puolet(otsikot,kopio_taulukoista, 1.0)
      }
      
      kopio_taulukoista@reseptit_ainesosat[fromindex,] <- alkup_resepti
      
    }
    
  }
  
  colnames(kopio_taulukoista@reseptit_ainesosat) <- otsikot@ainesosat
  
  yhteenveto <- cbind(kopio_taulukoista@reseptit_ainesosat, scores)
  
  filename_yhteenveto <- paste("tulokset/yhteenveto_",tulostetiedoto_prefiksi, toString(fromindex),toString(sukup_ika),iteraatioita, ".csv")
  
  write.table(yhteenveto, 
              file=filename_yhteenveto,sep=";",quote=FALSE,dec=",", col.names=NA)
  filename_loki <- paste("tulokset/loki_",tulostetiedoto_prefiksi, toString(fromindex),toString(sukup_ika),iteraatioita, ".csv")
  write.table(loki,file=filename_loki, sep=";",quote=FALSE,dec=",", col.names=NA)
  
  t <- new(
    "Tulokset",
    yhteenveto=yhteenveto,
    loki=loki)
  return(t)
}


optimointi_2 <- function( tulostetiedoto_prefiksi, fromindex, sukup_ika, taulukot, otsikot, reunaehdot )
{  
  print( c("resepti #", fromindex, sukup_ika))
  
  print("reseptitiedot")  
  reseptitiedot <- laske.Reseptitiedot(taulukot,otsikot)
  alkup_resepti <- taulukot@reseptit_ainesosat[fromindex,]
  print("alkup score")    
  alkup_score <- laske.reseptit_score(taulukot, otsikot, reseptitiedot, sukup_ika)
  print("uusi taulukko .. luonti")  
  kopio_taulukoista <- uusilla_ainesosilla(otsikot, taulukot)
  
  otsikot@reseptit_n <- 2  
  otsikot@reseptit <- as.character(1:2)
  kopio_taulukoista@otsikot <- otsikot
  print("uusi taulukko alustus")  
  kopio_taulukoista <- luo_uudet_reseptit_reseptista(otsikot, kopio_taulukoista, fromindex)
  
  print("reseptitiedot")  
  reseptitiedot <- laske.Reseptitiedot(kopio_taulukoista,otsikot)
  print("scores")  
  scores <- laske.reseptit_score(kopio_taulukoista, otsikot, reseptitiedot, sukup_ika)
  
  print("loki")    
  alkup_loki = scores[1,]
  loki <- alkup_loki
  
  last_iter <- 10000
  for(iteration in 1:last_iter)
  {   
           
      #print("reseptitiedot")  
      reseptitiedot <- laske.Reseptitiedot(kopio_taulukoista,otsikot)
      #print("scores")  
      
      scores <- laske.reseptit_score(kopio_taulukoista, otsikot, reseptitiedot, sukup_ika)
      lokirivi <- scores[1,]
      print(scores[,"front"])
      if(scores[1,"front"] > scores[2,"front"])
      {
        print(c("dominointi iteratiolla",iteration)) 
        t <- kopio_taulukoista@reseptit_ainesosat[1,]
        
        kopio_taulukoista@reseptit_ainesosat[1,] <- kopio_taulukoista@reseptit_ainesosat[2,]
        
        kopio_taulukoista@reseptit_ainesosat[2,] <- t
        
        lokirivi <- scores[2,]
      }
      
      loki <- rbind(loki,lokirivi)
      
      
      if(iteration != last_iter)
      { 
        rep_ind <- 1
        for(i in 1:sample(1:30,1) )
        {
          mutation <- sample(c("mutatoi_reseptia_ind", "vaihda_ainetta", "risteyta"), 1) 
          fun <-  match.fun(mutation)
          uusi_resepti <- fun(otsikot, kopio_taulukoista, 20.0, rep_ind,1)          
          kopio_taulukoista@reseptit_ainesosat[2,] <- uusi_resepti  
          rep_ind <- 2
        }
      }
    } 
  
  yhteenveto <- cbind(kopio_taulukoista@reseptit_ainesosat, scores)
  
  filename_yhteenveto <- paste("tulokset/yhteenveto_",tulostetiedoto_prefiksi, toString(fromindex),toString(sukup_ika),"_100.csv")
  
  write.table(yhteenveto, 
              file=filename_yhteenveto,sep=";",quote=FALSE,dec=",", col.names=NA)

  filename_loki <- paste("tulokset/loki_",tulostetiedoto_prefiksi, toString(fromindex),toString(sukup_ika),"_100.csv")
  write.table(loki,file=filename_loki, sep=";",quote=FALSE,dec=",", col.names=NA)
  
  t <- new(
    "Tulokset",
    yhteenveto=yhteenveto,
    loki=loki)
  
  return(t)
}







taulukot <- lue.csv.taulukot(rootpath=rootpath)
otsikot <- taulukot@otsikot
reseptitiedot <- laske.Reseptitiedot(taulukot,otsikot)

#lasketaan resepteille tietoja



sukup_ika="Female.10.years"

#alkup_scores <- laske.reseptit_score(taulukot,otsikot, alkup_reseptitiedot, sukup_ika)

#alkup_tilanne <- laske.score_log(alkup_scores)
  
tulostetiedoto_prefiksi <- "testi_8"

fromindex=2

#"Male.18.30.years"  --nämä on testattu ja ovat tuloksissa hieman eri nimillä
#"Female.10.years"

reunaehdot = new(
  "Reunaehdot",
  maksimi_tilavuus = taulukot@suositukset_volume
)

reunaehdot@energia_minimi = 0.8
reunaehdot@tarkasta_energia_minimi = TRUE

reunaehdot@tarkasta_maksimi_tilavuus = TRUE

reunaehdot@maksimi_co2 = reseptitiedot@reseptien_ymparistohaitat[fromindex,"CO2"]
reunaehdot@tarkasta_maksimi_co2 = TRUE

reunaehdot@maksimi_po4 = reseptitiedot@reseptien_ymparistohaitat[fromindex,"PO4"]
reunaehdot@tarkasta_maksimi_po4 = TRUE

#sukup_ika="Female.10.years"
#tulokset1 <- optimointi( tulostetiedoto_prefiksi, fromindex=fromindex, sukup_ika=sukup_ika, taulukot, otsikot, reunaehdot)

#sukup_ika="Male.18.30.years"
#tulokset2 <- optimointi( tulostetiedoto_prefiksi, fromindex=fromindex, sukup_ika=sukup_ika, taulukot, otsikot, reunaehdot)
  
sukup_ika="Female.15.years"
tulokset1 <- optimointi( tulostetiedoto_prefiksi, fromindex=fromindex, sukup_ika=sukup_ika, taulukot, otsikot, reunaehdot)




fromindex=141

sukup_ika="Female.10.years"
tulokset2 <- optimointi( tulostetiedoto_prefiksi, fromindex=fromindex, sukup_ika=sukup_ika, taulukot, otsikot, reunaehdot)

sukup_ika="Male.18.30.years"
tulokset3 <- optimointi( tulostetiedoto_prefiksi, fromindex=fromindex, sukup_ika=sukup_ika, taulukot, otsikot, reunaehdot)

sukup_ika="Female.15.years"
tulokset4 <- optimointi( tulostetiedoto_prefiksi, fromindex=fromindex, sukup_ika=sukup_ika, taulukot, otsikot, reunaehdot)

  