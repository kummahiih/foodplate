#syötteiden määritelmät
source("syotteet.R")

setClass(
  "Reunaehdot", 
  representation(
    energia_minimi = "numeric",
    ravinne_minimi = "numeric",
    maksimi_tilavuus = "matrix",
    veg_minimi =  "numeric",
    prot_minimi = "numeric",
    carb_minimi = "numeric",
    maksimi_co2 = "numeric",
    maksimi_po4 = "numeric",
    
    tarkasta_energia_minimi = "logical",
    tarkasta_ravinne_minimi = "logical",
    tarkasta_maksimi_tilavuus = "logical",
    tarkasta_veg_minimi =  "logical",
    tarkasta_prot_minimi = "logical",
    tarkasta_carb_minimi = "logical",
    tarkasta_maksimi_co2 = "logical",
    tarkasta_maksimi_po4 = "logical"
  )
)


#ei vielä käytetty
tarkista.resepti <- function(taulukot, yhteenveto, resepti_ind, reunaehdot)
{
  if(tarkasta_energia_minimi && yhteenveto[resepti_ind, "Energy score"] < reunaehdot@energia_minimi )
    return(FALSE)
  if(tarkasta_ravinne_minimi && yhteenveto[resepti_ind, "Nutrients score"] < reunaehdot@ravinne_minimi )
    return(FALSE)
  
  return(TRUE)
}

kasvata.annosta <- function(otsikot, taulukot, yhteenveto, reunaehdot, kerroin)
{
  for(resepti_ind in 1:otsikot@reseptit_n)
  {
    if(yhteenveto[resepti_ind, "Energy score"] < reunaehdot@energia_minimi  )
    {
      uusi_resepti <- taulukot@reseptit_ainesosat[resepti_ind,] * kerroin
      taulukot@reseptit_ainesosat[resepti_ind,] <- uusi_resepti
    }
  }
  return(taulukot)
}