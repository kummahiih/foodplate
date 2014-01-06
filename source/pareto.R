#
#@copyright: 2013 by Pauli Rikula <pauli.rikula@gmail.com>
#@license: MIT <http://www.opensource.org/licenses/mit-license.php>
#


dominointi <- function(A, B, operaatiot)
{
  #operaatiot : FALSE -> minimoidaan, TRUE -> maksimoidaan
  #palauttaa -1, jos A dominoi, 0, jos ei voi sanoa ja 1, jos B dominoi
  len = length(operaatiot)
  A_dominoi <- FALSE
  B_dominoi <- FALSE
  
  for(i in seq(1:len))
  {
    if(operaatiot[i])
    {
      if(A[i] > B[i])
      {
        A_dominoi <- TRUE
      }
      else if(A[i] < B[i])
      {
        B_dominoi <- TRUE
      }
    }
    else
    {
      if(A[i] < B[i])
      {
        A_dominoi <- TRUE
      }
      else if(A[i] > B[i])
      {
        B_dominoi <- TRUE
      }      
    }
    
    if(A_dominoi && B_dominoi || !A_dominoi && !B_dominoi )
      return(0)
  }
  if(A_dominoi)
    return(-1)
  return(1)  
}

laske_pareto <- function(arvot, operaatiot, maaraava_sarake, maksimi_pareto=NA)
{
  #operaatiot : FALSE -> minimoidaan, TRUE -> maksimoidaan
  jarjestykset <- order(arvot[,maaraava_sarake], decreasing=operaatiot[maaraava_sarake])
  
  pareto_vektori <- rep(NA, length(jarjestykset))
  
  pareto_ryhma <- 0
  
  
  while(TRUE)
  { 
    uusi <- NA
    pareto_ryhma <- pareto_ryhma + 1
    if(!is.na(maksimi_pareto) && maksimi_pareto <= pareto_ryhma)
    {
      
      for(i in jarjestykset)
      {
        if(is.na(pareto_vektori[i]))
        {
          pareto_vektori[i] <- maksimi_pareto
        }
      }
      break
    }
    
    for(i in jarjestykset)
    {
      if(is.na(pareto_vektori[i]))
      {
        uusi <- i
        break
      }      
    }
    #print(c("uusi", arvot[uusi,]))
    if(is.na(uusi))
      break
    
    #maksimi ja minimi kuuluu aina pareto -fronttiin
    pareto_vektori[uusi] <- pareto_ryhma
    

    #käydään alkiot läpi järjestyksessä, joten vain samankokoinen voi dominoida
    for(kandi in jarjestykset)
    {
      dominoiva_loyty <- FALSE
      dominoi <- FALSE
      
      #print(c("kandi",arvot[kandi,]))
      if(!is.na(pareto_vektori[kandi]))
        next
            
      for(verrokki in jarjestykset)
      {
        #print(c("verrokki",arvot[verrokki,]))
        if(is.na(pareto_vektori[verrokki]))
          next
        if(pareto_vektori[verrokki] != pareto_ryhma)
          next
        d = dominointi(arvot[kandi,], arvot[verrokki,], operaatiot)
        if(d == -1)
        {
          #print(c("kandi",arvot[kandi,],"dominoi",arvot[verrokki,]))
          pareto_vektori[verrokki] <- NA
          dominoi <- TRUE
        }
        if(d == 0)
          next        
        if(d == 1)
        {
          #print(c(arvot[verrokki,],"dominoi",arvot[kandi,]))
          dominoiva_loyty <- TRUE
          break
        }
      }
      
      if(!dominoiva_loyty)
      {
        #print(c(arvot[kandi,],"front",pareto_ryhma))
        pareto_vektori[kandi] <- pareto_ryhma
      }      
    }
  }
  return(pareto_vektori)
}

