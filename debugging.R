creaGruppoVariabili = function(data, varOrig, colVar)
{
  macroGruppi = matrix(nrow = nrow(data))
  for(i in 1:nrow(data))
  {
    if(data[i,colVar] == "(Intercept)")
    {
      macroGruppi[i,1] = "Intercetta"
    }else
    {
      if(sum(str_equal(data[i,colVar], varOrig))==1)
      {
        macroGruppi[i,1] = data[i,colVar] %>% as.character()
      }else
      {
        data[i,colVar] = data[i,colVar] %>% 
          str_sub(end = data[i,colVar] %>% 
                    str_locate_all(pattern = "_") %>% 
                    unlist() %>% 
                    tail(1) - 1)
        macroGruppi[i,1] = data[i,colVar] %>% as.character()
      }
    }
  }
  return(macroGruppi)
}
creaGruppoVariabili(log_coef, names(dfRid), "term")

