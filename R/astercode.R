astercode <-
function(dir="RLIB")
  {
    if(missing(dir))
      { dir='/home/lees/Class/GEOL_590_Inverse_Theory/Rwork/RLIB'
      } 
    LF = list.files(path=dir, pattern=".R", full.names=TRUE)
    for(i in 1:length(LF)) { source(LF[i]) }

  }
