module Classifications where

data Classifications = Classifications {
    title :: String
  }                                  
                                            
data TitleFactors = TitleFactors {
    headerText :: String       
    ,occursOnce :: Bool                                
  }
  deriving Show  
