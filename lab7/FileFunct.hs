module FileFunct
where

import StructFunct
import SupportFunct

saveFile :: [Factory] -> FilePath -> IO ()
saveFile factories outFile = do
  str <- getString factories ""
  writeFile outFile str
  where
    getString :: [Factory] -> [Char] -> IO [Char]
    getString list str = do
      let fieldDivider = ";"
      let structDivider = ['\n']
      let fList = head list
      let newString = str ++ (producer fList) ++ fieldDivider
                          ++ (productType fList) ++ fieldDivider
                          ++ (show $ capacity fList) ++ structDivider
      if length list == 1
        then return newString
        else getString (tail list) newString

loadFile :: [Char] -> [Factory]
loadFile content = do
  let str = content
  let listStrings = listOfStrings str
  let listFields = listOfFields listStrings
  let listFactories = listOfFactories listFields
  result <- listFactories
  return result
