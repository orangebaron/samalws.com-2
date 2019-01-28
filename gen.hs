import System.Directory (createDirectory,copyFile,listDirectory)
import Data.List.Split (splitOn)

parseFileLine :: String -> String
parseFileLine line = "<li><a href=\""++link++"\">"++name++"</a> - "++desc++"</li>\n" where [name,link,desc] = splitOn "|" line

parseFileBody :: [String] -> String
parseFileBody = (foldl (++) "") . (map parseFileLine)

parseFile :: String -> String
parseFile fileText = "<p>"++h++"</p>\n<ul>\n"++(parseFileBody t)++"</ul>" where (h:t) = init $ splitOn "\n" fileText

makeHtml :: FilePath -> IO ()
makeHtml path = do
  fileText <- readFile $ "pages/"++path
  temp1 <- readFile "src/template1.html"
  temp2 <- readFile "src/template2.html"
  writeFile ("generated/"++path++".html") (temp1++"<h1>"++path++"</h1>"++(parseFile fileText)++temp2)

doForEachFile :: (FilePath -> IO ()) -> FilePath -> IO ()
doForEachFile func folder = do
  contents <- listDirectory folder
  foldl (>>) (return ()) $ map func contents

main = do
  createDirectory "generated"
  doForEachFile makeHtml "pages"
  copyFile "src/main.css" "generated/main.css"
