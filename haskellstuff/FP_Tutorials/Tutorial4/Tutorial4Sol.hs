-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!
{-# LANGUAGE TemplateHaskell #-}
module Tutorial4Sol where

import Data.List (nub)
import Data.Char
import Test.QuickCheck
import Network.HTTP (simpleHTTP,getRequest,getResponseBody)


-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>
-- <sample data>

testURL     = "http://homepages.inf.ed.ac.uk/wadler/testpage.html"

testHTML :: String
testHTML =    "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 3.2//EN\">\n"
           ++ "<html>\n"
           ++ "<head>\n"
           ++ "<title>FP: Tutorial 4</title>\n"
           ++ "</head>\n"
           ++ "<body>\n"
           ++ "<h1>A Boring test page</h1>\n"
           ++ "<h2>for tutorial 4</h2>\n"
           ++ "<a href=\"https://course.inf.ed.ac.uk/inf1a\">Inf1A Learn</a><br>\n"
           ++ "<b>Lecturer:</b> <a href=\"mailto:wadler@inf.ed.ac.uk\">Philip Wadler</a><br>\n"
           ++ "<b>TA:</b> <a href=\"mailto:cchirita@exseed.ed.ac.uk\">Claudia-Elena Chirita</a>\n"
           ++ "</body>\n"
           ++ "</html>\n\n"

testLinks :: [Link]
testLinks =  ["https://course.inf.ed.ac.uk/inf1a\">Inf1A Learn",
              "mailto:wadler@inf.ed.ac.uk\">Philip Wadler",
              "mailto:cchirita@exseed.ed.ac.uk\">Claudia-Elena Chirita"]

testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Philip Wadler","wadler@inf.ed.ac.uk")
               , ("Claudia-Elena Chirita","cchirita@exseed.ed.ac.uk")]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)

ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr = unlines [ name ++ ": " ++ email | (name,email) <- addr ]

-- </system interaction>
-- <exercises>

-- 1.
sameString :: String -> String -> Bool
sameString str1 str2 = map toUpper str1 == map toUpper str2

-- Note that the following fails on input '\181'.
prop_con a = toLower a == toLower (toUpper a)
-- 2.
prefix :: String -> String -> Bool
prefix substr str = substr `sameString` take (length substr) str

-- prefix' substr str = map toUpper substr `isPrefixOf` map toUpper str

-- prop_prefix_pos is satisfied by a function that always returns True, for example.
prop_prefix_pos :: String -> Int -> Property
prop_prefix_pos str n = n >= 0 ==> prefix substr (map toUpper str)
  where
    substr = take n str
prop_prefix_neg :: String -> Int -> Property
prop_prefix_neg str n = 0 <= n && n < length str ==> (not $ prefix str substr)
  where
    substr = take n str

-- 3.
contains :: String -> String -> Bool
contains str substr = or [ prefix substr (drop i str) | i <- [0..length str] ]

prop_contains :: String -> Int -> Int -> Property
prop_contains str n m =
  0 <= m && 0 <= n ==> (map toUpper str) `contains` substr
  where
    substr = take n (drop m str)

-- 4.
takeUntil :: String -> String -> String
takeUntil substr [] = ""
takeUntil substr (c:cs) | prefix substr (c:cs) = ""
                        | otherwise = c : takeUntil substr cs

dropUntil :: String -> String -> String
dropUntil substr [] = ""
dropUntil substr str | prefix substr str = drop (length substr) str
                     | otherwise = dropUntil substr (tail str)

-- A List comprehension version
-- dropUntil :: String -> String -> String
-- dropUntil substr str = case [ s | s <- tails str, prefix substr s ] of
--     []  -> ""
--     s:_ -> drop (length substr) s

-- 5.
split :: String -> String -> [String]
split "" str  = error "Can't split on an empty string"
split sep str  
    | str `contains` sep = takeUntil sep str : split sep (dropUntil sep str)
    | otherwise        = [str]

reconstruct :: String -> [String] -> String
reconstruct _ []           = []
reconstruct _ [str]        = str
reconstruct sep (str:strs) = str ++ sep ++ reconstruct sep strs

-- Alternative using foldr1:
--
-- reconstruct sep = foldr1 f
--    where
--      f xs ys = xs ++ sep ++ ys
--
-- Alternative using concat:
-- 
-- reconstruct sep (str:strs) = str ++ concat (map (sep ++) strs)
-- 
-- Alternative using intersperse:
--
-- reconstruct sep strs | not (null strs) = concat (intersperse sep strs)

prop_split :: String -> String -> Property
prop_split sep str = not (null sep) ==> reconstruct sep (split sep str) `sameString` str

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML doc = map (takeUntil "</a>") (tail (split "<a href=\"" doc))

testLinksFromHTML :: Bool
testLinksFromHTML = linksFromHTML testHTML == testLinks

-- 7.
takeEmails :: [Link] -> [Link]
takeEmails links = [link | link <- links, prefix "mailto:" link]

-- Alternative solution: 
--
-- takeEmails links = filter (prefix "mailto:") links

-- 8.
link2pair :: Link -> (Name, Email)
link2pair link | prefix "mailto:" link = (name, email)
               | otherwise = error "link2pair: not a mail adress"
    where email = takeUntil "\">"  (dropUntil "mailto:" link)
          name  = dropUntil "\">" link

-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML html = nub [link2pair link | link <- takeEmails (linksFromHTML html)]

-- Alternative solution:
--
-- emailsFromHTML = nub . map link2pair . takeEmails . linksFromHTML

testEmailsFromHTML :: Bool
testEmailsFromHTML = emailsFromHTML testHTML == testAddrBook


-- ** Optional Material

-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail name addrs = [(n, e) | (n, e) <- addrs, n `contains` name]

-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML html name = findEmail name (emailsFromHTML html)

-- ** Automatically run QuickCheck properties named 'prop_*' and any other tests.
return []

main :: IO ()
main = do
  qc <- $quickCheckAll
  let showB b = if b then "Pass" else "Fail"
  putStrLn $ "QuickCheck: " ++ showB qc
  let units = and [testLinksFromHTML, testEmailsFromHTML]
  putStrLn $ "Unit tests: " ++ showB units
  html <- getURL testURL
  putStrLn $ "URL test: " ++ showB (html == testHTML)
