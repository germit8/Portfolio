module Tutorial4 where

import Data.List (nub)
import Data.Char
import Test.QuickCheck
-- import Network.HTTP (simpleHTTP,getRequest,getResponseBody)

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
{-
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
-}
-- </system interaction>
-- <exercises>

-- 1.
sameString :: String -> String -> Bool
sameString str1 str2 = map toLower str1 == map toLower str2


-- 2.
prefix :: String -> String -> Bool
prefix substr str | length str >= length substr = and [toLower x == toLower y | (x, y) <- zip substr str]
                  | otherwise = False

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
contains str "" = True
contains "" substr = False
contains (x:xs) substr | toLower x == toLower (substr !! 0) && prefix substr (x:xs) = True
                       | otherwise = contains xs substr


prop_contains :: String -> Int -> Int -> Property
prop_contains str m n = m >= 0 && n >= 0 ==> contains str substr
 where substr = take n (drop m str)


-- 4.
takeUntil :: String -> String -> String
takeUntil substr "" = ""
takeUntil substr (x:xs) | prefix substr (x:xs)    = ""
                        | otherwise               = x : takeUntil substr xs

dropUntil :: String -> String -> String
dropUntil "" str = str
dropUntil substr "" = ""
dropUntil substr (x:xs) | prefix substr (x:xs)    = drop (length substr) (x:xs)
                        | otherwise               = dropUntil substr xs


-- 5.
split :: String -> String -> [String]
split "" str = error "The separator argument cannot be an empty string"
split substr "" = [""]
split substr str | (not $ contains str substr) || (length substr >= length str) = [str]
                 | otherwise = takeUntil substr str : (split substr $ dropUntil substr str)

reconstruct :: String -> [String] -> String
reconstruct substr liststr = take (length helper - length substr) helper
 where helper = concat [concat [x, substr] | x <- liststr]

prop_split :: String -> String -> Property
prop_split sep str = not (null sep) ==> reconstruct sep (split sep str) `sameString` str

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML html = [ takeUntil "</a>" x | x <- (split "<a href=\"" html), contains x "</a>"]

testLinksFromHTML :: Bool
testLinksFromHTML = linksFromHTML testHTML == testLinks


-- 7.
takeEmails :: [Link] -> [Link]
takeEmails links = [ x | x <- links, contains x "mailto:"]


-- 8.
link2pair :: Link -> (Name, Email)
link2pair link = (dropUntil ">" link, dropUntil "mailto:" $ takeUntil "\">" link)


-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML html = [link2pair x | x <- takeEmails $ linksFromHTML html]

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook


-- Optional Material
-- sadly didn't find time to install wsl etc., so I will do it at some point later..

-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail = undefined


-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML = undefined


ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr = unlines [ name ++ ": " ++ email | (name,email) <- addr ]
