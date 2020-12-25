-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!
module Tutorial8Sol where
  
import System.Random
  
-- Importing the keymap module

-- import KeymapList
import KeymapTreeSol


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen =  maximum . map (length . fst . snd)

formatLine :: Int -> (Barcode, Item) -> String
formatLine n (b,(i,u)) = b ++ "..." ++ i ++ replicate (n + 3 - length i) '.' ++ u

showCatalogue :: Catalogue -> String
showCatalogue c = unlines (map (formatLine (longestProductLen xs)) xs)
    where xs = toList c

-- Note that the library function 'unlines' is equivalent to:
-- 
--   foldr (++) "" . map (\xs ys -> xs ++ "\n")

-- Exercise 2

-- The values it returns (for different barcodes) are
--   Just ("The Macannihav'nmor Highland Single Malt", "75ml bottle")
--   Just ("Bagpipes of Glory", "6-CD Box")
--   Just ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")
--   Just ("Universal deep-frying pan", "pc")
--   Nothing

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

catMaybes :: [Maybe a] -> [a]
catMaybes = concat . map maybeToList

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems ks db = catMaybes [ get k db | k <- ks ]

-- Exercise 4
--
-- 'readDB' takes approximately 1 second
--
-- 'gets' for 100 keys takes 0.90 seconds,
-- an average of 0.009 seconds per item.
--
-- a database of twice the size would have twice the lookup time
--
-- 'get' sees all (other) keys (104650) before it finds the last one

{-
*Tutorial8Sol> :l Tutorial8Sol
[1 of 2] Compiling KeymapList       ( KeymapList.hs, interpreted )
[2 of 2] Compiling Tutorial8Sol     ( Tutorial8Sol.hs, interpreted )
Ok, two modules loaded.
(0.04 secs,)
*Tutorial8Sol> db <- readDB
Done
(0.89 secs, 874,752,624 bytes)
*Tutorial8Sol> ks <- samples 100 db
(0.01 secs, 132,840 bytes)
*Tutorial8Sol> ks
["0038897322349","0634931011288","0042000862066","0096786205371","0084842101507","0028995009136","0028831005162","0085232112004","0075596246347","0656613577929","0777499354456","0040000213413","0688267506079","0073410015001","0719812396811","0074644449327","3291960006103","0042187450124","5060042640270","0639277261793","5010477323170","0625309120343","0650531540690","0020626852364","0036244543324","0043000104590","3086126733920","0030083302760","0042272001002","0012495839539","0096898067133","0312547227377","0321130762799","0041269550189","0072700001076","0057000002213","0018421445508","0038341089002","5010548002003","0070177050740","0074646723524","0033300518173","0307667391480","0040000585015","0822333007064","0011170032500","0053000053613","0041153000066","0010900010238","5022496104463","0033274186934","0073754152202","0059749805964","0754807140055","0037628934691","0074574833340","0035000761095","0810373016009","0020000123721","0079400555304","0644124490684","0011111407374","0043000200360","0018964034047","0015643185014","0688267529078","0792363964791","0027434013147","0041755008224","0026259811006","0041755001195","0688267371981","0788872231523","0024131111114","0076753298049","0075234800030","0026851004127","0076100553005","0054800343034","0076800005330","0634479596322","0688267057588","0048696007019","3596710061778","0073202130042","0037000119678","0688267372780","0046500329005","0634991142526","0735854328238","0747935229013","0035032749047","0020748566583","0078742370866","0074261180108","0039882011446","0072457010222","0706301331620","0028000002671","0074200914139"]
(2.39 secs, 1,837,824 bytes)
*Tutorial8Sol> gets ks db
[("MCKENZE MIXED VEGETABLES","16 oz"),("New Choice Chicken Broth","14.5 oz"),("Q-NRTH ULTR 18DBL RL","450.00 SF"),("FRESA CRAYON SOUR CANDY","2      C"),("WAKIM HUMMUS RSTD PEPPERS","8 oz"),("Folk Art Acrylic Paint - 913 Cinnamon","2 fl oz"),("OTB CNVSLTR BAG CASE PK COOLER","EACH"),("RED BULL PL","1750 ML"),("METALLICA - S&M","cassette"),("Ala Zingara: ...in a gypsy style","CD"),("\"KISS - You Wanted The Best","CD"),("I/O MX MINI-EASTER STCKBLE","10.5 oz"),("TOPS     FANCY WHL STRAWBRY 16.00 OZ","12 ct"),("ARNOLD BRICK OVN WHL WHT","32.0Z"),("\"OXO Good Grips 16\"\" Tongs\"","One Pair Tongs"),("Journey - Greatest Hits","CD"),("Organic Rape Seed Oil","6x500ml"),("B YET WHOLE TOMATO NS","14.50 OZ"),("Naked Potato Crisps - Replaced by 461749","35x50g"),("Picture Frame","\"4\"\" X 6\"\"\""),("Frusli Cranberry & Apple cereal bar","30g"),("Terry Stephen: In Your Presence","CD"),("\"Kohler \"\"Archer\"\" 8\"\" Lavarory Faucet // R11076-4D-BN\"","\"8\"\" Lavarory Faucet\""),("Freddie Pharkas Frontier Pharmacist EGA/VGA MS-DOS/Windows","3.5 HD"),("Relax with: Wind Chimes","CD"),("FRUIT & FIBRE HARV MED","13 oz"),("Bic Mini Eraser/Rewriter (Bic Mini Effaceur/R????criveur)","14 units"),("PICK","EA"),("Amy's Rice Crust Pizza","12 oz"),("DOLOMITE XMAS TEA FOR ONE","1.00 EA"),("Colossus: The Forbin Project","VHS"),("SUDAFED PE SINUS ALLERGY 24.00 CT","72 CT"),("SFWY NON ASPIRIN INFNT DROP","1.00 FO"),("*H-CREEPY PEEPERS MIX 30.00 OZ","12 ct"),("*MCTZ MACAROON SF CHC 10.00 OZ","12 ct"),("Heinz Junior Alphabet Beef","7.5oz / 213ml"),("Rogers Trans View","5 Sheets"),("COLGATE TOTAL FLOSS TART CNTRL","20 YD"),("Badger Golden Glory Ale (4.5%)","500ml"),("TWING JASMINE TEA","20 ct"),("Ozzy Osbourne - Blizzard of Ozz","CD"),("SOCIETY THRIFT CAN","4.65 OZ"),("TUMS Extra Strength EX Assorted Tropical Fruit","48 tablets"),("MARS MILKY WAY SPREAD M6","12.30 OZ"),("SHPR C/S SLRS GO DGTAL FLSH C 1.00 CT","10 ct"),("LL KETCHUP SQUEEZE BOTTLE","28Z"),("Borden Cheese Cheddar Sharp Shredded","8 oz"),("SAUCY SUSAN SAUCE","19 OZ"),("SHR RYNLD WRAP SLANTBACK 48.00 CT","1 ct"),("Henna Conditioning Shampo - Disc By Supplier","6x200ml"),("DOLLAR DAYS ASSORTED $1.00","EACH"),("ARALDITE ADHESIVE SYRINGE","EACH"),("Selection Glass Cleaner","950ml"),("AM CHC ICE CREAM BARS","12 ct"),("Chayanne: Exitos","DVD"),("CHRIS RANCH PESTO","8 oz"),("\"Colgate Max Fresh with Mini Breath Strips","1 oz (28.3g)"),("\"34B cream bra with black stripes","34B"),("Gg Sherry Wild Rice","10 oz"),("AXE Phoenix Deodorant Bodyspray","1oz. (28g)"),("Gel Candle","16 oz"),("*SHPR ASST. TOYS C/S 1.00 CT","96 CT"),("JELLO STRAW/BANANA GELATININ 3.00 OZ","24 ct"),("WILLIAMSON ROBIN - MUSIC FOR THE MABINOGI (CE","cassette"),("CHATEAU PRIEURE LAURENT","750 ml"),("TOPS SQUARES MIX-ORIGINAL 8.75 OZ","12 ct"),("\"Wheel Chock","8in x 5in x 4 in each"),("TWINLAB HERB TR BILBERRY EXTRT","14 CT"),("LNGRS PMRG/BB100 100%JC","64 fl oz"),("TF BUSH POLE NO CHARGE FIXTURE","70 IN"),("LNGRS ALL PMGRNTE JC         *","32.00 FO"),("GIANT XTRA WIDE NOODLES 16.00 OZ","12 ct"),("CUBAN CLASSICS- EL SON CUBANO","1 CDDA DISC"),("*(N)AST NYLON UTENSIL F/S","1 ct"),("X SHPR GOOD COOK COOKIE SHEET 1.00 CT","60 ct"),("DRIED BERRIES ASSTD","3 oz"),("8 IN 1 GUINEA PG HON CKES","1 ct"),("56-COLORSOFT MOISTURE COND","13 OZ"),("Uncle Bens Red Beans/Rice Soup","4.6 oz"),("LENDER BAGELS  ONION  6CT","17.1Z"),("BURNS BR: 3rd Degree Burns","CD"),("MOD 60CS TOPS TOMATOES","28 oz"),("QUESO FRESCO","12 oz"),("Auchan D?boucheur liquide","1L"),("Butcher Boy Prefried Hot Beef Burrito","4.5oz"),("Crest Pro-Health Night oral rinse","1L"),("GIANT CHILI SAUCE 12.00 FO","12 ct"),("GLD HLDY AR INFS PPK","9 oz"),("Who: Special Edition Ep","DVD"),("\"Foray Flourescent Highlighters","6 ct"),("Serta Galloway Comfort Choice 2 (Plush/Firm) Mattress","Twin"),("CITTERIO FRESCO PANCETTA","4 oz"),("SHLM STYLN BLACK RIBBED PNYHL","30 ct"),("Great Value Pinto Beans","15.5 oz"),("WANJASHAN RICE VINEGAR-SE","10 fl oz"),("TORNILLOS (NACHO TWISTS)","1.25Z"),("WALDEN FM R/C CREAMY FRENCH","12 oz"),("The Beloved - X","CD"),("CROSS&BLK MINT JELLY","12 Z"),("LEGGS ACTV SOCKS LW CUT WHITE*","3 ct")]
(1.90 secs, 178,243,184 bytes)
-}


-- Exercise 11
--
-- 'readDB' takes approximately 5 seconds
--
-- 'gets' for 100 keys takes 0.03 seconds,
-- an average of 0.0003 seconds per item.
--
-- 'get' sees at most as many keys as the tree is deep (40)
{-
*Tutorial8Sol> :r
[1 of 2] Compiling KeymapTreeSol    ( KeymapTreeSol.hs, interpreted )
[2 of 2] Compiling Tutorial8Sol     ( Tutorial8Sol.hs, interpreted )
Ok, two modules loaded.
*Tutorial8Sol> db <- readDB
Done
(5.06 secs, 2,028,284,728 bytes)
*Tutorial8Sol> ks <- samples 100 db
(0.00 secs, 132,840 bytes)
*Tutorial8Sol> ks
["0073056005275","0036800209756","0071641571518","0644209427741","0718154056148","0727361685027","0075596241823","0039286290614","0019014103409","0888072308473","0087396004218","0074570651320","0031568465574","0073286617743","0394441222986","0089536379958","0009800000920","0804663637722","0000002063348","0037000095200","0075278121108","0076476121990","0071518050016","0051328105601","0076397030074","0026800001733","0087701396052","0037466027302","0897924002025","0013022000064","0022802002143","0070057002012","0021466210000","0752219642723","0071794208170","0058000001510","0777496097271","0811435001018","0634479364921","5000436125402","0011150325752","0050000111251","0043000295540","0039934552002","0804868420013","0070501026403","0011225031151","0093624726425","0022110983202","0028400020381","0041322377913","0048188905243","0702727047824","0070560992299","0074649155223","0076476020484","0300623120025","0042200003160","0676695004120","0024094070428","0027067512239","0036000645057","0058947101113","0656613872222","0068900001053","0034641636083","0093863105357","0089606703713","0735379150406","0754807465905","0084248497563","0685431000128","0044700658031","0022299010508","0017801000399","0034000206834","0046675000808","0672288137893","0656613832226","0075992702423","0039800053268","0027084186994","0070662087268","0080109040305","0000054051133","0041653012941","0718604145460","0015388005592","0078216006208","0074299160721","0754807032701","0042723001162","0083960137016","0723503032087","0301693303127","0345800873110","0074644853629","0000943078104","5060059981144","0011205617351"]
(9.42 secs, 7,510,234,064 bytes)
*Tutorial8Sol> gets ks db
[("RIVAL HAND MIX","EACH"),("Tpco Cat Litter","25 Lb"),("SHPR SANFORD AMER 10CT PENCLS","60 ct"),("DH SGNTR DSSRTS STRW SWRL CA","28.82 OZ"),("MGNVSN REDI READER 2.75 5614","EACH"),("Cradle Of Filth - Vempire","CD"),("\"GRIFFITH","CD"),("ORGANIC PINEAPPLES","1 ct"),("Eukanuba Adult _Premium Performance","40 lbs"),("Return To Forever - The Anthology","CD"),("RKS Gournier Sauvignon","750 ml"),("Haagen Dazs Snack Size Bars Vanilla Milk Choc Almond","1/16"),("POOH RINGS","1 ct"),("Tork Perforated Towel Roll Natural","144.38 sq ft"),("OTB SVN SKN CRE LOTION XST BNS","20 oz"),("OTB PILLAR CAN WHITE DIP 37995","3X3 IN"),("*SHPR 1CS TIC TAC ESTR GFT BX","3.20 OZ"),("Lukenbill-Faller: Etched In Song","CD"),("KING CRAB LEGS U/20","LB"),("PAMPERS CRUISERS #5","2/62 PKG"),("FOSTER FARMS BREAST VARIETY PK","9 oz"),("Allegro Non Troppo**","Video Disc"),("HM CORNMEAL YELLOW","5 lb"),("MN CSN VEG GRILLED STEAK","10 oz"),("La Costena Adobo","8.25 oz"),("American Beauty Vermicelli Enriched","12 oz"),("IODINE DECOLORIZED","1 oz"),("* *JACK O LANTERN TRUFFLES","5 oz"),("Glucosamine Chondroitin with MSM 180 Capsules","1.05"),("Elysium Black Muscat Dessert Wine","750ml"),("ADRIATIC SARDINES IN OIL","24 oz"),("PHILLIPS SPCL CRABMEAT","16 oz"),("\"9\"\"\"\"FROSTED CRYSTL COLUMN GC-11S\"","EA"),("VICTORIA MANDARINA SODA","20 oz"),("LOBSTER TAILS","10 LB"),("Colgate - Regular Cavity Protection Fluoride Toothpaste","130 mL"),("AC/DC - The Razor's Edge (2003 Remastered Edition)","CD"),("SPIC AND SPAN SF LIQ. CLEANER","28.00 FO"),("Irradio: Doctors Work","CD"),("Tesco lemonade shandy","2 litre"),("RDY PAPER TOWEL VALU PK 8RL","8.00 RL"),("Nestle Good Start Infant Formula with DHA & ARA","25.7 oz."),("*$20 PTL RTE JEL GEL   MOD","6 oz"),("\"DSI 7\"\" VELVET PLSH MINI STCKN\"","1 ct"),("Tales Of The Kama Sutra 2: Monsoon (Spartan)","DVD"),("NTRGNA HAIR MASK TRPL MOIS DP","6 oz"),("VTIME SHMP STRAWBRY","27 Z"),("\"BRANDT","CD"),("*HUBA BUBA BSKT TREATS 2.00 OZ","144 CT"),("Rold Gold Tiny Twist Pretzels","32 oz (2 lbs.) 907.2g"),("Rich's Mozzarella Cheese Sticks","8 oz"),("\"Elmo","\"10\"\"\""),("King Of Bandit Jing Volume 1","DVD"),("BABY DLX YLW&WHT CRN 28.00 OZ","6 ct"),("DROWNING POOL - SINNER","CD"),("Wimbledon: 1979 and 1980","Video Disc"),("K-Y LONG LASTING FORMULA","3 ct"),("*(S)SHASTA ROOT BEER +CRV","3.00 LT"),("BenZona: Nocturnal Emotions","CD"),("DECECCO PENNETTE #42","1 lb"),("Clevite  Bearing  CB-663HX    (07A09CD1)","1 pc.  (made in USA)"),("Cotenelle toilet paper","2 ct"),("CHSPEAKE SPEC CRABMEAT","8 oz"),("The Griff Hamlin Band: The Griff Hamlin Band","CD"),("TUMS Extra Strength Antacid Calcium Carbonate Tablets USP","250 Tab"),("R&B OLD ENGLISH 4PC STEAK KNF 4.00 CT","12 ct"),("LinearAccess 2-Channel Digital Keypad","3llb"),("\"*X 15\"\"VALENTINE PILLOW\"","1 ct"),("A/H ULTRA RICH HAND THERAPY","2 oz"),("AC MAYONNAISE","16 fl oz"),("BRTN ISLN THNSLT SPRT GLV MEN","EACH"),("*ET TU GREEK SLD KIT (LINDSEY","12 oz"),("MOD COOK1/2 HAM WA 65803 42CS","40.00 LB"),("ARCH SOFT MOLASSES","11 OZ"),("Feit Electric Long Life Decorative Bulbs 15 Watt","2 Pieces"),("SHPR 14CS HERSH LE TROP ESC","1 oz"),("Oreo YoCrunch Yogurt","6.3 oz"),("MF Plush WM (220013867 MF-WMP)","1 pc 8x10"),("Da' F.l.o.c.k.: Soul Train","CD"),("Frank Sinatra - The Concert Sinatra","CD"),("energizer max  8 AA batteries","80c"),("WEE 3 FRIENDS DREAM","1    EA"),("NSSN CHOW MEIN THAI PEANUT","4 oz"),("MICKEY 0-9MN BOY/GIRL STS","1 ct"),("Soya Caramel Dessert in Pots","6x4x125g"),("EREWHON RICE TWICE","10oz (284g)"),("*M-ASST BISCOTTI SHIPPER","1 ct"),("kitchenaid ceramic ramekins set of 4 / flutted cinnamon red","5 0z capacity"),("TexJoy Poultry Seasoning","3oz./85g"),("OTB MIGHTY DUCKS PREPACK","36 PC"),("AMERICA'S CHOICE EGG NOG","64 fl oz"),("Wagner Miniature Automotive Lamp","10 pcs PC168"),("FINLANDIA VODKA 80P","1.0 L"),("ASPEN DG LEAD BLUE 4' 5/8","1 ct"),("NovoLog PenFill (rDNA orgin) Novo Nordisk","3 mL Cartidges"),("HINDS LOTION","7.8 OZ"),("\"WINTER","CD"),("HeatnBond Ultrahold","\"7/8\"\"x 10 Yds\""),("Organic Medley of Mushroo - Disc Slow Seller","6x360g"),("B YET TUSSIN COUGH SYRP","4 oz")]
(0.03 secs, 3,862,632 bytes)
-}

-- ** Input-output

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine (lines dbl))
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return (fst (toList db !! fst (randomR (0,size db - 1) g)))

samples :: Int -> Catalogue -> IO [Barcode]
samples n db = sequence [getSample db | i <- [1..n]]

gets :: [Barcode] -> Catalogue -> [Item]
gets ks db  =  [x | k <- ks, Just x <- [get k db]]
