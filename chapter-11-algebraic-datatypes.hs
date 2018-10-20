import           Data.Char
import           Data.List (findIndex, group, intercalate, sort, sortBy)

f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t

doubleUp :: [a] -> [a]
doubleUp []       = []
doubleUp xs@(x:_) = x : xs

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf allX@(x:xs) allY@(y:ys) = if elem x allY then isSubseqOf xs ys else False

capitalizeWords :: String -> [(String, String)]
capitalizeWords sent@(x:xs) =
  (map (\word@(y:ys) -> (word, (toUpper y): ys ))) . words $ sent

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord xs =
    let
        nonLetterHead = (takeWhile (not . isAlpha)) xs
        theRest = (dropWhile (not . isAlpha)) xs
    in
        nonLetterHead ++ ((toUpper $ head theRest) : tail theRest)

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = if p x then x : takeWhileInclusive p xs
                                     else x : []

getSentence = takeWhileInclusive (\x -> x /= '.')

getSentences :: String -> [String]
getSentences ""     = []
getSentences str =
    let sent = getSentence str
        lengthSent = length sent
    in sent : getSentences (drop lengthSent str)

capitalizeParagraph :: String -> String
capitalizeParagraph xs = (concat . (map capitalizeWord) . getSentences) xs


data Tap
    = Ones    Int
    | Twos    Int
    | Threes  Int
    | Fours   Int
    | Fives   Int
    | Sixes   Int
    | Sevens  Int
    | Eights  Int
    | Nines   Int
    | Zeroes  Int
    | Stars   Int
    | Pounds  Int
    deriving (Eq, Show)

charOnes = "1"
charTwos = "abc2"
charThrees = "def3"
charFours = "ghi4"
charFives = "jkl5"
charSixes = "mno6"
charSevens = "pqrs7"
charEights = "tuv8"
charNines = "wxyz9"
charZeroes = " 0"
charPounds = ".,#"
charStarsSymbolic = "><"

accessChar :: String -> Int -> Char
accessChar x y = x !! (mod (y - 1) (length x))

decodeTap :: Tap -> Char
decodeTap (Ones x)   = accessChar charOnes x
decodeTap (Twos x)   = accessChar charTwos x
decodeTap (Threes x) = accessChar charThrees x
decodeTap (Fours x)  = accessChar charFours x
decodeTap (Fives x)  = accessChar charFives x
decodeTap (Sixes x)  = accessChar charSixes x
decodeTap (Sevens x) = accessChar charSevens x
decodeTap (Eights x) = accessChar charEights x
decodeTap (Nines x)  = accessChar charNines x
decodeTap (Zeroes x) = accessChar charZeroes x
decodeTap (Stars x)  = accessChar charStarsSymbolic x
decodeTap (Pounds x) = accessChar charPounds x

decodeTaps :: [Tap] -> String
decodeTaps = (foldr formatRawDecode "") . (map decodeTap)

formatRawDecode :: Char -> String -> String
formatRawDecode '<' x      = x
formatRawDecode '>' ""     = ""
formatRawDecode '>' (x:xs) = (toUpper x):xs
formatRawDecode char str   = char:str

exampleEncodedTaps =
    [ (Stars 2)
    , (Fours 3)
    , (Zeroes 1)
    , (Fives 3)
    , (Sixes 3)
    , (Eights 3)
    , (Threes 2)
    , (Zeroes 1)
    , (Nines 3)
    , (Sixes 3)
    , (Eights 2)
    , (Pounds 1)
    ]

message =
    "I love you."

messageToTaps :: String -> [Tap]
messageToTaps message=
    foldr charToTaps [] message

iPlusOne x xs = (length (takeWhile ((/=) x) xs )) + 1

charToTaps :: Char -> [Tap] -> [Tap]
charToTaps char taps
    | (isUpper char) = (Stars 1) : (charToTaps (toLower char) taps)
    | elem char charOnes = (Ones (iPlusOne char charOnes)) : taps
    | elem char charTwos = (Twos (iPlusOne char charTwos)) : taps
    | elem char charThrees = (Threes (iPlusOne char charThrees)) : taps
    | elem char charFours = (Fours (iPlusOne char charFours)) : taps
    | elem char charFives = (Fives (iPlusOne char charFives)) : taps
    | elem char charSixes = (Sixes (iPlusOne char charSixes)) : taps
    | elem char charSevens = (Sevens (iPlusOne char charSevens)) : taps
    | elem char charEights = (Eights (iPlusOne char charEights)) : taps
    | elem char charNines = (Nines (iPlusOne char charNines)) : taps
    | elem char charZeroes = (Zeroes (iPlusOne char charZeroes)) : taps
    | elem char charPounds = (Pounds (iPlusOne char charPounds)) : taps
    | otherwise = (Stars 2) : taps -- Stars 2 is a noop

convo :: [String]
convo =
    [ "Wanna play 20 questions"
    , "Ya"
    , "U 1st hahah"
    , "Lol ok. Have u ever tasted alcohol"
    , "Lol ya"
    , "Wow ur cool haha. Ur turn"
    , "Ok. Do u think I am pretty Lol"
    , "Lol ya"
    , "Just making sure rofl ur turn"
    ]

convoPresses = map messageToTaps convo

extractPresses :: Tap -> Int
extractPresses (Ones x)   = x
extractPresses (Twos x)   = x
extractPresses (Threes x) = x
extractPresses (Fours x)  = x
extractPresses (Fives x)  = x
extractPresses (Sixes x)  = x
extractPresses (Sevens x) = x
extractPresses (Eights x) = x
extractPresses (Nines x)  = x
extractPresses (Zeroes x) = x
extractPresses (Stars x)  = x
extractPresses (Pounds x) = x

convoTaps = map (sum . map extractPresses) convoPresses

mostPopularLetter = map
    ( head
    . head
    . (sortBy (\x y -> compare (length y) (length x)))
    . group
    . sort
    . map toLower
    . filter isAlpha
    ) convo

coolestLetter =
    ( head
    . head
    . (sortBy (\x y -> compare (length y) (length x)))
    . group
    . sort
    . map toLower
    . filter isAlpha
    . concat
    ) convo

coolestWord =
    ( head
    . head
    . sortBy (\x y -> compare (length y) (length x))
    . group
    . sort
    . words
    . map toLower
    . filter (\x -> (isAlphaNum x) || x == ' ' )
    . intercalate " "
    ) convo


data Expr
    = Lit Integer
    | Add Expr Expr

a1 = Add (Lit 9001) (Lit 1)
a2 = Add (a1) (Lit 20001)
a3 = Add (Lit 1) a2

eval :: Expr -> Integer
eval (Lit x)                           = x
eval (Add (Lit x) (Lit y))             = x + y
eval (Add (Lit x) eyz@(Add y z))       = x + (eval eyz)
eval (Add exy@(Add x y) (Lit z))       = (eval exy) + z
eval (Add ewx@(Add w x) eyz@(Add y z)) = (eval ewx) + (eval eyz)

testEval = eval a3

printExpr :: Expr -> String
printExpr (Lit x)                           = show x
printExpr (Add (Lit x) (Lit y))             = (show x) ++ " + " ++ (show y)
printExpr (Add (Lit x) eyz@(Add y z))       = (show x) ++ " + " ++ (printExpr eyz)
printExpr (Add exy@(Add x y) (Lit z))       = (printExpr exy) ++ " + " ++ (show z)
printExpr (Add ewx@(Add w x) eyz@(Add y z)) = (printExpr ewx) ++ (printExpr eyz)
