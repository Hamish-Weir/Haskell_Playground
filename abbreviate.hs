-- Description:
-- The word i18n is a common abbreviation of internationalization in the developer community, used instead of typing the whole word and trying to spell it correctly. Similarly, a11y is an abbreviation of accessibility.

-- Write a function that takes a string and turns any and all "words" (see below) within that string of length 4 or greater into an abbreviation, following these rules:

-- A "word" is a sequence of alphabetical characters. By this definition, any other character like a space or hyphen (eg. "elephant-ride") will split up a series of letters into two words (eg. "elephant" and "ride").
-- The abbreviated version of the word should have the first letter, then the number of removed characters, then the last letter (eg. "elephant ride" => "e6t r2e").

isAlpha :: Char -> Bool
isAlpha c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

-- Phrase -> (Token we are currently making, Tokens we have found)
tokenise::[Char] -> ([Char],[[Char]])
tokenise [] = ([],[])
tokenise (x:xs) = 
    let (currentToken,foundTokens) = tokenise xs
    in if isAlpha x then 
        (x:currentToken,foundTokens)
    else if currentToken == [] then
        ([], [x] : foundTokens)
    else
        ([], [x] : currentToken : foundTokens)


abbreviateToken::[Char] -> [Char]
abbreviateToken [] = []
abbreviateToken x
    | length x < 4 = x
    | otherwise = [head x] ++ show (length x - 2) ++ [last x]
    -- let len = length x
    -- in if len >= 4 then [head x] ++ show (len - 2) ++ [last x]
    -- else (x)

abbreviateTokens::[[Char]]->[[Char]]
abbreviateTokens [] = []
abbreviateTokens (x:xs) = abbreviateToken x : abbreviateTokens xs

abbreviate::[Char] -> [Char]
abbreviate [] = []
abbreviate phrase = 
    let (maybeToken,tokens) = tokenise phrase
    in concat (abbreviateTokens (maybeToken : tokens))
    -- in abbreviateTokens (maybeToken : tokens)

main = do
    print $ abbreviate "a"
    print $ abbreviate "?"
    print $ abbreviate "a a"
    print $ abbreviate "? ?"
    print $ abbreviate "internationalization"
    print $ abbreviate "accessibility"
    print $ abbreviate "elephant-ride"
    print $ abbreviate "elephant-rides are really fun!"
    print $ abbreviate ""
    print $ abbreviate "a" == "a"
    print $ abbreviate "?" == "?"
    print $ abbreviate "a a" == "a a"
    print $ abbreviate "? ?" == "? ?"
    print $ abbreviate "internationalization" == "i18n"
    print $ abbreviate "accessibility" == "a11y"
    print $ abbreviate "elephant-rides" == "e6t-r3s"
    print $ abbreviate "elephant-rides are really fun!" == "e6t-r3s are r4y fun!"