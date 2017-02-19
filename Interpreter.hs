module Interpreter
  (
    -- * Types
    Prog,

    -- * Functions
    evalRaw,
    evalAdt,
  ) where

--Tema 2 Paradigme de Programare
--Mustata Alexandru-Ionut
--Grupa 326CB

import Data.Char

-------------------------------------------------------------------------------
--------------------------------- The Expr ADT  -------------------------------
-------------------------------------------------------------------------------
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
          | Equal Expr Expr
          | Smaller Expr Expr
          | Symbol String
          | Value Int deriving (Show, Read)

-- [Optional] TODO Implement a parser for the Expr ADT.

-------------------------------------------------------------------------------
---------------------------------- The Prog ADT -------------------------------
-------------------------------------------------------------------------------
data Prog = Eq String Expr
          | Seq Prog Prog
          | If Expr Prog Prog
          | While Expr Prog
          | Return Expr deriving (Show, Read)

-- [Optional] TODO Implement a parser for the Prog ADT.
--

-- [Optional] TODO The *parse* function.  It receives a String - the program in
-- a "raw" format and it could return *Just* a program as an instance of the
-- *Prog* data type if no parsing errors are encountered, or Nothing if parsing
-- failed.
--
-- This is composed with evalAdt to yield the evalRaw function.
parse :: String -> Maybe Prog
parse text =
    case (createProg text) of
        Right program -> Just program
        Left eroare -> Nothing 

-------------------------------------------------------------------------------
-------------------------------- The Interpreter ------------------------------
-------------------------------------------------------------------------------

-- TODO The *evalAdt* function.  It receives the program as an instance of the
-- *Prog* data type and returns an instance of *Either String Int*; that is,
-- the result of interpreting the program.
--
-- The result of a correct program is always an Int.  This is a simplification
-- we make in order to ease the implementation.  However, we wrap this Int with
-- a *Either String* type constructor to handle errors.  The *Either* type
-- constructor is defined as:
--
-- data Either a b = Left a | Right b
--
-- and it is generally used for error handling.  That means that a value of
-- *Left a* - Left String in our case - wraps an error while a value of *Right
-- b* - Right Int in our case - wraps a correct result (notice that Right is a
-- synonym for "correct" in English).
-- 
-- For further information on Either, see the references in the statement of
-- the assignment.
--
evalAdt :: Prog -> Either String Int
evalAdt program = 
    {-
    La evaluarea unui program, prioritatea de analiza a tuplului rezultat este:
    -> daca s-a produs o eroare atunci intorc eroarea;
    -> daca nu s-a gasit un program Return pe calea executiei semnalez acest fapt;
    -> altfel totul a decurs corect deci programul are un rezultat de intors.
    -}
    case (evalProg [] program) of
        (_,_,Left eroare) -> Left eroare
        (_,False, _) -> Left "Missing return"
        (_,_,Right numar) -> Right numar

-- The *evalRaw* function is already implemented, but it relies on the *parse*
-- function which you have to implement.
--
-- Of couse, you can change this definition.  Only its name and type are
-- important.
evalRaw :: String -> Either String Int
evalRaw rawProg = case parse rawProg of
                    Just prog -> evalAdt prog
                    Nothing   -> Left "Syntax error"

{-
Noul tip definit ce va reprezenta un Map<String,Int> ce va contine asocierile
dintre numele variabilelor si valorile acestora.
-}
type Dictionary = [(String,Int)]

{-
Functie pentru a obtine valoarea unei variabile dintr-un dictionar. Daca
variabila nu exista se va intoarce o eroare.
-} 
valueOf::String->Dictionary->Either String Int
valueOf _ [] = Left "Uninitialized variable"
valueOf nume (x:xs)
    | fst(x)==nume = Right (snd(x)) 
    | otherwise = valueOf nume xs

{-
Functie pentru a determina daca o variabila exista intr-un dictionar.
-}
contains::String->Dictionary->Bool
contains _ [] = False
contains nume (p:perechi)
    | fst(p)==nume = True
    | otherwise = contains nume perechi

{-
Functie care va sterge perechea (nume, valoare) asociata unei variabile
dintr-un dictionar, daca variabila exista in dictionar, altfel dictionarul
ramane nemodificat.   
-}
deleteVariabila::String->Dictionary->Dictionary
deleteVariabila _ [] = []
deleteVariabila nume (p:perechi)
    | fst(p)==nume = perechi
    | otherwise = p:(deleteVariabila nume perechi)

{-
Functie pentru adaugarea unei perechi (nume, valoare) corespunzatoare
unei variabile. Daca variabila exista in dictionar atunci se sterge
vechea pereche si se inlocuieste cu cea noua, altfel se adauga pur si 
simplu perechea in dictionar.
-}
addPair::String->Int->Dictionary->Dictionary
addPair nume numar dictionar = if (contains nume dictionar)
                                then (nume, numar):(deleteVariabila nume dictionar)
                                else (nume, numar):dictionar

{-
Functia evalueaza o expresie primita ca parametru in cadrul de determinat
de dictionarul primit ca parametru. Daca o variabila din expresie nu se
regaseste in dictionar atunci se va intoarce eroarea "Uninitialized variable".
-}
evalExpr::Dictionary->Expr->Either String Int
evalExpr dictionar (Add expresie1 expresie2) =
    case (evalExpr dictionar expresie1, evalExpr dictionar expresie2) of
        (Right nr1, Right nr2) -> Right(nr1+nr2)
        _ -> Left "Uninitialized variable"

evalExpr dictionar (Sub expresie1 expresie2) =
    case (evalExpr dictionar expresie1, evalExpr dictionar expresie2) of
        (Right nr1, Right nr2) -> Right(nr1-nr2)
        _ -> Left "Uninitialized variable"

evalExpr dictionar (Mult expresie1 expresie2) =
    case (evalExpr dictionar expresie1, evalExpr dictionar expresie2) of
        (Right nr1, Right nr2) -> Right(nr1*nr2)
        _ -> Left "Uninitialized variable"

evalExpr dictionar (Equal expresie1 expresie2) =
    case (evalExpr dictionar expresie1, evalExpr dictionar expresie2) of
        (Right nr1, Right nr2)
            | nr1==nr2 -> Right(1)
            | otherwise -> Right(0)
        _ -> Left "Uninitialized variable"

evalExpr dictionar (Smaller expresie1 expresie2) =
    case (evalExpr dictionar expresie1, evalExpr dictionar expresie2) of
        (Right nr1, Right nr2)
            | nr1 < nr2 -> Right(1)
            | otherwise -> Right(0)
        _ -> Left "Uninitialized variable"

evalExpr dictionar (Symbol nume) = 
    case (valueOf nume dictionar) of
        (Right valoare) -> Right valoare
        (Left eroare) -> Left eroare

evalExpr dictionar (Value numar) = Right(numar)

{-
Functia primeste un program ce trebuie evaluat in cadrul contextului primit
si intoarce un tuplu ce contine dictionarul modificat in urma rularii programului,
raspunsul True/False daca s-a intalnit un program Return pe cale executiei si
rezultatul sau eroarea care a aparut in cadrul executiei.
-}
evalProg::Dictionary->Prog->(Dictionary, Bool, Either String Int)
evalProg dictionar (Eq nume expresie) =
    case (evalExpr dictionar expresie) of
        (Right numar) -> ((addPair nume numar dictionar), False, Right numar)
        (Left eroare) -> ([], False, Left eroare)

evalProg dictionar (Seq program1 program2) =
    case (evalProg dictionar program1) of
       (_, _, Left eroare) -> ([], False, Left eroare)
       (dictionarNou, False, Right numar) ->
            case (evalProg dictionarNou program2) of
                (_, _, Left eroare) -> ([], False, Left eroare)
                (dictionarNouNou, valoareReturn, Right numar) -> (dictionarNouNou, valoareReturn, Right numar)     

evalProg dictionar (Return expresie) =
    case (evalExpr dictionar expresie) of
        (Right numar) -> (dictionar, True, Right numar)
        (Left eroare) -> (dictionar, True, Left eroare)

evalProg dictionar (If expresie program1 program2) =
    case (evalExpr dictionar expresie) of
        (Right numar)
            | numar/=0 ->
                case (evalProg dictionar program1) of
                    (_, _, Left eroare) -> ([], False, Left eroare)
                    (_, True, Right numar) -> ([], True, Right numar)
                    (dictionarNou, valoareReturn, Right numar) -> (dictionarNou, valoareReturn, Right numar)
            | otherwise ->
                case (evalProg dictionar program2) of
                    (_, _, Left eroare) -> ([], False, Left eroare)
                    (_, True, Right numar) -> ([], True, Right numar)
                    (dictionarNou, valoareReturn, Right numar) -> (dictionarNou, valoareReturn, Right numar)
        (Left eroare) -> ([], False, Left eroare)

evalProg dictionar (While expresie program) =
    case (evalExpr dictionar expresie) of
        (Right numar)
            | numar/=0 ->
                case (evalProg dictionar program) of
                    (_, _, Left eroare) -> ([], False, Left eroare)
                    (_, True, Right numar) -> ([], True, Right numar)
                    (dictionarNou, valoareReturn, Right numar) ->
                        case (evalProg dictionarNou (While expresie program)) of
                            (dictionarNouNou, valoareReturn, Right numar) -> (dictionarNouNou, valoareReturn, Right numar)
            | otherwise -> (dictionar, False, Right numar)
        (Left eroare) -> ([], False, Left eroare)

{-
Functia primeste ca String o expresie si o returneaza in forma postfixata
reprezentata printr-o lista ce contine numere, variabile si operatori sau
lista cu String-ul "Eroare" daca aceasta a continut cel putin o eroare
sintactica. Aceasta reprezinta un apel initial la functia toPostfix.
-}
createPostfix::String->[String]
createPostfix text = 
    case (toPostfix (text) False False [] []) of
        Right lista -> reverse lista
        _ -> ["Eroare"]

{-
Functia transforma recursiv expresia primita (fluxul de caractere) (1) intr-o
expresie in forma postfixata tinand cont de asociativitatile fiecarui operator
si de ce anume s-a intalnit anterior in cadrul expresiei (numar, variabila (2) 
sau operator (3) ). 2 si 3 sunt necesare deoarece o expresie nu este sintactic
corecta daca:
    -> exista doi operatori consecutivi sau doua numere/variabile consecutive;
    -> expresia incepe sau se termina cu operator.
Coada (4) reprezinta rezultatul final si este transmis in recursivitate cu rol
de acumulator. Stiva (5) reprezinta tot un acumulator folosit de algoritmul 
Shunting-yard la anumite momente.
          1.flux 2.numVar 3.op 4.coada    5.stiva  -}
toPostfix::String->Bool->Bool->[String]->[String]->Either Int [String]
toPostfix [] numVar op coada stiva 
    | op = Left 0
    | not numVar = Left 0
    | filter (\x->x=="(") stiva /=[] = Left 0
    | otherwise = Right ((reverse stiva)++coada)
toPostfix text numVar op coada stiva
    | isSpace (head text) = toPostfix (tail text) numVar op coada stiva
    | isDigit (head text) = if numVar then Left 0
                            else
                                let 
                                    numar = takeWhile (isDigit) text
                                in
                                    toPostfix (drop (length numar) text) True False (numar:coada) stiva 
    | isAlpha (head text) = if numVar then Left 0
                            else
                                let
                                    variabila = takeWhile (\ch -> isAlpha ch || isDigit ch) text
                                in
                                    toPostfix (drop (length variabila) text) True False (variabila:coada) stiva
    | head text =='+' = if op || ((not op) && (not numVar)) then Left 0 
                        else
                            let
                                operatori = takeWhile (\op -> op == "+" || op == "-" || op == "*") stiva
                            in
                                toPostfix (tail text) False True ( (reverse operatori)++coada) ("+":( drop (length operatori) stiva))
    | head text =='-' = if op || ((not op) && (not numVar)) then Left 0
                        else
                            let
                                operatori = takeWhile (\op -> op == "+" || op == "-" || op == "*") stiva
                            in
                                toPostfix (tail text) False True ( (reverse operatori)++coada) ("-":( drop (length operatori) stiva))
    | head text =='*' = if op || ((not op) && (not numVar)) then Left 0
                        else
                            let
                                operatori = takeWhile (\op ->op == "*") stiva
                            in
                                toPostfix (tail text) False True ( (reverse operatori)++coada) ("*":( drop (length operatori) stiva))
    | head text =='(' =
            toPostfix (tail text) numVar op coada ("(":stiva)
    | head text ==')' =
        let
            operatori = takeWhile (\op -> op/="(") stiva
        in
            if operatori == stiva then Left 0 
                else toPostfix (tail text) numVar op ( (reverse operatori)++coada) (drop (1+ (length operatori)) stiva)
    | head text =='<' = if op || ((not op) && (not numVar)) then Left 0
                        else
                            let
                                operatori = takeWhile (\op -> op == "<" || op == "+" || op == "-" || op == "*") stiva
                            in
                                toPostfix (tail text) False True ( (reverse operatori)++coada) ("<":( drop (length operatori) stiva))
    | head text =='=' && (length text) == 1 = Left 0
    | head text =='=' && head (tail text) /= '=' = Left 0
    | head text =='=' && head (tail text) == '=' = if op || ((not op) && (not numVar)) then Left 0
                                                    else
                                                        let
                                                            operatori = takeWhile (\op -> op == "==" || op == "<" || op == "+" || op == "-" || op == "*") stiva
                                                        in
                                                            toPostfix (tail (tail text)) False True ( (reverse operatori)++coada) ("==":( drop (length operatori) stiva))
    | otherwise = Left 0

{-
Functia inglobeaza mecanismul de parsare a unei expresii primita ca
String si intoarce expresia in forma TDA daca parsarea s-a terminat
cu succes, altfel se returneaza "Syntax error".
-}
createExpr::String->Either String Expr
createExpr text = 
    case (createPostfix text) of
        ["Eroare"] -> Left "Syntax error"
        lista -> 
            case (transform lista []) of
                Right expresie -> Right expresie
                _ -> Left "Syntax error"

{-
Functia implementeaza algoritmul de calcul al unei expresii in forma
postfixata, dar in loc sa intoarca un numar returneaza o expresie in
forma TDA sau o "Syntax error" daca expresia nu este corecta din punct
de vedere sintactic.
-}
transform::[String]->[Expr]->Either String Expr
transform [] stiva = Right (head(stiva))
transform (e:text) stiva
    | isDigit (head e) = transform text ((Value (read e ::Int)):stiva)
    | isAlpha (head e) = transform text ((Symbol e):stiva)
    | e == "+" =
        let
            a = head (tail stiva)
            b = head stiva
        in
            transform text ( (Add a b):( tail (tail stiva) ) )
    | e == "*" =
        let
            a = head (tail stiva)
            b = head stiva
        in
            transform text ( (Mult a b):( tail (tail stiva) ) )
    | e == "-" =
        let
            a = head (tail stiva)
            b = head stiva
        in
            transform text ( (Sub a b):( tail (tail stiva) ) )
    | e == "<" =
        let
            a = head (tail stiva)
            b = head stiva
        in
            transform text ( (Smaller a b):( tail (tail stiva) ) )
    | e == "==" =
        let
            a = head (tail stiva)
            b = head stiva
        in
            transform text ( (Equal a b):( tail (tail stiva) ) )
    | otherwise = Left "Syntax error"

{-
Functia elimina spatiile albe de la inceputul unui String.
-}
eliminaSpatiiInceput::String->String
eliminaSpatiiInceput text = dropWhile (\ch-> isSpace ch) text

{-
Functia elimina spatiile albe de la sfarsitul unui String.
-}
eliminaSpatiiSfarsit::String->String
eliminaSpatiiSfarsit text = reverse (eliminaSpatiiInceput (reverse text))

{-
Functia construieste structura "(<expresie>)" dintr-un String ce incepe
cu "(" si are ca acumulator un Int pentru a determina o inchidere corecta
a parantezelor.
-}
takeParanteza::String->Int->String
takeParanteza [] _ = []
takeParanteza text nrParanteze
    | nrParanteze == 1 && head(text)==')' = ")" 
    | head(text)=='(' = (head(text)):(takeParanteza (tail text) (nrParanteze+1))
    | head(text)==')' = (head(text)):(takeParanteza (tail text) (nrParanteze-1))
    | otherwise = (head text):(takeParanteza (tail text) nrParanteze)

{-
Functia construieste structura "{<program>}" dintr-un String ce incepe
cu "{" si are ca acumulator un Int pentru a determina o inchidere corecta
a acoladelor.
-}
takeAcolada::String->Int->String
takeAcolada [] _ = []
takeAcolada text nrAcolade
    | nrAcolade == 1 && head(text)=='}' = "}" 
    | head(text)=='{' = (head(text)):(takeAcolada (tail text) (nrAcolade+1))
    | head(text)=='}' = (head(text)):(takeAcolada (tail text) (nrAcolade-1))
    | otherwise = (head text):(takeAcolada (tail text) nrAcolade)

{-
Functia returneaza pozitia fata de inceputul unui String a primului
caracter ';' .
-}
pozitiePunctSiVirgula::String->Int->Int
pozitiePunctSiVirgula [] _ = -1
pozitiePunctSiVirgula text nr = if((head text)==';') then (nr+1) else (pozitiePunctSiVirgula (tail text) (nr+1))

{-
Functia primeste un program sub forma de String si intoarce un program sub
forma TDA daca nu s-a intalnit "Syntax error". Functia determina daca apelul
curent trebuie tratat ca sarire peste un spatiu alb, sau ca o parsare a unuia
dintre programele de tip While, If, Return sau Eq. In cazul parsarii acestor
programe, daca continutul a fost parsat cu succes si nu s-a "consumat" tot
String-ul primit ca parametru inseamna ca exista o secventa intre programul
deja parsat si cel care trebuie parsat din textul ramas.
-}
createProg::String->Either String Prog
createProg text
    | (isSpace (head(text))) = createProg (tail text)
    | (length text) > 5 && (text!!0)=='w' && (text!!1)=='h' && (text!!2)=='i' && (text!!3)=='l' && (text!!4)=='e' && (isSpace (text!!5) || (text!!5)=='(') =
        let
            textInceputExpresie = eliminaSpatiiInceput (drop 5 text)
        in 
            if (head(textInceputExpresie)/='(') then Left "Syntax error"
                else
                    let
                        textExpresie = takeParanteza textInceputExpresie 0
                    in
                        case (createExpr textExpresie) of
                            Left _ -> Left "Syntax error"
                            Right conditieParsata -> let
                                                        textInceputBucla = eliminaSpatiiInceput (drop (length textExpresie) textInceputExpresie)
                                                     in
                                                        let
                                                            corpWhile = takeAcolada textInceputBucla 0
                                                            interiorWhile = drop 1 (take ((length corpWhile)-1) corpWhile)
                                                            restDeParsat = drop (length corpWhile) textInceputBucla
                                                            lungimeRest = length (eliminaSpatiiInceput restDeParsat)
                                                        in
                                                            if(lungimeRest==0) then
                                                                case (createProg interiorWhile) of
                                                                    Left _ -> Left "Syntax error"
                                                                    Right program -> Right (While conditieParsata program)
                                                            else
                                                                case (createProg interiorWhile, createProg restDeParsat) of
                                                                    (Right program1, Right program2) -> Right (Seq (While conditieParsata program1) (program2))
                                                                    _ -> Left "Syntax error"
    | (length text) > 2 && (text!!0)=='i' && (text!!1)=='f' && (isSpace (text!!2) || (text!!2)=='(') =
        let
            textInceputExpresie = eliminaSpatiiInceput (drop 2 text)
        in
            if (head(textInceputExpresie)/='(') then Left "Syntax error"
                else
                    let
                        textExpresie = takeParanteza textInceputExpresie 0
                    in
                        case (createExpr textExpresie) of
                            Left _ -> Left "Syntax error"
                            Right conditieParsata -> let
                                                        textInceputThen = eliminaSpatiiInceput (drop (length textExpresie) textInceputExpresie)
                                                     in
                                                        if( (length textInceputThen) < 4 || (textInceputThen!!0)/='t' || (textInceputThen!!1)/='h' || (textInceputThen!!2)/='e' || (textInceputThen!!3)/='n' )
                                                            then Left "Syntax error"
                                                            else
                                                                let
                                                                    textInceputRamura1 = eliminaSpatiiInceput (drop 4 textInceputThen)
                                                                    corpRamura1 = takeAcolada textInceputRamura1 0
                                                                    interiorRamura1 = drop 1 (take ((length corpRamura1)-1) corpRamura1)
                                                                    restDeParsat1 = eliminaSpatiiInceput (drop (length corpRamura1) textInceputRamura1)
                                                                    lungimeRest1 = length restDeParsat1
                                                                in
                                                                    if( (length restDeParsat1) < 4 || (restDeParsat1!!0)/='e' || (restDeParsat1!!1)/='l' || (restDeParsat1!!2)/='s' || (restDeParsat1!!3)/='e' )
                                                                        then Left "Syntax error"
                                                                        else
                                                                            let

                                                                                textInceputRamura2 = eliminaSpatiiInceput (drop 4 restDeParsat1)
                                                                                corpRamura2 = takeAcolada textInceputRamura2 0
                                                                                interiorRamura2 = drop 1 (take ((length corpRamura2)-1) corpRamura2)
                                                                                restDeParsat2 = eliminaSpatiiInceput (drop (length corpRamura2) textInceputRamura2)
                                                                                lungimeRest2 = length restDeParsat2
                                                                            in 
                                                                                if(lungimeRest2==0)then
                                                                                    case (createProg interiorRamura1, createProg interiorRamura2) of
                                                                                        (Right program1, Right program2) -> Right (If conditieParsata program1 program2)
                                                                                        _ -> Left "Syntax error"
                                                                                else
                                                                                    case (createProg interiorRamura1, createProg interiorRamura2, createProg restDeParsat2) of
                                                                                        (Right program1, Right program2, Right program3) -> Right (Seq (If conditieParsata program1 program2) program3)
                                                                                        _ -> Left "Syntax error"
    | (length text) > 7 && (text!!0)=='r' && (text!!1)=='e' && (text!!2)=='t' && (text!!3)=='u' && (text!!4)=='r' && (text!!5)=='n' && (isSpace (text!!6)) =
        let
            faraReturn = eliminaSpatiiSfarsit (eliminaSpatiiInceput (drop 6 text))
        in
            if(last(faraReturn)/=';') then Left "Syntax error"
            else
                case ( createExpr (take ((length faraReturn)-1) faraReturn) ) of
                Right expresie -> Right (Return expresie)
                _ -> Left "Syntax error"
    | isLetter (head text) = 
        let
            variabila = (head text):(takeWhile isAlpha (tail text) )
            diferenta = (length text) - (length variabila)
        in
            if(diferenta==0) then Left "Syntax error"
            else 
                let
                    faraVariabila = eliminaSpatiiInceput (drop (length variabila) text)
                    poz = pozitiePunctSiVirgula text 0
                    rest = eliminaSpatiiInceput (if (poz >= 0 ) then drop poz text else [])
                in
                    if(poz== -1) then Left "Syntax error"
                    else if(head(faraVariabila)/='=') then Left "Syntax error"
                        else
                            if((length rest)==0) then
                                let
                                    faraVariabila2 = eliminaSpatiiSfarsit (eliminaSpatiiInceput faraVariabila)
                                    expresie = tail (init faraVariabila2)
                                in
                                    case (createExpr expresie) of
                                        Right expres -> Right (Eq variabila expres)
                                        _ -> Left "Syntax error"
                            else
                                let
                                    poz2 = pozitiePunctSiVirgula faraVariabila 0
                                    expresie = init (take (poz2-1) (tail faraVariabila))
                                in
                                    case (createExpr expresie, createProg rest) of
                                        (Right expres, Right program) -> Right (Seq (Eq variabila expres) program)
                                        _ -> Left "Syntax error"
    | otherwise = Left "Syntax error"
