

-------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <  y    = x : merge    xs (y:ys)
    | x == y    = x : merge    xs    ys
    | otherwise = y : merge (x:xs)   ys

minus :: Ord a => [a] -> [a] -> [a]
minus xs [] = xs
minus [] ys = []
minus (x:xs) (y:ys)
    | x <  y    = x : minus    xs (y:ys)
    | x == y    =     minus    xs    ys
    | otherwise =     minus (x:xs)   ys

flipminus :: Ord a => [a] -> [a] -> [a]
flipminus xs [] = xs
flipminus [] ys = []
flipminus xs ys = flip minus xs ys

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = msort (take n xs) `merge` msort (drop n xs)
  where
    n = length xs `div` 2

type Node = Int
type Map  = [(Node,Node)]

bothWays :: Node -> Map -> [Node]
bothWays _ [] = []
bothWays n ((x,y):xs)
    | n == x    = y : bothWays n xs
    | n == y    = x : bothWays n xs
    | otherwise =     bothWays n xs

type Location  = String
type Character = String

type Party = [Character]


------------------------- PART 1: Events

data Game = Won | Game Node Party [Party]
  deriving (Eq,Show)

-- What do they mean by your file needs to type check? Include a?
start :: Game
start =  Game 6 [] characters

end :: Game
end = Won

type Event = Game -> Game

applyAt :: Int -> (a -> a) -> [a] -> [a]
applyAt i f xs
    | i >= length xs = error "Index out of bounds"
    | otherwise      = ys ++ [f x] ++ zs
        where (ys, x:zs) = splitAt i xs

updateAt :: Node -> Party -> Party -> Event
updateAt _ _ _ Won = Won
updateAt m xs ys (Game n p ps) = Game n p (applyAt m (merge ys) (applyAt m (flipminus xs) ps))

{-
updateAt :: Node -> Party -> Party -> Event
updateAt m xs ys Won = Won
updateAt m xs ys (Game n p ps) = (Game n p merged)
    where
        (zs, w:ws) = splitAt m ps
        minused = zs ++ [minus w xs] ++ ws
        merged = applyAt m (merge ys) minused

update :: Party -> Party -> Party -> Event
update xs ys zs Won           = Won
update xs ys zs (Game n p ps) = (Game n q merged)
    where
        q = merge (minus p xs) ys
        (vs, w:ws) = splitAt n ps
        minused = vs ++ [minus w xs] ++ ws
        merged = applyAt n (merge zs) minused
-}

update :: Party -> Party -> Party -> Event
update _  _  _   Won          = Won
update xs ys zs (Game n p ps) = Game n q qs
    where
        q  = merge (minus p xs) ys
        qs = applyAt n (merge zs) (applyAt n (flipminus xs) ps)


------------------------- PART 2: Dialogues

data Dialogue = End     String
              | Choice  String  [( String , Dialogue )]
              | Action  String  Event

exitWords = ["X","x","Q","q","Exit","exit","Quit","quit"]

enumerate :: Int -> [String] -> String
enumerate n xs = unlines [ "  " ++ show i ++ ". " ++ x | (i,x) <- zip [n..] xs ]

dialogue :: Game -> Dialogue -> IO Game
dialogue game (End s) = do
    putStrLn s
    return game
dialogue game (Action s e) = do
    putStrLn s
    return (e game)
dialogue game (Choice s xs) = do
    putStrLn s
    let ys = [ x | (x,_) <- xs ]
    putStrLn (enumerate 1 ys)
    input <- getLine
    let intInput = (read input :: Int)
    if input `elem` exitWords
      then do
          putStrLn ""
          return game
      else if intInput `elem` [1..length(ys)]
           then do
               let zs = [ x | (_,x) <- xs ]
               dialogue game (zs !! (intInput - 1))
           else dialogue game (Choice s xs)


findDialogue :: Party -> Dialogue
findDialogue p = search p dialogues
    where
        search [] _ = End "There is nothing we can do."
        search _ [] = End "There is nothing we can do."
        search p ((q,d):ds)
            | p == q    = d
            | otherwise = search p ds


------------------------- PART 3: The game loop

loop :: Game -> IO ()
loop  Won          = do
    return ()
loop (Game n p ps) = do
    putStr "You are in "
    putStrLn (locations !! n)
    let ns = bothWays n theMap
    let xs = [ locations !! x | x <- ns ]
    putStr "You can travel to\n"
    putStr (enumerate 1 xs)
    putStr "With you are\n"
    putStr (enumerate (length xs + 1) p)
    putStr "You can see\n"
    putStr (enumerate (length xs + length p + 1) (characters !! n `minus` p))
    putStr "What will you do?\n"
    str <- getLine
    if str `elem` exitWords
    then putStr ""
    else do
        let is = (map read $ (words str) :: [Int])
        if length is == 1 && (is !! 0) <= length xs
        then do
            let m = ns !! ((is !! 0) - 1)
            loop (Game m p ps)
        else do
            let startlen1  = length xs + 1
            let startlen2  = length xs + length p + 1
            let chars = characters !! n `minus` p
            let js = [ j | (x,j) <- (zip [startlen1..] p) ++ (zip [startlen2..] chars), x `elem` is ]
            let d = findDialogue js
            g <- dialogue (Game n p ps) d
            loop g

game :: IO ()
game = loop start

------------------------- PART 4: Solving the game

{-
data Dialogue = End     String
              | Choice  String  [( String , Dialogue )]
              | Action  String  Event
-}

talk' :: Dialogue -> [(Event,[Int])]
talk' (End _)       = []
talk' (Action _ e)  = [(e, [])]
talk' (Choice _ xs) = concat [ [(e, (choices x y)) | (e, y) <- talk' d] | ((_, d), x) <- (zip xs [1..]) ]
    where choices w z = w:z

talk :: Dialogue -> [(Event,String)]
--talk (End _)       = []
--talk (Action _ e)  = [(e, "")]
talk d = [ (e, "In the dialogue, choose " ++ s) | (e, s) <- t d ]
    where t (End _) = []
          t (Action _ e) = [(e, "")]
          t (Choice _ xs) = concat [ [(e, (choices x y)) | (e, y) <- t d] | ((_, d), x) <- (zip xs [1..]) ]
          choices w z = show w ++ " " ++ z


event :: String -> Event
event s _ = Game 0 ["Event: " ++ s] []

testDialogue :: Dialogue
testDialogue = Choice "Morpheus opens his palms"
 [("Take the blue pill", Action "" (event "You wake up in bed"))
 ,("Take the red pill",  Action "" (event "You are a battery"))]

testTalk' :: [(Game,[Int])]
testTalk' = [ (e Won,xs) | (e,xs) <- talk' testDialogue]

testTalk :: [(Game,String)]
testTalk = [ (e Won,str) | (e,str) <- talk testDialogue]


-------------------------

extend :: Map -> (Node,[Int]) -> [(Node,[Int])]
extend [] _ = []
extend m (n, xs) = [ (x, i:xs) | (x, i) <- zip (bothWays n m) [1..] ]

travel' :: Map -> [(Node,[Int])] -> [(Node,[Int])] -> [(Node,[Int])]
travel' m vs [] = vs
travel' m vs (r:rs)
    | (fst r) `elem` vnodes = travel' m vs rs
    | otherwise             = travel' m (vs ++ [r]) (rs ++ (try r))
        where vnodes = [ n | (n,_) <- vs ]
              try r = [ x | x <- (extend m r), (fst x) `notElem` vnodes ]

travel :: Map -> Game -> [(Game,String)]
travel m (Game n p ps) = [ ((Game w p ps), ("Travel to " ++ (locations !! w) ++ ": " ++ print (reverse z))) | (w,z) <- travel' m [] [(n,[])] ]
    where
      print []     = ""
      print (x:xs) = show x ++ " " ++ print xs

-------------------------

act :: Game -> [(Game,String)]
act = undefined

-- suitable :: Game -> Event -> Bool
suitable = undefined

solve :: IO ()
solve = undefined
  where
    --solveLoop :: Game -> String
    --solveLoop :: (Game,String) -> String



------------------------- Game data

characters :: [Party]
characters =
  [ ["Duke"]
  , ["Portal Gun"]
  , ["Priest"]
  , ["Lee"]
  , ["Chell","Cortana","Mario","Master Chief"]
  , ["Team Rocket"]
  , ["Peach","Rochelle"]
  ]

locations :: [Location]
locations =
  [ "You are not supposed to be here" -- 0
  , "Aperture Science" -- 1
  , "Church of Halo"   -- 2
  , "Macon"            -- 3
  , "Nintendo Land"    -- 4
  , "Pallet Town"      -- 5
  , "Princess Castle"  -- 6
  ]

theMap :: Map
theMap = [(1,5), (2,4), (2,6), (3,5), (4,5), (4,6)]


dialogues :: [(Party,Dialogue)]
dialogues =
 [ (["Mario"] , Choice "I need to save the Princess."
     [("Sure." ,          Action "Let's go." (update ["Mario"] ["Mario"] []))
     ,("Not right now." , Action "Ok."       (update ["Mario"] [] ["Mario"]))
     ])
 , (["Mario","Peach"] , Choice "Save me, Mario!"
    [("Sure." , Action "Thank you for bringing me my hero. Now I can conveniently leave this hat behind." (update ["Mario","Peach"] [] ["Baseball Cap"]))
    ,("Not right now." , End "Mario, pls.")])
 , (["Peach"] , End "That's *Princess* Peach to you, please. And where's my Mario?")
 , (["Master Chief"] , Choice "I want to marry Cortana. Can you escort us to the Church of Halo?"
     [("Sure." ,          Action "Let's go." (update ["Master Chief"] ["Master Chief"] []))
     ,("Not right now." , Action "Ok."       (update ["Master Chief"] [] ["Master Chief"]))
     ])
 , (["Cortana"] , Choice "I must go with Master Chief."
     [("Sure." ,          Action "Let's go." (update ["Cortana"] ["Cortana"] []))
     ,("Not right now." , Action "Ok."       (update ["Cortana"] [] ["Cortana"]))
     ])
 , (["Master Chief","Priest"] , End "I can't marry you without your bride-to-be.")
 , (["Cortana","Priest"] , End "I can't marry you without your husband-to-be.")
 , (["Priest"] , Choice "Welcome, my child. Have you accepted Master Chief as your savior?"
     [("Hail Master Chief (Blessed Be His Name)" , End "")])
 , (["Cortana","Master Chief","Priest"] , Choice "Do you, Master Chief, accept Cortana to be your beloved bride?"
      [("I don't", End "The Wedding is cancelled"),
       ("I do", Choice "And do you, Cortana, take Master Chief to be your beloved Husband?"
        [("I don't", End "The Wedding is cancelled"),
         ("I do", Action "What a beautiful wedding said the bridesmaid to the waiter. But what a shame, that there's some child lurking nearby." (update ["Cortana","Master Chief","Priest"] [] ["Clementine (hiding)"]) )
   ])])
 , (["Baseball Cap"] , Choice "It's a bit grubby, shall I take it?"
     [("Sure." ,          Action "Let's go." (update ["Baseball Cap"] ["Baseball Cap"] []))
     ,("Not right now." , Action "Ok."       (update ["Baseball Cap"] [] ["Baseball Cap"]))
     ])
 , (["Clementine (hiding)"] , End "I'm scared. Where are my parents?")
 , (["Baseball Cap", "Clementine (hiding)"] , Choice "Give the girl the hat?"
    [("Sure." , Action "I feel safe." (update ["Baseball Cap","Clementine (hiding)"] ["Clementine"] []))
    ,("Not right now." , End "")
    ])
 , (["Duke"] , End "Time to k*** a** and chew bubble gum. And I'm all outta gum.")
 , (["Clementine"] , Choice "Will you help me find my parents?"
      [("What do they look like?", Choice "My father's name is Lee"
        [("I asked what do they look like!", End "Sorry")
        ,("I know him, Let's go!", Action "Yay!" (update ["Clementine"] ["Clementine"] []))
      ])
      ,("Do you know your address?", Choice "I can't remember, I think it rhymes with Bacon"
        [("How do you not know your own address?", End "Sorry!"),
         ("Are you thinking of Macon?", Choice "Yes! That's it! Do you know where it is?"
          [("Sure, I can take you there", Action "Yay!" (update ["Clementine"] ["Clementine"] []))
          ,("I don't know how to get there", End "Okay then")
          ])
        ,("Are you hungry?", Choice "Yes! Do you have any chocolate?"
          [("I don't", End "Okay")
          ,("I can go find some", Action "Thanks, I'll stay here in case my parents come back" (update ["Clementine"] [] ["Clementine"]))
   ])])])
 , (["Clementine","Lee"] , Choice "GIVE ME BACK CLEMENTINE!"
     [("Sure...", Action "" (update ["Clementine","Lee"] ["Zombie Lee"] []))
     ])
 , (["Lee"] , End "Clem? Clem, where are you?!")
 , (["Zombie Lee"] , Choice "Uuurrurrhgghghhghgg."
     [("This way." ,  Action "Urg" (update ["Zombie Lee"] ["Zombie Lee"] []))
     ,("Not today." , Action "Hhuuuurgh" (update ["Zombie Lee"] [] ["Zombie Lee"]))
     ])
 , (["Rochelle"] , End "Girl, you should pray there aren't no Zombies around.")
 , (["Rochelle", "Zombie Lee"] , Action "What?! A zombie? You've left me for dead!" (update ["Rochelle","Zombie Lee"] [] ["Pikachu"]))
 , (["Chell"] , Choice "I've just got a volunteering position at Aperture Science. Can you help me find it? I'm not good with directions."
     [("This way." ,  Action "" (update ["Chell"] ["Chell"] []))
     ,("Not today." , Action "" (update ["Chell"] [] ["Chell"]))
    ])
 , (["Chell","Portal Gun"] , Action "This is your fault. It didn't have to be like this. I'm not kidding, now! Turn back, or I will kill you! I'm going to kill you, and all the cake is gone! You don't even care, do you? This is your last chance! ." (update ["Chell","Portal Gun"] [] [] . updateAt 4 ["Team Rocket"] ["Ash"]))
 , (["Team Rocket"] , End "Oh, prepare for trouble, that's what they should do. And make it double, we're grabbing Pikachu.")
 , (["Pikachu"] , Choice "Pika-Pika"
     [("*throw pokeball*"  , Action "" (update ["Pikachu"] ["Pikachu"] []))
     ,("Nope." ,             Action "" (update ["Pikachu"] [] ["Pikachu"]))
     ])
 , (["Ash", "Pikachu"] , Action "You win." (\_ -> Won))
 , (["Pikachu","Team Rocket"] , End "Hey, look at this! Get a load! Let's grab- ALL GLORY TO THE HYPNOTOAD")
 , (["Portal Gun"] , End "I am an inanimate object. What did you expect?")
 ]
