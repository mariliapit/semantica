import Estado

data AExp = Num Int
     |Var String
     |Som AExp AExp
     |Sub AExp AExp
     |Mul AExp AExp
  deriving(Show)

data BExp = TRUE
     | FALSE
     | Not BExp
     | And BExp BExp
     | Or  BExp BExp
     | Ig  AExp AExp
     | Let  AExp AExp
   deriving(Show)

data CExp =    While BExp CExp
     | If BExp CExp CExp
     | Seq CExp CExp
     | Atrib AExp AExp
     | DuplaAtrib AExp AExp AExp AExp
     | RepeatUntil CExp BExp
     | For AExp AExp AExp CExp
     | Skip
   deriving(Show)

aSmallStep :: (AExp,Estado) -> (AExp,Estado)
aSmallStep (Var x,s) = (Num (procuraVar s x),s)
aSmallStep (Som (Num x) (Num y), s) = (Num (x+y),s)
aSmallStep (Som (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s)
                                 in (Som (Num x) ef,s)
aSmallStep (Som e1 e2,s)  = let (ef,_) = aSmallStep (e1, s)
                            in (Som ef e2,s)

aSmallStep (Sub (Num x) (Num y), s) = (Num (x-y), s)
aSmallStep (Sub (Num x) e2, s) = let (ef,_) = aSmallStep (e2, s)
                                 in (Sub (Num x) ef,s)
aSmallStep (Sub e1 e2,s)  = let (ef,_) = aSmallStep (e1, s)
                            in (Sub ef e2,s)

aSmallStep (Mul (Num x) (Num y), s) = (Num (x*y), s)
aSmallStep (Mul (Num x) e2, s) = let (ef,_) = aSmallStep (e2, s)
                                 in (Mul (Num x) ef,s)
aSmallStep (Mul e1 e2,s)  = let (ef,_) = aSmallStep (e1, s)
                            in (Mul ef e2,s)

interpretA :: (AExp,Estado) -> (AExp,Estado)
interpretA (a, s) = if isFinalA a then (a, s) else interpretA (aSmallStep (a, s))

isFinalA :: AExp -> Bool
isFinalA (Num a) = True
isFinalA x = False

bSmallStep :: (BExp,Estado) -> (BExp,Estado)
bSmallStep (Not FALSE, s)      = (TRUE, s)
bSmallStep (Not TRUE, s)       = (FALSE, s)
bSmallStep (Not b, s) = let (bf,sf) = bSmallStep (b,s)
                        in (Not bf, sf)
bSmallStep (And TRUE b2, s)  = (b2, s)
bSmallStep (And FALSE b2, s) = (FALSE, s)
bSmallStep (And b1 b2, s)    = let (bf, sf) = bSmallStep (b1, s)
                              in (And bf b2, sf)

bSmallStep (Or TRUE b2, s)  = (TRUE, s)
bSmallStep (Or FALSE b2, s) = (b2, s)
bSmallStep (Or b1 b2, s)    = let (bf, sf) = bSmallStep (bf, s)
                             in (Or bf b2, sf)

bSmallStep (Ig (Num x) (Num y), s) = if (x == y) then (TRUE, s) else (FALSE, s)
bSmallStep (Ig (Num x) e2, s) = let (ef, sf) = aSmallStep(e2,s)
                               in (Ig (Num x) ef, sf)
bSmallStep (Ig e1 e2, s) = let(ef, sf) = aSmallStep(e1, s)
                          in (Ig ef e2, sf)

bSmallStep (Let (Num x) (Num y), s) = if (x < y) then (TRUE, s) else (FALSE, s)
bSmallStep (Let (Num x) e2, s) = let (ef,sf) = aSmallStep(e2, s)
                                in (Let (Num x) ef, sf)
bSmallStep (Let e1 e2, s) = let(ef, sf) = aSmallStep(e1, s)
                           in (Let ef e2, sf)


interpretB :: (BExp,Estado) -> (BExp,Estado)
interpretB (b,s) = if isFinalB b then (b,s) else interpretB (bSmallStep (b,s))

isFinalB :: BExp -> Bool
isFinalB TRUE = True
isFinalB FALSE = True
isFinalB x = False


cSmallStep :: (CExp,Estado) -> (CExp,Estado)
cSmallStep (If FALSE c1 c2, s) = (c2, s)
cSmallStep (If TRUE c1 c2 , s) = (c1, s)
cSmallStep (If b c1 c2, s) = let (bf, sf) = bSmallStep(b,s)
                             in (If bf c1 c2, s)

cSmallStep (Seq Skip c, s)  = (c, s)
cSmallStep (Seq c1 c2, s)  = let (cf, sf) = cSmallStep (c1, s)
                             in (Seq cf c2, sf)

cSmallStep (Atrib (Var x) (Num n),s) = let(sf) = (mudaVar s x n)
                                       in (Skip, sf)
cSmallStep (Atrib (Var x) e,s) = let(ef,sf) = aSmallStep(e,s)
                                 in (Atrib (Var x) ef,sf)

cSmallStep (While b c, s) = (If b (Seq c (While b c)) Skip, s)

cSmallStep (DuplaAtrib (Var x) (Var y) e1 e2, s) = (Seq (Atrib (Var x) e1) (Atrib (Var y) e2), s)

cSmallStep (RepeatUntil c b, s) = (Seq c (If b Skip (RepeatUntil c b)), s)

cSmallStep (For (Var x) e1 e2 c, s) = (Seq (Atrib (Var x) e1) (While (Let (Var x) e2) c), s)


interpretC :: (CExp,Estado) -> (CExp,Estado)
interpretC (c,s) = if isFinalC c then (c,s) else interpretC (cSmallStep (c,s))

isFinalC :: CExp -> Bool
isFinalC Skip = True
isFinalC _ = False

meuEstado :: Estado
meuEstado = [("x",0), ("y",0), ("z",0)]


-- RODANDO O EXEMPLO:
-- Hugs> interpretA (exemplo, meuEstado)

exemplo :: AExp
exemplo = Som (Num 3) (Som (Var "x") (Var "y"))

-- *Main> interpretB (exemplo2,meuEstado)
-- (TRUE,[("x",3),("y",0),("z",0)])

exemplo2 :: BExp
exemplo2 = And (And TRUE (Not FALSE)) (And (Not (Not TRUE)) TRUE)

-- *Main> interpretC (exemplo3,meuEstado)

exemplo3 :: CExp
exemplo3 = Seq (Atrib (Var "y") (Num 5)) (If (Ig (Num 4) (Num 5))  (Atrib (Var "x") (Num 5)) (Skip))

exemplo4:: CExp
exemplo4 = Seq (Atrib (Var "x") (Num 4)) (Atrib (Var "y") (Num 3))

exemplo5:: CExp
exemplo5 = DuplaAtrib (Var "x") (Var "y") (Num 4) (Num 5)

exemplo6 :: CExp
exemplo6 = RepeatUntil (Atrib (Var "x") (Som (Var "x") (Num 2))) (Ig (Var "x") (Num 10))

exemplo7 :: CExp
exemplo7 = (While (Not (Ig (Var "x") (Num 5))) (Atrib (Var "x") (Som (Var "x") (Num 1))))

exemplo8 :: CExp
exemplo8 = For (Var "x") (Num 2) (Num 10) (Seq (Atrib (Var "y") (Som (Var "y") (Num 1))) (Atrib (Var "x") (Som (Var "x") (Num 1))))
