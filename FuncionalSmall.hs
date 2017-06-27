data Exp = Num Int | TRUE | FALSE | Var String | Soma Exp Exp | Sub Exp Exp
    | Mult Exp Exp | And Exp Exp | Or Exp Exp | Not Exp | IF Exp Exp Exp
    | Ap Exp Exp | Fun String Tipo Exp | Let String Tipo Exp Exp
    deriving (Eq,Show)
data Tipo = INT | BOOL | F Tipo Tipo
    deriving (Eq, Show)


runFP :: Exp -> Exp
runFP exp = if isFinal exp then exp else runFP (smallStep exp)

isFinal :: Exp -> Bool
isFinal (Num n) = True
isFinal TRUE = True
isFinal FALSE = True
isFinal (Fun s t e) = True
isFinal _ = False


smallStep :: Exp -> Exp
smallStep (Soma (Num n1) (Num n2)) = Num (n1 + n2)
smallStep (Soma (Num n1) e2 ) = Soma (Num n1) (smallStep e2)
smallStep (Soma e1 e2) = Soma (smallStep e1) e2

smallStep (Sub (Num n1) (Num n2)) = Num (n1 - n2)
smallStep (Sub (Num n1) e2) = Sub (Num n1) (smallStep e2)
smallStep (Sub e1 e2) = Sub (smallStep e1) e2

smallStep (Mult (Num n1) (Num n2)) = Num (n1 * n2)
smallStep (Mult (Num n1) e2) = Mult (Num n1) (smallStep e2)
smallStep (Mult e1 e2) = Mult (smallStep e1) e2

smallStep (And TRUE b) = b
smallStep (And FALSE b) = FALSE
smallStep (And b1 b2) = And (smallStep b1) b2

smallStep (Or TRUE b) = TRUE
smallStep (Or FALSE b) = b
smallStep (Or b1 b2) = Or (smallStep b1) b2

smallStep (Not TRUE) = FALSE
smallStep (Not FALSE) = TRUE
smallStep (Not b) = Not(smallStep b)

smallStep (IF e1 e2 e3)
    | e1 == TRUE = e2
    | e1 == FALSE = e3
    | otherwise = IF (smallStep e1) e2 e3

smallStep (Ap (Fun str tipo e1) e2) =
    if isFinal e2
        then subs str e2 e1
        else Ap (Fun str tipo e1) (smallStep e2)
smallStep (Ap e1 e2) = Ap (smallStep e1) e2


-- smallStep

subs :: String -> Exp -> Exp -> Exp
subs var val (Var str)
    | var == str = val
    | otherwise = Var str
subs var val (Num n) = Num n
subs var val (Soma exp1 exp2) = Soma (subs var val exp1) (subs var val exp2)
subs var val (Sub exp1 exp2) = Sub (subs var val exp1) (subs var val exp2)
subs var val (And exp1 exp2) = And (subs var val exp1) (subs var val exp2)
subs var val (Or exp1 exp2) = Or (subs var val exp1) (subs var val exp2)
subs var val (Not exp) = Not (subs var val exp)
subs var val (Fun str tipo exp) = subs str val exp
subs var val (Ap exp1 exp2) = Ap (subs var val exp1) (subs var val exp2)

-- subs var val ? = ?
-- {v/x}e1 = subs x v e1

progTeste :: Exp
progTeste = Soma (Soma (Num 4) (Num 5)) (Num 12)

prog1 :: Exp
prog1 = Ap (IF TRUE (Fun "x" INT (Soma (Var "x") (Num 1))) (Fun "x" INT (Soma (Var "x") (Num 2)))) (Num 2)

-- > (if True then (Fun x:Int in x + 1) (Fun x:Int in x+2) 2
-- Resp: 3


prog2 :: Exp
prog2 = Let "x" (F INT INT) (Fun "x" INT (Soma (Var "x") (Num 1))) (Ap (Var "x") (Num 10))

-- > (Let x: Int -> Int = (fun x : Int => x + 1) in x 10
-- Resp: 11


prog3 :: Exp
prog3 = Fun "f1" (F INT INT) (Fun "y" BOOL (Soma (Num 4) (Ap (Var "f1") (Num 1))))

-- > fun f1 : Int -> Int => (fun y: Bool => 4 + (f1 1))

prog4 :: Exp
prog4 = Let "x" (F INT INT) (Fun "x" INT (Soma (Var "x") (Num 1))) (Var "x")

