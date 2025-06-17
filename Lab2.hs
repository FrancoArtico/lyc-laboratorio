{-# LANGUAGE GADTs #-}

--         ∞
-- fix f = ⊔ fⁱ ⊥
--        i=0
fix :: (a -> a) -> a
fix f = f (fix f)

type Iden = String
type Σ = Iden -> Int

update :: Σ -> Iden -> Int -> Σ
update σ v n v' = if v == v' then n else σ v'

eInicial, eIniTest :: Σ
eInicial = \v -> 0
eIniTest = \v -> 0

{- Ω ≈ (Σ' + Z × Ω + Z → Ω)⊥ -}
data Ω = Normal Σ | Abort Σ | Out (Int, Ω) | In (Int -> Ω)
{- Notar:
   * Normal : Σ → Ω
   * Abort  : Σ → Ω
   * Out    : (Z, Ω) → Ω
   * In     : (Z → Ω) → Ω
-}

data Expr a where
  -- Expresiones enteras
  Const  :: Int       -> Expr Int                      -- n
  Var    :: String    -> Expr Int                      -- v
  Plus   :: Expr Int  -> Expr Int -> Expr Int          -- e + e'
  Dif   :: Expr Int -> Expr Int -> Expr Int            -- e - e'
  Times :: Expr Int -> Expr Int -> Expr Int            -- e * e'
  Div   :: Expr Int -> Expr Int -> Expr Int            -- e / e'

  -- Expresiones booleanas
  ContBool  :: Bool                   -> Expr Bool
  Eq        :: Expr Int  -> Expr Int  -> Expr Bool         -- e = e'
  Neq       :: Expr Int  -> Expr Int  -> Expr Bool       -- e /= e'
  Less      :: Expr Int  -> Expr Int  -> Expr Bool       -- e < e'
  And       :: Expr Bool -> Expr Bool -> Expr Bool       -- b && b'
  Or        :: Expr Bool -> Expr Bool -> Expr Bool       -- b || b'
  Not       :: Expr Bool              -> Expr Bool       -- ¬b

  -- Comandos
  Skip   :: Expr Ω                                     -- skip
  Local  :: Iden      -> Expr Int -> Expr Ω -> Expr Ω  -- newvar v := e in e'
  Assign :: Iden      -> Expr Int           -> Expr Ω  -- v := e
  Fail   ::                                    Expr Ω  -- fail
  Catch  :: Expr Ω    -> Expr Ω             -> Expr Ω  -- catch c with c'
  While  :: Expr Bool -> Expr Ω             -> Expr Ω  -- while b do c
  If     :: Expr Bool -> Expr Ω   -> Expr Ω -> Expr Ω  -- if b then c else c'
  Seq    :: Expr Ω    -> Expr Ω             -> Expr Ω  -- c ; c'
  Output :: Expr Int                        -> Expr Ω  -- !e
  Input  :: String                          -> Expr Ω  -- ?v

class DomSem dom where
  sem :: Expr dom -> Σ -> dom

instance DomSem Int where
  sem (Const a)    _ = a
  sem (Var v)      σ = σ v
  sem (Plus e1 e2) σ = sem e1 σ + sem e2 σ
  sem (Dif e1 e2) σ = sem e1 σ - sem e2 σ
  sem (Times e1 e2) σ = sem e1 σ * sem e2 σ
  sem (Div e1 e2) σ = if sem e2 σ /= 0 then div (sem e1 σ) (sem e2 σ) else 0

instance DomSem Bool where
  sem (Eq e1 e2) σ = sem e1 σ == sem e2 σ
  sem (Neq e1 e2) σ = sem e1 σ /= sem e2 σ
  sem (Less e1 e2) σ = sem e1 σ < sem e2 σ
  sem (And b1 b2) σ = sem b1 σ && sem b2 σ
  sem (Or b1 b2) σ = sem b1 σ || sem b2 σ
  sem (Not b) σ = not (sem b σ)

{- Función de control para Ω -}

(*.) :: (Σ -> Ω) -> Ω -> Ω
(*.) f (Normal σ)  = f σ
(*.) _ (Abort σ)   = Abort σ
(*.) f (Out (n,ω)) = Out (n, f *. ω)
(*.) f (In g)      = In ((f *.) . g)

(†.) :: (Σ -> Σ) -> Ω -> Ω
(†.) f (Normal σ)  = Normal $ f σ
(†.) f (Abort σ)   = Abort $ f σ
(†.) f (Out (n,ω)) = Out (n, f †. ω)
(†.) f (In g)      = In ((f †.) . g)

(+.) :: (Σ -> Ω) -> Ω -> Ω
(+.) _ (Normal σ)  = Normal σ
(+.) f (Abort σ)   = f σ
(+.) f (Out (n,ω)) = Out (n, f +. ω)
(+.) f (In g)      = In ((f +.) . g)

instance DomSem Ω where
  sem Skip σ = Normal σ
  sem Fail σ = Abort σ
  sem (Assign v e) σ = Normal (\v' -> if v' == v then sem e σ else σ v')
  sem (If b c c') σ = if sem b σ then sem c σ else sem c' σ
  sem (Seq c c') σ = (*.) (sem c') (sem c σ)
  sem (Local v e c) σ = (†.) (\σ' -> update σ' v (σ v)) (sem c (update σ v (sem e σ)))
  sem (Catch c c') σ = (+.) (sem c') (sem c σ)
  sem (While b c) σ = fix (\f σ' -> if sem b σ' then (*.) f (sem c σ') else Normal σ') σ
  sem (Output e) σ = Out (sem e σ, Normal σ)
  sem (Input v) σ = In (\k -> Normal (\v' -> if v' == v then k else σ v'))

class Eval dom where
  eval :: Expr dom -> Σ -> IO ()

instance Eval Int where
  eval e = print . sem e

instance Eval Bool where
  eval e = print . sem e

instance Eval Ω where
  eval e = unrollOmega . sem e
    where unrollOmega :: Ω -> IO ()
          unrollOmega (Normal σ)   = return ()
          unrollOmega (Abort σ)    = putStrLn "Abort"
          unrollOmega (Out (n, ω)) = print n >> unrollOmega ω
          unrollOmega (In f)       = getLine >>= unrollOmega . f . read

prog1 = Output (Const 8)

ejemplo1 = eval prog1 eIniTest

{- Debe devolver 8 -}

prog2 = Seq (Input "x") (Output (Plus (Const 2) (Var "x")))

ejemplo2 = eval prog2 eIniTest