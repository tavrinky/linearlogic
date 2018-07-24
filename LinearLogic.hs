
{-# LANGUAGE DeriveFunctor #-}

module LinearLogic (Statement,
                    Statement'(..)
                    Expr,
                    allToTheRight,
                    simpleDual,
                    initialSequence,
                    initialCut,
                    unit,
                    introduceBottom,
                    tensor,
                    par,
                    tensorUnit,
                    parUnit,
                    with,
                    plusl,
                    plusr,
                    top, 
                    whyNotIntro,
                    whyNotCut,
                    whyNotRuleOne,
                    whyNotRuleTwo
                  ) where 

type Statement a = [Expr a]

data Statement' a = [Expr a] :|- [Expr a] deriving (Functor, Eq, Show)



data Expr a = Var a 
            | Dual (Expr a)
            | (Expr a) :*: (Expr a) 
            | (Expr a) :+: (Expr a) 
            | (Expr a) :&: (Expr a) 
            | (Expr a) :|: (Expr a) 
            | Unit 
            | Void 
            | Top 
            | Bottom 
            | OfCourse (Expr a) 
            | WhyNot (Expr a) deriving (Functor, Eq, Show)

allToTheRight (lexprs :|- rexprs) = (rexprs ++ map Dual lexprs)

simpleDual (Dual (Dual a)) = a 
simpleDual (Dual (a :*: b)) = (Dual a) :|: (Dual b)
simpleDual (Dual (a :+: b)) = (Dual a) :&: (Dual b)
simpleDual (Dual (a :|: b)) = (Dual a) :*: (Dual b)
simpleDual (Dual (a :&: b)) = (Dual a) :+: (Dual b)
simpleDual (Dual Unit) = Bottom 
simpleDual (Dual Void) = Top 
simpleDual (Dual Bottom) = Unit 
simpleDual (Dual Top) = Void 
simpleDual (Dual (WhyNot a)) = OfCourse $ Dual a 
simpleDual (Dual (OfCourse a)) = WhyNot $ Dual a 
simpleDual (Dual e) = Dual e 

initialSequence a = [Var a, Dual $ Var a]

initialCut as bs a = newas ++ newbs where 
      newas = as // a 
      newbs = bs // (simpleDual $ Dual a)

unit = [Unit]
introduceBottom = (Bottom:)

tensor (a:as) (b:bs) = (a :*: b):(as++bs)

par (a:b:bs) = a :|: b:bs


tensorUnit = Unit : []
parUnit (a:as) = Unit:a:as

with (a:as) (b:bs) = if as == bs then a :&: b : as ++ bs else error "Inequal contexts in With"

plusl (a:as) b = a :+: b : as 
plusr (b:bs) a = a :+: b : bs 

top gamma = Top : gamma

whyNotIntro gamma a = (WhyNot a) : gamma 
whyNotCut ((WhyNot a):(WhyNot b):gamma) = if a == b then WhyNot a : gamma else error "Inequal vars in whyNotCut"

whyNotRuleOne (a:g@((WhyNot _):as)) = (OfCourse a) : g 
whyNotRuleTwo (a:gamma) = WhyNot a : gamma

--from Joshua-Gordon
ls // e = takeWhile (/= e) ls ++ tail (dropWhile (/= e) ls)