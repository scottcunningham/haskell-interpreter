
I want these language extensions for my syntactic sugaring tricks at the end

> {-# Language MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-} 

I want my own definition of lookup and I want to write my own function
named "print".

> import Prelude hiding (lookup, print)

> import qualified Data.Map as Map
> import Data.Maybe

I want to get at the standard "print" function using the name System.print

> import qualified System.IO as System

I plan to use these monads to construct the parts of my interpreter

> import Control.Monad.Identity
> import Control.Monad.Error
> import Control.Monad.Reader
> import Control.Monad.State
> import Control.Monad.Writer

> import Control.Concurrent

{-------------------------------------------------------------------}
{- The pure expression language                                    -}
{-------------------------------------------------------------------}

> data Val = I Int | B Bool
>            deriving (Eq, Show, Read)

> data Expr = Const Val
>      | Add Expr Expr | Sub Expr Expr  | Mul Expr Expr | Div Expr Expr
>      | And Expr Expr | Or Expr Expr | Not Expr 
>      | Eq Expr Expr | Gt Expr Expr | Lt Expr Expr
>      | Var String
>    deriving (Eq, Show, Read)

> type Name = String 
> type Env = Map.Map Name Val

> lookup k t = case Map.lookup k t of
>                Just x -> return x
>                Nothing -> fail ("Unknown variable "++k)

{-- Monadic style expression evaluator, 
 -- with error handling and Reader monad instance to carry dictionary
 --}

> type Eval a = ReaderT Env (ErrorT String Identity) a 
> runEval env ex = runIdentity ( runErrorT ( runReaderT ex env) )

This evaluator could be a little neater 

Integer typed expressions

> evali op e0 e1 = do e0' <- eval e0
>                     e1' <- eval e1
>                     case (e0', e1') of
>                          (I i0, I i1) -> return $ I (i0 `op` i1)
>                          _            -> fail "type error in arithmetic expression"

Boolean typed expressions

> evalb op e0 e1 = do e0' <- eval e0
>                     e1' <- eval e1
>                     case (e0', e1') of
>                          (B i0, B i1) -> return $ B (i0 `op` i1)
>                          _            -> fail "type error in boolean expression"

Operations over integers which produce booleans

> evalib op e0 e1 = do e0' <- eval e0
>                      e1' <- eval e1
>                      case (e0', e1') of
>                           (I i0, I i1) -> return $ B (i0 `op` i1)
>                           _            -> fail "type error in arithmetic expression"

Evaluate an expression

> eval :: Expr -> Eval Val
> eval (Const v) = return v
> eval (Add e0 e1) = do evali (+) e0 e1
> eval (Sub e0 e1) = do evali (-) e0 e1
> eval (Mul e0 e1) = do evali (*) e0 e1
> eval (Div e0 e1) = do evali div e0 e1

> eval (And e0 e1) = do evalb (&&) e0 e1
> eval (Or e0 e1) = do evalb (||) e0 e1

> eval (Not e0  ) = do evalb (const not) e0 (Const (B True)) 
>                        where not2 a _ = not a -- hack, hack

> eval (Eq e0 e1) = do evalib (==) e0 e1
> eval (Gt e0 e1) = do evalib (>) e0 e1
> eval (Lt e0 e1) = do evalib (<) e0 e1
                        
> eval (Var s) = do env <- ask
>                   lookup s env


{-------------------------------------------------------------------}
{- The statement language                                          -}


> data Statement = Assign String Expr
>                | If Expr Statement Statement
>                | While Expr Statement
>                | Print Expr
>                | Seq Statement Statement
>                | Try Statement Statement
>                | Fork Statement
>                | Input Name 
>                | Pass                    
>       deriving (Eq, Read, Show)


The 'Pass' statement is useful when making Statement an instance of
Monoid later on, we never actually expect to see it in a real program.

> type Run a = StateT Env (ErrorT String IO) a 
> runRun p =  runErrorT ( runStateT p Map.empty) 

> set :: (Name, Val) -> Run ()
> set (s,i) = state $ (\table -> ((), Map.insert s i table))

> exec :: Statement -> Run ()

> exec (Assign s v) = do st <- get  
>                        Right val <- return $ runEval st (eval v)  
>                        set (s,val)

> exec (Seq s0 s1) = do exec s0 >> exec s1

> exec (Print e) = do st <- get
>                     Right val <- return $ runEval st (eval e) 
>                     liftIO $ System.print val
>                     return () 

> exec (Input name) = do
>   liftIO $ System.putStrLn "input>"
>   input <- liftIO $ System.getLine
>   let value = (read input)::Int
>   set (name, (I value))
>   return ()

The transformer libraries define an overloaded "liftIO" operation that passes the required operation along the stack of monads to the next "liftIO" in line until the actual IO monad is reached. In this case it's equivalent to :

 lift . lift . System.IO.print

because we have to pass through StateT and ErrorT to reach the IO monad.


> exec (If cond s0 s1) = do st <- get
>                           Right (B val) <- return $ runEval st (eval cond)
>                           if val then do exec s0 else do exec s1

> exec (While cond s) = do st <- get
>                          Right (B val) <- return $ runEval st (eval cond)
>                          if val then do exec s >> exec (While cond s) else return ()

> exec (Fork s) = do st <- get
>                    liftIO $ forkIO $ do { runErrorT $ (runStateT $ exec $ s) st; return () }
>                    return ()                  

> exec (Try s0 s1) = do catchError (exec s0) (\e -> exec s1)

We never actually expect to encounter one of these, the programs should run fine if we left this equation out:
                        
> exec Pass = return ()

{- Pour some sugar over this -}
{- This next section deals exclusively with defining convenience functions -}
{- which we will use when writing the actual programs in the DSL. -}

A couple of convenience functions so that we don't have to litter the program
with ``obvious'' constructors

> int = Const . I
> bool = Const . B
> var = Var


The idea is that we can put a simple value on the RHS and have Haskell select the correct
instance by inspecting the type.

> class SmartAssignment a where
>   assign :: String -> a -> Statement

> instance SmartAssignment Int where
>   assign v i = Assign v (Const (I i))

> instance SmartAssignment Bool where
>   assign v b = Assign v (Const (B b))

> instance SmartAssignment Expr where
>   assign v e = Assign v e

going further with this (and only providing the classes and instances we actually usein the example program, but there could be others)

> class PrettyExpr a b where
>   (.*) :: a -> b -> Expr
>   (.-) :: a -> b -> Expr


> instance PrettyExpr String String where
>   x .* y = (Var x) `Mul` (Var y)
>   x .- y = (Var x) `Sub` (Var y)

> instance PrettyExpr String Int where
>   x .* y = (Var x) `Mul` (Const (I y))
>   x .- y = (Var x) `Sub` (Const (I y))

Making use of this we can write a program in a slightly nicer style:

I feel we're hitting the point of diminishing returns here, but I
fancy one last example of using a monad. Something to remove the need
to explicitely write "Seq" inbetween each pair of statements. Recall
that I said that >>= could be though of as a progammable semicolon?

> type Program = Writer Statement ()

The writer monad has an operation, "tell" which appends a piece of
output to an accumulated value. For this to work the type we are
accumulating (Statement, in this case) must be have both an appending
(plus-like) operation and a base (zero-like) operation. In algebra
something with that structure is called a Monoid:

> instance Monoid Statement where
>   mempty = Pass
>   mappend a b = a `Seq` b

The idea is that the bind of the Writer monad will be used to apply
Seq (our semicolon-like operation) between each "statement". The zero
statement is needed for theoretical completeness, but it will only
appear in a result if we were to write something like this:

junk :: Program
junk = return ()

For this reason we never expect to see it in a real "compiled"
statement, so there's no case for it in the exec function. 

Converting a Program to a Statement just means running the writer monad:

> compile :: Program -> Statement
> compile p = snd . runIdentity $ runWriterT p

Executing a "program" means compiling it and then running the
resulting Statement with an empty variable map.

runS just runs through a statement:

> runS :: Env -> Statement -> IO ()
> runS env s = do result <- runErrorT $ (runStateT $ exec s) env
>                 case result of
>                   Right ( (), env ) -> do dump env
>                   Left exn -> System.print ("Uncaught exception: "++exn)

Dump folds the Env into a single printable string with each variable on a new line in the form "name : value"

> dump :: Env -> IO ()
> dump env = do
>             System.hPutStrLn System.stderr $ Map.foldrWithKey (\ k a acc -> acc ++ (show k) ++ " : " ++ (show a) ++ "\n") "\nVar dump:\n" env

 
And run just compiles a program into a Statement and uses runS:

> run :: Env -> Program -> IO ()
> run env p = runS env $ compile p

And finally some convenience functions for our syntactic sugar:

> infixl 1 .=
> (.=) :: String -> Expr -> Program 
> var .= val = tell $ assign var val


if is a keyword in Haskell so I can't hide it. I renamed it so: 

> iif :: Expr -> Program -> Program -> Program
> iif cond tthen eelse = tell $ If cond (compile tthen) (compile eelse)

> while :: Expr -> Program -> Program
> while cond body = tell $ While cond (compile body)

This is why I wanted to hide the system function "print":

> print :: Expr -> Program
> print e = tell $ Print e

> try :: Program -> Program -> Program
> try block recover = tell $ Try (compile block) (compile recover)

> input :: Name -> Program
> input var = tell $ Input (var)

Phew.

After all that our embedded imperative language is ready to go. Here's the factorial function in all it's glory:

The program now gets the factorial of a value taken from user input!

> prog10 :: Program
> prog10 = do
>            "arg"     .= int 0
>            input "arg"
>            "scratch" .= var "arg"
>            "total"   .= int 1
>            while ( (var "scratch") `Gt` (int 1) ) (
>             do "total"   .=  "total" .* "scratch"
>                "scratch" .= "scratch" .- (1::Int)
>                print $ var "scratch" 
>             )
>            print $ var "total"

Since the let clause in this function is a little involved, an explanation:
    - Opens a file
    - Splits it line-by-line
    - Parses each line into a Statement
    - Recursively turns this list into a recursive Statement with Seq
    - Runs the result using runS above

> interpret :: String -> IO () 
> interpret fn = do
>     file <- liftIO $ readFile fn
>     let s = foldl (\ s1 s2 -> Seq s1 s2) Pass $ map (\ s -> (read s)::Statement) $ lines file
>     runS Map.empty s 

> main = interpret "in.gs"
