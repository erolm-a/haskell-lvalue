{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Control.LValue where

import           Data.IORef                     ( IORef
                                                , newIORef
                                                , readIORef
                                                , writeIORef
                                                )
import           Data.Array
import           Data.Array.IO
import           Data.Array.MArray

import qualified Prelude                       as P
import           Prelude                        ( pure
                                                , (>>=)
                                                )

-- Handly alias
type IO = P.IO
type Bool = P.Bool
return = P.return

(<$>) = (P.<$>)
(<*>) = (P.<*>)

infixr 0 $
($) = (P.$)

data RValue
data LValue -- unused

type Getter a = IO a
type Setter a = a -> IO ()

data Expr' v a where
    -- An Expr can only yield values, thus it's a RValue
    Expr ::Getter a -> Expr' RValue a
    Var ::Getter a -> Setter a -> Expr' v a

-- Execute an expression.
runExpression :: Expr' v a -> IO a
runExpression (Expr t ) = t
runExpression (Var t _) = t

lvalueFromExpr :: Expr' v a -> IO (Expr' v a)
lvalueFromExpr expr = do
  value <- runExpression expr
  ref   <- newIORef value
  return (Var (readIORef ref) (writeIORef ref))

lvalueFromNaked :: a -> IO (Expr' v a)
lvalueFromNaked value = do
  ref <- newIORef value
  return (Var (readIORef ref) (writeIORef ref))

-- |A Java-like way of instantiating variables is with the new operator
new = lvalueFromNaked

-- |Create a new lvalue given another expression. Similar to C++ copy
-- |constructor
copy = lvalueFromExpr

-- |Create an rvalue from a naked value.
constant :: a -> Expr' RValue a
constant value = Expr $ return value

-- |Sum two expressions into one expression. The result is a rvalue.
-- |If you need to combine a sum (+) and an assignment (=:) use (+=)
infixl 6 +
e1 + e2 = Expr $ (P.+) <$> runExpression e1 <*> runExpression e2

-- |Sum an expression to a simple value. This is useful when a user
-- |wants to sum a literal to a *value without the latter being an *value.
-- |For example, y <- copy $ x + 3
infixl 6 +.
e1 +. v = Expr $ (P.+ v) <$> runExpression e1

-- |Sum two expressions into one expression. The result is a rvalue.
-- |If you need to combine a sum (+) and an assignment (=:) use (+=)
infixl 6 -
e1 - e2 = Expr $ (P.-) <$> runExpression e1 <*> runExpression e2

infixl 6 -.
e1 -. v = Expr $ (P.- v) <$> runExpression e1

infixl 7 *
e1 * e2 = Expr $ (P.*) <$> runExpression e1 <*> runExpression e2

infixl 7 *.
e1 *. v = Expr $ (P.* v) <$> runExpression e1

negate e = Expr $ (P.negate) <$> runExpression e

abs e = Expr $ (P.abs) <$> runExpression e

signum e = Expr $ (P.signum) <$> runExpression e

infix 3 &&
(&&) :: Expr' v Bool -> Expr' v Bool -> Expr' RValue Bool
expr1 && expr2 = Expr $ (P.&&) <$> runExpression expr1 <*> runExpression expr2

and = (&&)

infix 2 ||
(||) :: Expr' v Bool -> Expr' v Bool -> Expr' RValue Bool
expr1 || expr2 = Expr $ (P.||) <$> runExpression expr1 <*> runExpression expr2

or = (||)

not :: Expr' v Bool -> Expr' RValue Bool
not expr = Expr $ (P.not) <$> runExpression expr

infix 4 >
(>) :: P.Ord a => Expr' v a -> Expr' v a -> Expr' RValue Bool
expr1 > expr2 = Expr $ (P.>) <$> runExpression expr1 <*> runExpression expr2

infix 4 >.
expr1 >. value = expr1 > (constant value)

infix 4 >=
(>=) :: P.Ord a => Expr' v a -> Expr' v a -> Expr' RValue Bool
expr1 >= expr2 = Expr $ (P.>=) <$> runExpression expr1 <*> runExpression expr2

infix 4 >=.
expr1 >=. value = expr1 >= (constant value)

infix 4 <
(<) :: P.Ord a => Expr' v a -> Expr' v a -> Expr' RValue Bool
expr1 < expr2 = not $ expr1 >= expr2

infix 4 <.
expr1 <. value = expr1 < (constant value)

infix 4 <=
(<=) :: P.Ord a => Expr' v a -> Expr' v a -> Expr' RValue Bool
expr1 <= expr2 = not $ expr1 > expr2

infix 4 <=.
expr1 <=. value = expr1 <= (constant value)

-- |Assign. Note that, quite differently from C, assignments cannot be nested.
-- |Thus, something like a =: b =: c =: etc. is illegal
infixl 0 =:
(=:) :: Expr' v a -> Expr' RValue a -> IO ()
Var _ setter =: e = runExpression e >>= setter

-- |Add-and-assign operation
infix 0 +=
var@(Var _ setter) += e = runExpression (var + e) >>= setter

-- |Subtract-and-assign operation
infix 0 -=
var@(Var _ setter) -= e = runExpression (var - e) >>= setter

-- |Multiply-and-assign operation
infix 0 *=
var@(Var getter setter) *= e = runExpression (var * e) >>= setter

-- |Swap two lvalues.
swap :: Expr' v a -> Expr' v a -> IO ()
var1@(Var getter1 setter1) `swap` var2@(Var getter2 setter2) = do
  t <- runExpression var1
  getter2 >>= setter1
  setter1 t

-- |Create a 1-D array. An array is neither a lvalue nor a rvalue,
-- |But a "lvalue-generator".
-- |The basic idea is that after indexing an array we *get* a lvalue,
-- |But arrays here are not first class citizens. To extract one, use
-- |`takeArray`
-- |For example:
-- | do { myarr <- arr[10]; arr[0] =: new 1; arr[1] =: arr[0] +. 2; }
arr :: [Expr' v P.Int] -> IO ([Expr' v P.Int] -> Expr' v a)
arr index = do
  let extractIdx [idx] = runExpression idx
  index' <- extractIdx index
  array  <- newArray (0, index' P.- 1) P.undefined :: IO (IOArray P.Int a)
  return
    (\newIndex -> Var
      (extractIdx newIndex >>= readArray array)
      (\value ->
        extractIdx newIndex >>= \index'' -> writeArray array index'' value
      )
    )


