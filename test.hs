{-# LANGUAGE NoImplicitPrelude#-}

import LValue as LV

import Prelude as P

main = do
    x <- new 3
    y <- copy x
    y =: x * y
    runExpression y P.>>= \val -> putStrLn P.$ "Now y contains " ++ show val

    -- (x + y) =: new 42 -- Error: rvalues are not assignable
    let z = constant 42
    x += z
    y += z
    -- z += z -- Error: constants are rvalues, thus not assignable
    runExpression y P.>>= \val -> putStrLn P.$ "Now y contains " ++ show val
