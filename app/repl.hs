import Foreign.HR.Types
import Foreign.HR.SExpr
import Foreign.HR.Embedded
import Control.Monad (when)
import System.IO
import Foreign.HR.Internals(rNilValue)

run = do
  putStr "hR> "
  hFlush stdout
  line <- getLine
  when(line /= "") $ do
    SExpression [p] <- sCall "parse" [(Just "text", SString [Just line]), (Just "srcfile", SNull)]
    print p
    o <- sCall "eval" [(Nothing, p)]
    print o
    run

main = do
  initialize Nothing
  putStrLn "rNilValue..."
  x <- rNilValue
  print x
  run
  end
