import           TigerAbs
import           TigerSymbol
import           TigerTips
import           TigerEscap  (calcularEEsc)
import           TigerParser (parse)
import           TigerSeman (calcularSeman)
import           TigerTrans (BExp)
import           TigerPrettyIr (renderBIr)

import           Tools

main :: IO ()
main =
  -- testGood "./test/test_code/" tester "merge.tig"

  putStrLn "\n==== Good loc ====" >>
  testDir good_loc (testIr good_loc tester)

testIr :: Show a => String -> (String -> Either a (BExp, Tipo)) -> String -> IO ()
testIr loc = test loc ( badRes . show )
                      ( goodRes . renderBIr . fst)

tester :: String -> Either Symbol (BExp, Tipo)
tester = either (\s -> Left s)
                calcularSeman
         .  (either (fail $ "Testing Escapes: Parser error")
                   calcularEEsc
            . parse )

preprocessIr :: Either Symbol (BExp, Tipo) -> Either Symbol String
preprocessIr (Left s) = Left s
preprocessIr (Right (bexp, _)) = Right (renderBIr bexp)