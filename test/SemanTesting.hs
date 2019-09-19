import           TigerAbs
import           TigerSymbol
import           TigerTips
import           TigerEscap  (calcularEEsc)
import           TigerParser (parse)
import           TigerSeman (calcularSeman)
import           TigerTrans (BExp)

import           Tools

main :: IO ()
main =
  -- test "./test/test_code/good" (const redfail) (const bluenice) tester "test44.tig"
  putStrLn "\n==== Orden ====" >>
  testDir good_loc (testSTDGood tester2) >>
  putStrLn "\n==== Good loc ====" >>
  testDir good_loc (testSTDGood tester) >>
  putStrLn "\n==== Type Loc ====" >>
  testDir type_loc (testBad type_loc tester)


-- tester :: String -> Either Symbol Exp
-- tester = either (fail $ "Testing Escapes: Parser error")
--                 calcularEEsc
--          . parse
tester :: String -> Either Symbol (BExp, Tipo)
tester = either (\s -> Left s)
                 calcularSeman
         .  (either (fail $ "Testing Escapes: Parser error")
                   calcularEEsc
            . parse )
tester2 :: String -> Either Symbol Exp
tester2 = either (fail $ "Testing Escapes: Parser error")
                calcularEEsc
         . parse