import           TigerAbs
import           TigerSymbol
import           TigerTips
import           TigerEscap  (calcularEEsc)
import           TigerParser (parse)
import           TigerSeman (calcularSeman)

import           Tools

main :: IO ()
main =
  putStrLn "\n==== Good loc ====" >>
  testDir good_loc (testSTDGood tester) >>
  putStrLn "\n==== Type Loc ====" >>
  testDir type_loc (testBad type_loc tester)


-- tester :: String -> Either Symbol Exp
-- tester = either (fail $ "Testing Escapes: Parser error")
--                 calcularEEsc
--          . parse
tester :: String -> Either Symbol ((), Tipo)
tester = either (\s -> Left s)
                 calcularSeman
         .  (either (fail $ "Testing Escapes: Parser error")
                   calcularEEsc
            . parse )