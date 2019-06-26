import           TigerAbs
import           TigerSymbol
import           TigerTips
import           TigerEscap  (calcularEEsc)
import           TigerParser (parse)
import           TigerSeman (calcularSeman)

import           Tools

main :: IO ()
main =
  -- putStrLn "\n==== [escapa.tig, intro.tig] ====" >>
  -- test "./test/test_code" (const bluefail) (const rednice) tester "escapa.tig" >>
  -- test "./test/test_code" (const redfail) (const bluenice) tester "intro.tig" >>
  putStrLn "\n==== Good loc ====" >>
  -- testGood "./test/test_code/good" tester "merge.tig"
  testDir good_loc (testSTDGood tester) >>
  putStrLn "\n==== Type Loc ====" >>
  testDir type_loc (testGood type_loc tester)
  -- putStrLn "\n======= Test ESCAPES FIN ======="


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