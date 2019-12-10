import           TigerAbs
import           TigerSymbol
import           TigerTips
import           TigerEscap  (calcularEEsc)
import           TigerParser (parse)
import           TigerSeman (calcularSeman, calcularEstadoSeman, Estado)
import           TigerTrans (BExp)
import           TigerPrettyIr (renderBIr)

import           Tools

main :: IO ()
main =
  -- testGood "./test/test_code/" testEstado "merge.tig"

  putStrLn "\n==== Good loc (Estado) ====" >>
  -- testDir good_loc (testIr good_loc tester)
  testDir working_loc (testEstadoIr good_loc testerEstado) >>

  putStrLn "\n==== Good loc ====" >>
  testDir working_loc (testIr good_loc tester)

testIr :: Show a => String -> (String -> Either a (BExp, Tipo)) -> String -> IO ()
testIr loc = test loc ( badRes . show )
                      ( goodRes . renderBIr . fst)

tester :: String -> Either Symbol (BExp, Tipo)
tester = either (\s -> Left s)
                calcularSeman
         .  (either (fail $ "Testing Escapes: Parser error")
                   calcularEEsc
            . parse )

-- preprocessIr :: Either Symbol (BExp, Tipo) -> Either Symbol String
-- preprocessIr (Left s) = Left s
-- preprocessIr (Right (bexp, _)) = Right (renderBIr bexp)

testerEstado :: String -> Either Symbol Estado
testerEstado = either (\s -> Left s)
                (Right . calcularEstadoSeman)
         .  (either (fail $ "Testing Escapes: Parser error")
                   calcularEEsc
            . parse )

testEstadoIr :: Show a => String -> (String -> Either a Estado) -> String -> IO ()
testEstadoIr loc = test loc ( badRes . show ) (goodRes . show)