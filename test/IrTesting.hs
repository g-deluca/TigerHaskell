import           TigerAbs
import           TigerEscap    (calcularEEsc)
import           TigerFrame    (Frag)
import           TigerParser   (parse)
import           TigerPrettyIr (renderBIr, renderFrag)
import           TigerSeman    (Estado, calcularEstadoSeman, calcularFrags,
                                calcularSeman)
import           TigerSymbol
import           TigerTips
import           TigerTrans    (BExp)

import           Data.List

import           Tools

main :: IO ()
main
  -- testGood "./test/test_code/" testEstado "merge.tig"
 =
  putStrLn "\n==== Good loc (Estado) ====" >>
  -- testDir good_loc (testIr good_loc tester)
  testDir working_loc (testFragsIr working_loc testerFrags)

--   putStrLn "\n==== Good loc ====" >>
--   testDir working_loc (testIr good_loc tester)
testIr ::
     Show a => String -> (String -> Either a (BExp, Tipo)) -> String -> IO ()
testIr loc = test loc (badRes . show) (goodRes . renderBIr . fst)

tester :: String -> Either Symbol (BExp, Tipo)
tester =
  either (\s -> Left s) calcularSeman .
  (either (fail $ "Testing Escapes: Parser error") calcularEEsc . parse)

-- preprocessIr :: Either Symbol (BExp, Tipo) -> Either Symbol String
-- preprocessIr (Left s) = Left s
-- preprocessIr (Right (bexp, _)) = Right (renderBIr bexp)
testerEstado :: String -> Either Symbol Estado
testerEstado =
  either (\s -> Left s) (Right . calcularEstadoSeman) .
  (either (fail $ "Testing Escapes: Parser error") calcularEEsc . parse)

testEstadoIr ::
     Show a => String -> (String -> Either a Estado) -> String -> IO ()
testEstadoIr loc = test loc (badRes . show) (goodRes . show)

testerFrags :: String -> Either Symbol [Frag]
testerFrags =
  either (\s -> Left s) calcularFrags .
  (either (fail $ "Testing Escapes: Parser error") calcularEEsc . parse)

testFragsIr ::
     Show a => String -> (String -> Either a [Frag]) -> String -> IO ()
testFragsIr loc = test loc (badRes . show) (goodRes . unlines . map renderFrag)
