{-# LANGUAGE RecordWildCards #-}

import Data.Foldable (for_)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Transpose (transpose)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "transpose" $ for_ cases test
  where
    test Case {..} = it description assertion
      where
        assertion = transpose rows `shouldBe` expected

data Case
  = Case
      { description :: String,
        rows :: [String],
        expected :: [String]
      }

cases :: [Case]
cases =
  [ Case
      { description = "empty string",
        rows = [],
        expected = []
      },
    Case
      { description = "two characters in a row",
        rows = ["A1"],
        expected =
          [ "A",
            "1"
          ]
      },
    Case
      { description = "two characters in a column",
        rows =
          [ "A",
            "1"
          ],
        expected = ["A1"]
      },
    Case
      { description = "simple",
        rows =
          [ "ABC",
            "123"
          ],
        expected =
          [ "A1",
            "B2",
            "C3"
          ]
      },
    Case
      { description = "single line",
        rows = ["Single line."],
        expected =
          [ "S",
            "i",
            "n",
            "g",
            "l",
            "e",
            " ",
            "l",
            "i",
            "n",
            "e",
            "."
          ]
      },
    Case
      { description = "mixed line length",
        rows =
          [ "The longest line.",
            "A long line.",
            "A longer line.",
            "A line."
          ],
        expected =
          [ "TAAA",
            "h   ",
            "elll",
            " ooi",
            "lnnn",
            "ogge",
            "n e.",
            "glr",
            "ei ",
            "snl",
            "tei",
            " .n",
            "l e",
            "i .",
            "n",
            "e",
            "."
          ]
      },
    Case
      { description = "square",
        rows =
          [ "HEART",
            "EMBER",
            "ABUSE",
            "RESIN",
            "TREND"
          ],
        expected =
          [ "HEART",
            "EMBER",
            "ABUSE",
            "RESIN",
            "TREND"
          ]
      },
    Case
      { description = "rectangle",
        rows =
          [ "FRACTURE",
            "OUTLINED",
            "BLOOMING",
            "SEPTETTE"
          ],
        expected =
          [ "FOBS",
            "RULE",
            "ATOP",
            "CLOT",
            "TIME",
            "UNIT",
            "RENT",
            "EDGE"
          ]
      },
    Case
      { description = "triangle",
        rows =
          [ "T",
            "EE",
            "AAA",
            "SSSS",
            "EEEEE",
            "RRRRRR"
          ],
        expected =
          [ "TEASER",
            " EASER",
            "  ASER",
            "   SER",
            "    ER",
            "     R"
          ]
      }
  ]
-- 476e7b6f12efa841bd69756dea775a6ea43a9a33
