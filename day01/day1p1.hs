#!/usr/bin/env stack
-- stack --resolver lts-18.18 script

main :: IO ()
main = interact $ show . sum . map ((\as -> read ([head as, last as])) . filter (`elem` ['0'..'9'])) . lines
