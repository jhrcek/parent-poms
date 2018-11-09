{-# LANGUAGE OverloadedStrings #-}

module Options
    ( ImageFormat(..)
    , Options
    , imageFormat
    , parse
    , userHome
    ) where

import Control.Applicative ((<|>))
import Prelude hiding (FilePath)
import Turtle (FilePath, home)
import Turtle.Options (Parser, optRead, options)

data Options = Options
    { userHome    :: FilePath
    , imageFormat :: ImageFormat
    -- TODO maven profiles to enable when running maven command
    -- TODO Xmx for maven
    }

data ImageFormat
    = PNG
    | SVG
    deriving Read

parse :: IO Options
parse = do
    homeDir <- home
    options "Maven Parent POM hierarchy analyzer" $ parser homeDir

parser :: FilePath -> Parser Options
parser homeDir =
    Options homeDir <$> imageFormatParser

imageFormatParser :: Parser ImageFormat
imageFormatParser =
    optRead "format" 'f' "Output format of parent hierachy image. Supported values: PNG (Default), SVG"
    <|> pure PNG
