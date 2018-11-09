{-# LANGUAGE OverloadedStrings #-}

module Options
    ( ImageFormat(..)
    , Options
    , imageFormat
    , parse
    ) where

import Control.Applicative ((<|>))
import Turtle.Options (Parser, optRead, options)

newtype Options = Options
    { imageFormat :: ImageFormat
    -- TODO maven profiles to enable when running maven command
    -- TODO Xmx for maven
    }

data ImageFormat
    = PNG
    | SVG
    deriving Read

parse :: IO Options
parse =
    options "Maven Parent POM hierarchy analyzer" parser

parser :: Parser Options
parser =
    Options <$> imageFormatParser

imageFormatParser :: Parser ImageFormat
imageFormatParser =
    optRead "format" 'f' "Output format of parent hierachy image. Supported values: PNG (Default), SVG"
    <|> pure PNG
