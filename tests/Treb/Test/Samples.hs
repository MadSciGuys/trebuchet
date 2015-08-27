{-# LANGUAGE OverloadedStrings #-}
module Treb.Test.Samples where

import Data.Text (Text)
import qualified Codec.MIME.Type as MIME

adjectives :: [Text]
adjectives =
  [ "Synergistic"
  , "Quantabulous"
  , "Stupifying" ]

productNames :: [Text]
productNames =
  [ "Gainer"
  , "Remicade"
  , "Tylenol" ]

datablockNames :: [Text]
datablockNames =
  [ "Spectrogram"
  , "Chromatogram" ]

modelNames :: [Text]
modelNames =
  [ "MVA"
  , "Proto-Analysis" ]

recipeNames :: [Text]
recipeNames =
  [ "Chocolate Cake"
  , "Vanilla Cake" ]

elementNames :: [Text]
elementNames =
  [ "Hydrogen"
  , "Helium"
  , "Lithium"
  , "Beryllium"
  , "Boron"
  , "Carbon"
  , "Nitrogen"
  , "Oxygen"
  , "Fluorine"
  , "Neon"
  , "Sodium"
  , "Magnesium"
  , "Aluminum, Aluminium"
  , "Silicon"
  , "Phosphorus"
  , "Sulfur"
  , "Chlorine"
  , "Argon"
  , "Potassium"
  , "Calcium"
  , "Scandium"
  , "Titanium"
  , "Vanadium"
  , "Chromium"
  , "Manganese"
  , "Iron"
  , "Cobalt"
  , "Nickel"
  , "Copper"
  , "Zinc"
  , "Gallium"
  , "Germanium"
  , "Arsenic"
  , "Selenium"
  , "Bromine"
  , "Krypton"
  , "Rubidium"
  , "Strontium"
  , "Yttrium"
  , "Zirconium"
  , "Niobium"
  , "Molybdenum"
  , "Technetium"
  , "Ruthenium"
  , "Rhodium"
  , "Palladium"
  , "Silver"
  , "Cadmium"
  , "Indium"
  , "Tin"
  , "Antimony"
  , "Tellurium"
  , "Iodine"
  , "Xenon"
  , "Cesium"
  , "Barium"
  , "Lanthanum"
  , "Cerium"
  , "Praseodymium"
  , "Neodymium"
  , "Promethium"
  , "Samarium"
  , "Europium"
  , "Gadolinium"
  , "Terbium"
  , "Dysprosium"
  , "Holmium"
  , "Erbium"
  , "Thulium"
  , "Ytterbium"
  , "Lutetium"
  , "Hafnium"
  , "Tantalum"
  , "Tungsten"
  , "Rhenium"
  , "Osmium"
  , "Iridium"
  , "Platinum"
  , "Gold"
  , "Mercury"
  , "Thallium"
  , "Lead"
  , "Bismuth"
  , "Polonium"
  , "Astatine"
  , "Radon"
  , "Francium"
  , "Radium"
  , "Actinium"
  , "Thorium"
  , "Protactinium"
  , "Uranium"
  , "Neptunium"
  , "Plutonium"
  , "Americium"
  , "Curium"
  , "Berkelium"
  , "Californium"
  , "Einsteinium"
  , "Fermium"
  , "Mendelevium"
  , "Nobelium"
  , "Lawrencium"
  , "Rutherfordium"
  , "Dubnium"
  , "Seaborgium"
  , "Bohrium"
  , "Hassium"
  , "Meitnerium"
  , "Darmstadtium"
  , "Roentgenium"
  , "Copernicium"
  , "Ununtrium"
  , "Flerovium"
  , "Ununpentium"
  , "Livermorium"
  , "Ununseptium"
  , "Ununoctium" ]

datablockFieldNames :: [Text]
datablockFieldNames = [ "GMP", "GTP", "START_TIME", "END_TIME", "Barcode", "Lot", "Storage Time", "Storage Conditions", "A280", "Purity", "pH", "Monomer", "VAULTID", "AUTONAME", "SITE", "PHASE", "Product" ]

mimeTypes :: [MIME.Type]
mimeTypes =
  [ MIME.Type (MIME.Application a) [] | a <- ["pdf"] ] ++
  [ MIME.Type (MIME.Audio a) [] | a <- ["mpeg", "mpeg3"] ] ++
  [ MIME.Type (MIME.Image i) [] | i <- ["png", "jpeg", "gif"] ]
