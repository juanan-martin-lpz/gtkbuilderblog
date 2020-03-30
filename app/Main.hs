{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Main where

import qualified GI.Gtk as Gtk
import GI.Gtk.Structs
import GI.Gtk.Interfaces

import Data.Maybe (fromJust)
import Data.GI.Base.ShortPrelude
import Data.GI.Base.ManagedPtr (unsafeCastTo)
import Data.GI.Base
import Data.GI.Base.GType
import Data.GI.Base.GValue

import Data.Text

import Control.Monad.IO.Class

-- Funcion para crear una fila para el modelo de la combo
setComboRow :: Gtk.TreeStore -> Text -> IO ()
setComboRow store value = do
    -- Creamos un iterador
    iter <- Gtk.treeStoreAppend store Nothing
    -- Creamos un valor GValue
    gval <- newGValue gtypeString
    -- Asignamos el valor
    set_string gval (Just value)
    -- y lo guardamos en el modelo
    Gtk.treeStoreSetValue store iter 0 gval
        
    return ()

-- Funcion para crear una fila para el modelo de la Lista
setListRow :: Gtk.TreeStore -> Int32 -> Text -> IO ()
setListRow store ivalue value = do
    iter <- Gtk.treeStoreAppend store Nothing

    gvali <- newGValue gtypeInt
    -- No hay equivalente a Int en el API
    set_int gvali $ CInt ivalue

    gval <- newGValue gtypeString
    set_string gval (Just value)
       
    Gtk.treeStoreSetValue store iter 0 gvali
    Gtk.treeStoreSetValue store iter 1 gval
    
    return ()

-- Funcion para crear datos para el modelo de la combo
modelCombobox :: IO Gtk.TreeStore
modelCombobox = do
    store <- Gtk.treeStoreNew [gtypeString]    
    setComboRow store "Uno"
    setComboRow store "Dos"
    setComboRow store "Tres"
    
    return store

-- Funcion para crear datos para el modelo de la Lista
modelListbox :: IO Gtk.TreeStore
modelListbox = do
    -- Vamos a usar dos columnas
    store <- Gtk.treeStoreNew [gtypeInt, gtypeString]    
    setListRow store 1 "Uno"
    setListRow store 2 "Dos"
    setListRow store 3 "Tres"

    return store


main :: IO ()
main = do
    -- inicializamos Gtk
    Gtk.init Nothing

    -- Generamos los datos de ambos modelos
    comboModel <- modelCombobox >>= toTreeModel
    listModel <- modelListbox >>= toTreeModel

    -- Creamos un GtkBuilder y leemos el fichero que hemos creado con Glade
    builder <- Gtk.builderNew
    Gtk.builderAddFromFile builder "ui/form.ui"

    -- Nos hacemos con algunos componentes. builderGetObject devuelve un GObject que hay que castear
    win <- Gtk.builderGetObject builder "window" >>= unsafeCastTo Gtk.Window . fromJust
    salir <- Gtk.builderGetObject builder "salir" >>= unsafeCastTo Gtk.Button . fromJust
    combobox <- Gtk.builderGetObject builder "combo" >>= unsafeCastTo Gtk.ComboBox . fromJust
    listbox <- Gtk.builderGetObject builder "listbox" >>= unsafeCastTo Gtk.TreeView . fromJust
    

    -- Ajustamos el render del Combo

    -- Primero creamos un renderer para Texto
    cell <- Gtk.cellRendererTextNew

    -- Aniadimos los renderers al Layout y le decimos a que propiedad vamos a enlazar el modelo
    cellLayoutPackStart combobox cell True
    cellLayoutAddAttribute combobox cell "text" 0

    -- Seleccionamos la primera fila del modelo
    Gtk.comboBoxSetActive combobox 0

    -- Ajustamos el render de la Lista

    -- Primero creamos los renderers
    cellInt <- Gtk.cellRendererTextNew
    cellText <- Gtk.cellRendererTextNew

    -- Creamos las columnas
    tvcol <- Gtk.treeViewColumnNew

    -- Aniadimos los renderers a las columnas
    Gtk.treeViewColumnPackStart tvcol cellInt True
    Gtk.treeViewColumnPackStart tvcol cellText True
    
    -- Le decimos a que propiedad vamos a enlazar el modelo
    Gtk.treeViewColumnAddAttribute tvcol cellInt "text" 0
    Gtk.treeViewColumnAddAttribute tvcol cellText "text" 1
    
    -- Aniadimos la columna a la Lista
    Gtk.treeViewAppendColumn listbox tvcol
    
    -- Asignamos los modelos
    set combobox [#model := comboModel]
    set listbox [#model := listModel]

    -- Eventos generales
    on win #destroy Gtk.mainQuit
    on salir #clicked Gtk.mainQuit

    -- Mostramos la ventana
    #showAll win
    
    -- Gtk main loop
    Gtk.main

